{- |
    Module      :  $Header$
    Description :  Main module

    Command line tool for checking programming style of Curry files.
-}
module CASC (main) where

import AnsiCodes    (blue, green, red, yellow)
import Directory    (doesDirectoryExist, getDirectoryContents)
import Distribution
import FilePath
import IO           (hPutStrLn, hIsTerminalDevice, stdout, stderr)
import List         ((\\), intercalate, last, isSuffixOf)
import Maybe        (isJust)
import ReadShowTerm (readUnqualifiedTerm, readQTermFile)
import System       (exitWith, getProgName, system)

import AST.AddSpans            (apModule)
import AST.SpanAST             (Module)
import AST.SortSplit           (sortSplitModule)
import AutoCorr.AutoCorrPosAST (correctModule)
import Check.CheckPosAST       (checkModule)
import Check.LineLength        (checkLine)
import Config.ReadConfig
import Config.Types            (Check (CLineLength))
import Opts
import AST.RemoveSpans         (rsModule)
import Utils


-- |Get options from command line and pass them to main function `casc`
main :: IO ()
main = getCheckOpts >>= casc "casc"

-------------------------------------------------------------------------------
--- Main function of tool casc. Checks Mode and takes appropriate actions.
--- @param prog  - name of program
--- @param opts  - options which have been read in from command line
--- @param files - files or directories which are to be processed
--- @param errs  - errors that happend while reading  args from command line

casc :: String -> (Options, [String], [String]) -> IO ()
casc prog (opts, files, errs)
  | mode == ModeHelp    = printUsage prog
  | mode == ModeVersion = printVersion
  | null files          = badUsage prog ["no input files"]
  | mode == ModeCheck   = do fL <- mapIO (subFiles (optRecursive opts)) files
                             isTerminal <- hIsTerminalDevice stdout
                             checkAll (concat fL) (verb, col isTerminal)
  | not $ null errs     = badUsage prog errs
  | otherwise           = error "casc: no option"
  where
    mode = optMode      opts
    verb = optVerbosity opts
    col isterm = case optColor opts of
                   ColOn   -> True
                   ColOff  -> False
                   ColAuto -> isterm

    subFiles :: Bool -> String -> IO [String]
    subFiles rec f = do
      isdir <- doesDirectoryExist f
      if isdir
        then if rec
               then do
                 fs <- dirFiles f
                 submods <- mapIO (subFiles rec) fs >>= return . concat
                 return (map (intercalate "." . splitDirectories) submods)
               else curryFiles f
        else do
          let fs = stripCurrySuffix f
          mbsrc <- lookupModuleSourceInLoadPath fs
          if isJust mbsrc
            then return [fs]
            else error $
                   "Curry program or directory `" ++ f ++ "' does not exist."

type CallOpts = (Verbosity, Bool)

-------------------------------------------------------------------------------
--- Initiate checking procedure.
--- @param files - all files from invocation of `casc`
--- @param verb  - option: verbosity - if "quiet", don't print status

checkAll :: [String] -> CallOpts -> IO()
checkAll []          _    = putStrLn "No programs found, nothing to be done."
checkAll files@(_:_) (verb, col) = do

  let c = red "C" ++ yellow "A" ++ green "S" ++ blue "C"
  if col
    then wStatus verb
                $ "\nWelcome to " ++ c
               ++ ", the " ++ red "C" ++ "urry "
               ++ yellow "A" ++ "utomatic "
               ++ green "S" ++ "tyle "
               ++ blue "C" ++ "hecker.\n\n"
               ++ "The following modules will be checked:\n"
               ++ intercalate " " files ++ "\n"
    else wStatus verb
                  $ "\nWelcome to CASC, the Curry Automatic Style Checker.\n\n"
                 ++ "The following modules will be checked:\n"
                 ++ intercalate ", " files ++ "\n"

  -- Pass programs along to checkOne, but only if there are features to check
  when (not $ null checkList) $ mapIO_  (checkOne (verb, col)) files

-------------------------------------------------------------------------------
--- Execute checking procedure for one file.
--- @param file - the file to be checked
--- @param verb - option: verbosity - if "quiet", don't print status

checkOne :: CallOpts -> String -> IO ()
checkOne (verb, col) file = do
  wStatusLn verb $ "\nProcessing program `" ++ file ++ "'..."
  let progname = stripCurrySuffix file
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing            -> error $ "Source file not found!"
    Just (dir,srcfile) ->
      catch (checkOneSourceFile (verb, col) progname dir srcfile)
            (\err -> hPutStrLn stderr $ "Internal error: " ++ showError err)

checkOneSourceFile :: CallOpts -> String -> String -> String -> IO ()
checkOneSourceFile (verb, col) progname dir srcfile = do

  -- Invoke front end
  let options = addTarget TOKS (setQuiet True defaultParams)
  callFrontendWithParams CY options progname

  -- Retrieve token stream
  let tokenFile = inCurrySubdir (dir </> takeFileName progname) <.> "tokens"
  toks <- liftIO filterTS (readQTermFile tokenFile)
  wDebug verb $ "Tokens: " ++ show toks

  -- Retrieve abstract syntax tree in an order synchronous with tokenstream
  let astFile = inCurrySubdir (dir </> takeFileName progname) <.> "cy"
  astStr <- readFile astFile
  let ast = sortSplitModule $ readUnqualifiedTerm ["Prelude", "AST.AST"] astStr
  wDebug verb $ "AST: " ++ show ast

  -- Extend AST with positions
  wStatus verb "Extending abstract syntax tree... "
  let posAST = either error id $ fst $ apModule ast toks

  wDebug verb $ "Extended AST: " ++ show posAST

  -- Retrieve source file
  src <- readFile srcfile

  -- Check whether structures in active checks are valid
  wStatusLn verb "Checking... "
  let msgs         = checkModule posAST
                     ++ condMsg (shallCheck CLineLength) (checkLine src)
      msgsNotEmpty = (not $ null msgs)
  when (not $ null msgs) $ do
    when (verb == VerbQuiet) $ putStrLn $ "Program: " ++ progname
    putStrLn $ prettyMsg col msgs
  wStatusLn verb "Done."

  -- Initiate autocorrection if parameter in config file is set to "yes"
  let corrNotEmpty = (not $ null corrList)
      shallCorr    = (autoCorr == "yes" || autoCorr == "y")
      corrActive   = corrNotEmpty && msgsNotEmpty && shallCorr
  when corrActive $ correctOne (progname <.> "curry") posAST verb

-- |Autocorrection for one file.
-- |Overwrite .curry/file.cy with corrected AST.
-- |Later, the .curry file itself should be overwritten by the exaxtly
-- |pretty printed corrected PosAST.
correctOne :: String -> Module -> Verbosity -> IO ()
correctOne file posAST verb =
  do let corrOut   = ".curry" </> replaceExtension file ".cy"
         corrAst   = correctModule posAST
         rpAst     = rsModule corrAst
     when (verb == VerbDebug)
          (putStrLn $ "Corrected PosAST: " ++ show corrAst)
     when (verb == VerbDebug)
          (putStrLn $ "With removed positions: " ++ show rpAst)
     writeFile corrOut $ show rpAst
     when (verb == VerbStatus)
          (putStrLn $ "Corrections have been applied to AST file, " ++
                      "but source file still unchanged.")

-- |Take a path. If it ends with a ".curry"-file, drop the file name.
pathName :: String -> String
pathName d = if (takeExtension d == ".curry")
               then dropFileName d
               else d

-- |Get list of .curry files in a given directory.
curryFiles :: String -> IO [String]
curryFiles dirpath = dirFiles dirpath >>= return . filter (isSuffixOf ".curry")

-- |Get list of directory contents and combine each file name to a path
-- |while filtering unneeded entries
dirFiles :: String -> IO [String]
dirFiles dirpath = do
  dcont <- getDirectoryContents dirpath >>= return . (\\ [".", "..", ".curry"])
  cont <- mapIO (\f -> curryOrDir f >>= \b -> return $ if b then [f] else [])
                dcont >>= return . concat
  return $ if dirpath == "." then cont else map (combine dirpath) cont
 where
   curryOrDir f = if ".curry" `isSuffixOf` f
                    then return True
                    else doesDirectoryExist (combine dirpath f)

-- |Print usage message
help :: IO ()
help = do
  putStr "Usage: " >> getProgName >>= putStr >> putStrLn " COMMAND [OPTIONS]"
  putStrLn "  Curry Style Checker"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  help"
  exitWith 0

-- |Print the usage information of the command line tool
printUsage :: String -> IO ()
printUsage prog = putStrLn $ cascTitle ++ "\n\n" ++ usage prog

-- |Print the program version
printVersion :: IO ()
printVersion = putStrLn $ "This is " ++ cascTitle

-- |Show a greeting of CASC
cascTitle :: String
cascTitle = "CASC (Curry Automatic Style Checker), version " ++ cascVersion

-- |Number of current casc version
cascVersion :: String
cascVersion = "1.0 of 29/08/2016"

-- |Print errors and abort execution on bad parameters
badUsage :: String -> [String] -> IO ()
badUsage prog errs = do
  putStrsLn $ map (\ err -> prog ++ ": " ++ err) errs
  error $ "Try 'curry style --help' for more information"
