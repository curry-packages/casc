{- |
    Module      :  $Header$
    Description :  Casc options

    This module defines data structures holding options for casc
    and utility functions for parsing the command line arguments.
-}
module Opts where

import GetOpt
import List   (maximum, intercalate)
import System (getArgs, getProgName)


-- |Retrieve the 'Options'
getCheckOpts :: IO (Options, [String], [String])
getCheckOpts = do
  args <- getArgs
  let (opts, files, errs) = parseOpts args
  return (opts, files, errs)

-- -----------------------------------------------------------------------------
-- Options
-- -----------------------------------------------------------------------------

-- |Casc options
data Options = Options
  { optMode       :: Mode       -- ^ Modus operandi
  , optVerbosity  :: Verbosity  -- ^ Verbosity level
  , optRecursive  :: Bool       -- ^ Check hierarchical modules in subdirs?
  , optColor      :: ColMode
  }

data ColMode
  = ColOn
  | ColOff
  | ColAuto

-- |Modus operandi of the program
data Mode
  = ModeHelp    -- ^ Show help information and exit
  | ModeVersion -- ^ Show version and exit
  | ModeCheck   -- ^ Check files

-- |Verbosity level
data Verbosity
  = VerbQuiet  -- ^ be quiet
  | VerbStatus -- ^ show status of check
  | VerbDebug  -- ^ show debug options

-- |All available options
options :: [OptDescr (OptErr -> OptErr)]
options =
  [ Option "h" ["help"]
      (NoArg (onOpts $ \ opts -> opts { optMode = ModeHelp }))
      "display this help and exit"
  , Option "V" ["version"]
      (NoArg (onOpts $ \ opts -> opts { optMode = ModeVersion }))
      "show the version number and exit"
  , mkOptDescr onOpts "v" ["verbosity"] "n" "verbosity level" verbDescriptions
  , Option "q"  ["quiet"]
      (NoArg (onOpts $ \ opts -> opts { optVerbosity = VerbQuiet } ))
      "set verbosity level to quiet"
  , Option "d" ["debug"]
      (NoArg (onOpts $ \ opts -> opts { optVerbosity = VerbDebug } ))
      "set verbosity level to debug"
  , Option "r" ["recursive"]
      (NoArg (onOpts $ \ opts -> opts { optRecursive = True }))
      "recursively check hierarchical modules in subdirectories"
  , mkOptDescr onOpts "c" ["color"] "n" "color level" colDescriptions
  ]

colors :: [(ColMode, String, String)]
colors = [ ( ColOff,  "0", "off"  )
         , ( ColOn,   "1", "on"   )
         , ( ColAuto, "2", "auto" )
         ]

-- |Description and flag of verbosities
verbosities :: [(Verbosity, String, String)]
verbosities = [ ( VerbQuiet,  "0", "quiet" )
              , ( VerbStatus, "1", "status")
              , ( VerbDebug,  "2", "debug" )
              ]

-- |Default casc options
defaultOptions :: Options
defaultOptions = Options
  { optMode       = ModeCheck
  , optVerbosity  = VerbStatus
  , optRecursive  = False
  , optColor      = ColAuto
  }


-- -----------------------------------------------------------------------------
-- Parsing of the command line options
-- -----------------------------------------------------------------------------
-- |Option error
type OptErr = (Options, [String])

-- |An 'OptErrTable' consists of a list of entries of the following form:
--   * a flag to be recognized on the command line
--   * an explanation text for the usage information
--   * a modification funtion adjusting the options structure
-- The type is parametric about the option's type to adjust.
type OptErrTable opt = [(String, String, opt -> opt)]

-- |Use function on options
onOpts :: (Options -> Options) -> OptErr -> OptErr
onOpts f (opts, errs) = (f opts, errs)

-- |Add error
addErr :: String -> OptErr -> OptErr
addErr err (opts, errs) = (opts, errs ++ [err])

-- |Make option description
mkOptDescr :: ((opt -> opt) -> OptErr -> OptErr)
           -> String -> [String] -> String -> String -> OptErrTable opt
           -> OptDescr (OptErr -> OptErr)
mkOptDescr lift flags longFlags arg what tbl
  = Option flags
           longFlags
           (ReqArg (parseOptErr lift what tbl) arg)
           ("set " ++ what ++ " `" ++ arg ++ "', where `" ++ arg
                   ++ "' is one of\n" ++ renderOptErrTable tbl)

-- |Parse option errors
parseOptErr :: ((opt -> opt) -> OptErr -> OptErr)
            -> String -> OptErrTable opt -> String -> OptErr -> OptErr
parseOptErr lift what table opt
  = case lookup3 opt table of
      Just f  -> lift f
      Nothing -> addErr $ "unrecognized " ++ what ++ '`' : opt ++ "'\n"
    where
      lookup3 _ []                  = Nothing
      lookup3 k ((k', _, v2) : kvs)
        | k == k'                   = Just v2
        | otherwise                 = lookup3 k kvs

-- |Render option error table
renderOptErrTable :: OptErrTable opt -> String
renderOptErrTable ds
  = intercalate "\n" $ map (\(k, d, _) -> "  " ++ rpad maxLen k ++ ": " ++ d) ds
    where
      maxLen = maximum $ map (\(k, _, _) -> length k) ds
      rpad n x = x ++ replicate (n - length x) ' '

-- |Verbosity description
verbDescriptions :: OptErrTable Options
verbDescriptions = map toDescr verbosities
  where
  toDescr (flag, name, desc)
    = (name, desc, \ opts -> opts { optVerbosity = flag })

-- |Color description
colDescriptions :: OptErrTable Options
colDescriptions = map toDescr colors
  where
  toDescr (flag, name, desc)
    = (name, desc, \ opts -> opts { optColor = flag })

-- |Parse the command line arguments
parseOpts :: [String] -> (Options, [String], [String])
parseOpts = updateOpts defaultOptions

-- |Update options
updateOpts :: Options -> [String] -> (Options, [String], [String])
updateOpts opts args = (opts', files, errs ++ errs2)
  where
    (opts', errs2) = foldl (flip ($)) (opts, []) optErrs
    (optErrs, files, errs) = getOpt Permute options args

-- |Print the usage information of the command line tool.
usage :: String -> String
usage _ = usageInfo header options
 where
  header = "Usage: curry style [OPTIONS] ... MODULES ... DIRECTORIES ...\n"
