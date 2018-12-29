{- |
    Module      :  $Header$
    Description :  Auxiliary functions

    This module provides some auxiliary functions.
-}
module Utils where

import Distribution (installDir)
import FilePath
import List         (init, last)
import Opts         (Verbosity (..))
import Sort         (quickSortBy)

import System.Console.ANSI.Codes (red, yellow)

import AST.Span          (Span, showPos)
import AST.Token
import AST.PositionUtils (line, col)
import Check.Types       (Message (..), CheckF)

-- -----------------------------------------------------------------------------
-- General auxiliary functions
-- -----------------------------------------------------------------------------

-- |Extract first element of quadruple
fst4:: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

-- |Extract second element of quadruple
snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

-- |Extract third element of quadruple
thrd4 :: (a, b, c, d) -> c
thrd4 (_, _, c, _) = c

-- |Extract fourth element of quadruple
frth4 :: (a, b, c, d) -> d
frth4 (_, _, _, d) = d

-- -----------------------------------------------------------------------------
-- Auxiliary functions for printing status
-- -----------------------------------------------------------------------------

-- |Print status
status :: Show a => String -> a -> IO ()
status str x = putStrLn ("\n" ++ str) >> putStrLn ("\n" ++ show x)

-- |PutStrLn for a list of strings
putStrsLn :: [String] -> IO()
putStrsLn = mapIO_ putStrLn

-- |If 'VerbStatus' (not quiet) or 'VerbDebug': Put string into new line
wStatusLn :: Verbosity -> String -> IO()
wStatusLn v s = wOutput v $ putStrLn s

-- |If 'VerbStatus' (not quiet) or 'VerbDebug': Put string into same line
wStatus :: Verbosity -> String -> IO()
wStatus v s = wOutput v $ putStr s

-- |Check verbosity
wOutput :: Verbosity -> (IO () -> IO ())
wOutput v = when (v `elem` [VerbDebug, VerbStatus])

-- |If 'VerbDebug': Put string into new line
wDebug :: Verbosity -> String -> IO()
wDebug v s = when (v == VerbDebug) $ putStrLn s


-- ----------------------------------------------------------------------------
-- Auxiliary functions for messages
-- ----------------------------------------------------------------------------

-- |Conditional Message: return message if predicate is `True`
condMsg :: Bool -> [Message] -> [Message]
condMsg p msg = if p then msg else []

-- |Pretty print messages
prettyMsg :: Bool -> [Message] -> String
prettyMsg col msgs = "\n" ++ (showMsg col $ sortMsg msgs)

-- |Show Messages
showMsg :: Bool -> [Message] -> String
showMsg _ []  = ""
showMsg col ((Message p m) :ms)
  | col       = yellow ("Line " ++ showPos p) ++ " "
             ++ red m ++ "\n" ++ showMsg col ms
  | otherwise = "Line " ++ showPos p ++ " " ++ m ++ "\n" ++ showMsg col ms

-- |Sort Messages by position to which they refer.
-- |We need this because messages concerning line length are unordered
sortMsg :: [Message] -> [Message]
sortMsg msg = quickSortBy leqMsg msg

-- |Less or equal function on messages
leqMsg :: Message -> Message -> Bool
leqMsg (Message p1 _) (Message p2 _) =
  if line p1 == line p2
    then col p1 <= col p2
    else line p1 <= line p2


-- -----------------------------------------------------------------------------
-- Auxiliary functions for tokenstream
-- -----------------------------------------------------------------------------

-- |Remove all comments from the tokenstream for synchronisation with AST
filterTS :: [(Span, Token)] -> [(Span, Token)]
filterTS = filter $ not . isComment . snd

-- |Is token a comment?
isComment :: Token -> Bool
isComment t = case t of
  LineComment   _ -> True
  NestedComment _ -> True
  _               -> False
