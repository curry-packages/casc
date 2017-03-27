{- |
    Module      :  Check.LineLength
    Description :  Check length of lines

    This module contains the check function for line length.
    The messages are only saved if the check
    is switched ON in the config (`shallCheck Check`)
-}
module Check.LineLength where

import Check.Types       (Message (..))
import Config.ReadConfig (maxLength)

--- |Convert source string step by step:
--- 1) replace tabs by two blanks for better character counting
--- 2) convert string into list of strings, separated by `lines`
--- 3) number lines consecutively
--- 4) calls the lineLength function for every line
--- @return - list of messages containing a warning for each illegally long line

checkLine :: String -> [Message]
checkLine src = concatMap (lineLength maxLength)
                          (zip [1 ..] $ lines $ tabs2Blanks src)

--- |Count characters in a line
--- @param maxl    -  maximum line length read out from config file
--- @param lnumber -  number of line
--- @param l       -  string with content of line
--- @return        -  list containing message if l is longer than maxl

lineLength :: Int -> (Int, String) -> [Message]
lineLength maxl (lnumber, l) =
  let ll = length l
  in ([ Message (lnumber, 0)
                ("Line is longer than "
                  ++ show maxl
                  ++ " characters.") | ll > maxl ])

-- |Convert each tab in a string into 2 blanks
tabs2Blanks :: String -> String
tabs2Blanks s = case s of
  ""     -> ""
  c : rs -> if (c == '\t')
              then "  " ++ tabs2Blanks rs
              else [c] ++ tabs2Blanks rs
