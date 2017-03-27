module Test.TestGuardedRhs where

abs n
  | n < 0     = - n
   | otherwise = n

