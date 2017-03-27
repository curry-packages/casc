{- |
    Module      :  Check.Rhs
    Description :  Check and validation for Rhs

    This module contains the check function
    and validation functions for Rhs.
    The messages are only saved if the check
    is switched ON in the config (`shallCheck Check`)
-}
module Check.Rhs (rhsCheck) where

import AST.PositionUtils
import AST.Span          (Pos, start)
import AST.SpanAST
import Check.Types       (CheckF, Message (..))
import Config.Types      (Check (..))
import Config.ReadConfig (shallCheck)
import Utils             (condMsg)

-- ----------------------------------------------------------------------------
-- Check Function
-- ----------------------------------------------------------------------------

-- |Check Rhs
rhsCheck :: CheckF Rhs
rhsCheck rhs = case rhs of
  GuardedRhs s1 ces ss _ _ -> eqs ++ bars
    where
      p1   = start s1
      eqs  = condMsg (shallCheck CRhsEq && not (validGrhsEq ces))
                     [Message (condExprPos $ head ces)
                              "GuardedRhs: Equality signs are not aligned."]
      bars = condMsg (shallCheck CRhsBars && not (validGrhsBars p1 (map start ss)))
                     [Message p1 "GuardedRhs: Bars are not aligned."]
  _                        -> []

-- ----------------------------------------------------------------------------
-- Validation Functions
-- ----------------------------------------------------------------------------

--- Validates the correct alignment of a guarded righthandside of an equation.
--- The following layout is considered valid:
---
--- abs n
---    | n < 0     = -n
---    | otherwise = n

-- |GuardedRhs: Bars all in one column
validGrhsBars :: Pos -> [Pos] -> Bool
validGrhsBars p1 ps@(p2:_) = (col p1 == col p2) && allColEq ps
validGrhsBars _  []        = True

-- |GuardedRhs: Equality signs all in one column
validGrhsEq :: [CondExpr] -> Bool
validGrhsEq ces = (allColEq $ map condExprPos ces)
