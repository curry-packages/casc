{- |
    Module      :  Check.TypeExpr
    Description :  Check and validation for TypeExpr

    This module contains the check function
    and validation functions for TypeExpr.
    The messages are only saved if the check
    is switched ON in the config (`shallCheck Check`)
-}
module Check.TypeExpr (typeExprCheck) where

import AST.PositionUtils
import AST.Span          (Pos, start)
import AST.SpanAST
import Check.Types       (CheckF, Message (..))
import Config.Types      (Check (..))
import Config.ReadConfig (shallCheck)
import Utils             (condMsg)

--- ----------------------------------------------------------------------------
--- Check Function
--- ----------------------------------------------------------------------------

-- |Check TypeExpr
typeExprCheck :: CheckF TypeExpr
typeExprCheck te = case te of
  ArrowType _ s te2
    | shallCheck CTypeArrow && not (validArrowType (start s) te2)
      -> [Message (start s) "ArrowType: There is no blank behind the arrow."]
  _   -> []


--------------------------------------------------------------------------------
--- Validation Functions
--- ----------------------------------------------------------------------------

-- |ArrowType is valid if there's a blank behind the arrow
validArrowType :: Pos -> TypeExpr -> Bool
validArrowType p te2 = col p == ((col $ typeExprPos te2) - 3)
-- todo: recognize whether there is a blank before the arrow
