{- |
    Module      :  Check.Expression
    Description :  Check and validation for Expression

    This module contains the check function
    and validation functions for Expression.
    The messages are only saved if the check
    is switched ON in the config (`shallCheck Check`)
-}
module Check.Expression (expressionCheck) where

import AST.PositionUtils
import AST.Span          (Pos, start)
import AST.SpanAST
import Check.Types       (CheckF, Message (..))
import Config.ReadConfig (shallCheck)
import Config.Types      (Check (..))
import Utils             (condMsg)

-- ----------------------------------------------------------------------------
-- Check Function
-- ----------------------------------------------------------------------------

-- |Check Expression
expressionCheck :: CheckF Expression
expressionCheck e = case e of
  IfThenElse si i st t se el -> keywords ++ subexpr
    where
      pi = start si
      pt = start st
      pe = start se
      keywords
        = condMsg (shallCheck CIfThenElseKW && not (validITEkw pi pt pe))
                  [Message pi "IfThenElse: Wrong indentation of keywords."]
      subexpr
        = condMsg (shallCheck CIfThenElseSubExpr
                           && not (validITEsubE pi i pt t pe el))
                  [Message pi
                           "IfThenElse: Wrong indentation of subexpressions."]
  Let sl ds si _             -> keywords ++ decl ++ eq
    where pl = start sl
          keywords
            = condMsg (shallCheck CLet && not (validLetKW pl (start si)))
                      [Message pl
                               "Let: Keywords 'let' and 'in' are not aligned."]
          decl
            = condMsg (shallCheck CLet && not (validLetDecl ds))
                      [Message (declPos $ head ds)
                               "Let: Declarations are not aligned."]
          eq
            = condMsg (shallCheck CLet && not (validLetEq ds))
                      [Message (declPos $ head ds)
                               "Let: Equality signs are not aligned."]
  Case _ pc _ _ alts
    | shallCheck CCase && not (validAlts alts) ->
        [Message (start pc) "Case: Arrows are not aligned."]
  _                          -> []

-- ----------------------------------------------------------------------------
-- Validation Functions
-- ----------------------------------------------------------------------------

--- --------------------------------------------------------------------------
--- Validates correct position of `if`, `then` and `else` keywords.
--- The following layouts are considered valid:
---        if ... then ... else ...
---
---        if ... then ...
---               else ...
---
---        if ...
---          then ...
---          else ...

-- |Valid if indentation of keywords is as mentioned above
validITEkw :: Pos -> Pos -> Pos -> Bool
validITEkw pi pt pe =
    (line pi == line pt && line pt == line pe)
 || (line pi == line pt && col pt == col pe)
 || (col pt == ((col pi) + 2) && col pt == col pe)


-- |Valid if space between keywords and subexpressions is as mentioned above
validITEsubE :: Pos -> Expression -> Pos -> Expression -> Pos -> Expression
             -> Bool
validITEsubE pi i pt t pe el =
     (col (exprPos  i) == ((col pi) + 3))
  && (col (exprPos  t) == ((col pt) + 5))
  && (col (exprPos el) == ((col pe) + 5))


--- --------------------------------------------------------------------------
--- Validates correct alignment of a `let` expression.
--- The following layouts are considered valid:
---
---         let decl in expr
---
---         let decl1 = ...
---             decl2 = ...
---             ...
---         in ...
-- |Valid if keywords `let` and `in` are aligned
validLetKW :: Pos -> Pos -> Bool
validLetKW pl pi = (col pl == col pi) || (line pl == line pi)

-- |Valid if declarations are aligned
validLetDecl :: [Decl] -> Bool
validLetDecl ds = (allColEq $ map declPos ds)

-- |Valid if equality signs are aligned
validLetEq :: [Decl] -> Bool
validLetEq ds = (allColEq $ map declPosEq ds)

--- --------------------------------------------------------------------------
--- Validates correct layout of case alternatives.
--- The following layouts are considered valid:
---            case e of E1 -> ...
---                      E2 -> ...
---
---            case e of
---              E1 -> ...
---              E2 -> ...

-- |Valid if alternatives are aligned
validAlts :: [Alt] -> Bool
validAlts alts
  | null alts = True
  | otherwise = allColEq $ map altPos alts
