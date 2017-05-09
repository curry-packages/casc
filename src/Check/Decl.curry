{- |
    Module      :  Check.Decl
    Description :  Check and validation for Decl

    This module contains the check function
    and validation functions for Decl.
    The messages are only saved if the check
    is switched ON in the config (`shallCheck Check`)
-}
module Check.Decl (declCheck) where

import AST.Span          (Pos, start)
import AST.SpanAST
import AST.PositionUtils
import Check.Types       (CheckF, Message (..))
import Config.ReadConfig (shallCheck)
import Config.Types      (Check (..))
import Utils             (condMsg)

-- ----------------------------------------------------------------------------
-- Check Function
-- ----------------------------------------------------------------------------

-- |Check Decl
declCheck :: CheckF Decl
declCheck d = case d of
  DataDecl sd _ _ mse cds@(((ConstrDecl _ _ _ _ _ _ _):_)) sbs _ _ _ _ _ -> bars ++ constr ++ comp
    where
      pd   = start sd
      mpe  = case mse of
              Nothing -> Nothing
              Just se -> Just (start se)
      pbs  = map start sbs
      bars
        = condMsg (shallCheck CDataBars && not (validDeclBars mpe pbs))
                  [Message (head pbs) "DataDecl: Bars are not aligned."]
      constr
        = condMsg (shallCheck CDataConstructors && not (validDeclConstr cds))
                  [Message pd "DataDecl: Constructors are not aligned."]
      comp
        = condMsg (shallCheck CDataComponents && not (validDeclComp cds))
                  [Message pd "DataDecl: First components are not aligned."]
  _                                                   -> []

-- ----------------------------------------------------------------------------
-- Validation Functions
-- ----------------------------------------------------------------------------

--- ----------------------------------------------------------------------------
--- The following functions validate the correct alignment of data declarations.
--- The following layouts are considered valid:
---
---         data color = X .. | Y .. | Z ..
---
---         data example = X ..
---                      | Y ..
---                      | Z ..

-- |Valid if "=" and bars are aligned or all in one line.
validDeclBars :: (Maybe Pos) -> [Pos] -> Bool
validDeclBars pe pbs = case pe of
  Nothing -> True
  Just p   -> if (null pbs)
                then True
                else ((col p == col  (head pbs)) && (allColEq pbs))
                  || (line p == line (head pbs)) && (allLinesEq pbs)

-- |Valid if constructor names are aligned or all in one line.
validDeclConstr :: [ConstrDecl] -> Bool
validDeclConstr cds
  | null cds  = True
  | otherwise =    (allColEq   $ map constrDeclPos cds)
                || (allLinesEq $ map constrDeclPos cds)

-- |Valid if components are aligned or all in one line.
validDeclComp :: [ConstrDecl] -> Bool
validDeclComp cds
  | null cds  = True
  | otherwise =    (allColEq   $ map firstCDConstrTypePos cds)
                || (allLinesEq $ map firstCDConstrTypePos cds)
