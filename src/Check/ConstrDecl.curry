{- |
    Module      :  Check.ConstrDecl
    Description :  Check and validation for ConstrDecl

    This module contains the check function
    and validation functions for ConstrDecl.
    The messages are only saved if the check
    is switched ON in the config (`shallCheck Check`)
-}
module Check.ConstrDecl (constrDeclCheck) where

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

-- |Check ConstrDecl
constrDeclCheck :: CheckF ConstrDecl
constrDeclCheck cd = case cd of
  RecordDecl _ _ sl fds ss sr -> commas ++ constr ++ doublecolons ++ types
    where
      pl = start sl
      ps = map start ss
      pr = start sr
      commas
        = condMsg
            (shallCheck CRecordCommas && not (validConstrDeclC pl ps pr))
            [Message pl "RecordDecl: Commas and Brackets are not aligned."]
      constr
        = condMsg
            (shallCheck CRecordFieldNames && not (validConstrDeclID fds))
            [Message pl "RecordDecl: Field names are not aligned."]
      doublecolons
        = condMsg
            (shallCheck CRecordDoubleColons && not (validConstrDeclDC fds))
            [Message pl "RecordDecl: Doublecolons are not aligned."]
      types
        = condMsg
            (shallCheck CRecordTypes && not (validConstrDeclT fds))
            [Message pl "RecordDecl: Types are not aligned."]
  _                           -> []

-- ----------------------------------------------------------------------------
-- Validation Functions
-- ----------------------------------------------------------------------------

--- The following functions validate the correct layout of a record declaration.
--- The following layout is considered valid:
---
---               data Person = Person
---                  { firstName :: String
---                  , lastName  :: String
---                  , age       :: Int
---                  }

-- |Valid if Brackets and Commas all in one column
validConstrDeclC :: Pos -> [Pos] -> Pos -> Bool
validConstrDeclC pl ps pr =
     ((col (head ps) == col pl)
  && (allColEq ps) && (col pl == col pr))

-- |Valid if Doublecolons all in one column
validConstrDeclDC :: [FieldDecl] -> Bool
validConstrDeclDC fds
  | null fds  = True
  | otherwise = allColEq (map fieldDeclDCPos fds)

-- |Valid if Constructors all in one column
validConstrDeclID :: [FieldDecl] -> Bool
validConstrDeclID fds
  | null fds  = True
  | otherwise = allColEq (map fieldDeclIDPos fds)

-- |Valid if Types all in one column
validConstrDeclT :: [FieldDecl] -> Bool
validConstrDeclT fds
  | null fds  = True
  | otherwise = allColEq (map fieldDeclTEPos fds)
