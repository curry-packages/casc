{- |
    Module      :  Config.Types
    Description :  Read config file

    This module contains types, constants and a conversion function
    for reading the config file.
-}
module Config.Types where

import List         (isInfixOf)
import Pretty       ((<+>), (<$$>), pPrint, text)

-- |All available checks
data Check
  = CDataBars
  | CDataComponents
  | CDataConstructors
  | CCase
  | CExportList
  | CIfThenElseKW
  | CIfThenElseSubExpr
  | CImport
  | CLet
  | CLineLength
  | CRecordCommas
  | CRecordDoubleColons
  | CRecordFieldNames
  | CRecordTypes
  | CRhsBars
  | CRhsEq
  | CTypeArrow

-- |Convert string from config file into element of type Check
cvCheck :: String -> Check
cvCheck s
  | isInfixOf "LineLength"                   s = CLineLength
  | isInfixOf "Data.Bars"                    s = CDataBars
  | isInfixOf "Data.Components"              s = CDataComponents
  | isInfixOf "Data.Constructors"            s = CDataConstructors
  | isInfixOf "Export.List"                  s = CExportList
  | isInfixOf "Expression.Case"              s = CCase
  | isInfixOf "Expression.IfThenElseKW"      s = CIfThenElseKW
  | isInfixOf "Expression.IfThenElseSubExpr" s = CIfThenElseSubExpr
  | isInfixOf "Expression.Let"               s = CLet
  | isInfixOf "Import.Explicit"              s = CImport
  | isInfixOf "Record.Commas"                s = CRecordCommas
  | isInfixOf "Record.DoubleColons"          s = CRecordDoubleColons
  | isInfixOf "Record.FieldNames"            s = CRecordFieldNames
  | isInfixOf "Record.Types"                 s = CRecordTypes
  | isInfixOf "Rhs.Bars"                     s = CRhsBars
  | isInfixOf "Rhs.Eq"                       s = CRhsEq
  | isInfixOf "Type.Arrow"                   s = CTypeArrow
  | otherwise                                  = error help

-- |Name of config file
configName :: String
configName = ".cascrc"

-- |Default config list: every feature is to be checked (1)
defaultCfg :: [(Check, Int)]
defaultCfg = map (\ c -> (c, 1))
  [ CDataBars
  , CDataComponents
  , CDataConstructors
  , CCase
  , CExportList
  , CIfThenElseKW
  , CIfThenElseSubExpr
  , CImport
  , CLet
  , CLineLength
  , CRecordCommas
  , CRecordDoubleColons
  , CRecordFieldNames
  , CRecordTypes
  , CRhsBars
  , CRhsEq
  , CTypeArrow
  ]

-- |Default value for maximum line length
defaultLgth :: Int
defaultLgth = 80

-- |Default value for autocorrection
defaultAutoC :: String
defaultAutoC = "no"

-- |Help text that is shown if the config file is not filled out correctly
help :: String
help = pPrint $
       text "Syntax error in config file. Here are some tips:"
  <$$> text "* Comments start with '--'"
  <$$> text "* The list of available checks has to be completed"
       <+> text "with numbers 0-2 like this:"
  <$$> text "** 0: don't check"
  <$$> text "** 1: check and show warn message"
  <$$> text "** 2: check and correct automatically if possible"
  <$$> text "* maxLength specifies the desired maximum for the check"
       <+> text "LineLength and has to be a natural number < 1000."
