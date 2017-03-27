{- |
    Module      :  Check.ImportExport
    Description :  Check and validation for Import and Export

    This module contains the check function
    and validation functions for Import and Export.
    The messages are only saved if the check
    is switched ON in the config (`shallCheck Check`)
-}
module Check.ImportExport (impDeclCheck, expSpecCheck) where

import AST.Ident
import AST.PositionUtils
import AST.Span          (Pos, start)
import AST.SpanAST
import Check.Types       (CheckF, Message (..))
import Config.ReadConfig (shallCheck)
import Config.Types      (Check (..))
import Utils             (condMsg)

-- ----------------------------------------------------------------------------
-- Check Functions
-- ----------------------------------------------------------------------------

-- |Check ImportDecl
impDeclCheck :: CheckF ImportDecl
impDeclCheck (ImportDecl s _ mid _ _ _ mis)
  | shallCheck CImport = case mis of
    Just _   -> []
    Nothing  ->
      condMsg
        (not (midQualifiers mid == ["Prelude"]))
        [Message (start s)
                 $ "List imported entities explicitly "
                   ++ "(unless there are too many)."]
  | otherwise          = []

-- |Check ExportSpec
expSpecCheck :: CheckF ExportSpec
expSpecCheck (Exporting sl _ ss sr)
  = condMsg (shallCheck CExportList && not (validExport (start sl) (map start ss) (start sr)))
            [Message (start sl)
                     "Exportlist: Parens and Commas are not aligned correctly."]


-- ----------------------------------------------------------------------------
-- Validation Functions
-- ----------------------------------------------------------------------------

--- ---------------------------------------------------------------------------
--- Validates correct layout of export list.
--- The following layouts are considered valid:
---             module Example (f1, f2, ...) where
---
---             module Example ( f1
---                            , f2
---                            , ...
---                            ) where

-- |Valid if parens and commas are aligned correctly
validExport :: Pos -> [Pos] -> Pos -> Bool
validExport pl ps pr
  | (null ps) = True
  | otherwise =
      -- parens and commas all in one column
      (col (head ps) == col pl)   && (allColEq ps)   && (col pl  == col pr)
    ||
      -- parens and all commas in one line
      (line (head ps) == line pl) && (allLinesEq ps) && (line pl == line pr)
