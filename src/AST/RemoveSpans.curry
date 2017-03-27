{- |
    Module      :  AST.RemoveSpans
    Description :  Remove span information from SpanAST

    This module provides functions for converting an extended SpanAST back into
    a simple AST. All previously added span informations are removed by tree
    traversal.
    This conversion allows us to make use of the built-in pretty printer for
    Abstract Syntax Trees.
-}
module AST.RemoveSpans (rsModule) where

import Char (toUpper)

import           AST.AST     as AST
import           AST.Ident   as I
import           AST.Span
import qualified AST.SpanAST as SpanAST
import           AST.PositionUtils

-- |Remove span information from Module
rsModule :: SpanAST.Module -> AST.Module
rsModule (SpanAST.Module mps _ mi _ mes ids ds)
  = AST.Module (map rsModulePragma mps) (rsMIdent mi) (rsExportSpec mes)
               (map rsImportDecl ids) (map rsDecl ds)

-- |Remove span information from ModulePragma
rsModulePragma :: SpanAST.ModulePragma -> AST.ModulePragma
rsModulePragma mp = case mp of
  SpanAST.LanguagePragma sp es _ _ -> AST.LanguagePragma (start sp) (map rsExtension es)
  SpanAST.OptionsPragma  sp mt s _ -> AST.OptionsPragma  (start sp) (stringToTool mt) s

-- |Convert String to Tool
stringToTool :: (Maybe String) -> (Maybe AST.Tool)
stringToTool ms = case ms of
  Nothing -> Nothing
  Just s | ((map toUpper s) == "KICS2")  -> Just KICS2
         | ((map toUpper s) == "PAKCS")  -> Just PAKCS
         | ((map toUpper s) == "CYMAKE") -> Just CYMAKE
         | otherwise                       -> Just (UnknownTool s)

-- |Remove span information from Extension
rsExtension :: SpanAST.Extension -> AST.Extension
rsExtension e = case e of
  SpanAST.KnownExtension   sp ke -> AST.KnownExtension   (start sp) ke
  SpanAST.UnknownExtension sp s  -> AST.UnknownExtension (start sp) s

-- |Remove span information from ExportSpec
rsExportSpec :: Maybe SpanAST.ExportSpec -> Maybe AST.ExportSpec
rsExportSpec es = case es of
  Just (SpanAST.Exporting sp es1 _ _) -> Just $ AST.Exporting (start sp) (map rsExport es1)
  Nothing                             -> Nothing

-- |Remove span information from Export
rsExport :: SpanAST.Export -> AST.Export
rsExport e = case e of
  SpanAST.Export         sqi          -> AST.Export         (rsSymQualIdent sqi)
  SpanAST.ExportTypeWith qi _  is _ _ -> AST.ExportTypeWith (rsQualIdent qi) (map rsIdent is)
  SpanAST.ExportTypeAll  qi _  _  _   -> AST.ExportTypeAll  (rsQualIdent qi)
  SpanAST.ExportModule    _ mi        -> AST.ExportModule   (rsMIdent mi)

-- |Remove span information from ImportDecl
rsImportDecl :: SpanAST.ImportDecl -> AST.ImportDecl
rsImportDecl (SpanAST.ImportDecl sp _ mi _ q mmi misp) = let mi' = rsMIdent mi
  in case mmi of
       Just m  -> AST.ImportDecl (start sp) mi' q (Just (rsMIdent m)) (rsMaybeImportSpec misp)
       Nothing -> AST.ImportDecl (start sp) mi' q Nothing (rsMaybeImportSpec misp)

-- |Remove span information from ImportSpec
rsMaybeImportSpec :: Maybe SpanAST.ImportSpec -> Maybe AST.ImportSpec
rsMaybeImportSpec misp = case misp of
  Just (SpanAST.Importing sp is _  _ ) -> Just $ AST.Importing (start sp) (map rsImport is)
  Just (SpanAST.Hiding    sp _ is _ _) -> Just $ AST.Hiding    (start sp) (map rsImport is)
  Nothing                              -> Nothing

-- |Remove span information from Import
rsImport :: SpanAST.Import -> AST.Import
rsImport imp = case imp of
  SpanAST.Import         i          -> AST.Import         (rsSymIdent i)
  SpanAST.ImportTypeWith i _ is _ _ -> AST.ImportTypeWith (rsIdent i) (map rsIdent is)
  SpanAST.ImportTypeAll  i _ _  _   -> AST.ImportTypeAll  (rsIdent i)

-- |Remove span information from Decl
rsDecl :: SpanAST.Decl -> AST.Decl
rsDecl d = case d of
  SpanAST.InfixDecl i mp ipos _
    -> AST.InfixDecl (infPos i) (rsInfix i)
                     (rsPrecedence mp) (map rsSymIdent ipos)
  SpanAST.DataDecl sp i is _ cds _
    -> AST.DataDecl (start sp) (rsIdent i) (map rsIdent is) (map rsConstrDecl cds)
  SpanAST.NewtypeDecl sp i is _ ncd
    -> AST.NewtypeDecl (start sp) (rsIdent i) (map rsIdent is) (rsNewConstrDecl ncd)
  SpanAST.TypeDecl sp i is _ te
    -> AST.TypeDecl (start sp) (rsIdent i) (map rsIdent is) (rsTypeExpr te)
  SpanAST.TypeSig sis _ _ te
    -> AST.TypeSig (sidPos $ head sis) (map rsSymIdent sis) (rsTypeExpr te)
  SpanAST.FunctionDecl i eqs
    -> AST.FunctionDecl (idPos i) (rsIdent i) (map rsEquation eqs)
  SpanAST.ForeignDecl sp cc mps i _ te
    -> AST.ForeignDecl (start sp) (rsCallConv cc) (convert mps) (rsSymIdent i) (rsTypeExpr te)
      where convert mps' = case mps' of
              Just (_, s) -> Just s
              Nothing     -> Nothing
  SpanAST.ExternalDecl is _  _
    -> AST.ExternalDecl (sidPos $ head is) (map rsSymIdent is)
  SpanAST.PatternDecl p rhs
    -> AST.PatternDecl (patPos p) (rsPattern p) (rsRhs rhs)
  SpanAST.FreeDecl is _  _
    -> AST.FreeDecl (idPos $ head is) (map rsIdent is)

-- |Remove span information from Infix
rsInfix :: SpanAST.Infix -> AST.Infix
rsInfix i = case i of
  SpanAST.InfixL _ -> AST.InfixL
  SpanAST.InfixR _ -> AST.InfixR
  SpanAST.Infix  _ -> AST.Infix

-- |Remove span information from Precedence
rsPrecedence :: Maybe SpanAST.Precedence -> Maybe AST.Precedence
rsPrecedence p = case p of
  Just (_, i) -> Just i
  Nothing     -> Nothing

-- |Remove span information from ConstrDecl
rsConstrDecl :: SpanAST.ConstrDecl -> AST.ConstrDecl
rsConstrDecl cd = case cd of
  SpanAST.ConstrDecl is i tes
    -> AST.ConstrDecl (idPos i) (map rsIdent is) (rsIdent i) (map rsTypeExpr tes)
  SpanAST.ConOpDecl is te1 i te2
    -> AST.ConOpDecl  (idPos i) (map rsIdent is) (rsTypeExpr te1) (rsIdent i) (rsTypeExpr te2)
  SpanAST.RecordDecl is i _ fds _ _
    -> AST.RecordDecl (idPos i) (map rsIdent is) (rsIdent i) (map rsFieldDecl fds)

-- |Remove span information from NewConstrDecl
rsNewConstrDecl :: SpanAST.NewConstrDecl -> AST.NewConstrDecl
rsNewConstrDecl ncd = case ncd of
  SpanAST.NewConstrDecl is i te
    -> AST.NewConstrDecl (idPos i) (map rsIdent is) (rsIdent i) (rsTypeExpr te)
  SpanAST.NewRecordDecl is i1 _ (i2, _, te) _
    -> AST.NewRecordDecl (idPos i1) (map rsIdent is) (rsIdent i1) (rsIdent i2, rsTypeExpr te)

-- |Remove span information from FieldDecl
rsFieldDecl :: SpanAST.FieldDecl -> AST.FieldDecl
rsFieldDecl (SpanAST.FieldDecl is _ _ te)
  = AST.FieldDecl (idPos $ head is) (map rsIdent is) (rsTypeExpr te)

-- |Remove span information from CallConv
rsCallConv :: SpanAST.CallConv -> AST.CallConv
rsCallConv cc = case cc of
  SpanAST.CallConvPrimitive _ -> AST.CallConvPrimitive
  SpanAST.CallConvCCall     _ -> AST.CallConvCCall

-- |Remove span information from TypeExpr
rsTypeExpr :: SpanAST.TypeExpr -> AST.TypeExpr
rsTypeExpr te = case te of
  SpanAST.ConstructorType _ qi tes _
    -> AST.ConstructorType (rsQualIdent qi) (map rsTypeExpr tes)
  SpanAST.VariableType i
    -> AST.VariableType (rsIdent i)
  SpanAST.TupleType _ tes _ _
    -> AST.TupleType (map rsTypeExpr tes)
  SpanAST.ListType _ te1 _
    -> AST.ListType (rsTypeExpr te1)
  SpanAST.ArrowType te1 _ te2
    -> AST.ArrowType (rsTypeExpr te1) (rsTypeExpr te2)
  SpanAST.ParenType _ te1 _
    -> AST.ParenType (rsTypeExpr te1)

-- |Remove span information from Equation
rsEquation :: SpanAST.Equation -> AST.Equation
rsEquation (SpanAST.Equation lhs rhs)
  = AST.Equation (lhsPos lhs) (rsLhs lhs) (rsRhs rhs)

-- |Remove span information from Lhs
rsLhs :: SpanAST.Lhs -> AST.Lhs
rsLhs lhs = case lhs of
  SpanAST.FunLhs si  ps    -> AST.FunLhs (rsSymIdent si) (map rsPattern ps)
  SpanAST.OpLhs  p1  si p2 -> AST.OpLhs (rsPattern p1)
                                       (rsSymIdent si)
                                       (rsPattern p2)
  SpanAST.ApLhs lhs1 ps    -> AST.ApLhs (rsLhs lhs1) (map rsPattern ps)

-- |Remove span information from Rhs
rsRhs :: SpanAST.Rhs -> AST.Rhs
rsRhs rhs = case rhs of
  SpanAST.SimpleRhs  sp e   _ ds   -> AST.SimpleRhs (start sp) (rsExpression e)
                                                     (map rsDecl ds)
  SpanAST.GuardedRhs _ ces _ _  ds -> AST.GuardedRhs (map rsCondExpr ces)
                                                    (map rsDecl ds)

-- |Remove span information from CondExpr
rsCondExpr :: SpanAST.CondExpr -> AST.CondExpr
rsCondExpr (SpanAST.CondExpr e1 _ e2)
  = AST.CondExpr (exprPos e1) (rsExpression e1) (rsExpression e2)

-- |Remove span information from Literal
rsLiteral :: SpanAST.Literal -> AST.Literal
rsLiteral l = case l of
  SpanAST.Char   _ c -> AST.Char   c
  -- The 'Ident'-argument used for supporting ad-hoc polymorphism
  -- on integer numbers gets lost here
  SpanAST.Int    _ i -> AST.Int    (AST.Ident virtualPos "_" 0) i
  SpanAST.Float  _ d -> AST.Float  d
  SpanAST.String _ s -> AST.String s

-- |Remove span information from Pattern
rsPattern :: SpanAST.Pattern -> AST.Pattern
rsPattern p = case p of
  SpanAST.LiteralPattern  l   -> AST.LiteralPattern (rsLiteral l)
  SpanAST.NegativePattern i l -> AST.NegativePattern (rsIdent i) (rsLiteral l)
  SpanAST.VariablePattern i   -> AST.VariablePattern (rsIdent i)
  SpanAST.ConstructorPattern qi ps
    -> AST.ConstructorPattern (rsQualIdent qi) (map rsPattern ps)
  SpanAST.InfixPattern p1 qi p2
    -> AST.InfixPattern (rsPattern p1) (rsQualIdent qi) (rsPattern p2)
  SpanAST.ParenPattern _  p1 _ -> AST.ParenPattern (rsPattern p1)
  SpanAST.RecordPattern      qi _  fps _ _
    -> AST.RecordPattern (rsQualIdent qi) (map rsFieldP fps)
  SpanAST.TuplePattern       _  ps _   _   -> AST.TuplePattern (map rsPattern ps)
  SpanAST.ListPattern        _  ps _   _   -> AST.ListPattern (map rsPattern ps)
  SpanAST.AsPattern          i  _  p1      -> AST.AsPattern (rsIdent i) (rsPattern p1)
  SpanAST.LazyPattern        _  p1         -> AST.LazyPattern (rsPattern p1)
  SpanAST.FunctionPattern    qi ps
    -> AST.FunctionPattern (rsQualIdent qi) (map rsPattern ps)
  SpanAST.InfixFuncPattern   p1 qi p2
    -> AST.InfixFuncPattern (rsPattern p1) (rsQualIdent qi) (rsPattern p2)

-- |Remove span information from Ident
rsIdent :: I.Ident -> AST.Ident
rsIdent (I.Ident sp n u) = AST.Ident (start sp) n u

-- |Remove span information from QualIdent
rsQualIdent :: I.QualIdent -> AST.QualIdent
rsQualIdent (I.QualIdent mmi i) = let i' = rsIdent i in case mmi of
  Just mi -> AST.QualIdent (Just (rsMIdent mi)) i'
  Nothing -> AST.QualIdent Nothing i'

-- |Remove span information from SymIdent and turn it back into Ident
rsSymIdent :: I.SymIdent -> AST.Ident
rsSymIdent (I.SymIdent _ i _) = rsIdent i

-- |Remove span information from SymQualIdent and turn it back into QualIdent
rsSymQualIdent :: I.SymQualIdent -> AST.QualIdent
rsSymQualIdent (I.SymQualIdent _ qi _) = rsQualIdent qi

-- |Remove span information from ModuleIdent
rsMIdent :: I.ModuleIdent -> AST.ModuleIdent
rsMIdent (I.ModuleIdent sp qs) = AST.ModuleIdent (start sp) qs

-- |Remove span information from Expression
rsExpression :: SpanAST.Expression -> AST.Expression
rsExpression e = case e of
  SpanAST.Literal      l                -> AST.Literal (rsLiteral l)
  SpanAST.Variable     qsi              -> AST.Variable (rsSymQualIdent qsi)
  SpanAST.Constructor  qsi              -> AST.Constructor (rsSymQualIdent qsi)
  SpanAST.Paren        _  e1  _         -> AST.Paren (rsExpression e1)
  SpanAST.Typed        e1 _   te        -> AST.Typed (rsExpression e1)
                                                    (rsTypeExpr te)
  SpanAST.Record       qi _   fes  _ _  -> AST.Record (rsQualIdent qi) (map rsFieldE fes)
  SpanAST.RecordUpdate e1 _   fes  _ _
    -> AST.RecordUpdate (rsExpression e1) (map rsFieldE fes)
  SpanAST.Tuple          _  es  _    _  -> AST.Tuple (map rsExpression es)
  SpanAST.List           _  es  _    _  -> AST.List (map rsExpression es)
  SpanAST.ListCompr      _  e1  _    sts _ _
    -> AST.ListCompr (rsExpression e1) (map rsStatement sts)
  SpanAST.EnumFrom       _  e1  _    _  -> AST.EnumFrom (rsExpression e1)
  SpanAST.EnumFromThen   _  e1  _    e2 _ _
    -> AST.EnumFromThen (rsExpression e1) (rsExpression e2)
  SpanAST.EnumFromTo     _  e1  _    e2 _
    -> AST.EnumFromTo (rsExpression e1) (rsExpression e2)
  SpanAST.EnumFromThenTo _  e1  _    e2 _ e3 _
    -> AST.EnumFromThenTo (rsExpression e1) (rsExpression e2) (rsExpression e3)
  SpanAST.UnaryMinus     i  e1
    -> AST.UnaryMinus (rsIdent i) (rsExpression e1)
  SpanAST.Apply          e1 e2 -> AST.Apply (rsExpression e1) (rsExpression e2)
  SpanAST.InfixApply     e1 iop e2
    -> AST.InfixApply (rsExpression e1) (rsInfixOp iop) (rsExpression e2)
  SpanAST.LeftSection    _  e1  iop _
    -> AST.LeftSection (rsExpression e1) (rsInfixOp iop)
  SpanAST.RightSection   _  iop e1  _
    -> AST.RightSection (rsInfixOp iop) (rsExpression e1)
  SpanAST.Lambda _  ps  _   e1 -> AST.Lambda (map rsPattern ps) (rsExpression e1)
  SpanAST.Let    _  ds _ e1    -> AST.Let (map rsDecl ds) (rsExpression e1)
  SpanAST.Do     _  sts e1     -> AST.Do (map rsStatement sts) (rsExpression e1)
  SpanAST.IfThenElse _  e1  _  e2 _ e3
    -> AST.IfThenElse (rsExpression e1) (rsExpression e2) (rsExpression e3)
  SpanAST.Case       ct _   e1  _ alts     -> AST.Case (rsCaseType ct)
                                                      (rsExpression e1)
                                                      (map rsAlt alts)

-- |Remove span information from InfixOp
rsInfixOp :: SpanAST.InfixOp -> AST.InfixOp
rsInfixOp iop = case iop of
  SpanAST.InfixOp     qis -> AST.InfixOp     (rsSymQualIdent qis)
  SpanAST.InfixConstr qis -> AST.InfixConstr (rsSymQualIdent qis)

-- |Remove span information from Statement
rsStatement :: SpanAST.Statement -> AST.Statement
rsStatement s = case s of
  SpanAST.StmtExpr e      -> AST.StmtExpr (rsExpression e)
  SpanAST.StmtDecl _ ds   -> AST.StmtDecl (map rsDecl ds)
  SpanAST.StmtBind _ p  e -> AST.StmtBind (rsPattern p)   (rsExpression e)

-- |Remove span information from CaseType
rsCaseType :: SpanAST.CaseType -> AST.CaseType
rsCaseType ct = case ct of
  SpanAST.Rigid -> AST.Rigid
  SpanAST.Flex  -> AST.Flex

-- |Remove span information from Alt
rsAlt :: SpanAST.Alt -> AST.Alt
rsAlt (SpanAST.Alt p rhs) = AST.Alt (patPos p) (rsPattern p) (rsRhs rhs)

-- |Remove span information from Field Pattern
rsFieldP :: SpanAST.Field SpanAST.Pattern -> AST.Field AST.Pattern
rsFieldP (SpanAST.Field qi _ p) = AST.Field (qidPos qi) (rsQualIdent qi) (rsPattern p)

-- |Remove span information from Field Expression
rsFieldE :: SpanAST.Field SpanAST.Expression -> AST.Field AST.Expression
rsFieldE (SpanAST.Field qi _ e) = AST.Field (qidPos qi) (rsQualIdent qi) (rsExpression e)
