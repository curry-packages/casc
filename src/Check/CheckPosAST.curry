{- |
    Module      :  Check.CheckPosAST
    Description :  Checking of PosAST

    This module provides a tree traversal for the PosAST while
    applying checks to each node and the computation of subterms
    for all data structures of the PosAST which are defined recursively.
-}
module Check.CheckPosAST where

-- import AST.AST               (Pos, Ident (..), QualIdent (..))
import AST.AddSpans       (apModule)
import AST.Ident
import AST.SpanAST
import AST.Span           (Pos)
import AST.SortSplit      (sortSplitModule)
import Check.ConstrDecl   (constrDeclCheck)
import Check.Decl         (declCheck)
import Check.Expression   (expressionCheck)
import Check.ImportExport (impDeclCheck, expSpecCheck)
import Check.Pattern      (patternCheck)
import Check.Rhs          (rhsCheck)
import Check.TypeExpr     (typeExprCheck)
import Check.Types        (CheckF)

-- ----------------------------------------------------------------------------
-- Traverse PosAST and apply check functions.
-- ----------------------------------------------------------------------------

-- |Check Module
checkModule :: CheckF Module
checkModule (Module _ _ _ _ mexp ids ds)
  =    checkExpSpec mexp
    ++ concatMap checkImpDecl ids
    ++ concatMap checkDecl ds

-- |Check ImportDecl
checkImpDecl :: CheckF ImportDecl
checkImpDecl i = impDeclCheck i

-- |Check ExportSpec
checkExpSpec :: CheckF (Maybe ExportSpec)
checkExpSpec exps = case exps of
  Just exp   -> [ m | m <- expSpecCheck exp]
  _          -> []

-- |Check Decl
checkDecl :: CheckF Decl
checkDecl d = case d of
  DataDecl     _   _   _   _  cds _ _ _ _ _ _ ->
    [ m | m <- declCheck d ] ++ concatMap checkConstrDecl cds
  NewtypeDecl  _   _   _   _  ncd _ _ _ _ _   -> checkNewConstrDecl ncd
  TypeDecl     _   _   _   _  te     -> checkTypeExpr te
  TypeSig      _   _   _   qte       -> checkQualTypeExpr qte
  FunctionDecl _   eqs               -> concatMap checkEquation eqs
  ForeignDecl  _   _   _   _  _  te  -> checkTypeExpr te
  PatternDecl  pat rhs               -> checkPattern pat ++ checkRhs rhs
  _                                  -> []

-- |Check ConstrDecl
checkConstrDecl :: CheckF ConstrDecl
checkConstrDecl cd = case cd of
  ConstrDecl _ _ _ _ _ _   tes         -> concatMap checkTypeExpr tes
  ConOpDecl  _ _ _ _ _ te1 _   te2     -> checkTypeExpr te1 ++ checkTypeExpr te2
  RecordDecl _ _ _ _ _ _   _   fds _ _ -> [ m | m <- constrDeclCheck cd ]
                                          ++ concatMap checkFieldDecl fds

-- |Check NewConstrDecl
checkNewConstrDecl :: CheckF NewConstrDecl
checkNewConstrDecl ncd = case ncd of
  NewConstrDecl _ te              -> checkTypeExpr te
  NewRecordDecl _ _  (_, _, te) _ -> checkTypeExpr te

-- |Check FieldDecl
checkFieldDecl :: CheckF FieldDecl
checkFieldDecl (FieldDecl _ _ _ te) = checkTypeExpr te

-- |Check TypeExpr
checkTypeExpr :: CheckF TypeExpr
checkTypeExpr te = [ m | te' <- subTypeExprs te, m <- typeExprCheck te']

-- |Check QualTypeExpr
checkQualTypeExpr :: CheckF QualTypeExpr
checkQualTypeExpr (QualTypeExpr _ _ te) = checkTypeExpr te

-- |Check Equation
checkEquation :: CheckF Equation
checkEquation (Equation lhs rhs) = checkLhs lhs ++ checkRhs rhs

-- |Check Lhs
checkLhs :: CheckF Lhs
checkLhs lhs = case lhs of
  FunLhs _ ps -> concatMap checkPattern ps
  OpLhs p1 _ p2   ->    [ m | p1' <- subPattern p1, m <- check p1' ]
                     ++ [ m | p2' <- subPattern p2, m <- check p2' ]
    where check = patternCheck
  ApLhs _ ps      -> concatMap checkPattern ps

-- |Check Rhs
checkRhs :: CheckF Rhs
checkRhs rhs = case rhs of
  SimpleRhs _ e _ ds      -> checkExpression e ++ concatMap checkDecl ds
  GuardedRhs _ ces _ _ ds -> [ m | m <- rhsCheck rhs ]
                             ++ concatMap checkCondExpr ces
                             ++ concatMap checkDecl ds

-- |Check CondExpr
checkCondExpr :: CheckF CondExpr
checkCondExpr (CondExpr e1 _ e2) = checkExpression e1 ++ checkExpression e2

-- |Check Pattern
checkPattern :: CheckF Pattern
checkPattern p = [ m | p' <- subPattern p, m <- patternCheck p' ]

-- |Check Expression
checkExpression :: CheckF Expression
checkExpression e = case e of
    ListCompr _ _   _  sts _  _   -> [ m | e' <- subExprs e, m <- check e' ]
                                     ++ concatMap checkStatement sts
    Do        _ sts _             -> [ m | e' <- subExprs e, m <- check e' ]
                                     ++ concatMap checkStatement sts
    Case      _ _   _ _    alts   -> [ m | e' <- subExprs e, m <- check e' ]
                                     ++ concatMap checkAlt alts
    _                             -> [ m | e' <- subExprs e, m <- check e' ]
  where
    check = expressionCheck

-- |Check Statement
checkStatement :: CheckF Statement
checkStatement s = case s of
  StmtExpr e       -> checkExpression e
  StmtDecl _ ds    -> concatMap checkDecl ds
  StmtBind _ pat e -> checkPattern pat ++ checkExpression e

-- |Check Alt
checkAlt :: CheckF Alt
checkAlt (Alt pat rhs) = checkPattern pat ++ checkRhs rhs


-- ----------------------------------------------------------------------------
-- Compute subterms for all data structures which are defined recursively
-- ----------------------------------------------------------------------------

-- |Sub-TypeExprs
subTypeExprs :: TypeExpr -> [TypeExpr]
subTypeExprs te = te : case te of
  ConstructorType _             -> []
  ApplyType       te1 te2       -> concatMap subTypeExprs [te1, te2]
  VariableType    _             -> []
  TupleType       _   tes _   _ -> concatMap subTypeExprs tes
  ListType        _   te1 _     -> subTypeExprs te1
  ArrowType       te1 _   te2   -> concatMap subTypeExprs [te1, te2]
  ParenType       _   te1 _     -> subTypeExprs te1

-- |Sub-Lhs
subLhs :: Lhs -> [Lhs]
subLhs lhs = lhs : case lhs of
  ApLhs  lhs1 _       -> subLhs lhs1
  _                   -> []

-- |Sub-Patterns
subPattern :: Pattern -> [Pattern]
subPattern p = p : case p of
  LiteralPattern     _              -> []
  NegativePattern    _  _           -> []
  VariablePattern    _              -> []
  ConstructorPattern _  ps          -> concatMap subPattern ps
  InfixPattern       p1  _ p2       -> concatMap subPattern [p1, p2]
  ParenPattern       _  p1 _        -> subPattern p1
  RecordPattern      _  _  fps _ _  -> concatMap subFieldPat fps
    where subFieldPat (Field _ _ p1) = subPattern p1
  TuplePattern       _  ps _   _    -> concatMap subPattern ps
  ListPattern        _  ps _   _    -> concatMap subPattern ps
  AsPattern          _  _  p1       -> subPattern p1
  LazyPattern        _  p1          -> subPattern p1
  FunctionPattern    _  ps          -> concatMap subPattern ps
  InfixFuncPattern   p1 _  p2       -> concatMap subPattern [p1, p2]

-- |Sub-Expressions
subExprs :: Expression -> [Expression]
subExprs e = e : case e of
  Literal        _                  -> []
  Variable       _                  -> []
  Constructor    _                  -> []
  Paren          _  e1    _         -> subExprs e1
  Typed          e1 _  _            -> subExprs e1
  Record         _  _  fs _  _      -> concatMap subFieldExprs fs
  RecordUpdate   e1 _  fs _  _      -> subExprs e1 ++ concatMap subFieldExprs fs
  Tuple          _  es _  _         -> concatMap subExprs es
  List           _  es _  _         -> concatMap subExprs es
  ListCompr      _  e1 _  _  _ _    -> subExprs e1
  EnumFrom       _  e1 _  _         -> subExprs e1
  EnumFromThen   _  e1 _  e2 _ _    -> concatMap subExprs [e1, e2]
  EnumFromTo     _  e1 _  e2 _      -> concatMap subExprs [e1, e2]
  EnumFromThenTo _  e1 _  e2 _ e3 _ -> concatMap subExprs [e1, e2, e3]
  UnaryMinus     _  e1              -> subExprs e1
  Apply          e1 e2              -> concatMap subExprs [e1, e2]
  InfixApply     e1 _  e2           -> concatMap subExprs [e1, e2]
  LeftSection    _  e1 _  _         -> subExprs e1
  RightSection   _  _  e2 _         -> subExprs e2
  Lambda         _  _  _  e1        -> subExprs e1
  Let            _  ds _  e1        -> (concatMap declExprs ds) ++ (subExprs e1)
  Do             _  _  e1           -> subExprs e1
  IfThenElse     _  e1 _  e2 _ e3   -> concatMap subExprs [e1, e2, e3]
  Case           _  _  e1 _  _      -> subExprs e1
 where
   declExprs d = case d of
     PatternDecl _ rhs -> rhsExprs rhs
     _                 -> []
   rhsExprs rhs = case rhs of
     SimpleRhs _ e1 _ _ -> subExprs e1
     _                  -> []
   subFieldExprs (Field _ _ e1) = subExprs e1
