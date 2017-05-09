{- |
    Module      :  AST.PositionUtils
    Description :  Auxiliary functions for positions

    This module provides some auxiliary functions concerning positions.
-}
module AST.PositionUtils where

import qualified AST.Ident     as I
import           AST.Span (Pos, Span, start, virtualPos)
import           AST.SpanAST
import           AST.Token

-- |Return the line of a position
line :: Pos -> Int
line pos = fst pos

-- |Return the column of a position
col :: Pos -> Int
col pos = snd pos

-- |Check whether the columns in a list of positions are all equal
allColEq :: [Pos] -> Bool
allColEq xs = case xs of
  [] -> True
  _  -> all (== col (head xs)) (map col (tail xs))

-- |Check whether the lines in a list of positions are all equal
allLinesEq :: [Pos] -> Bool
allLinesEq xs = case xs of
  [] -> True
  _  -> all (== line (head xs)) (map line (tail xs))

-- |move given position by n columns
moveColBy :: Pos -> Int -> Pos
moveColBy (l,c) n = (l, c + n)

--- |Change a position by a delta
--- @param l  -  line
--- @param c  -  column
--- @param ml -  delta line
--- @param mc -  delta column
--- @return   -  input position changed by delta line and delta column

relocate :: Pos -> (Int, Int) -> Pos
relocate (l, c) (ml, mc) = (l + ml, c + mc)

-- ----------------------------------------------------------------------------
-- Get positions of AST elements
-- ----------------------------------------------------------------------------

-- |Get start position of QualIdent
qidPos :: I.QualIdent -> Pos
qidPos (I.QualIdent _ i) = idPos i

-- |Get start position of Ident
idPos :: I.Ident -> Pos
idPos (I.Ident sp _ _) = start sp

-- |Get start position of Ident that might be surrounded by
-- |some kind of symbols, e.g. parens or backticks
sidPos :: I.SymIdent -> Pos
sidPos (I.SymIdent mpl i _) = case mpl of
  Just pl -> start pl
  Nothing -> idPos i

-- |Get start position of QualIdent that might be surrounded by
-- |some kind of symbols, e.g. parens or backticks
sqidPos :: I.SymQualIdent -> Pos
sqidPos (I.SymQualIdent mpl qi _) = case mpl of
  Just pl -> start pl
  Nothing -> qidPos qi

-- |Get start position of Export
exportPos :: Export -> Pos
exportPos e = case e of
  Export         qi          -> sqidPos qi
  ExportTypeWith qi _  _ _ _ -> qidPos  qi
  ExportTypeAll  qi _  _ _   -> qidPos  qi
  ExportModule   _  mi       -> start (I.midSpan mi)

-- |Get start position of Decl
declPos :: Decl -> Pos
declPos d = case d of
  PatternDecl pat _ -> patPos pat
  _                 -> virtualPos

-- |Get position of equality sign in Decl
declPosEq :: Decl -> Pos
declPosEq d = case d of
  PatternDecl _ rhs -> rhsPos rhs
  _                 -> virtualPos

-- |Get position of Infix Keyword
infPos :: Infix -> Pos
infPos i = case i of
  InfixL s -> start s
  InfixR s -> start s
  Infix  s -> start s

-- |Get start position of ConstrDecl
constrDeclPos :: ConstrDecl -> Pos
constrDeclPos c = case c of
  ConstrDecl s _ _ _ _ i _        -> maybe (idPos i) start s
  ConOpDecl  s _ _ _ _ te _ _     -> maybe (typeExprPos te) start s
  RecordDecl s _ _ _ _ i  _ _ _ _ -> maybe (idPos i) start s

-- |Get Type positions of ConstrDecl
constrDeclConstrTypePos :: ConstrDecl -> [Pos]
constrDeclConstrTypePos cd = case cd of
  ConstrDecl _ _ _ _ _ _ cts       -> if null cts
                                        then [virtualPos]
                                        else map typeExprPos cts
  RecordDecl _ _ _ _ _ _ _ fds _ _ -> if null fds
                                        then [virtualPos]
                                        else map fieldDeclTEPos fds
  _                                -> [virtualPos]

-- |Get position of first Type in ConstrDecl
firstCDConstrTypePos :: ConstrDecl -> Pos
firstCDConstrTypePos cd = case cd of
  ConstrDecl _ _ _ _ _ _  cts -> if null cts
                                   then virtualPos
                                   else typeExprPos $ head cts
  _                           -> virtualPos

-- |Get position of commas in FieldDecl
fieldDeclCPos :: FieldDecl -> [Pos]
fieldDeclCPos (FieldDecl _ sps _ _) = map start sps

-- | Get position of doublecolon in FieldDecl
fieldDeclDCPos :: FieldDecl -> Pos
fieldDeclDCPos (FieldDecl _ _ sp _) = start sp

-- |Get position of first constructor in FieldDecl
fieldDeclIDPos :: FieldDecl -> Pos
fieldDeclIDPos (FieldDecl ids _ _ _) = idPos $ head ids

-- |Get position of TypeExpression in FieldDecl
fieldDeclTEPos :: FieldDecl -> Pos
fieldDeclTEPos (FieldDecl _ _ _ te) = typeExprPos te

-- |Get start position of TypeExpr
typeExprPos :: TypeExpr -> Pos
typeExprPos te = case te of
  ConstructorType qi         -> qidPos qi
  ApplyType       te1 _      -> typeExprPos te1
  VariableType    i          -> idPos i
  TupleType       sp   _ _ _ -> start sp
  ListType        sp   _ _   -> start sp
  ArrowType       te1 _ _    -> typeExprPos te1
  ParenType       sp   _ _   -> start sp

-- |Get position of equality sign or rightarrow in Rhs
rhsPos :: Rhs -> Pos
rhsPos rhs = case rhs of
  SimpleRhs  sp _ _ _   -> start sp
  GuardedRhs sp _ _ _ _ -> start sp

-- |Get start position of Lhs
lhsPos :: Lhs -> Pos
lhsPos lhs = case lhs of
  FunLhs si _   -> sidPos si
  OpLhs  p  _ _ -> patPos p
  ApLhs  l  _   -> lhsPos l

-- |Get start position of CondExpr
condExprPos :: CondExpr -> Pos
condExprPos (CondExpr _ sp _) = start sp

-- |Get start position of Literal
litPos :: Literal -> Pos
litPos l = case l of
  Char   sp _ -> start sp
  Int    sp _ -> start sp
  Float  sp _ -> start sp
  String sp _ -> start sp

-- |Get start position of Pattern
patPos :: Pattern -> Pos
patPos p = case p of
  LiteralPattern     l             -> litPos l
  NegativePattern    sp _          -> start sp
  VariablePattern    i             -> idPos  i
  ConstructorPattern qi  _         -> qidPos qi
  InfixPattern       pat _   _     -> patPos pat
  ParenPattern       _   pat _     -> patPos pat
  RecordPattern      qi  _   _ _ _ -> qidPos qi
  TuplePattern       spl  _   _ _  -> start spl
  ListPattern        spl  _   _ _  -> start spl
  AsPattern          i   _   _     -> idPos  i
  LazyPattern        sp  _         -> start sp
  FunctionPattern    qi  _         -> qidPos qi
  InfixFuncPattern   pat _   _     -> patPos pat

-- |Get start position of Expression
exprPos :: Expression -> Pos
exprPos e = case e of
  Literal        l               -> litPos   l
  Variable       v               -> sqidPos  v
  Constructor    qi              -> sqidPos  qi
  Paren          spl _ _         -> start spl
  Typed          e1 _ _          -> exprPos  e1
  Record         qi _ _ _ _      -> qidPos   qi
  RecordUpdate   e1 _ _ _ _      -> exprPos  e1
  Tuple          spl _ _ _       -> start spl
  List           spl _ _ _       -> start spl
  ListCompr      spl _ _ _ _ _   -> start spl
  EnumFrom       spl _ _ _       -> start spl
  EnumFromThen   spl _ _ _ _ _   -> start spl
  EnumFromTo     spl _ _ _ _     -> start spl
  EnumFromThenTo spl _ _ _ _ _ _ -> start spl
  UnaryMinus     sp _            -> start sp
  Apply          e1 _            -> exprPos  e1
  InfixApply     e1 _ _          -> exprPos  e1
  LeftSection    spl _ _ _       -> start spl
  RightSection   spl _ _ _       -> start spl
  Lambda         sp  _ _ _       -> start sp
  Let            sp  _ _ _       -> start sp
  Do             sp  _ _         -> start sp
  IfThenElse     spi _ _ _ _ _   -> start spi
  Case           _  sp _ _ _     -> start sp

-- |Get start position of Statement
stmtPos :: Statement -> Pos
stmtPos s = case s of
  StmtExpr       e -> exprPos e
  StmtDecl     sp _ -> start sp
  StmtBind _ pat _ -> patPos pat

-- |Get position of rightarrow in Alt
altPos :: Alt -> Pos
altPos (Alt _ rhs) = rhsPos rhs
