{- |
    Module      :  AST.SpanAST
    Description :  Extension of Abstract Syntax Tree

    This module contains the description of the AST which has been
    extended by span information (i.e. start and end positions) (SpanAST).
    The inline comments document for which tokens span information was added.
-}
module AST.SpanAST where

import AST.AST   (KnownExtension (..), Tool (..))
import AST.Ident (ModuleIdent, Ident, QualIdent, SymIdent, SymQualIdent)
import AST.Span  (Span)

-- ---------------------------------------------------------------------------
-- Modules
-- ---------------------------------------------------------------------------

-- |Curry module
--  1. (Maybe Span) -  KW_module
--  2. (Maybe Span) -  KW_where
data Module = Module [ModulePragma] (Maybe Span) ModuleIdent (Maybe Span)
                     (Maybe ExportSpec) [ImportDecl] [Decl]
 deriving Show

-- |Module pragma
data ModulePragma
  -- 1. Span   -  PragmaLanguage
  -- 2. [Span] -  Commas
  -- 3. Span   -  PragmaEnd
  = LanguagePragma Span [Extension] [Span] Span
  -- 1. Span   - PragmaOptions
  -- 2. Span   - PragmaEnd
  | OptionsPragma  Span (Maybe String) String Span
 deriving Show

-- |Specified language extensions, either known or unknown.
data Extension
  = KnownExtension   Span KnownExtension   -- 1. Span  -  Id "KnownExtension"
  | UnknownExtension Span String           -- 1. Span  -  Id "Name"
 deriving Show

-- |Export specification
-- 1. Span   -  LeftParen
-- 2. [Span] -  Commas
-- 3. Span   -  RightParen
data ExportSpec = Exporting Span [Export] [Span] Span
 deriving Show

-- |Single exported entity
data Export
  = Export         SymQualIdent
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  | ExportTypeWith QualIdent Span [Ident] [Span] Span
  -- 1. Span   -  LeftParen
  -- 2. Span   -  DotDot
  -- 3. Span   -  RightParen
  | ExportTypeAll  QualIdent Span Span Span
  -- 1. Span   -  KW_module
  | ExportModule   Span ModuleIdent
 deriving Show

-- |Import declaration
-- 1. Span         -  KW_import
-- 2. (Maybe Span) -  Id_qualified
-- 3. (Maybe Span) -  Id_as
data ImportDecl = ImportDecl Span (Maybe Span) ModuleIdent (Maybe Span) Qualified
                             (Maybe ModuleIdent) (Maybe ImportSpec)
 deriving Show

-- |Flag to signal qualified import
type Qualified = Bool

-- |Import specification
data ImportSpec
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  = Importing Span [Import] [Span] Span
  -- 1. Span   -  Id_hiding
  -- 2. Span   -  LeftParen
  -- 3. [Span] -  Commas
  -- 4. Span   -  RightParen
  | Hiding    Span Span [Import] [Span] Span
 deriving Show

-- |Single imported entity
data Import
  = Import         SymIdent
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  | ImportTypeWith Ident Span [Ident] [Span] Span
  -- 1. Span   -  LeftParen
  -- 2. Span   -  DotDot
  -- 3. Span   -  RightParen
  | ImportTypeAll  Ident Span Span Span
 deriving Show

-- ---------------------------------------------------------------------------
-- Declarations (local or top-level)
-- ---------------------------------------------------------------------------

-- |Declaration in a module
data Decl
  -- 1. [Span] -  Backquotes
  = InfixDecl    Infix (Maybe Precedence) [SymIdent] [Span]
  -- 1. Span         -  KW_data
  -- 2. (Maybe Span) -  Equals
  -- 3. [Span]       -  Bars
  -- 4. (Maybe Span) -  KW_deriving
  -- 5. (Maybe Span) -  LeftParen
  -- 6. [Span]       -  Commas
  -- 7. (Maybe Span) -  RightParen
  | DataDecl     Span Ident [Ident] (Maybe Span) [ConstrDecl] [Span] (Maybe Span) (Maybe Span) [QualIdent] [Span] (Maybe Span)
  -- 1. Span         -  KW_newtype
  -- 2. Span         -  Equals
  -- 3. (Maybe Span) -  KW_deriving
  -- 4. (Maybe Span) -  LeftParen
  -- 5. [Span]       -  Commas
  -- 6. (Maybe Span) -  RightParen
  | NewtypeDecl  Span Ident [Ident] Span NewConstrDecl (Maybe Span) (Maybe Span) [QualIdent] [Span] (Maybe Span)
  -- 1. Span   -  KW_type
  -- 2. Span   -  Equals
  | TypeDecl     Span Ident [Ident] Span TypeExpr
  -- 1. [Span] - Commas
  -- 2. Span   - DotDot
  | TypeSig      [SymIdent] [Span] Span QualTypeExpr
  | FunctionDecl Ident [Equation]
  -- 1. Span                   -  KW_foreign
  -- 2. (Maybe (Span, String)) -  Id "name.h"
  -- 3. Span                   -  DotDot
  | ForeignDecl  Span CallConv (Maybe (Span, String)) SymIdent Span TypeExpr
  -- 1. [Span] -  Commas
  -- 2. Span   -  KW_external
  | ExternalDecl [SymIdent] [Span] Span
  | PatternDecl  Pattern Rhs
  -- 1. [Span] -  Commas
  -- 2. Span   -  KW_free
  | FreeDecl     [Ident] [Span] Span
  -- 1. Span   -  KW_default
  -- 2. Span   -  LeftParen
  -- 3. [Span] -  Commas
  -- 4. Span   -  RightParen
  | DefaultDecl  Span Span [TypeExpr] [Span] Span
  -- 1. Span         -  KW_class
  -- 2. (Maybe Span) -  DoubleArrow
  -- 3. (Maybe Span) -  KW_where
  | ClassDecl    Span Context (Maybe Span) Ident Ident (Maybe Span) [Decl]
  -- 1. Span         -  KW_instance
  -- 2. (Maybe Span) -  DoubleArrow
  -- 3. (Maybe Span) -  KW_where
  | InstanceDecl Span Context (Maybe Span) QualIdent InstanceType (Maybe Span) [Decl]
 deriving Show

-- |Operator precedence
type Precedence = (Span, Int)

-- |Fixity of operators
data Infix
  = InfixL Span   -- 1. Span -  KW_InfixL
  | InfixR Span   -- 1. Span -  KW_InfixR
  | Infix  Span   -- 1. Span -  KW_Infix
 deriving Show

-- |Constructor declaration for algebraic data types
data ConstrDecl
  -- 1. (Maybe Span) -  Id_forall
  -- 2. (Maybe Span) -  Dot
  -- 3. (Maybe Span) -  DoubleArrow
  = ConstrDecl (Maybe Span) [Ident] (Maybe Span)  Context (Maybe Span) Ident [TypeExpr]
  -- 1. (Maybe Span) -  Id_forall
  -- 2. (Maybe Span) -  Dot
  -- 3. (Maybe Span) -  DoubleArrow
  | ConOpDecl  (Maybe Span) [Ident] (Maybe Span) Context (Maybe Span) TypeExpr Ident TypeExpr
  -- 1. (Maybe Span) -  Id_forall
  -- 2. (Maybe Span) -  Dot
  -- 3. (Maybe Span) -  DoubleArrow
  -- 4. Span         -  LeftBrace
  -- 5. [Span]       -  Commas
  -- 6. Span         -  RightBrace
  | RecordDecl (Maybe Span) [Ident] (Maybe Span) Context (Maybe Span) Ident Span [FieldDecl] [Span] Span
 deriving Show

-- |Constructor declaration for renaming types (newtypes)
data NewConstrDecl
  = NewConstrDecl Ident TypeExpr
  -- 1. Span -  LeftBrace
  -- 2. Span -  DotDot
  -- 3. Span -  RightBrace
  | NewRecordDecl Ident Span (Ident, Span, TypeExpr) Span
 deriving Show

-- |Declaration for labelled fields
-- 1. [Span] -  Commas
-- 2. Span   -  DotDot
data FieldDecl = FieldDecl [Ident] [Span] Span TypeExpr
 deriving Show

-- |Calling convention for C code
data CallConv
  = CallConvPrimitive Span    -- 1. Span -  Id_primitive
  | CallConvCCall     Span    -- 1. Span -  Id_ccall
 deriving Show

-- |Type expressions
data TypeExpr
  = ConstructorType QualIdent
  | ApplyType       TypeExpr TypeExpr
  | VariableType    Ident
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  | TupleType       Span [TypeExpr] [Span] Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  RightBracket
  | ListType        Span TypeExpr Span
  -- 1. Span   - RightArrow
  | ArrowType       TypeExpr Span TypeExpr
  -- 1. Span   - LeftParen
  -- 2. Span   - RightParen
  | ParenType       Span TypeExpr Span
 deriving Show

-- |Qualified type expressions
-- 1. (Maybe Span) -  DoubleArrow
data QualTypeExpr = QualTypeExpr Context (Maybe Span) TypeExpr
 deriving Show

-- ---------------------------------------------------------------------------
-- Type classes
-- ---------------------------------------------------------------------------

-- 1. (Maybe Span) -  LeftParen
-- 2. [Span]       -  Commas
-- 3. (Maybe Span) -  RightParen
data Context = Context (Maybe Span) [Constraint] [Span] (Maybe Span)
 deriving Show

data Constraint = Constraint QualIdent TypeExpr
 deriving Show

type InstanceType = TypeExpr

-- ---------------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------------

-- |Function defining equation
data Equation = Equation Lhs Rhs
 deriving Show

-- |Left-hand-side of an 'Equation' (function identifier and patterns)
data Lhs
  = FunLhs SymIdent [Pattern]
  | OpLhs  Pattern SymIdent Pattern
  | ApLhs  Lhs [Pattern]
 deriving Show

-- |Right-hand-side of an 'Equation'
data Rhs
  -- 1. Span         -  Equals or RightArrow
  -- 2. (Maybe Span) -  KW_where
  = SimpleRhs  Span Expression (Maybe Span) [Decl]
  -- 1. Span         -  First Bar
  -- 2. [Span]       -  more Bars
  -- 3. (Maybe Span) -  KW_where
  | GuardedRhs Span [CondExpr] [Span] (Maybe Span) [Decl]
 deriving Show

-- |Conditional expression (expression conditioned by a guard)
data CondExpr = CondExpr Expression Span Expression  -- 1. Span -  Equals
 deriving Show

-- |Literal
data Literal
  = Char   Span Char       -- 1. Span -  CharTok
  | Int    Span Int        -- 1. Span -  IntTok
  | Float  Span Float      -- 1. Span -  FloatTok
  | String Span String     -- 1. Span -  StringTok
 deriving Show

-- |Constructor term (used for patterns)
data Pattern
  = LiteralPattern     Literal
  -- 1. Span -  Minus
  | NegativePattern    Span Literal
  | VariablePattern    Ident
  | ConstructorPattern QualIdent [Pattern]
  | InfixPattern       Pattern QualIdent Pattern
  -- 1. Span   -  LeftPattern
  -- 2. Span   -  RightPattern
  | ParenPattern       Span Pattern Span
  -- 1. Span   -  LeftBrace
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBrace
  | RecordPattern      QualIdent Span [Field Pattern] [Span] Span
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  | TuplePattern       Span [Pattern] [Span] Span
  -- 1. Span   -  LeftBracket
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBracket
  | ListPattern        Span [Pattern] [Span] Span
  -- 1. Span   -  At
  | AsPattern          Ident Span Pattern
  -- 1. Span   -  Tilde
  | LazyPattern        Span Pattern
  | FunctionPattern    QualIdent [Pattern]
  | InfixFuncPattern   Pattern QualIdent Pattern
 deriving Show

-- |Expression
data Expression
  = Literal           Literal
  | Variable          SymQualIdent
  | Constructor       SymQualIdent
  -- 1. Span   -  LeftParen
  -- 2. Span   -  RightParen
  | Paren             Span Expression Span
  -- 1. Span   -  DotDot
  | Typed             Expression Span QualTypeExpr
  -- 1. Span   -  LeftBrace
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBrace
  | Record            QualIdent Span [Field Expression] [Span] Span
  -- 1. Span   -  LeftBrace
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBrace
  | RecordUpdate      Expression Span [Field Expression] [Span] Span
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  | Tuple             Span [Expression] [Span] Span
  -- 1. Span   -  LeftBracket
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBracket
  | List              Span [Expression] [Span] Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  Bar
  -- 3. [Span] -  Commas
  -- 4. Span   -  RightBracket
  | ListCompr         Span Expression Span [Statement] [Span] Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  DotDot
  -- 3. Span   -  RightBracket
  | EnumFrom          Span Expression Span Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  Comma
  -- 3. Span   -  DotDot
  -- 4. Span   -  RightBracket
  | EnumFromThen      Span Expression Span Expression Span Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  DotDot
  -- 3. Span   -  RightBracket
  | EnumFromTo        Span Expression Span Expression Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  Comma
  -- 3. Span   -  DotDot
  -- 4. Span   -  RightBracket
  | EnumFromThenTo    Span Expression Span Expression Span Expression Span
  -- 1. Span -  Minus
  | UnaryMinus        Span Expression
  | Apply             Expression Expression
  | InfixApply        Expression InfixOp Expression
  -- 1. Span   - LeftParen
  -- 2. Span   - RightParen
  | LeftSection       Span Expression InfixOp Span
  -- 1. Span   - LeftParen
  -- 2. Span   - RightParen
  | RightSection      Span InfixOp Expression Span
  -- 1. Span   - Backslash
  -- 2. Span   - RightArrow
  | Lambda            Span [Pattern] Span Expression
  -- 1. Span   - KW_let
  -- 2. Span   - KW_in
  | Let               Span [Decl] Span Expression
  -- 1. Span   - KW_do
  | Do                Span [Statement] Expression
  -- 1. Span   - KW_if
  -- 2. Span   - KW_then
  -- 3. Span   - KW_else
  | IfThenElse        Span Expression Span Expression Span Expression
  -- 1. Span   - KW_case
  -- 2. Span   - KW_of
  | Case              CaseType Span Expression Span [Alt]
 deriving Show

-- |Infix operation
data InfixOp
  = InfixOp     SymQualIdent
  | InfixConstr SymQualIdent
 deriving Show

-- |Statement
data Statement
  = StmtExpr Expression
  | StmtDecl Span [Decl]                -- 1. Span -  KW_let
  | StmtBind Span Pattern Expression    -- 1. Span -  LeftArrow
 deriving Show

-- |Type of case expressions
data CaseType
  = Rigid
  | Flex
 deriving Show

-- |Single case alternative
data Alt = Alt Pattern Rhs
 deriving Show

-- |Record field
data Field a = Field QualIdent Span a    -- 1. Span -  Equals
 deriving Show
