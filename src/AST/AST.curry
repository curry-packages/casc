{- |
    Module      :  AST
    Description :  Abstract Syntax Tree

    This module contains the description of the curry abstract syntax tree (AST)
    and useful functions on the elements of the AST.
-}
module AST.AST where

import Char (isAlphaNum)
import List (intercalate)

import AST.Span (Pos)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- |Simple identifier
data Ident = Ident
  { idPosition :: Pos      -- ^ Source code 'Position'
  , idName     :: String   -- ^ Name of the identifier
  , idUnique   :: Int      -- ^ Unique number of the identifier
  }
 deriving Show

-- |Qualified identifier
data QualIdent = QualIdent
  { qidModule :: Maybe ModuleIdent -- ^ optional module identifier
  , qidIdent  :: Ident             -- ^ identifier itself
  }
 deriving Show

-- | Module identifier
data ModuleIdent = ModuleIdent
  { midPosition   :: Pos      -- ^ source code 'Position'
  , midQualifiers :: [String] -- ^ hierarchical idenfiers
  }
 deriving Show

-- |Specified language extensions, either known or unknown.
data Extension
  = KnownExtension   Pos KnownExtension -- ^ a known extension
  | UnknownExtension Pos String         -- ^ an unknown extension
 deriving Show

data KnownExtension
  = AnonFreeVars              -- ^ anonymous free variables
  | FunctionalPatterns        -- ^ functional patterns
  | NegativeLiterals          -- ^ negative literals
  | NoImplicitPrelude         -- ^ no implicit import of the prelude
  | ExistentialQuantification -- ^ existential quantification
 deriving Show

-- |Different Curry tools which may accept compiler options.
data Tool = KICS2 | PAKCS | CYMAKE | UnknownTool String
 deriving Show


-- ---------------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------------

-- |Hierarchical module name
moduleName :: ModuleIdent -> String
moduleName = intercalate "." . midQualifiers

-- |Hierarchical name of qualified Ident
qidName :: QualIdent -> String
qidName q = case q of
  (QualIdent Nothing  i) -> idName i
  (QualIdent (Just m) i) -> intercalate "." [(moduleName m), (idName i)]

-- |Set position of ModuleIdent
setMIdPos :: Pos -> ModuleIdent -> ModuleIdent
setMIdPos p m = m { midPosition = p }

-- |Set position of Ident
setIdPos :: Pos -> Ident -> Ident
setIdPos p i = i { idPosition = p }

-- |Set position of QualIdent
setQIdPos :: Pos -> QualIdent -> QualIdent
setQIdPos p q = q { qidIdent = setIdPos p (qidIdent q) }

-- |Check whether an 'Ident' identifies an infix operation
isInfixOp :: Ident -> Bool
isInfixOp (Ident _ s _) = all (`elem` "~!@#$%^&*+-=<>:?./|\\") s


-- ---------------------------------------------------------------------------
-- Definition of AST: Module
-- ---------------------------------------------------------------------------

-- |Curry module
data Module = Module [ModulePragma] ModuleIdent (Maybe ExportSpec)
                     [ImportDecl] [Decl]
 deriving Show

-- |Module pragma
data ModulePragma
  = LanguagePragma Pos [Extension]
  | OptionsPragma  Pos (Maybe Tool) String
 deriving Show

-- |Export specification
data ExportSpec = Exporting Pos [Export]
 deriving Show

-- |Single exported entity
data Export
  = Export         QualIdent
  | ExportTypeWith QualIdent [Ident]
  | ExportTypeAll  QualIdent
  | ExportModule   ModuleIdent
 deriving Show

-- |Import declaration
data ImportDecl = ImportDecl Pos ModuleIdent Qualified
                             (Maybe ModuleIdent) (Maybe ImportSpec)
 deriving Show

-- |Flag to signal qualified import
type Qualified = Bool

-- |Import specification
data ImportSpec
  = Importing Pos [Import]
  | Hiding    Pos [Import]
 deriving Show

-- |Single imported entity
data Import
  = Import         Ident
  | ImportTypeWith Ident [Ident]
  | ImportTypeAll  Ident
 deriving Show

-- ---------------------------------------------------------------------------
-- Declarations (local or top-level)
-- ---------------------------------------------------------------------------

-- |Declaration in a module
data Decl
  = InfixDecl    Pos Infix (Maybe Precedence) [Ident]
  | DataDecl     Pos Ident [Ident] [ConstrDecl] [QualIdent]
  | NewtypeDecl  Pos Ident [Ident] NewConstrDecl [QualIdent]
  | TypeDecl     Pos Ident [Ident] TypeExpr
  | TypeSig      Pos [Ident] QualTypeExpr
  | FunctionDecl Pos Ident [Equation]
  | ForeignDecl  Pos CallConv (Maybe String) Ident TypeExpr
  | ExternalDecl Pos [Ident]
  | PatternDecl  Pos Pattern Rhs
  | FreeDecl     Pos [Ident]
  | DefaultDecl  Pos [TypeExpr]
  | ClassDecl    Pos Context Ident Ident [Decl]
  | InstanceDecl Pos Context QualIdent InstanceType [Decl]
 deriving Show

-- ---------------------------------------------------------------------------
-- Infix declaration
-- ---------------------------------------------------------------------------

-- |Operator precedence
type Precedence = Int

-- |Fixity of operators
data Infix
  = InfixL -- ^ left-associative
  | InfixR -- ^ right-associative
  | Infix  -- ^ no associativity
 deriving Show

-- |Constructor declaration for algebraic data types
data ConstrDecl
  = ConstrDecl Pos [Ident] Context Ident [TypeExpr]
  | ConOpDecl  Pos [Ident] Context TypeExpr Ident TypeExpr
  | RecordDecl Pos [Ident] Context Ident [FieldDecl]
 deriving Show

-- |Constructor declaration for renaming types (newtypes)
data NewConstrDecl
  = NewConstrDecl Pos Ident TypeExpr
  | NewRecordDecl Pos Ident (Ident, TypeExpr)
 deriving Show

-- |Declaration for labelled fields
data FieldDecl = FieldDecl Pos [Ident] TypeExpr
 deriving Show

-- |Calling convention for C code
data CallConv
  = CallConvPrimitive
  | CallConvCCall
 deriving Show

-- |Type expressions
data TypeExpr
  = ConstructorType QualIdent
  | ApplyType       TypeExpr TypeExpr
  | VariableType    Ident
  | TupleType       [TypeExpr]
  | ListType        TypeExpr
  | ArrowType       TypeExpr TypeExpr
  | ParenType       TypeExpr
 deriving Show

-- |Qualified type expressions
data QualTypeExpr = QualTypeExpr Context TypeExpr
 deriving Show

-- ---------------------------------------------------------------------------
-- Type classes
-- ---------------------------------------------------------------------------

type Context = [Constraint]

data Constraint = Constraint QualIdent TypeExpr
 deriving Show

type InstanceType = TypeExpr

-- ---------------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------------

-- |Equation
data Equation = Equation Pos Lhs Rhs
 deriving Show

-- |Left-hand-side of an `Equation` (function identifier and patterns)
data Lhs
  = FunLhs Ident [Pattern]
  | OpLhs  Pattern Ident Pattern
  | ApLhs  Lhs [Pattern]
 deriving Show

-- |Right-hand-side of an `Equation`
data Rhs
  = SimpleRhs  Pos Expression [Decl]
  | GuardedRhs [CondExpr] [Decl]
 deriving Show

-- |Conditional expression (expression conditioned by a guard)
data CondExpr = CondExpr Pos Expression Expression
 deriving Show

-- |Literal
data Literal
  = Char   Char
  | Int    Int
  | Float  Float
  | String String
 deriving Show

-- |Constructor term (used for patterns)
data Pattern
  = LiteralPattern     Literal
  | NegativePattern    Literal
  | VariablePattern    Ident
  | ConstructorPattern QualIdent [Pattern]
  | InfixPattern       Pattern QualIdent Pattern
  | ParenPattern       Pattern
  | RecordPattern      QualIdent [Field Pattern]
  | TuplePattern       [Pattern]
  | ListPattern        [Pattern]
  | AsPattern          Ident Pattern
  | LazyPattern        Pattern
  | FunctionPattern    QualIdent [Pattern]
  | InfixFuncPattern   Pattern QualIdent Pattern
 deriving Show

-- |Expression
data Expression
  = Literal           Literal
  | Variable          QualIdent
  | Constructor       QualIdent
  | Paren             Expression
  | Typed             Expression QualTypeExpr
  | Record            QualIdent [Field Expression]
  | RecordUpdate      Expression [Field Expression]
  | Tuple             [Expression]
  | List              [Expression]
  | ListCompr         Expression [Statement]
  | EnumFrom          Expression
  | EnumFromThen      Expression Expression
  | EnumFromTo        Expression Expression
  | EnumFromThenTo    Expression Expression Expression
  | UnaryMinus        Expression
  | Apply             Expression Expression
  | InfixApply        Expression InfixOp Expression
  | LeftSection       Expression InfixOp
  | RightSection      InfixOp Expression
  | Lambda            [Pattern] Expression
  | Let               [Decl] Expression
  | Do                [Statement] Expression
  | IfThenElse        Expression Expression Expression
  | Case              CaseType Expression [Alt]
 deriving Show

-- |Infix operation
data InfixOp
  = InfixOp     QualIdent
  | InfixConstr QualIdent
 deriving Show

-- |Statement (used for do-sequence and list comprehensions)
data Statement
  = StmtExpr Expression
  | StmtDecl [Decl]
  | StmtBind Pattern Expression
 deriving Show

-- |Type of case expressions
data CaseType
  = Rigid
  | Flex
 deriving Show

-- |Single case alternative
data Alt = Alt Pos Pattern Rhs
 deriving Show

-- |Record field
data Field a = Field Pos QualIdent a
 deriving Show
