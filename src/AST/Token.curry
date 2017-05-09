{- |
    Module      :  AST.Token
    Description :  Token

    This module contains the data structure for curry tokens.
-}
module AST.Token (Token (..)) where

type Ident = String

-- |Category of curry tokens
data Token
  -- literals
  = CharTok       Char
  | IntTok        Int
  | FloatTok      Float
  | StringTok     String

  -- identifiers
  | Id            Ident  -- identifier
  | QId           Ident  -- qualified identifier
  | Sym           String -- symbol
  | QSym          String -- qualified symbol

  -- punctuation symbols
  | LeftParen            -- (
  | RightParen           -- )
  | Semicolon            -- ;
  | LeftBrace            -- {
  | RightBrace           -- }
  | LeftBracket          -- [
  | RightBracket         -- ]
  | Comma                -- ,
  | Underscore           -- _
  | Backquote            -- `

  -- layout
  | LeftBraceSemicolon   -- {; (turn off layout)
  | VSemicolon           -- virtual ;
  | VRightBrace          -- virtual }

  -- reserved keywords
  | KW_case
  | KW_class
  | KW_data
  | KW_default
  | KW_deriving
  | KW_do
  | KW_else
  | KW_external
  | KW_fcase
  | KW_foreign
  | KW_free
  | KW_if
  | KW_import
  | KW_in
  | KW_infix
  | KW_infixl
  | KW_infixr
  | KW_instance
  | KW_let
  | KW_module
  | KW_newtype
  | KW_of
  | KW_then
  | KW_type
  | KW_where

  -- reserved operators
  | At                   -- @
  | Colon                -- :
  | DotDot               -- ..
  | DoubleColon          -- ::
  | Equals               -- =
  | Backslash            -- \
  | Bar                  -- |
  | LeftArrow            -- <-
  | RightArrow           -- ->
  | Tilde                -- ~
  | DoubleArrow          -- =>

  -- special identifiers
  | Id_as
  | Id_ccall
  | Id_forall
  | Id_hiding
  | Id_interface
  | Id_primitive
  | Id_qualified

  -- special operators
  | SymDot               -- .
  | SymMinus             -- -

  -- special symbols
  | SymStar              -- *

  -- pragmas
  | PragmaLanguage                      -- {-# LANGUAGE
  | PragmaOptions (Maybe String) String -- {-# OPTIONS
  | PragmaHiding                        -- {-# HIDING
  | PragmaMethod                        -- {-# METHOD
  | PragmaModule                        -- {-# MODULE
  | PragmaEnd                           -- #-}

  -- comments
  | LineComment   String
  | NestedComment String

  -- end-of-file token
  | EOF
 deriving (Eq, Show)
