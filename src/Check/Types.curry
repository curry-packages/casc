{- |
    Module      :  Check.Types
    Description :  Types for Checks

    This module provides the data structure for Messages and a type for Checks.
-}
module Check.Types where

import AST.Span (Pos)

-- |A message contains
-- | * the position it refers to
-- | * a string describing the problem
data Message = Message Pos String

-- |Signature for check functions
type CheckF a = a -> [Message]
