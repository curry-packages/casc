{- |
    Module      :  Check.Pattern
    Description :  Check and validation for Pattern (incomplete)

    This module contains a template for the check function
    and the validation functions for Patterns.
    This module should be expanded to be able to check patterns
    e.g. for the alignment of their arguments.
-}
module Check.Pattern (patternCheck) where

import Check.Types (CheckF)

-- ----------------------------------------------------------------------------
-- Check Function
-- ----------------------------------------------------------------------------

-- |Check Pattern
patternCheck :: CheckF Pattern
patternCheck _ = []


-- ----------------------------------------------------------------------------
-- Validation Functions
-- ----------------------------------------------------------------------------

-- |Valid if ...
validPattern :: Pos -> Bool
validPattern = error "PatternCheck: not implemented yet!"
