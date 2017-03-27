--- Small examples of every possible AST.Pattern for testing purposes

module Test.TestPatterns where

-- Literal pattern
a 3 = 3

-- Negative pattern
b (-3) = -3

-- Variable pattern
c x = 5

-- Constructor pattern
d True = 1

-- Infix pattern
-- ...

-- Paren pattern
f (x) = x

-- Record pattern
data C = C { l1, l3 :: Int, l2     :: Int }


g C{l1 = e1, l2 = e2} = 1

-- Tuple pattern
h (x, y) = 3

-- List pattern
i [x, y] = 5

-- As pattern
j a@(b, c) = a

-- Lazy pattern
k ~p = 3
