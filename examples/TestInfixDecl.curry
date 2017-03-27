module Test.TestInfixDecl where

--- Small example for AST.InfixDecl and AST.OpLhs

infixl 5 `fun`
infixr 1 `times`
infix *:*, `plus`

fun :: Int -> Int -> Int
x `fun`  y = x + y

(*:*) :: Int -> Int -> Int
x  *:*    y = x * y

plus :: Int -> Int -> Int
x `plus` y = x + y

times :: Int -> Int -> Int
x `times` y = x * y

