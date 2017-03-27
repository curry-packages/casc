module Test.Expressions where



-- This is a very long comment line ................................................
testLet :: Int
testLet
  = let x   = 1
        y    = 2
        z   = 3
      in  x + y + z


greater4 :: Int -> Bool
greater4 x = if x > 4
               then True
                else False




