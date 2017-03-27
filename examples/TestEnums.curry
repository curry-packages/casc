module Test.TestEnums where

--- Small examples of every possible enum for testing purposes

enumFrom = [1 ..]

enumFromThen = [1, 3 ..]

enumFromTo = [1 .. 10]

enumFromThenTo = [1, 4 .. 12]

listCompr = [(i,j) | i <- [1,2], j <- [1..4]]
