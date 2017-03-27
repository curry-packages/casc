--- File for testing whether the order of functions
--- and data declaration affects the extension or the checks.

module Test.TestOrder where

f x = 3


data C a = C a


null []    = True
g    x     = x
null (_:_) = False
