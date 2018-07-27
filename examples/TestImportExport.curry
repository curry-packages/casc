module Test.TestImportExport ((+*)) where

import System.Console.GetOpt (ArgOrder (..), ArgDescr (NoArg, ReqArg))
import Data.List   ((\\), intercalate)
import Data.Maybe


(+*) x y = ( x + y ) + ( x * y)

f3 x = 3 * x
