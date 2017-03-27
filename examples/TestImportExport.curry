module Test.TestImportExport ((+*)) where

import GetOpt (ArgOrder (..), ArgDescr (NoArg, ReqArg))
import List   ((\\), intercalate)
import Maybe


(+*) x y = ( x + y ) + ( x * y)

f3 x = 3 * x
