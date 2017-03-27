{- |
    Module      :  AutoCorr.AutoCorrPosAST
    Description :  Automatic correction of PosAST

    This module contains an incomplete tree traversal for reorganizing
    a PosAST. All functions follow the scheme
      `correctTerm :: BrokenTerm -> CorrectTerm`
    This module should be expanded for a full tree traversal and for
    the autocorrection of more expressions and elements.
    Note: If you run casc with a autocorrected .cy-file, it will still
          show the error messages because it during the extension it gets it's
          positions from the tokenstream. The "wrong" positions are still
          in there because the corrected AST doesn't get pretty printed
          into source code yet.
-}
module AutoCorr.AutoCorrPosAST (correctModule) where

import AST.SpanAST
import AST.PositionUtils (col, line, relocate)
import AST.Span          (Pos, start, end)
import Config.ReadConfig (shallCorrect)
import Config.Types      (Check (..))

-- |Traverse PosAST starting with node Module
correctModule :: Module -> Module
correctModule (Module mps mp1 mi mp2 mesp ids ds)
  = Module mps mp1 mi mp2 mesp ids (map correctDecl ds)

-- |Traverse PosAST, node Decl
correctDecl :: Decl -> Decl
correctDecl d =
  case d of
       FunctionDecl i   eqs -> FunctionDecl i   (map correctEquation eqs)
       PatternDecl  pat rhs -> PatternDecl  pat (correctRhs rhs)
       _                    -> d

-- |Traverse PosAST, node Equation
correctEquation :: Equation -> Equation
correctEquation (Equation lhs rhs) = Equation lhs (correctRhs rhs)

-- |Traverse PosAST, node Rhs
correctRhs :: Rhs -> Rhs
correctRhs rhs = case rhs of
  SimpleRhs p e mp ds -> SimpleRhs p (correctExpression e) mp ds
  _                   -> rhs

-- |Correct Expression
correctExpression :: Expression -> Expression
correctExpression expr =
  case expr of
    Let sl ds si e
      -> if (shallCorrect CLet)
           then Let sl  (map correctDecl ds)
                    si' (correctExpression e)
           else Let sl  (map correctDecl ds)
                    si  (correctExpression e)
      where pi  = start si
            pl  = start sl
            si' = ((line pi, col pl), end si)
    IfThenElse si i st t se e
      -> if (shallCorrect CIfThenElseKW)
           then IfThenElse si  (correctExpression i)
                           st' (correctExpression t)
                           se' (correctExpression e)
           else IfThenElse si  (correctExpression i)
                           st  (correctExpression t)
                           se  (correctExpression e)
       where pt = start st
             pe = start se
             st' = ((line pt, col pt + 2), end st)
             se' = ((line pe, col pt + 2), end se)
    _ -> expr
