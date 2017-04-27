module Math where

import ExprTree
import Parser

-- | Definition of a polynomial term.
data PolyTerm = PolyTerm [(Int, Float)]

instance Show PolyTerm where
  show (PolyTerm xs) = "p<" ++ show' xs ++ ">"
    where show' [] = ""
          show' ((o,c):xs) =
            let s = show c ++ (if o == 0 then "0" else "x" ++ (if o == 1 then "" else "^" ++ show o))
            in s ++ (if null xs then "" else " + " ++ show' xs)

-- | Definition of a polynomial expression tree.
type PolyExpr = ExprTree PolyTerm

convertTreeToPolyTree :: Expr -> PolyExpr
convertTreeToPolyTree = fmap convert'
  where convert' (Numeric v)  = PolyTerm [(0, 1)]
        convert' (Variable _) = PolyTerm [(1, 1)]

evaluateExpr (ExprValue (Numeric v)) = v
evaluateExpr (ExprOp op e1 e2) = let v1 = evaluateExpr e1
                                     v2 = evaluateExpr e2
                                 in eval' op v1 v2
  where eval' Add v1 v2 = v1 + v2
        eval' Sub v1 v2 = v1 - v2
        eval' Mul v1 v2 = v1 * v2
        eval' Div v1 v2 = v1 / v2
evaluateExpr (ExprFn fn e) = let v = evaluateExpr e
                             in eval' fn v
  where eval' Log v = (log v) / (log 10)
        eval' Ln v  = log v

solveLinEq (ExprOp Eq lhs rhs) =
  let expr = ExprOp Sub lhs rhs
  in expr

processMathExpression s =
  case parse s of
    Left (ParseError msg) -> "Error: " ++ msg
    Right expr ->
      case expr of
        ExprOp Eq _ _ -> show $ solveLinEq $ convertTreeToPolyTree expr
        _             -> show $ evaluateExpr expr


------------------------------------------------------------

ex1 = "(3+(4-1))*5"
ex2 = "2 * x + 0.5 = 1"
ex3 = "2x + 1 = 2(1-x)"
