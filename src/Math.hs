module Math where

import Parser

data PolyExpr = PolyExprOp Op PolyExpr PolyExpr
              | PolyExprFn Fn PolyExpr
              | PolyExprValue { coeff :: Float
                              , order :: Int
                              }

instance Show PolyExpr where
  show (PolyExprOp op e1 e2) = "(" ++ show op ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (PolyExprFn fn e)     = "(" ++ show fn ++ " " ++ show e ++ ")"
  show (PolyExprValue c o)   = show c ++ "x^" ++ show o

convertTreeToPolyTree = convert'
  where convert' (ExprOp op e1 e2)        = PolyExprOp op (convert' e1) (convert' e2)
        convert' (ExprFn fn e)            = PolyExprFn fn (convert' e)
        convert' (ExprValue (Numeric v))  = PolyExprValue v 0
        convert' (ExprValue (Variable _)) = PolyExprValue 1 1

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

solveLinEq = id

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
