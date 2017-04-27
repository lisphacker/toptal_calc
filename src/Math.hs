module Math
  ( processMathExpression
  ) where

import ExprTree
import Parser
import Data.List
import Debug.Trace

-- | Definition of a polynomial term.
data PolyTerm = PolyTerm { terms :: [(Int, Float)] }

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
  where convert' (Numeric v)  = PolyTerm [(0, v)]
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

matchPolyTerms ts1 ts2 =
  let ts1' = sortOn fst ts1
      ts2' = sortOn fst ts2
  in match ts1' ts2'
     where match [] [] = ([], [])
           match [] ((o,c):ts) = let (mts1, mts2) = match [] ts
                                  in ((o, 0):mts1, (o, c):mts2)
           match ((o,c):ts) [] = let (mts1, mts2) = match ts []
                                  in ((o, c):mts1, (o, 0):mts2)
           match l1@((o1,c1):ts1) l2@((o2,c2):ts2) =
                   case compare o1 o2 of
                     LT -> let (mts1, mts2) = match ts1 l2
                           in ((o1, c1):mts1, (o1, 0):mts2)
                     GT -> let (mts1, mts2) = match l1 ts2
                           in ((o2, 0):mts1, (o2, c2):mts2)  
                     EQ -> let (mts1, mts2) = match ts1 ts2
                           in ((o1, c1):mts1, (o2, c2):mts2)
                            
simplifyPolyTerm ts =
  let ts' = groupBy (\(o1, _) (o2, _) -> o1 == o2) $ sortOn fst ts
      ts'' = map (\g -> foldl1 (\(o, cz) (_, cx) -> (o, cz + cx)) g) ts'
  in filter (\(_, c) -> c /= 0) ts''

flattenPolyExpr :: PolyExpr -> PolyExpr
flattenPolyExpr v@(ExprValue _) = v
flattenPolyExpr (ExprOp op e1 e2) =
  let fe1 = flattenPolyExpr e1
      fe2 = flattenPolyExpr e2
      fe = flatten op fe1 fe2
  in
    fe
  where
    flatten op (ExprValue (PolyTerm ts1)) (ExprValue (PolyTerm ts2)) = 
      case op of
        Add -> ExprValue $ PolyTerm $ processAddSub (+) ts1 ts2
        Sub -> ExprValue $ PolyTerm $ processAddSub (-) ts1 ts2
        Mul -> ExprValue $ PolyTerm $ processMul ts1 ts2
    flatten _ _ _ = error $ "Unable to reduce one or both of " ++ show e1 ++ " " ++ show e2
    
    processAddSub fn ts1 ts2 =
      let (ts1', ts2') = matchPolyTerms ts1 ts2
      in simplifyPolyTerm $ map (\((o,c1),(_,c2)) -> (o,fn c1 c2)) $ zip ts1' ts2'
      
    processMul ts1 ts2 = simplifyPolyTerm $ [(o1 + o2, c1 * c2) | (o1, c1) <- ts1, (o2, c2) <- ts2]
                                             
{-
solvePolyExpr (ExprValue (PolyTerm ts)) =
  let ts' = pad $ simplifyPolyTerm ts
  in ts'
  where pad ts = 
  -}
  
solveLinEq (ExprOp Eq lhs rhs) =
  let expr = ExprOp Sub lhs rhs
      fe = flattenPolyExpr expr
  in fe--solvePolyExpr fe

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
