{-|
Module      : Math
Description : Mathematical expression solver
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Solves aithmatic expressions as well as linear and quadratic equations in a single variable.
-}
module Math
  ( processMathExpression
  , MathError(..)
  ) where

import ExprTree
import Parser
import Data.List
import Debug.Trace
import Error

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

-- | Error data type.
data MathError = MathError ErrorCode
               deriving (Eq, Show)

mathError = Left . MathError

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

flattenPolyExpr :: PolyExpr -> Either MathError PolyExpr
flattenPolyExpr v@(ExprValue _) = Right v
flattenPolyExpr (ExprOp op e1 e2) =
  let fe1 = flattenPolyExpr e1
      fe2 = flattenPolyExpr e2
  in
    case fe1 of
      e@(Left _) -> e
      Right e1   -> case fe2 of
        e'@(Left _) -> e'
        Right e2    -> flatten op e1 e2
  where
    flatten op (ExprValue (PolyTerm ts1)) (ExprValue (PolyTerm ts2)) = 
      case op of
        Add -> Right $ ExprValue $ PolyTerm $ processAddSub (+) ts1 ts2
        Sub -> Right $ ExprValue $ PolyTerm $ processAddSub (-) ts1 ts2
        Mul -> Right $ ExprValue $ PolyTerm $ processMul ts1 ts2
    flatten _ _ _ = mathError $ ErrUnableToReduceSubExprs (show e1) (show e2)
    
    processAddSub fn ts1 ts2 =
      let (ts1', ts2') = matchPolyTerms ts1 ts2
      in simplifyPolyTerm $ map (\((o,c1),(_,c2)) -> (o,fn c1 c2)) $ zip ts1' ts2'
      
    processMul ts1 ts2 = simplifyPolyTerm $ [(o1 + o2, c1 * c2) | (o1, c1) <- ts1, (o2, c2) <- ts2]

solvePolyExpr :: PolyExpr -> Either MathError [Float]
solvePolyExpr (ExprValue (PolyTerm ts)) =
  let ts' = pad 0 $ simplifyPolyTerm ts
      order = length ts' - 1
  in case order of
    1 -> let a = snd (ts' !! 1)
             b = snd (ts' !! 0)
         in Right $ [-b / a]
    2 -> let a = snd (ts' !! 2)
             b = snd (ts' !! 1)
             c = snd (ts' !! 0)
             rootTerm = sqrt (b * b - 4 * a * c)
         in Right $ [(-b + rootTerm) / (2 * a), (-b - rootTerm) / (2 * a)]
    otherwise -> mathError ErrCannotSolveNonLinEqn
    
  where pad _  []         = []
        pad o' ((o,c):ts) = if o == o' then (o,c):pad (o' + 1) ts else (o',0):pad (o' + 1) ((o,c):ts)
        
solveLinEq :: PolyExpr -> Either MathError [Float]
solveLinEq (ExprOp Eq lhs rhs) =
  let expr = ExprOp Sub lhs rhs
  in case flattenPolyExpr expr of
    Left e   ->  Left e
    Right fe -> solvePolyExpr fe

-- | Parses and evaluates math expressions, and solves linear/quadratic equations.
processMathExpression s =
  case parse s of
    Left (ParseError ec) -> Left $ MathError ec
    Right expr -> case expr of
        ExprOp Eq _ _ -> case solveLinEq $ convertTreeToPolyTree expr of
          Left (MathError ec) -> Left $ MathError ec
          Right results        -> Right results
        _             -> Right $ [evaluateExpr expr]

------------------------------------------------------------

ex1 = "(3+(4-1))*5"
ex2 = "2 * x + 0.5 = 1"
ex3 = "2x + 1 = 2(1-x)"
