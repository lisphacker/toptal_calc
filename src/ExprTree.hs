{-|
Module      : ExprTree
Description : Generic expression tree
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Generic expression tree.
-}
module ExprTree
  ( GenExprTree(..)
  , ExprTree(..)
  , Op(..)
  , Fn(..)
  ) where

import Control.Monad

-- | Generic expression tree structure.
data GenExprTree op fn val = ExprOp op (GenExprTree op fn val) (GenExprTree op fn val) -- ^ Binary operation
                        | ExprFn fn (GenExprTree op fn val)                            -- ^ Unary function call
                        | ExprValue val                                                -- ^ Atomic value

instance (Show op, Show fn, Show val) => Show (GenExprTree op fn val) where
  show (ExprOp op expr1 expr2) = "(" ++ (show op) ++ " " ++ (show expr1) ++ " " ++ (show expr2) ++ ")"
  show (ExprFn fn expr)        = "(" ++ (show fn) ++ " " ++ (show expr) ++ ")"
  show (ExprValue val)         = show val

instance Functor (GenExprTree op fn) where
  fmap f (ExprOp op e1 e2) = ExprOp op (fmap f e1) (fmap f e2)
  fmap f (ExprFn fn e) = ExprFn fn (fmap f e)
  fmap f (ExprValue v) = ExprValue (f v)

-- | Binary operations.
data Op = Add | Sub | Mul | Div | Eq
        deriving (Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Eq  = "="

-- | Unary functions.
data Fn = Log | Ln

instance Show Fn where
  show Log = "log"
  show Ln  = "ln"

-- | Expression tree over value types.
type ExprTree val = GenExprTree Op Fn val


