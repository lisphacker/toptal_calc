{-|
Module      : Error
Description : Error codes
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Error codes.
-}
module Error where
         
-- | Error codes
data ErrorCode = ErrUnexpectedCharInInputAt String
               | ErrUnableToParseNumberAt String
               | ErrCannotResolveToConstantDueToUnresolvedVariables
               | ErrNoDivInLinEqn
               | ErrNoFnInLinEqn
               | ErrOnlyVarInLinEqn
               | ErrTooManyInequalities
               | ErrMismatchedParens1
               | ErrMismatchedParens2
               | ErrMismatchedParens3
               | ErrEmptyInput
               | ErrCannotParseExpr

               | ErrUnableToReduceSubExprs String String
               | ErrCannotSolveNonLinEqn
               deriving (Eq)

-- | Converts error codes to messages
errMsg (ErrUnexpectedCharInInputAt ctx) = "Unexpected character at " ++ ctx
errMsg (ErrUnableToParseNumberAt ctx) = "Unable to parse number at " ++ ctx
errMsg ErrCannotResolveToConstantDueToUnresolvedVariables = "Cannot resolve expression to a constant value due to unresolvable variables"
errMsg ErrNoDivInLinEqn = "Cannot perform division operations in a linear equation"
errMsg ErrNoFnInLinEqn = "Functions are not permitted in a linear equation"
errMsg ErrOnlyVarInLinEqn = "Must have exactly one variable in a single linear equation"
errMsg ErrTooManyInequalities = "Too many equalities in equation"
errMsg ErrMismatchedParens1 = "Mismatched parantheses"
errMsg ErrMismatchedParens2 = "Mismatched parantheses"
errMsg ErrMismatchedParens3 = "Mismatched parantheses"
errMsg ErrEmptyInput = "Empty input"
errMsg ErrCannotParseExpr = "Unable to parse expression"

errMsg (ErrUnableToReduceSubExprs ctx1 ctx2) = "Unable to reduce one or both of " ++ ctx1 ++ " and " ++ ctx2
errMsg ErrCannotSolveNonLinEqn = "Unable to solve non-linear equations"

instance Show ErrorCode where
  show err = errMsg err
  
