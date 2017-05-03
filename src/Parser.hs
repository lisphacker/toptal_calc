{-|
Module      : Parser
Description : Mathematical expression parser
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Parses mathematics expressions into an expression tree using the Shunting Yard algorithm.
-}
module Parser
       ( Value(..)
       , Expr(..)
       , ParseError(..)
       , parse
       ) where

import Data.Char
import Data.List
import Text.Read hiding (prec)
import Debug.Trace

import ExprTree

-- | Operator associativity/
data Assoc = L | R
           deriving (Eq)

-- | Returns operator precedence priority.
prec :: Op -> Int
prec Add    = 2
prec Sub    = 2
prec Mul    = 3
prec Div    = 3
prec Eq     = 0

-- | Returns operator associativity.
assoc :: Op -> Assoc
assoc Add = L
assoc Sub = L
assoc Mul = L
assoc Div = L
assoc Eq  = L

-- | Definition of an atomic value.
data Value = Numeric Float -- ^ Numeric value
           | Variable Char -- ^ Variable (single character)
             
instance Show Value where
  show (Numeric f)  = show f
  show (Variable c) = show c

-- | Definition of token (lexeme) types extracted by the lexer.
data Token = TokenOp Op | TokenFn Fn | TokenValue Value | TokenBrOpen | TokenBrClose | TokenNeg
             deriving (Show)

-- | Error data type.
data ParseError = ParseError String
                deriving (Show)

parseError = Left . ParseError

-- | Tokenizes a string containing a mathematical expression into a list of tokens or returns an error.
tokenize :: String -> Either ParseError [Token]
tokenize [] = Right []
tokenize s@(c:cs)
  | c == ' '          = tokenize cs
  | c == '+'          = ((:) (TokenOp Add)) <$> tokenize cs
  | c == '-'          = ((:) (TokenOp Sub)) <$> tokenize cs
  | c == '*'          = ((:) (TokenOp Mul)) <$> tokenize cs
  | c == '/'          = ((:) (TokenOp Div)) <$> tokenize cs
  | c == '='          = ((:) (TokenOp Eq)) <$> tokenize cs
  | c == '('          = ((:) (TokenBrOpen)) <$> tokenize cs
  | c == ')'          = ((:) (TokenBrClose)) <$> tokenize cs
  | take 3 s == "log" = ((:) (TokenFn Log)) <$> tokenize (drop 3 s)
  | take 2 s == "ln"  = ((:) (TokenFn Ln)) <$> tokenize (drop 3 s)
  | isAlpha c         = ((:) (TokenValue (Variable c))) <$> tokenize cs
  | c == '.'          = parseNumber s
  | isDigit c         = parseNumber s
  | otherwise         = parseError $ "Unexpected character at " ++ (take 10 s)
    where parseNumber s = let s' = readNumber s
                              in case readMaybe s' of
                                   Nothing  -> parseError $ "Unable to parse number at " ++ (take 10 s)
                                   Just val -> ((:) (TokenValue (Numeric val))) <$> tokenize (drop (length s') s)
          readNumber s = let (n1, r1) = testS readInt s
                             (dot, r2) = testS (readChar ".") r1
                             (n2, r3) = testS readInt r2
                             (e, r4) = testS (readChar "e") r3
                             (sgn, r5) = testS (readChar "+-") r4
                             (n3, _) = testS readInt r5
                         in if e == "" then n1 ++ dot ++ n2 else n1 ++ dot ++ n2 ++ e ++ sgn ++ n3
                                
          testS f s = if s == "" then ("", "") else f s
          readInt s = span isDigit s
          readChar cs s = if elem (head s) cs then (head s:[], tail s) else ("", s)

-- | Validate token stream
validateTokenStream :: [Token] -> Either ParseError [Token]
validateTokenStream ts = let nvar = countVariables ts
                             neq  = countEq ts
                             nfn  = countFn ts
                             ndiv = countDiv ts
                         in if neq == 0
                               -- Expression
                            then
                              if nvar == 0
                              then
                                Right ts
                              else
                                parseError "Cannot resolve expression to a constant value due to unresolvable variables"
                            else
                              if neq == 1
                                 -- Equation
                              then
                                if nvar == 1
                                then
                                  if nfn == 0
                                  then
                                    if ndiv == 0
                                    then
                                      Right ts
                                    else
                                      parseError "Cannot perform division operations in a linear equation"
                                  else
                                    parseError "Functions are not permitted in a linear equation"
                                else
                                  parseError "Must have exactly one variable in a single linear equation"
                              else
                                parseError "Too many equalities in equation"
  where countVariables = length . group . sort . countVar
        
        countVar []                             = ""
        countVar ((TokenValue (Variable v)):ts) = v:(countVar ts)
        countVar (t:ts)                         = countVar ts

        countEq = countOp Eq 0
        countDiv = countOp Div 0
        
        countOp _  c []                 = c
        countOp op c ((TokenOp op'):ts) = let c' = if op == op' then c + 1 else c
                                              in countOp op c' ts
        countOp op c (_:ts)             = countOp op c ts
  
        countFn = countFn' 0
        
        countFn' c []               = c
        countFn' c ((TokenFn _):ts) = countFn' (c + 1) ts
        countFn' c (t:ts)           = countFn' c ts
  
-- | Postprocesses a token stream to manage unary negatives and implcit multiplies.
postProcessTokenStream :: [Token] -> [Token]
postProcessTokenStream [] = []
postProcessTokenStream tokens = postProc Nothing tokens
  where postProc Nothing               ((TokenOp Sub):tokens) = TokenNeg : postProc (Just TokenNeg) tokens
        postProc (Just TokenBrOpen)    ((TokenOp Sub):tokens) = TokenNeg : postProc (Just TokenNeg) tokens
        postProc (Just (TokenOp _))    ((TokenOp Sub):tokens) = TokenNeg : postProc (Just TokenNeg) tokens
        postProc (Just TokenNeg)       ((TokenOp Sub):tokens) = TokenNeg : postProc (Just TokenNeg) tokens
        postProc (Just (TokenValue _)) (TokenBrOpen:tokens)   = TokenOp Mul : TokenBrOpen : postProc (Just TokenBrOpen) tokens
        postProc (Just (TokenValue _)) (v@(TokenValue _):tokens)   = TokenOp Mul : v : postProc (Just v) tokens
        postProc _                     (token:tokens)         = token : postProc (Just token) tokens
        postProc _                     []                     = []

-- | Definition of an expression tree.        
type Expr = ExprTree Value

-- | Parses a infix token stream to a postfix token stream if possible or returns an error.
infix2postfix :: [Token] -> Either ParseError [Token]
infix2postfix ts =
  case in2post [] [] ts of
    Left e -> Left e
    Right tokens -> Right $ reverse tokens
  
  where in2post outQ stk [] = windup outQ stk
        in2post outQ stk (v@(TokenValue _):ts) = in2post (v:outQ) stk ts
        in2post outQ stk (fn@(TokenFn _):ts) = in2post outQ (fn:stk) ts
        in2post outQ stk (TokenNeg:ts) = in2post outQ (TokenNeg:stk) ts
        in2post outQ stk ((TokenOp op):ts) = let (outQ', stk') = processOp outQ stk op
                                             in in2post outQ' ((TokenOp op):stk') ts
        in2post outQ stk (TokenBrOpen:ts) = in2post outQ (TokenBrOpen:stk) ts
        in2post outQ stk (TokenBrClose:ts) =
          let eQS = processBrClose outQ stk
          in case eQS of
            Left e -> Left e
            Right (outQ', stk') -> in2post outQ' stk' ts
        
        processOp outQ stk op =
          if null stk
          then
            (outQ, stk)
          else
            case head stk of
              TokenOp op' -> if (assoc op == L && prec op <= prec op') || (assoc op == R && prec op < prec op')
                             then
                               processOp ((TokenOp op'):outQ) (tail stk) op
                             else
                               (outQ, stk)
              TokenNeg    -> if op == Eq
                             then
                               processOp (TokenNeg:outQ) (tail stk) op
                             else
                               (outQ, stk)
              otherwise   -> (outQ, stk)
                               
        processBrClose outQ []                       = parseError "Mismatched parantheses"
        processBrClose outQ (op@(TokenOp _):reststk) = processBrClose (op:outQ) reststk
        processBrClose outQ (fn@(TokenFn _):reststk) = processBrClose (fn:outQ) reststk
        processBrClose outQ (TokenNeg:reststk)       = processBrClose (TokenNeg:outQ) reststk
        processBrClose outQ stk                      = processBrClose' outQ stk
        
        processBrClose' outQ (TokenBrOpen:reststk) = processBrClose'' outQ reststk
        processBrClose' _    _                     = parseError "Mismatched parantheses"
        
        processBrClose'' outQ (fn@(TokenFn _):reststk) = Right $ ((fn:outQ) , reststk)
        processBrClose'' outQ stk                      = Right $ (outQ, stk)

        windup outQ [] = Right outQ
        windup outQ (op@(TokenOp _):reststk) = windup (op:outQ) reststk
        windup outQ (fn@(TokenFn _):reststk) = windup (fn:outQ) reststk
        windup outQ (TokenNeg:reststk)       = windup (TokenNeg:outQ) reststk
        windup outQ _ = parseError "Mismatched parantheses"

zeroExpr = ExprValue (Numeric 0.0)

-- | Parses a string into an expression tree if possible or returns an error.                                            
parse :: String -> Either ParseError Expr
parse s = do
  ifts <- postProcessTokenStream <$> tokenize s
  vifts <- validateTokenStream ifts
  pfts <- infix2postfix vifts
  expr <- post2tree [] pfts
  return expr
  
  where post2tree []          []                    = parseError "Empty input"
        post2tree (stkTop:[]) []                    = Right stkTop
        post2tree stk         ((TokenValue val):ts) = post2tree ((ExprValue val):stk) ts
        post2tree stk         ((TokenOp op):ts)     = if length stk >= 2
                                                      then
                                                        let e1 = head stk
                                                            e2 = (head . tail) stk
                                                        in post2tree ((ExprOp op e2 e1):((tail . tail) stk)) ts
                                                      else
                                                        parseError "Unable to parse expression"
        post2tree stk         ((TokenFn fn):ts)     = if length stk >= 1
                                                      then
                                                        let e = head stk
                                                        in post2tree ((ExprFn fn e):(tail stk)) ts
                                                      else
                                                        parseError "Unable to parse expression"
        post2tree stk        (TokenNeg:ts)          = if length stk >= 1
                                                      then
                                                        let e = head stk
                                                        in post2tree ((ExprOp Sub zeroExpr  e):(tail stk)) ts
                                                      else
                                                        parseError "Unable to parse expression"

------------------------------------------------------------

ex1 = "(3+(4-1))*5"
ex2 = "2 * x + 0.5 = 1"
ex3 = "2x + 1 = 2(1-x)"
