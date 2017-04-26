module Parser where

import Data.Char
import Data.Maybe
import Text.Read hiding (prec)
import Control.Applicative
import Control.Monad
import Debug.Trace

data Op = Add | Sub | Mul | Div | Eq | OpTerm
        deriving (Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Eq  = "="

data Assoc = L | R
           deriving (Eq)

prec :: Op -> Int
prec Add    = 2
prec Sub    = 2
prec Mul    = 3
prec Div    = 3
prec Eq     = 0
prec OpTerm = -100

assoc :: Op -> Assoc
assoc Add = L
assoc Sub = L
assoc Mul = L
assoc Div = L
assoc Eq  = L

data Fn = Log | Ln

instance Show Fn where
  show Log = "log"
  show Ln  = "ln"

data Value = Numeric Float
           | Variable Char
           | ValueTerm
             
instance Show Value where
  show (Numeric f)  = show f
  show (Variable c) = show c

data Token = TokenOp Op | TokenFn Fn | TokenValue Value | TokenBrOpen | TokenBrClose | TokenNeg
             deriving (Show)

data ParseError = ParseError String
                deriving (Show)

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
  | take 3 s == "ln"  = ((:) (TokenFn Ln)) <$> tokenize (drop 3 s)
  | isAlpha c         = ((:) (TokenValue (Variable c))) <$> tokenize cs
  | c == '.'          = parseNumber s
  | isDigit c         = parseNumber s
  | otherwise         = Left $ ParseError $ "Unexpected character at " ++ (take 10 s)
    where parseNumber s = let s' = readNumber s
                              in case readMaybe s' of
                                   Nothing  -> Left $ ParseError $ "Unable to parsee number at " ++ (take 10 s)
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

token0 = TokenValue (Numeric 0.0)

postProcessTokenStream :: [Token] -> [Token]
postProcessTokenStream [] = []
postProcessTokenStream tokens = postProc Nothing tokens
  where postProc Nothing               ((TokenOp Sub):tokens) = TokenNeg : postProc (Just TokenNeg) tokens
        postProc (Just TokenBrOpen)    ((TokenOp Sub):tokens) = TokenNeg : postProc (Just TokenNeg) tokens
        postProc (Just (TokenOp _))    ((TokenOp Sub):tokens) = TokenNeg : postProc (Just TokenNeg) tokens
        postProc (Just (TokenValue _)) (TokenBrOpen:tokens)   = TokenOp Mul : TokenBrOpen : postProc (Just TokenBrOpen) tokens
        postProc (Just (TokenValue _)) (v@(TokenValue _):tokens)   = TokenOp Mul : v : postProc (Just v) tokens
        postProc _                     (token:tokens)         = token : postProc (Just token) tokens
        postProc _                     []                     = []
        
data Expr = ExprOp Op Expr Expr
          | ExprFn Fn Expr
          | ExprValue Value

instance Show Expr where
  show (ExprOp op expr1 expr2) = "(" ++ (show op) ++ " " ++ (show expr1) ++ " " ++ (show expr2) ++ ")"
  show (ExprFn fn expr)        = "(" ++ (show fn) ++ " " ++ (show expr) ++ ")"
  show (ExprValue val)         = show val

infix2postfix :: [Token] -> Either ParseError [Token]
infix2postfix ts =
  case in2post [] [] ts of
    Left e -> Left e
    Right tokens -> Right $ reverse tokens
  
  where in2post outQ stk [] = windup outQ stk
        in2post outQ stk (v@(TokenValue _):ts) = in2post (v:outQ) stk ts
        in2post outQ stk (fn@(TokenFn _):ts) = in2post outQ (fn:stk) ts
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
              otherwise   -> (outQ, stk)
                               
        processBrClose outQ []                       = Left $ ParseError "Mismatched parantheses"
        processBrClose outQ (op@(TokenOp _):reststk) = processBrClose (op:outQ) reststk
        processBrClose outQ stk                      = processBrClose' outQ stk
        
        processBrClose' outQ (TokenBrOpen:reststk) = processBrClose'' outQ reststk
        processBrClose' _    _                     = Left $ ParseError "Mismatched parantheses"
        
        processBrClose'' outQ (fn@(TokenFn _):reststk) = Right $ ((fn:outQ) , reststk)
        processBrClose'' outQ stk                      = Right $ (outQ, stk)

        windup outQ [] = Right outQ
        windup outQ (op@(TokenOp _):reststk) = windup (op:outQ) reststk
        windup outQ (fn@(TokenFn _):reststk) = windup (fn:outQ) reststk
        windup outQ _ = Left $ ParseError "Mismatched parantheses"

                                            
parse :: String -> Either ParseError Expr
parse s = do
  ifts <- postProcessTokenStream <$> tokenize s
  pfts <- infix2postfix ifts
  expr <- post2tree [] pfts
  return expr
  
  where post2tree (stkTop:[]) [] = Right stkTop
        post2tree stk ((TokenValue val):ts) = post2tree ((ExprValue val):stk) ts
        post2tree stk ((TokenOp op):ts) = if length stk >= 2
                                          then
                                            let e1 = head stk
                                                e2 = (head . tail) stk
                                            in post2tree ((ExprOp op e2 e1):((tail . tail) stk)) ts
                                          else
                                            Left $ ParseError "Unable to parse expression"
        post2tree stk ((TokenFn fn):ts) = if length stk >= 1
                                          then
                                            let e = head stk
                                            in post2tree ((ExprFn fn e):(tail stk)) ts
                                          else
                                            Left $ ParseError "Unable to parse expression"
