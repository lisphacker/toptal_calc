module Parser where

import Data.Char

data Op = Add | Sub | Mul | Div | Eq | OpTerm

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Eq  = "="

data Assoc = L | R

prec :: Op -> Int
prec Add = 2
prec Sub = 2
prec Mul = 3
prec Div = 3
prec Eq  = 0

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

tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(c:cs)
  | c == ' '          = tokenize cs
  | c == '+'          = TokenOp Add : tokenize cs
  | c == '-'          = TokenOp Sub : tokenize cs
  | c == '*'          = TokenOp Mul : tokenize cs
  | c == '/'          = TokenOp Div : tokenize cs
  | c == '='          = TokenOp Eq : tokenize cs
  | c == '('          = TokenBrOpen : tokenize cs
  | c == ')'          = TokenBrClose : tokenize cs
  | take 3 s == "log" = TokenFn Log : tokenize (drop 3 s)
  | take 3 s == "ln"  = TokenFn Ln : tokenize (drop 3 s)
  | isAlpha c         = TokenValue (Variable c) : tokenize cs
  | c == '.'          = parseNumber s
  | isDigit c         = parseNumber s
    where parseNumber s = let s' = readNumber s
                              in TokenValue (Numeric (read s')) : tokenize (drop (length s') s)
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
        postProc (Just (TokenValue v)) (TokenBrOpen:tokens)   = TokenOp Mul : TokenBrOpen : postProc (Just TokenBrOpen) tokens
        postProc _                     (token:tokens)         = token : postProc (Just token) tokens
        postProc _                     []                     = []
  
data Expr = ExprOp Op Expr Expr
          | ExprFn Fn Expr
          | ExprValue Value
          | ExprTerm

instance Show Expr where
  show (ExprOp op expr1 expr2) = "(" ++ (show op) ++ " " ++ (show expr1) ++ " " ++ (show expr2) ++ ")"
  show (ExprFn fn expr)        = "(" ++ (show fn) ++ " " ++ (show expr) ++ ")"
  show (ExprValue val)         = show val

data OpOrFn = OFOp Op | OFFn Fn | OFTerm

parseTokenStream :: [Token] -> (Expr, [Token])
parseTokenStream tokens = parse' [OFTerm] [ExprTerm] tokens
  where parse' (TokenBrOpen:ts) opStk valStk = let (expr, ts') = parse' ts 
        


parse :: String -> Expr
parse = fst . parseTokenStream . postProcessTokenStream . tokenize
