module Parser where

import Data.Char
import Data.Maybe
import Text.Read
import Control.Applicative
import Control.Monad
import Debug.Trace

data Op = Add | Sub | Mul | Div | Eq | OpTerm

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Eq  = "="

data Assoc = L | R

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
          | ExprTerm

instance Show Expr where
  show (ExprOp op expr1 expr2) = "(" ++ (show op) ++ " " ++ (show expr1) ++ " " ++ (show expr2) ++ ")"
  show (ExprFn fn expr)        = "(" ++ (show fn) ++ " " ++ (show expr) ++ ")"
  show (ExprValue val)         = show val
  show ExprTerm                = "<ExprTerm>"

data OpOrFn = OFOp Op | OFFn Fn | OFBrOpen | OFTerm
  deriving (Show)

opfnOp (OFOp op) = op
opfnFn (OFFn fn) = fn

{-
parseTokenStream :: [Token] -> (Expr, [Token])
parseTokenStream tokens = parse' [OFTerm] [ExprTerm] [ValueTerm] tokens
  where parse' opStk exprStk valStk [] = cleanupStacks opStk exprStk valStk
        parse' opStk exprStk valStk (TokenBrOpen:ts) = let (expr, ts') = parse' [OFTerm] [ExprTerm] [ValueTerm] ts
                                                       in parse' opStk (expr:exprStk) (ValueTerm:valStk) ts'
        parse' opStk exprStk valStk (TokenBrClose:ts) = cleanupStacks opStk exprStk valStk
        parse' opStk exprStk valStk ((TokenFn fn):ts) = parse' ((TokenFn fn):opStk) exprStk valStkl ts
        parse' opStk exprStk valStk ((TokenOp (Op op)):ts) =
          let opStkTop = head opStk
          in if prec op < prec opStkTop
             then
               let (opStk', exprStk', valStk', expr) = cleanupStacks opStk exprStk valStk
               in parse' (Op op):opStk' (expr:exprStk') valStk' ts
             else
               parse' (Op op):opStk exprStk valStk ts
        parse' opStk exprStk valStk ((TokenValue val):ts) =
          let la = listToMaybe ts
              opStkTop = head opStk
          in if la == TokenBrOpen
             then
               error "Should not reach here"
             else
               if stkTop == OpTerm
               then
                 parse' opStk exprStk valStk ts
               else
                 let (right, exprStk') = fromJust $ uncons exprStk
                     (op, opStk) = fromJust $ uncons opStk
                     
-}

{-
parseTokenStream :: [Token] -> Either ParseError (Expr, [Token])
parseTokenStream ts = parse' [OFTerm] [ExprTerm] ts
  where parse' opStk exprStk [] = windup opStk exprStk
        parse' opStk exprStk ((TokenValue val):ts) = parse' opStk ((ExprValue val):exprStk) ts
        parse' opStk exprStk ((TokenFn fn):ts) = parse' ((OFFn fn):opStk) exprStk ts
        parse' opStk exprStk ((TokenOp op):ts) = 

        windup opStk exprStk = trace ("opStk = " ++ (show opStk) ++ ", exprStk = " ++ (show exprStk)) $ Left $ ParseError "X"
        makeOpExpr opStk exprStk = if length opStk == 0
                                   then
                                     ([], [], Left $ ParseError "Expected an operator")
                                   else
                                     case length exprStk of
                                       0 -> ([], [], Left $ ParseError "Operator require two parameters, found none")
                                       1 -> ([], [], Left $ ParseError "Operator require two parameters, found only one")
                                       _ -> let op = opfnOp $ head opStk
                                                expr1 = head exprStk
                                                expr2 = (head . tail) exprStk
                                                opStk' = tail opStk
                                                exprStk' = (tail . tail) exprStk
                                            in (opStk', exprStk', Right $ ExprOp op expr1 expr2)
-}

parseTokenStream :: [Token] -> Either ParseError [Token]
parseTokenStream ts = in2post [] [] ts
  where in2post outQ stk [] = outQ
        in2post outQ stk (v@(TokenValue _):ts) = in2post (v:outQ) stk ts
        in2post outQ stk (fn@(TokenFn _):ts) = in2post outQ (fn:stk) ts
        in2post outQ stk ((TokenOp op):ts) = let (op', stk', outQ') = processOp op stk outQ
                                             in in2post outQ' ((TokenOp op):stk) ts

        processOp op stk outQ = case 

        {-
        parse' opStk exprStk ((TokenFn fn):ts) = parse' ((OFFn fn):opStk) exprStk ts
        parse' opStk exprStk ((TokenOp op):ts) = 

        windup opStk exprStk = trace ("opStk = " ++ (show opStk) ++ ", exprStk = " ++ (show exprStk)) $ Left $ ParseError "X"
        makeOpExpr opStk exprStk = if length opStk == 0
                                   then
                                     ([], [], Left $ ParseError "Expected an operator")
                                   else
                                     case length exprStk of
                                       0 -> ([], [], Left $ ParseError "Operator require two parameters, found none")
                                       1 -> ([], [], Left $ ParseError "Operator require two parameters, found only one")
                                       _ -> let op = opfnOp $ head opStk
                                                expr1 = head exprStk
                                                expr2 = (head . tail) exprStk
                                                opStk' = tail opStk
                                                exprStk' = (tail . tail) exprStk
                                            in (opStk', exprStk', Right $ ExprOp op expr1 expr2)


-}
{-
parse :: String -> Either ParseError Expr
parse s = let ets = postProcessTokenStream <$> tokenize s
          in case ets of
               Left e -> Left e
               Right ts -> fmap fst $ parseTokenStream ts

-}
