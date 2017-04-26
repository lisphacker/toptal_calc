module Parser.TinyParsec where

import Control.Applicative
import Control.Monad

newtype Parser a = Parser { parse :: String -> [(a,String)] }

data ParseError = ParseError String

runParser :: Parser a -> String -> Either ParseError a
runParser p s =
  case parse p s of
    [(res, [])]   -> Right res
    [(_,   rest)] -> Left $ ParseError "Parser terminated before finishing input"
    _             -> Left $ ParseError "Parser error"

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(x, s') -> parse (f x) s') $ parse p s

insert :: a -> Parser a
insert x = Parser $ \s -> [(x,s)]

failure :: Parser a
failure = Parser $ \s -> []

combine :: Parser a -> Parser a -> Parser a
combine p1 p2 = Parser $ \s -> (parse p1 s) ++ (parse p2 s)

option :: Parser a -> Parser a -> Parser a
option p1 p2 = Parser $ \s ->
  case parse p1 s of
    []  -> parse p2 s
    val -> val
  
instance Functor Parser where
  fmap f (Parser pf) = Parser $ (\s -> [(f x, s') | (x, s') <- pf s])

instance Applicative Parser where
  pure x = insert x
  (Parser f) <*> (Parser x) = Parser $ (\s -> [(f a,s2) | (f,s1) <- f s, (a,s2) <- x s1])

instance Monad Parser where
  return x = insert x
  p >>= f = bind p f

instance Alternative Parser where
  empty = failure
  (<|>) = option

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

some :: Parser a -> Parser [a]
some x = some'
  where many' = some' <|> pure []
        some' = (:) <$> x <*> many'

many :: Parser a -> Parser [a]
many x = many'
  where many' = some' <|> pure []
        some' = (:) <$> x <*> many'

char :: Parser Char
char = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c, cs)]

assert :: (Char -> Bool) -> Parser Char
assert f = bind char $ \c -> if f c then insert c else failure


  
