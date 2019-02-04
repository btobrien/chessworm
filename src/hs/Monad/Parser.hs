
module Monad.Parser where

import Control.Applicative
import Data.Char
import Data.Maybe

tryAlso :: Alternative m => (a -> m a) -> a -> m a
tryAlso p x = p x <|> pure x

(<<) :: Monad m => m a -> m b -> m a
pa << pb = do
    x <- pa
    pb
    return x

newtype Parser a = P (String -> Maybe (a,String))

parse :: Parser a -> String -> Maybe (a,String)
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
    [] -> Nothing 
    (x:xs) -> Just (x,xs))

end :: Parser ()
end = P (\inp -> case inp of
    "" -> Just ((),"") 
    _ -> Nothing)

instance Functor Parser where
-- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
        Nothing -> Nothing
        Just (v,out) -> Just (g v, out))

instance Applicative Parser where
--  pure :: a -> Parser a
    pure v = P (\inp -> Just (v,inp))
--  <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
        Nothing -> Nothing
        Just (g,out) -> parse (fmap g px) out)

instance Monad Parser where
--  return :: a -> Parser a
    return v = P (\inp -> Just (v,inp))
--  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
        Nothing -> Nothing
        Just (x,out) -> parse (f x) out)

instance Alternative Parser where
--  empty :: Parser a
    empty = P $ const Nothing
--  <|> :: Parser a -> Parser a -> Parser a
    px <|> py = P (\inp -> case parse px inp of
        Nothing -> parse py inp
        x -> x)

runParser :: Parser a -> String -> a
runParser p xs = case (parse p xs) of
    Just (n,[]) -> n
    Just (_,leftovers) -> error ("leftovers: " ++ leftovers)
    Nothing -> error "invalid input"

readParser :: Read a => Parser String -> Parser a
readParser = fmap read

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower =  sat isLower

upper :: Parser Char
upper =  sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string = foldr (\x p -> do char x; fmap (x:) p) (return [])

space :: Parser ()
space = many (sat isSpace) >> return ()

nat :: Parser Int
nat = readParser (some digit)

neg :: Parser Int
neg = char '-' >> fmap (negate) nat

int :: Parser Int
int = nat <|> neg

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

word :: Parser String
word = token . some $ (sat $ not.isSpace)

delim :: Char -> Parser String
delim d = token $ many (sat $ not.(==d)) << char d

inside :: Char -> Char -> Parser String
inside a b = space >> char a >> delim b

quote :: Parser String
quote = inside '"' '"'

natural :: Parser Int
natural = token nat

negative :: Parser Int
negative = token neg

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token . string

emptyList :: Parser [a]
emptyList = symbol "[" >> symbol "]" >> return []

nonEmptyListOf :: Parser a -> Parser [a]
nonEmptyListOf p =  do
    symbol "["
    x <- p
    xs <- many (symbol "," >> p)
    symbol "]"
    return (x:xs)

listOf :: Parser a -> Parser [a]
listOf p = emptyList <|> nonEmptyListOf p

integers :: Parser [Int]
integers = listOf integer

