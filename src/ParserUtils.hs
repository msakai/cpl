module ParserUtils
    ( tok
    , char'
    , string'
    , Ident
    , ident
    ) where

import Text.ParserCombinators.Parsec
import Data.Char

type Ident = String

tok :: Parser a -> Parser a
tok p = do x <- p
           spaces
           return x

char' :: Char -> Parser Char
char' = tok . char

string' :: String -> Parser String
string' = tok . string

ident :: Parser Ident
ident  = tok $ many1 $ satisfy $ \c -> isAlphaNum c || (c `elem` "'!_")
