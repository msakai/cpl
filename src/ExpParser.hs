-----------------------------------------------------------------------------
-- |
-- Module      :  ExpParser
-- Copyright   :  (c) Masahiro Sakai 2009
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module ExpParser
    ( Exp (..)
    , exp
    , def
    , evalExp
    , identityNames
    ) where

import qualified CDT
import qualified Exp as E
import ParserUtils

import Prelude hiding (exp)
import Control.Monad
import Control.Monad.Except
import Text.Parsec hiding (string')
import Text.Parsec.String (Parser)
import Data.List (find)
import Data.Maybe
import qualified Data.Map as Map
import Text.Printf

----------------------------------------------------------------------------

data Exp
    = Comp Exp Exp
    | Ident String [Exp]
    deriving (Show, Read)

exp :: Parser Exp
exp =
    do xs <- sepBy1 pident (tok (char '.' `mplus` char '\x2218'))
       return (foldr1 Comp xs)
    where pident =
              do s  <- ident
                 xs <- option [] $ between (char' '(') (char' ')')
                                 $ exp `sepBy` char' ','
                 spaces
                 return (Ident s xs)

def :: Parser (E.Id, [E.Id], Exp)
def = do
  spaces
  s <- ident
  ps <- option [] $ between (char' '(') (char' ')') $ ident `sepBy` char' ','
  spaces
  char' '='
  body <- exp
  spaces
  return (s, ps, body)

----------------------------------------------------------------------------

type CDTEnv = [CDT.CDT]

evalExp :: CDTEnv -> (Map.Map String Int) -> Exp -> Either String E.Exp
evalExp cenv env = f
  where
    f :: Exp -> Either String E.Exp
    f (Comp a b) = do
      a' <- f a
      b' <- f b
      return (E.Comp a' b')
    f (Ident s args) | s `elem` identityNames = do
      unless (length args == 0) $
        throwError $ printf "%s: wrong number of arguments (given %d, expected %d)" s (length args) (0 :: Int)
      return E.Identity
    f (Ident s args)  = do
      let arity = length args
      args' <- mapM f args
      case () of
        _ | Just o <- find (\o -> CDT.functName o  == s) cenv -> do
          unless (CDT.functArity o == arity) $
            throwError $ printf "%s: wrong number of arguments (given %d, expected %d)" s arity (CDT.functArity o)
          return (E.Funct o args')
        _ | Just o <- find (\o -> CDT.factName o  == s) cenv -> do
          unless (CDT.nNats o == arity) $
            throwError $ printf "%s: wrong number of arguments (given %d, expected %d)" s arity (CDT.nNats o)
          return (E.Fact o args')
        _ | Just n <- find (\n -> CDT.natName n == s) [n | o <- cenv, n <- CDT.nats o] -> do
          unless (arity == 0) $
            throwError $ printf "%s: wrong number of arguments (given %d, expected %d)" s arity (0 :: Int)
          return (E.Nat n)
        _ | Just arity' <- Map.lookup s env -> do
          unless (arity == arity') $
            throwError $ printf "%s: wrong number of arguments (given %d, expected %d)" s arity arity'
          return (E.Var s args')
        _ -> throwError ("unknown symbol: " ++ s)

identityNames :: [String]
identityNames = ["I", "id"]

----------------------------------------------------------------------------
