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
    ) where

import qualified CDT
import qualified Exp as E
import ParserUtils

import Prelude hiding (exp)
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Maybe
import qualified Data.Map as Map

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

evalExp :: CDTEnv -> (Map.Map String Int) -> Exp -> Maybe E.Exp
evalExp cenv env = listToMaybe . f
    where f (Comp a b) =
              do a' <- f a
                 b' <- f b
                 return (E.Comp a' b')
          f (Ident "I" args) =
              do guard (length args == 0)
                 return E.Identity
          f (Ident s args)  =
              do let arity = length args
                 args' <- mapM f args
                 o <- cenv
                 msum [ do guard $ CDT.functName o  == s &&
                                   CDT.functArity o == arity
                           return (E.Funct o args')
                      , do guard $ CDT.factName o == s &&
                                   CDT.nNats o == arity
                           return (E.Fact o args')
                      , do guard $ arity == 0
                           n <- CDT.nats o
                           guard $ CDT.natName n == s
                           return (E.Nat n)
                      , do guard $ Just arity == Map.lookup s env
                           return (E.Var s args')
                      ]

----------------------------------------------------------------------------
