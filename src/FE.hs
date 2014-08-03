{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FE
-- Copyright   :  (c) Masahiro Sakai 2004,2009
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Functorial expression and functorial calculus.
--
-----------------------------------------------------------------------------

module FE
    ( VarId
    , GenFE (..)
    , FE
    , fold
    , GenCFE (..)
    , CFE
    ) where

import Variance
import Funct
import {-# SOURCE #-} CDT (CDT, functName, functVariance, _eqCDT)

import Data.List (transpose, intercalate)

----------------------------------------------------------------------------

type VarId = Int

showVarId :: VarId -> String
showVarId v = seq m $ seq n $
              '*' : (table !! n) : (if m /= 0 then show m else "")
    where table = ['a'..'z']
          tableSize = length table
          (m,n) = v `divMod` tableSize

----------------------------------------------------------------------------
-- Functorial Expression

data GenFE f = Var !VarId | Ap !f [GenFE f] --deriving Eq
type FE = GenFE CDT

instance Eq FE where
    Var i == Var j     = i==j
    Ap f xs == Ap g ys = _eqCDT f g && xs==ys
    _ == _ = False

instance Show FE where
    show = fold showVarId f
        where f funct args =
                  case functVariance funct of
                  [] -> functName funct
                  _  -> functName funct ++ "(" ++ intercalate "," args ++ ")"

{-# INLINE fold #-}
fold :: (VarId -> a) -> (f -> [a] -> a) -> GenFE f -> a
fold f g = func
    where func (Var v) = f v
          func (Ap funct l) = g funct (map func l)

----------------------------------------------------------------------------
-- Closed Functorial Expression

data GenCFE f = CFE !Int !(GenFE f)
type CFE = GenCFE CDT

instance Funct f => Funct (GenCFE f) where
    variance (CFE n fe) = fold f g fe
        where f v = [if x==v then Covariance else FreeVariance | x <- [0..(n-1)] ]
              g obj args = case variance obj of
                           [] -> nFree
                           xs -> [ joinL [x `mult` y | (x,y) <- zip xs col]
                                 | col <- transpose args ]
              nFree = replicate n FreeVariance

----------------------------------------------------------------------------
