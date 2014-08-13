{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Variance
-- Copyright   :  (c) Masahiro Sakai 2009
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Variance calculation.
--
-----------------------------------------------------------------------------

module Variance
    ( Variance(..)
    , top
    , bottom
    , join
    , meet
    , joinL
    , meetL
    , mult
    , mnemonic
    ) where

#if __GLASGOW_HASKELL__ >= 706
import Prelude hiding (join)
#endif

data Variance
    = Covariance     --- +
    | Contravariance --- -
    | FixedVariance  --- T
    | FreeVariance   --- ⊥
    deriving (Show,Read,Eq)

mnemonic :: Variance -> String
mnemonic Covariance     = "+"
mnemonic Contravariance = "-"
mnemonic FixedVariance  = "T"
mnemonic FreeVariance   = "_|_"

bottom, top :: Variance
bottom = FreeVariance
top    = FixedVariance

join,meet :: Variance -> Variance -> Variance
join a b | a==b            = a
         | a==FreeVariance = b
         | b==FreeVariance = a
         | otherwise       = FixedVariance
meet a b | a==b             = a
         | a==FixedVariance = b
         | b==FixedVariance = a
         | otherwise        = FreeVariance

joinL,meetL :: [Variance] -> Variance
joinL = foldl join bottom
meetL = foldl meet top

{-

・|⊥|+ |- |T
--+--+--+--+--
⊥|⊥|⊥|⊥|⊥
+ |⊥|+ |- |T
- |⊥|- |+ |T
T |⊥|T |T |T

-}
mult :: Variance -> Variance -> Variance
mult _ FreeVariance  = FreeVariance
mult FreeVariance _  = FreeVariance
mult _ FixedVariance = FixedVariance
mult FixedVariance _ = FixedVariance
mult a Covariance    = a
mult Covariance a    = a
mult Contravariance Contravariance = Covariance
