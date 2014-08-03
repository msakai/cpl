{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Type
-- Copyright   :  (c) Masahiro Sakai 2006,2009
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Type
    ( GenType (..)
    , Type
    , FType (..)
    ) where

import FE
import Subst
import {-# SOURCE #-} CDT (CDT)

import Data.List (nub)

data GenType f
    = (:->)
    { dom :: !(GenFE f)
    , cod :: !(GenFE f)
    }

type Type = GenType CDT

instance Eq Type where
    (a :-> b) == (c :-> d) = a==c && b==d

instance FEs Type where
    apply s (a :-> b) = apply s a :-> apply s b
    tv (a :-> b)      = nub (tv a ++ tv b)

instance Show Type where
    show (a :-> b) = show a ++ " -> " ++ show b

data FType = FType !Int [Type] Type
