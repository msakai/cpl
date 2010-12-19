-----------------------------------------------------------------------------
-- |
-- Module      :  Exp
-- Copyright   :  (c) Masahiro Sakai 2009
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Expression of CPL.
--
-----------------------------------------------------------------------------

module Exp
    ( Id
    , Exp (..)
    , comp
    , expandFunct
    ) where

import qualified CDT
import qualified FE
import Type (GenType(..))
import Data.List (intercalate)

----------------------------------------------------------------------------

type Id = String

data Exp
    = Identity
    | Comp Exp Exp
    | Nat !CDT.Nat
    | Fact !CDT.CDT ![Exp]
    | Funct !CDT.CDT ![Exp]
    | Var !Id ![Exp]

instance Show Exp where
    show Identity = "I"
    show (Comp a b) = show a ++ "." ++ show b
    show (Nat nat)   = CDT.natName nat
    show (Fact fact args) = CDT.factName fact ++ showArgs args
    show (Funct funct args) = CDT.functName funct ++ showArgs args
    show (Var name args) = name ++ showArgs args

showArgs :: [Exp] -> String
showArgs [] = ""
showArgs args = "(" ++ intercalate "," (map show args) ++ ")"

{-# INLINE comp #-}
comp :: Exp -> Exp -> Exp
comp a Identity = a
comp Identity b = b
comp a b        = Comp a b

expandFunct :: CDT.CDT -> [Exp] -> Exp
expandFunct _   []   = Identity
expandFunct obj args = Fact obj (map g (CDT.nats obj))
    where g nat = FE.fold h expandFunct cod `comp`
                  (Nat nat `comp` FE.fold h expandFunct dom)
              where h 0 = Identity
                    h i = args !! (i-1)
                    dom:->cod = CDT.natDeclType nat
