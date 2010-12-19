-----------------------------------------------------------------------------
-- |
-- Module      :  Statement
-- Copyright   :  (c) Masahiro Sakai 2004,2009
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Statement
    ( ConditionalEquation (..)
    , Equation (..)
    , eqs
    , ceq
    , feq
    , statements
    ) where

import CDT
import Exp
import qualified FE
import Data.List

infix 4 :=:
infixr 3 :=>

data ConditionalEquation = [Equation] :=> Equation
data Equation = Exp :=: Exp

instance Show ConditionalEquation where
    show (premisses :=> body) =
        case premisses of
        [] -> show body
        _  -> intercalate " & " (map show premisses) ++ " => " ++ show body

instance Show Equation where
    show (a :=: b) = show a ++ "=" ++ show b

eqs :: CDT.CDT -> [ConditionalEquation]
eqs obj = map f (CDT.nats obj)
    where f nat =
              case CDT.objectType obj of
              LeftObject ->
                  [] :=>
                  FE.fold g mkFunct (CDT.natDeclCod nat) `comp` Nat nat
                  :=: (factArgs !! CDT.natIndex nat) `comp`
                      FE.fold g mkFunct (CDT.natDeclDom nat)
              RightObject ->
                  [] :=>
                  Nat nat `comp` FE.fold g mkFunct (CDT.natDeclDom nat)
                  :=: FE.fold g mkFunct (CDT.natDeclCod nat) `comp`
                      (factArgs !! CDT.natIndex nat)
          factArgs = map (\i -> Var ("f" ++ show i) []) [0 .. CDT.nNats obj - 1]
          g 0 = Fact obj factArgs
          g _ = Identity

ceq :: CDT -> ConditionalEquation
ceq obj = map f (CDT.nats obj) :=> (u :=: Fact obj args)
    where f nat =
              case CDT.objectType obj of
              LeftObject ->
                  FE.fold g mkFunct (CDT.natDeclCod nat) `comp` Nat nat
                  :=:
                  (args !! CDT.natIndex nat) `comp`
                    FE.fold g mkFunct (CDT.natDeclDom nat)
              RightObject ->
                  Nat nat `comp` FE.fold g mkFunct (CDT.natDeclDom nat)
                  :=:
                  FE.fold g mkFunct (CDT.natDeclCod nat) `comp`
                  (args !! CDT.natIndex nat)
          args = map (\i -> Var ("f" ++ show i) []) [0 .. CDT.nNats obj - 1]
          u = Var "g" []
          g 0 = u
          g _ = Identity

feq :: CDT -> ConditionalEquation
feq obj = [] :=> Funct obj functArgs :=: Fact obj factArgs
    where functArgs = map f [0 .. CDT.functArity obj - 1]
              where f i = Var ("f" ++ show i) []
          factArgs  = map f (CDT.nats obj)
              where f nat = FE.fold g mkFunct (CDT.natDeclCod nat) `comp`
                            Nat nat `comp`
                            FE.fold g mkFunct (CDT.natDeclDom nat)
                    g 0 = Identity
                    g n = functArgs !! (n-1)

statements :: CDT -> [ConditionalEquation]
statements obj = eqs obj ++ [feq obj, ceq obj]

-----------------------------------------------------------------------------

mkFunct :: CDT -> [Exp] -> Exp
mkFunct _ []     = Identity
mkFunct obj args = Funct obj args
