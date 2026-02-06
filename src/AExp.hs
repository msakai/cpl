-----------------------------------------------------------------------------
-- |
-- Module      :  AExp
-- Copyright   :  (c) Masahiro Sakai 2004,2009
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Annotated Expression.
--
-----------------------------------------------------------------------------

module AExp
  ( AExp(..)
  , aexpType
  , isElement

  , TypeScheme
  , AExpScheme
  , quantify

  , skelton
  ) where

import Variance
import Funct
import qualified FE
import qualified CDT
import Type (GenType(..), Type)
import Subst
import qualified Exp as E
import Exp (Id)

import Data.List (nub, intercalate)

----------------------------------------------------------------------------

-- Annotated expression with type information
data AExp
  = Identity FE.FE
  | Comp AExp AExp
  | Nat !CDT.Nat [FE.FE]
  | Fact !CDT.CDT [AExp] [FE.FE]
  | Funct !CDT.CDT [AExp]
  | Var !Id [FE.FE] [AExp] Type

instance FEs AExp where
  apply s (Identity annotation) = Identity (apply s annotation)
  apply s (Comp a b)            = Comp (apply s a) (apply s b)
  apply s (Nat sym annotation)  = Nat sym (apply s annotation)
  apply s (Fact obj args annotations) =
      Fact obj (apply s args) (apply s annotations)
  apply s (Funct obj args) =
      Funct obj (apply s args)
  apply s (Var v annotations args typ) =
      Var v (apply s annotations) (apply s args) (apply s typ)
  tv (Identity x)          = tv x
  tv (Comp a b)            = nub (tv a ++ tv b)
  tv (Nat _ annotations)   = tv annotations
  tv (Fact _ args _)       = tv args
  tv (Funct _ args)        = tv args
  tv (Var _ annotations args _) = nub (tv annotations ++ tv args)

instance Show AExp where
  show (Identity fe) = "I" ++ showAnnotations [fe]
  show (Comp a b) = show a ++ "." ++ show b
  show (Nat sym annotations) =
      CDT.natName sym ++ showAnnotations annotations
  show (Fact obj args annotations) =
      CDT.factName obj ++ showAnnotations annotations ++ showAExpArgs args
  show (Funct obj args) =
      CDT.factName obj ++ showAExpArgs args
  show (Var v annotations args _) = v ++ showAnnotations annotations ++ showAExpArgs args

showAnnotations :: [FE.FE] -> String
showAnnotations [] = ""
showAnnotations xs = "[" ++ intercalate "," (map show xs) ++ "]"

showAExpArgs :: [AExp] -> String
showAExpArgs [] = ""
showAExpArgs xs = "(" ++ intercalate "," (map show xs) ++ ")"

skelton :: AExp -> E.Exp
skelton = f
  where
    f (Identity _) = E.Identity
    f (Comp a b) = f a `E.Comp` f b
    f (Nat sym _) = E.Nat sym
    f (Fact sym args _) = E.Fact sym (map f args)
    f (Funct sym args) = E.Funct sym (map f args)
    f (Var name _ args _) = E.Var name (map f args)

-- XXX
aexpType :: AExp -> Type
aexpType = f
  where
    f (Identity a) = a :-> a
    f (Comp g h) = dom (f h) :-> cod (f g)
    f (Nat nat annotation) =
      apply (zip [0..] annotation) (CDT.natType nat)
    f (Fact obj _ annotation) =
      apply (zip [0..] annotation) (CDT.factDestType obj)
    f (Funct obj args) = FE.Ap obj xs :-> FE.Ap obj ys
      where
        (xs,ys) = foldr phi ([],[]) (zip (variance obj) (map aexpType args))
        phi (v, (x:->y)) (xs,ys) =
          case v of
            Contravariance -> (y:xs, x:ys)
            _ -> (x:xs, y:ys)
    f (Var _ _ args typ) = typ -- FIXME?

isElement :: AExp -> Bool
isElement x =
  case dom (AExp.aexpType x) of
    FE.Var _ -> True
    FE.Ap obj _ -> CDT.isTerminalObject obj

----------------------------------------------------------------------------

-- data Scheme t = Forall !Int t
type Scheme t = (Int, t)
type TypeScheme = Scheme Type
type AExpScheme = Scheme AExp

quantify :: FEs t => t -> Scheme t
quantify x = (length vars, apply s x)
  where
    vars = tv x
    s = zip vars [FE.Var i | i<-[0..]]

----------------------------------------------------------------------------

simp :: AExp -> AExp
simp (Comp a b) =
  case (simp a, simp b) of
    (Identity _, b') -> b'
    (a', Identity _) -> a'
    (a', b') -> Comp a' b'
simp orig@(Fact obj args _) =
  if a==b && all f (zip (CDT.nats obj) (map simp args))
    then Identity a
    else orig
  where
    (a :-> b) = aexpType orig
    f (nat, Nat sym _) = nat==sym
    f _ = False
simp orig@(Funct _ args) =
  if a==b && all f (map simp args)
    then Identity a
    else orig
  where
    (a :-> b) = aexpType orig
    f (Identity _) = True
    f _ = False
simp x = x
