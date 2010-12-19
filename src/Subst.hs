{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Subst
-- Copyright   :  (c) Masahiro Sakai 2006,2009
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  
--
-- Based on "Typing Haskell in Haskell".
-- http://www.cse.ogi.edu/~mpj/thih/
-----------------------------------------------------------------------------

module Subst
    ( Subst
    , nullSubst
    , (+->)
    , FEs (..)
    , (@@)
    , merge
    , mgu
    , match
    , varBind
    ) where

import FE
import {-# SOURCE #-} CDT (_eqCDT)

import Data.List (nub, intersect)
import Data.Maybe (fromMaybe)


type Subst = [(VarId,FE)]

nullSubst :: Subst
nullSubst = []

(+->) :: VarId -> FE -> Subst
u+->t = [(u,t)]

class FEs t where
    apply :: Subst -> t -> t
    tv    :: t -> [VarId]

instance FEs FE where
    apply s = fold (\v -> fromMaybe (Var v) (lookup v s)) Ap
    tv      = nub . fold return (const concat)

instance FEs a => FEs [a] where
    apply s = map (apply s)
    tv      = nub . concat . map tv

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u,t) <- s2] ++ s1

merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1 ++ s2) else fail "merge fails"
    where agree = all (\v -> apply s1 (Var v :: FE) == apply s2 (Var v))
                      (map fst s1 `intersect` map fst s2)

mgu :: Monad m => FE -> FE -> m Subst
mgu (Ap obj1 args1) (Ap obj2 args2) =
    if obj1 `_eqCDT` obj2
       then mguList args1 args2
       else fail "types do not unify"
mgu (Var u) t = varBind u t
mgu t (Var u) = varBind u t

mguList :: Monad m => [FE] -> [FE] -> m Subst
mguList (a:as) (b:bs) =
    do s1 <- mgu a b
       s2 <- mguList (apply s1 as) (apply s1 bs)
       return (s2 @@ s1)
mguList [] [] = return nullSubst
mguList _ _   = fail "types do not unify"

match :: Monad m => FE ->  FE -> m Subst
match (Ap obj1 args1) (Ap obj2 args2) =
    if obj1 `_eqCDT` obj2
       then matchList args1 args2
       else fail "types do not unify"
match (Var u) t = varBind u t
match _ _ = fail "types do not unify"

matchList :: Monad m => [FE] -> [FE] -> m Subst
matchList (a:as) (b:bs) =
    do s1 <- match a b
       s2 <- matchList (apply s1 as) (apply s1 bs)
       return (s2 @@ s1)
matchList [] [] = return nullSubst
matchList _ _   = fail "types do not unify"    

varBind :: Monad m => VarId -> FE -> m Subst
varBind u t | t == Var u    = return nullSubst
            | u `elem` tv t = fail "occurs check fails"
            | otherwise     = return (u +-> t)
