{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Typing
  ( Typing (..)
  , inferType
  , inferType2

  , TI
  , Env
  , runTI
  , getSubst
  , extSubst
  , appSubst
  , unify
  , newFEVar
  ) where

import Variance
import Funct
import qualified FE
import qualified CDT
import Type (GenType (..), Type, FType (..))
import Subst
import AExp
import qualified Exp as E

import Data.List (nub)
import Control.Monad.Error
import Control.Monad.RWS
import qualified Data.Map as Map

----------------------------------------------------------------------------

infix 6 :!
data Typing = AExp :! Type

tm :: Typing -> AExp
tm (e :! _) = e

ty :: Typing -> Type
ty (_ :! t) = t

instance FEs Typing where
  tv (e :! t)      = nub (tv e ++ tv t)
  apply s (e :! t) = apply s e :! apply s t

----------------------------------------------------------------------------

-- Type inference monad
newtype TI a = TI (RWST Env () TIState (Either String) a)
  deriving (Monad, MonadState TIState, MonadReader Env, MonadError String)

type Env = Map.Map E.Id (Either FType Type)
type TIState = (Int, Subst)

runTI :: Env -> TI a -> Either String a
runTI env (TI m) = liftM fst (evalRWST m env initialState)
  where initialState = (0, nullSubst)

getSubst :: TI Subst
getSubst = do
  (_,s) <- get
  return s

extSubst :: Subst -> TI ()
extSubst s2 = do
  (i,s1) <- get
  put (i, s2@@s1)
  return ()

appSubst :: (FEs a) => a -> TI a
appSubst x = do
  s <- getSubst
  return (apply s x)

unify :: FE.FE -> FE.FE -> TI ()
unify a b = do
  s1 <- getSubst
  s2 <- mgu (apply s1 a) (apply s1 b)
  extSubst s2

newFEVar :: TI FE.FE
newFEVar = do
  (i,s) <- get
  put (i+1,s)
  return $ FE.Var i

----------------------------------------------------------------------------

inferType :: E.Exp -> TI Typing
inferType E.Identity = iIdentity
inferType (E.Comp a b) =  do
  a' <- inferType a
  b' <- inferType b
  iComp a' b'
inferType (E.Nat nat) = iNat nat
inferType (E.Fact obj args)  = iFact obj  =<< mapM inferType args
inferType (E.Funct obj args) = iFunct obj =<< mapM inferType args
inferType (E.Var v args) = iVar v =<< mapM inferType args

inferType2 :: [E.Id] -> E.Exp -> TI ([Type], Typing)
inferType2 ps e = do
  ps <- liftM Map.fromList $ forM ps $ \p -> do
    dom <- newFEVar
    cod <- newFEVar
    return (p, dom :-> cod)
  local (Map.union (Map.map Right ps)) $ do
    t <- inferType e
    return (map snd (Map.toList ps), t)

----------------------------------------------------------------------------
-- introduction rules

iIdentity :: TI Typing
iIdentity = do
  v <- newFEVar
  return $ Identity v :! v :-> v

iComp :: Typing -> Typing -> TI Typing
iComp (f :! domf :-> codf) (g :! domg :-> codg) = do
  unify domf codg
  return $ Comp f g :! domg :-> codf

iNat :: CDT.Nat -> TI Typing
iNat nat = do
  annotation <- sequence $ replicate (CDT.natNTypeParams nat) newFEVar
  return $ Nat nat annotation :! apply (zip [0..] annotation) (CDT.natType nat)

iFact :: CDT.CDT -> [Typing] -> TI Typing
iFact obj args = do
  annotation <- sequence $ replicate (CDT.factNTypeParams obj) newFEVar
  let s = zip [0..] annotation
  unifyParams (apply s (CDT.factParams obj)) args
  return $ Fact obj (map tm args) annotation :! apply s (CDT.factDestType obj)

iFunct :: CDT.CDT -> [Typing] -> TI Typing
iFunct obj args = do
  (doml,codl) <- unifyParams (variance obj) args
  return $ Funct obj (map tm args) :! FE.Ap obj doml :-> FE.Ap obj codl
  where
    unifyParams [] [] = return ([],[])
    unifyParams (v:vs) ((_ :! d:->c) : as) = do
      (doms,cods) <- unifyParams vs as
      case v of
        Covariance     -> return (d:doms, c:cods)
        Contravariance -> return (c:doms, d:cods)
        FixedVariance  -> unify d c >> return (d:doms, d:cods)
        FreeVariance   -> do
          x <- newFEVar
          y <- newFEVar
          return (x:doms, y:cods)
    unifyParams _ _ = throwError "wrong number of arguments"

iVar :: E.Id -> [Typing] -> TI Typing
iVar v args = do
  env <- ask
  case Map.lookup v env of
    Just (Right t) ->
      return $ Var v [] [] t :! t
    Just (Left (FType n typs typ)) -> do
      annotation <- sequence (replicate n newFEVar)
      let s = zip [0..] annotation
      unifyParams (apply s typs) args
      let t = apply s typ
      return $ Var v annotation (map tm args) t :! t
    Nothing -> throwError $ "no such variable: " ++ v

unifyParams :: [Type] -> [Typing] -> TI ()
unifyParams [] [] = return ()
unifyParams ((pdom :-> pcod):ps) ((_ :! adom:->acod):as) = do
  unify pdom adom
  unify pcod acod
  unifyParams ps as
  return ()
unifyParams _ _ = throwError "wrong number of arguments"

----------------------------------------------------------------------------
