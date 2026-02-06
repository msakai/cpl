{- # OPTIONS -ddump-simpl -ddump-stg # -}
-----------------------------------------------------------------------------
-- |
-- Module      :  Simp
-- Copyright   :  (c) Masahiro Sakai 2004-2009
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Simplifier
--
-----------------------------------------------------------------------------

module Simp
    ( CompiledExp
    , compile
    , decompile
    , simp
    , simpWithTrace
    ) where

import qualified Exp as E
import qualified CDT
import qualified FE
import Exp (Id)
import Type
import Control.Monad
import Control.Monad.RWS
import Data.Array
import qualified Data.Map as Map

----------------------------------------------------------------------------

data CompiledExp
  = Identity
  | Comp CompiledExp CompiledExp
  | LNat !CDT.Nat
  | RNat !CDT.Nat
  | LFact !CDT.CDT [CompiledExp] !(Array Int CompiledExp)
  | RFact !CDT.CDT [CompiledExp]
  | Var E.Exp CompiledExp

compile :: (Map.Map E.Id ([E.Id], E.Exp)) -> E.Exp -> CompiledExp
compile env = f
  where
    f E.Identity         = Identity
    f (E.Comp a b)       = f a `comp` f b
    f (E.Funct sym args) = expandFunct sym (map f args)
    f (E.Fact sym args)  = mkFact sym (map f args)
    f (E.Nat sym)        = mkNat sym
    f src@(E.Var v args) = Var src (compile env' body)
      where
        (ps, body) = env Map.! v
        -- Note that Map.union is left biased
        env' = Map.union (Map.fromList [(p, ([], arg)) | (p,arg) <- zip ps args]) env

{-# INLINE mkFact #-}
mkFact :: CDT.CDT -> [CompiledExp] -> CompiledExp
mkFact obj args =
  case CDT.objectType obj of
    CDT.LeftObject -> lfact
      where
        lfact = LFact obj args (listArray (0, CDT.nNats obj - 1) l)
        l = zipWith f args (CDT.nats obj)
        f arg nat =
          case CDT.natIsUnconditioned nat of --- optimize
            True  -> arg
            False -> arg `comp` (subst1 lfact (CDT.natDeclDom nat))
    CDT.RightObject -> RFact obj args

{-# INLINE mkNat #-}
mkNat :: CDT.Nat -> CompiledExp
mkNat sym =
  case CDT.objectType (CDT.natCDT sym) of
    CDT.LeftObject  -> LNat sym
    CDT.RightObject -> RNat sym

decompile :: CompiledExp -> E.Exp
decompile Identity = E.Identity
decompile (Comp e1 e2) = E.Comp (decompile e1) (decompile e2)
decompile (LNat nat) = E.Nat nat
decompile (RNat nat) = E.Nat nat
decompile (LFact sym args _) = E.Fact sym (map decompile args)
decompile (RFact sym args)   = E.Fact sym (map decompile args)
decompile (Var src _) = src

----------------------------------------------------------------------------

simp :: Bool -> CompiledExp -> CompiledExp
simp full startExp =
  if full
  then simpFull startExp
  else simpLazy startExp

{-# NOINLINE simpFull #-}
{-# NOINLINE simpLazy #-}
simpFull, simpLazy :: CompiledExp -> CompiledExp
simpFull = simpImpl True
simpLazy = simpImpl False

{-# INLINE simpImpl #-}
simpImpl :: Bool -> CompiledExp -> CompiledExp
simpImpl full startExp = seq full $ simp1 startExp Identity
  where
    simp1 :: CompiledExp -> CompiledExp -> CompiledExp

    simp1 Identity c = c --- IDENT

    simp1 (Comp a b) c = simp1 a (simp1 b c) --- COMP

    simp1 e@(LNat _) c = e `comp` c -- L-NAT
    simp1 (RNat sym) c
      | full = simp1_FULL_R_NAT sym c --- FULL-R-NAT
      | otherwise = simp1_R_NAT sym c --- R-NAT

    simp1 (LFact _ _ table) c = --- L-FACT
      case split c of
      (LNat sym, c') -> simp1 (table ! (CDT.natIndex sym)) c'
      _ -> impossible

    simp1 e@(RFact obj args) c
      | full && CDT.isUnconditioned obj = simp1_FULL_C_FACT obj args c
      | otherwise = e `comp` c -- R-FACT

    simp1 (Var _ e) c = simp1 e c

    ----------------------------------

    simp1_R_NAT :: CDT.Nat -> CompiledExp -> CompiledExp
    simp1_R_NAT sym c =
      case simp2 c sym of
      (factR@(RFact _ args), c'') ->
        if CDT.natIsUnconditioned sym --- optimize
        then simp1 (args !! CDT.natIndex sym) c''
        else simp1 (subst1 factR (CDT.natDeclCod sym))
                   (simp1 (args !! CDT.natIndex sym) c'')
      _ -> impossible

    simp1_FULL_R_NAT :: CDT.Nat -> CompiledExp -> CompiledExp
    simp1_FULL_R_NAT sym factP =
        case pickupFactR sym factP of
        (factR@(RFact _ args), factP') ->
            if CDT.natIsUnconditioned sym
            then simp1 (args !! CDT.natIndex sym) factP'
            else simp1 (subst1 factR (CDT.natDeclCod sym))
                       (simp1 (args !! CDT.natIndex sym) factP')
        _ -> impossible

    simp1_FULL_C_FACT :: CDT.CDT -> [CompiledExp] -> CompiledExp -> CompiledExp
    simp1_FULL_C_FACT obj args p = RFact obj (zipWith f args (CDT.nats obj))
        {- 並列処理出来るのって、ここのzipWithくらいだろうか -}
        where
          f e nat =
            case CDT.natDeclDom nat of
              FE.Var 0 -> simp1 e p
              fe       -> e `comp` subst1 p fe -- ack(3,3)で16000回くらい

    ----------------------------------

    simp2 :: CompiledExp -> CDT.Nat -> (CompiledExp, CompiledExp)
    simp2 c sym = f (CDT.natProjectionSequence sym) c
        where
          f :: [Int] -> CompiledExp -> (CompiledExp, CompiledExp)
          f [] c      = split c --- R-NAT-V
          f (j:js) c_ =         --- R-NAT-F
            case split c_ of
            (RFact p args, c) ->
                case g 0 args (CDT.nats p) of
                    (factR, args') -> (factR, (RFact p args'))
                where
                  g i (arg:args) (nat:nats)
                      | i==j =
                          case f js (simp1 arg c) of
                            (factR', arg') -> (factR',arg':args')
                      | otherwise =
                          let arg' = arg `comp` subst1 c (CDT.natDeclDom nat)
                          in (factR, arg':args')
                      where (factR, args') = g (i+1) args nats
                  g _ [] [] = (undefined, [])
                  g _ _ _ = impossible
            _ -> impossible

{-# INLINE pickupFactR #-}
pickupFactR :: CDT.Nat -> CompiledExp -> (CompiledExp,CompiledExp)
pickupFactR sym = f (CDT.natProjectionSequence sym)
  where
    f :: [Int] -> CompiledExp -> (CompiledExp, CompiledExp)
    f [] e = split e
    f (j:js) (RFact p args) =
        case processArgs 0 args of
          (factR, args') -> (factR, RFact p args')
      where
        processArgs :: Int -> [CompiledExp] -> (CompiledExp, [CompiledExp])
        processArgs i (arg:args)
            | i==j =
                case f js arg of
                  (factR, arg') -> (factR, arg':args)
            | otherwise =
                case processArgs (i+1) args of
                  (factR, args') -> (factR, arg:args')
        processArgs _ _ = impossible
    f _ _ = impossible

----------------------------------------------------------------------------

type Trace = [(Int,CompiledExp,CompiledExp)]
type M = RWS Int Trace ()

runM :: M x -> Trace
runM x =
  case runRWS x 0 () of
    (_,_,c) -> c

trace :: CompiledExp -> CompiledExp -> M ()
trace e c = do
  depth <- ask
  seq depth $ seq e $ seq c $ tell [(depth,e,c)]

deepen :: M a -> M a
deepen = local (+1)

simpWithTrace :: Bool -> CompiledExp -> Trace
simpWithTrace full startExp = seq full $ runM $ do
    c <- simp1 startExp Identity
    trace Identity c
    return ()
  where
    simp1 :: CompiledExp -> CompiledExp -> M CompiledExp

    simp1 a c = trace a c >> simp1' a c

    simp1' Identity c = return c

    simp1' (Comp a b) c = do
      c' <- deepen (simp1 b c)
      simp1 a c'

    simp1' e@(LNat _) c = return (e `comp` c) --- L-NAT
    simp1' (RNat sym) c
      | full = simp1_FULL_R_NAT sym c --- FULL-R-NAT
      | otherwise = simp1_R_NAT sym c --- R-NAT

    simp1' (LFact _ _ table) c = --- L-FACT
      case split c of
        (LNat sym, c') -> simp1 (table ! (CDT.natIndex sym)) c'
        _ -> impossible

    simp1' e@(RFact obj args) c
      | full && CDT.isUnconditioned obj = simp1_FULL_C_FACT obj args c --- FULL-C-FACT
      | otherwise = return (e `comp` c) -- R-FACT

    simp1' (Var _ e) c = simp1 e c

    ----------------------------------

    simp1_R_NAT :: CDT.Nat -> CompiledExp -> M CompiledExp
    simp1_R_NAT sym c = do
      tmp <- simp2 c sym
      case tmp of
        (factR@(RFact _ args), c'') ->
          if CDT.natIsUnconditioned sym
          then simp1 (args !! CDT.natIndex sym) c''
          else simp1 (subst1 factR (CDT.natDeclCod sym) `comp` (args !! CDT.natIndex sym)) c''
        _ -> impossible

    simp1_FULL_R_NAT :: CDT.Nat -> CompiledExp -> M CompiledExp
    simp1_FULL_R_NAT sym factP =
      case pickupFactR sym factP of
        (factR@(RFact _ args), factP') ->
          if CDT.natIsUnconditioned sym
          then simp1 (args !! CDT.natIndex sym) factP'
          else simp1 (subst1 factR (CDT.natDeclCod sym) `comp` (args !! CDT.natIndex sym)) factP'
        _ -> impossible

    simp1_FULL_C_FACT :: CDT.CDT -> [CompiledExp] -> CompiledExp -> M CompiledExp
    simp1_FULL_C_FACT obj args p = do
      args' <- zipWithM f args (CDT.nats obj)
      return (RFact obj args')
      where
        f e nat =
          case CDT.natDeclDom nat of
            FE.Var 0 -> simp1 e p
            fe       -> return (e `comp` subst1 p fe) -- ack(3,3)で16000回くらい

    ----------------------------------

    simp2 :: CompiledExp -> CDT.Nat -> M (CompiledExp, CompiledExp)
    simp2 c sym = f (CDT.natProjectionSequence sym) c
      where
        f :: [Int] -> CompiledExp -> M (CompiledExp, CompiledExp)
        f [] c      = return (split c) --- R-NAT-V
        f (j:js) c_ =                  --- R-NAT-F
          case split c_ of
            (RFact p args, c) -> do
              (factR, args') <- g 0 args (CDT.nats p)
              return (factR, (RFact p args'))
              where
                g _ [] [] = return (undefined, [])
                g i (arg:args) (nat:nats)
                  | i==j = do
                    (_, args') <- g (i+1) args nats
                    tmp <- simp1 arg c
                    (factR', arg') <- f js tmp
                    return (factR',arg':args')
                  | otherwise = do
                    (factR, args') <- g (i+1) args nats
                    let arg' = arg `comp` subst1 c (CDT.natDeclDom nat)
                    return (factR, arg':args')
                g _ _ _ = impossible
            _ -> impossible

----------------------------------------------------------------------------

{-# INLINE comp #-}
comp :: CompiledExp -> CompiledExp -> CompiledExp
comp a Identity = a
comp Identity b = b
comp a b        = Comp a b

split :: CompiledExp -> (CompiledExp, CompiledExp)
split (Comp a b) =
    case split a of
    (c, d) -> (c, d `comp` b)
split a = (a,Identity)
{-
split :: CompiledExp -> (CompiledExp, CompiledExp)
split (Comp a b) = go a b
  where
    go (Comp a b) r = go a (Comp b r)
    go e r = (e, r)
split e = (e, Identity)
-}

subst1 :: CompiledExp -> FE.FE -> CompiledExp
subst1 x e = FE.fold f expandFunct e
  where
    f 0 = x
    f _ = Identity

expandFunct :: CDT.CDT -> [CompiledExp] -> CompiledExp
expandFunct _   []   = Identity
expandFunct obj args = mkFact obj (map g (CDT.nats obj))
  where
    g nat = FE.fold h expandFunct cod `comp` (mkNat nat `comp` FE.fold h expandFunct dom)
      where
        h 0 = Identity
        h i = args !! (i-1)
        dom:->cod = CDT.natDeclType nat

impossible :: a
impossible = error "impossible happens"
