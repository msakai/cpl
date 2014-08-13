{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CDT
-- Copyright   :  (c) Masahiro Sakai 2004,2009
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Categorical Data Type
--
-----------------------------------------------------------------------------

module CDT
  ( ObjectType (..)
  , CDT
  , mkCDT
  , objectType
  , nats
  , nNats
  , isUnconditioned

  , functName
  , functVariance
  , functArity

  , Nat
  , natName
  , natNTypeParams
  , natType
  , natCDT
  , natIndex
  , natDeclType
  , natDeclDom
  , natDeclCod
  {- for optimization -}
  , natProjectionSequence
  , natIsUnconditioned

  , factName
  , factParams
  , factDestType
  , factNTypeParams

  , _eqCDT -- XXX

  , isComputable
  , isProductiveIn
  , showFunctNameWithVariance

  , isTerminalObject
  ) where

import Variance
import Funct
import FE
import Type
import Subst (tv, apply)

#if __GLASGOW_HASKELL__ >= 706
import Prelude hiding (join)
#endif
import Data.List (findIndices, transpose, find, findIndex, intercalate)


data ObjectType
  = LeftObject
  | RightObject
  deriving (Show, Read, Eq)

data CDT
  = CDT
  { objectType      :: !ObjectType
  , nats            :: ![Nat]
  , nNats           :: !Int -- = length of nats
  , isUnconditioned :: !Bool

  , functName       :: !String
  , functVariance   :: ![Variance]
  , functArity      :: !Int -- = length of functVariance

  , factName        :: !String
  , factParams      :: ![Type]
  , factDestType    :: Type -- ループを避けるために正格フラグを付けない
  }

instance Funct CDT where
  variance CDT{ functVariance = vs } = vs

instance Eq CDT where
  a==b = objectType a == objectType b &&
         nNats a      == nNats b &&
         functName a  == functName b &&
         functArity a == functArity b &&
         all (\(n1,n2) -> natName n1 == natName n2 &&
                          natDeclType n1 == natDeclType n2)
             (zip (nats a) (nats b))

_eqCDT :: CDT -> CDT -> Bool
_eqCDT a b = a==b

data Nat
  = Nat
  { natName     :: !String
  , natType     :: !Type
  , natCDT      :: !CDT
  , natIndex    :: !Int
  , natDeclType :: !Type

  {- for optimization -}

  -- right object でしか意味が無いので!を付けない
  , natProjectionSequence :: [Int]

  , natIsUnconditioned :: !Bool
    {- LeftObjectならnatDeclのdomに、
     - RightObjectならnatDeclのcodに
     - そのオブジェクト(Var 0)が現われていない場合に真。
     -}
  }

instance Eq Nat where
  a==b = natCDT a == natCDT b && natIndex a == natIndex b

{-# INLINE natNTypeParams #-}
natNTypeParams :: Nat -> Int
natNTypeParams = functArity . natCDT

{-# INLINE natDeclDom #-}
natDeclDom :: Nat -> FE
natDeclDom = dom . natDeclType

{-# INLINE natDeclCod #-}
natDeclCod :: Nat -> FE
natDeclCod = cod . natDeclType

{-# INLINE factNTypeParams #-}
factNTypeParams :: CDT -> Int
factNTypeParams factObj = functArity factObj + 1


mkCDT :: ObjectType -> String -> Int -> String -> [(String,Type)] -> CDT
mkCDT t functName functArity factName natDecls = object
  where
    object =
      CDT
      { objectType      = t
      , nats            = nats
      , nNats           = length (natDecls)
      , isUnconditioned = isUnconditioned

      , functName       = functName
      , functVariance   = vs
      , functArity      = functArity

      , factName     = factName
      , factParams   = map snd natDecls
      , factDestType = factDestType
      }

    vs = map joinL . transpose . map f $ natDecls
      where
        f (_,dom:->cod) =
          zipWith g (tail (variance (CFE (1+functArity) dom)))
                    (tail (variance (CFE (1+functArity) cod)))
        g = case t of
              LeftObject  ->
                \a b -> a `join` (Contravariance `mult` b)
              RightObject ->
                \a b -> (Contravariance `mult` a) `join` b

    factDestType =
      case t of
        LeftObject  -> a :-> b
        RightObject -> b :-> a
      where a = Ap object [Var x | x <- [1..functArity]]
            b = Var 0

    nats = zipWith f natDecls [0..]
      where
        f (name, declType@(dom :-> cod)) idx = nat
          where
            nat = Nat{ natName     = name
                     , natType     = apply s declType
                     , natCDT      = object
                     , natIndex    = idx
                     , natDeclType = declType
                     , natProjectionSequence =
                         case t of
                           LeftObject -> error "not a right object"
                           RightObject -> makeProjectionSequence dom
                     , natIsUnconditioned =
                         case t of
                           LeftObject  -> not (0 `elem` tv dom)
                           RightObject -> not (0 `elem` tv cod)
                     }
            s = [(x, h x) | x <- [0..functArity]]
              where
                h 0 = Ap object [Var x | x <- [0..(functArity-1)]]
                h n = Var (n-1)

    isUnconditioned = all f natDecls
      where
        f (_, dom :-> cod) =
          case t of
            LeftObject  -> 0 `notElem` tv dom
            RightObject -> 0 `notElem` tv cod

----------------------------------------------------------------------------

isComputable :: CDT -> Bool
isComputable obj =
  case objectType obj of
    LeftObject -> all f (nats obj)
      where
        f nat = case natDeclCod nat of
                  Var 0 -> True
                  _     -> False
    RightObject -> all f (nats obj)
      where
        f nat = feIsProductiveIn (natDeclDom nat) 0

isProductiveIn :: CDT -> Int -> Bool
isProductiveIn obj i =
  objectType obj == RightObject &&
  isUnconditioned obj &&
  all (\(dom :-> _) -> i+1 `notElem` tv dom) natDecls &&
  case filter (\(_ :-> cod) -> i+1 `elem` tv cod) natDecls of
    [Var 0 :-> cod] -> feIsProductiveIn cod (i+1)
    _ -> False
  where natDecls = map natDeclType (nats obj)

feIsProductiveIn :: FE -> Int -> Bool
feIsProductiveIn (Var m) n = m==n
feIsProductiveIn (Ap functObj args) n =
  case findIndices (\arg -> n `elem` tv arg) args of
    [i] -> isProductiveIn functObj i
    _   -> False

----------------------------------------------------------------------------

makeProjectionSequence :: FE -> [Int]
makeProjectionSequence fe = 
  case fe of
    FE.Var 0 -> []
    FE.Var _ ->
      error "BUG: not a natural transformation of a computable right object"
    FE.Ap functObj args ->
      case findIndex (\arg -> 0 `elem` tv arg) args of
        Just i -> getProjection functObj i
        _      -> error "BUG"

getProjection :: CDT -> Int -> [Int]
getProjection obj i =
  case find f (nats obj) of
    Just nat -> natIndex nat : makeProjectionSequence (natDeclDom nat)
    _        -> error "BUG"
  where
    f nat = i+1 `elem` tv (natDeclCod nat) && 
            case natDeclDom nat of
              FE.Var 0 -> True
              _        -> False

----------------------------------------------------------------------------

showFunctNameWithVariance :: CDT -> String
showFunctNameWithVariance funct =
  functName funct ++
    case variance funct of
      [] -> ""
      vs -> "(" ++ intercalate "," (map Variance.mnemonic vs) ++ ")"

----------------------------------------------------------------------------

isTerminalObject :: CDT -> Bool
isTerminalObject obj = objectType obj == RightObject && null (nats obj)
