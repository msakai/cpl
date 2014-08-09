-----------------------------------------------------------------------------
-- |
-- Module      :  CPLSystem
-- Copyright   :  (c) Masahiro Sakai 2004,2009

-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module CPLSystem
    ( VarTable
    , System (..)
    , emptySystem
    , parseExp
    , checkName
    , parseCDT
    , parseDef
    , addCDT
    , letExp
    , simp
    ) where

import CDT
import qualified CDTParser
import qualified ExpParser
import Exp
import qualified AExp
import qualified Simp
import Type
import qualified Typing
import Typing (Typing(..))

-- FIXME
import qualified FE
import qualified Subst

import Control.Monad
import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

type CDTEnv = [CDT]

type VarTable = Map.Map Id ([Id], Exp, FType)

data System
    = System
    { objects  :: !CDTEnv
    , varTable :: !VarTable
    , trace    :: !Bool
    , lastExp  :: !(Maybe Exp)
    }

emptySystem :: System
emptySystem =
    System{ objects  = []
          , varTable = Map.empty
          , trace    = False
          , lastExp  = Nothing
          }

parseExp :: System -> String -> Either String (Int, Typing)
parseExp sys str =
  case parse ExpParser.exp "" str of
    Left err -> Left (show err)
    Right e ->
      case ExpParser.evalExp (objects sys) (arityEnv sys) e of
        Nothing -> Left "invalid expression" -- FIXME
        Just e2 ->
          Typing.runTI (tiEnv sys) $ do
            t <- Typing.inferType (substIt sys e2)
            t2 <- Typing.appSubst t
            return $ AExp.quantify t2

type Def = (Id, [Id], AExp.AExp, FType)

parseDef :: System -> String -> Either String Def
parseDef sys str =
  case parse ExpParser.def "" str of
    Left err -> Left (show err)
    Right (name,ps,e) ->
      case ExpParser.evalExp (objects sys) (Map.union (Map.fromList (zip ps (repeat 0))) (arityEnv sys)) e of
        Nothing -> Left "invalid definition" -- FIXME
        Just e1 ->
          Typing.runTI (tiEnv sys) $ do
            (ts, t) <- Typing.inferType2 ps e1
            ts2 <- Typing.appSubst ts
            (ae :! t') <- Typing.appSubst t
            let vars = Subst.tv (t' : ts2)
                s = zip vars [FE.Var i | i<-[0..]]
            return (name, ps, Subst.apply s ae, FType (length vars) (Subst.apply s ts2) (Subst.apply s t'))

arityEnv :: System -> Map.Map Id Int
arityEnv sys = if isJust (lastExp sys) then Map.insert "it" 0 env0 else env0
  where env0 = Map.map (\(ps,_,_) -> length ps) (varTable sys)

tiEnv :: System -> Map.Map Id (Either FType Type)
tiEnv sys = Map.map (\(_, _, t) -> Left t) (varTable sys)

substIt :: System -> Exp -> Exp
substIt sys e = 
  case lastExp sys of
    Nothing -> e
    Just it -> f e
      where
        f Identity         = Identity
        f (Comp a b)       = Comp (f a) (f b)
        f e'@(Nat _)       = e'
        f (Fact obj args)  = Fact obj $ map f args
        f (Funct obj args) = Funct obj $ map f args
        f (Var "it" [])    = it
        f (Var v args)     = Var v $ map f args

checkName :: System -> String -> Either String ()
checkName sys name =
  if name `elem` names
    then Left $ "\"" ++ name ++ "\" is already used"
    else return ()
  where
    vt = varTable sys
    objs = objects sys
    names = Map.keys vt ++
            map CDT.functName objs ++
            map CDT.factName objs  ++
            concatMap (map CDT.natName . CDT.nats) objs

parseCDT :: System -> String -> Either String CDT.CDT
parseCDT sys src = case parse CDTParser.cdtDecl "" src of
                   Left err   -> Left (show err)
                   Right decl -> CDTParser.evalCDTDecl (objects sys) decl

addCDT :: System -> CDT.CDT -> Either String System
addCDT sys obj =
    if CDT.isComputable obj
    then do checkName sys (CDT.functName obj)
            checkName sys (CDT.factName obj)
            mapM_ (checkName sys . CDT.natName) (CDT.nats obj)
            return sys{ objects = obj : objects sys }
    else Left "not a computable object"

compile :: System -> Exp -> Simp.CompiledExp
compile sys e = Simp.compile env e
  where env = Map.map (\(ps,body,_) -> (ps, body)) (varTable sys)

letExp :: System -> Def -> Either String System
letExp sys (name,ps,e,ftype) = do
  checkName sys name
  return sys{ varTable = Map.insert name (ps, AExp.skelton e, ftype) (varTable sys) }

simp :: System -> Bool -> Exp -> [(Int, Simp.CompiledExp, Simp.CompiledExp)]
simp sys full e =
    let e' = compile sys e
        traces =
            if trace sys
               then Simp.simpWithTrace full e'
               else [(0, compile sys Identity, Simp.simp full e')]
    in traces
