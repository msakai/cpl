-----------------------------------------------------------------------------
-- |
-- Module      :  CDTParser
-- Copyright   :  (c) Masahiro Sakai 2006,2009
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module CDTParser
    ( FE
    , Type
    , CDTDecl
    , cdtDecl
    , evalCDTDecl
    ) where

import qualified FE
import qualified Type as T
import Type (GenType(..))
import CDT
import ParserUtils

import Text.Parsec hiding (string')
import Text.Parsec.String (Parser)
import Control.Monad
import Data.List

type FE   = FE.GenFE String
type Type = T.GenType String
data CDTDecl = CDTDecl !ObjectType String !Int String [(String, Type)]

cdtDecl :: Parser CDTDecl
cdtDecl = 
    do t <- mplus (string' "left"  >> return LeftObject)
                  (string' "right" >> return RightObject)
       string' "object"
       name <- ident
       params <- option [] $ between (char' '(') (char' ')')
                           $ sepBy ident (char' ',')
       spaces
       string' "with"
       fact_name <- ident
       let endObject  = string' "end" >> string' "object"
           normalDecl = do optional (try (string' "is"))
                           manyTill (try (nat_decl (name : params)))
                                    (try endObject)
           emptyDecl  = endObject >> return []
       nat_decls <- normalDecl <|> emptyDecl
       return $ CDTDecl t name (length params) fact_name nat_decls

nat_decl :: [String] -> Parser (String, Type)
nat_decl params = 
    do name <- ident
       char' ':'
       let f x = x `elemIndex` params
       a <- fe f
       string' "->" <|> string' "→"
       b <- fe f
       return (name, a :-> b)

fe :: (String -> Maybe Int) -> Parser FE
fe lookupVar = fe'
    where fe' = 
            do name <- ident
               params <- option [] $ between (char' '(') (char' ')')
                                   $ sepBy fe' (char' ',')
               spaces
               return $ case lookupVar name of
                        Just n  -> FE.Var n
                        Nothing -> FE.Ap name params

-----------------------------------------------------------------------------

type CDTEnv = [CDT]

evalCDTDecl :: CDTEnv -> CDTDecl -> Either String CDT
evalCDTDecl cenv (CDTDecl lr name arity fact_name nat_decls) =
    do nat_decls' <- mapM (evalNatDecl cenv) nat_decls
       return $ mkCDT lr name arity fact_name nat_decls'

evalNatDecl :: CDTEnv -> (String, Type) -> Either String (String, T.Type)
evalNatDecl cenv (name, a :-> b) =
    do a' <- evalFE cenv a
       b' <- evalFE cenv b
       return (name, a' :-> b')

evalFE :: CDTEnv -> FE -> Either String FE.FE
evalFE _ (FE.Var n) = return (FE.Var n)
evalFE cenv (FE.Ap sym xs) =
    do ys <- mapM (evalFE cenv) xs
       case find (\cdt -> sym == CDT.functName cdt) cenv of
        Just f ->
            if CDT.functArity f == length xs
               then return (FE.Ap f ys)
               else Left "wrong number of arguments"
        Nothing ->
            Left $ "no such functor or variable: " ++ sym

-----------------------------------------------------------------------------
