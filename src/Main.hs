{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Masahiro Sakai 2004,2009
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Main where

import CDT
import Exp
import qualified Statement
import qualified CPLSystem as Sys
import qualified AExp
import Type
import Typing (Typing(..))
import qualified Simp

import Data.Maybe
import Data.List
import Data.Char (isSpace)
import System.Environment
import System.Exit
import System.IO
import Control.Monad.State.Strict -- haskeline's MonadException requries strict version
import System.Console.GetOpt
#if defined(USE_READLINE_PACKAGE)
import qualified System.Console.SimpleLineEditor as SLE
import Control.Exception (bracket)
import Control.Monad.Error (catchError)
#elif defined(USE_HASKELINE_PACKAGE)
import System.Console.Haskeline
#else
import Control.Exception (bracket)
import Control.Monad.Error (catchError)
#endif

----------------------------------------------------------------------------

type UIState = Sys.System

#ifdef USE_HASKELINE_PACKAGE
type UI a = StateT UIState (InputT IO) a
#else
type UI a = StateT UIState IO a
#endif

initialState :: UIState
initialState = Sys.emptySystem

----------------------------------------------------------------------------
--- Utility

shift :: String -> (String, String)
shift = break isSpace . dropWhile isSpace

strip :: String -> String
strip = reverse . f . reverse . f
    where f = dropWhile isSpace

indent :: Int -> String -> String
indent n = unlines . map (prefix++) . lines
    where prefix = replicate n ' '

showObjectInfo :: CDT.CDT -> String
showObjectInfo obj =
    t ++ showFunctNameWithVariance obj ++ "\n" ++
    "- natural transformations:\n" ++
    natsStr ++
    "- factorizer:\n" ++ factorizerInfoStr ++
    "- equations:\n" ++ indent 4 equations ++
    "- unconditioned: " ++ (if CDT.isUnconditioned obj then "yes" else "no") ++
    "\n" ++
    "- productive: (" ++ productiveStr ++ ")\n"
    where t = case CDT.objectType obj of
              LeftObject  -> "left object "
              RightObject -> "right object "
          natsStr = indent 4 $ concatMap f (CDT.nats obj)
              where f nat = CDT.natName nat ++ ": " ++
                            show (CDT.natType nat) ++ "\n"
          productiveStr = intercalate "," (map f [0 .. CDT.functArity obj - 1])
              where f n = if CDT.isProductiveIn obj n
                          then "yes"
                          else "no"
          factorizerInfoStr =
              indent 4 $
                  upper ++ "\n" ++
                  replicate (max (length upper) (length lower)) '-' ++ "\n" ++
                  lower ++ "\n"
              where upper = intercalate "  " $ zipWith f factArgs $ factParams obj
                        where f fact typ = show fact ++ ": " ++ show typ
                    lower =
                        show (Fact obj factArgs) ++ ": " ++
                        show (factDestType obj)
                    factArgs = map f (CDT.nats obj)
                        where f nat = Var ("f" ++ show (CDT.natIndex nat)) []
          equations = concat (map (++"\n") (eqs ++ [feq, ceq]))
              where eqs = zipWith g [(1::Int)..] (Statement.eqs obj)
                        where g n eq = "("++lr++"EQ" ++ show n ++ "): " ++
                                       show eq
                    feq = "("++lr++"FEQ): " ++ show (Statement.feq obj)
                    ceq = "("++lr++"CEQ): " ++ show (Statement.ceq obj)
                    lr = case CDT.objectType obj of
                         LeftObject  -> "L"
                         RightObject -> "R"

readLine :: String -> UI String
#if defined(USE_READLINE_PACKAGE)
readLine prompt = liftIO $ fmap (fromMaybe "") (SLE.getLineEdited prompt)
#elif defined(USE_HASKELINE_PACKAGE)
readLine prompt = fmap (fromMaybe "") (lift (getInputLine prompt))
#else
readLine prompt = liftIO $ putStr prompt >> getLine
#endif

----------------------------------------------------------------------------

type Command = String -> UI ()

commandTable :: [(String, Command)]
commandTable =
    [ ("show",  cmdShow)
    , ("edit",  cmdEdit)
    , ("simp",  cmdSimp)
    , ("let",   cmdLet)
    , ("load",  cmdLoad)
    , ("quit",  cmdQuit)
    , ("exit",  cmdQuit)
    , ("bye",   cmdQuit)
    , ("help",  cmdHelp)
    , ("set",   cmdSet)
    , ("reset", cmdReset)

    , ("left",  cmdLeft)
    , ("right", cmdRight)
    ]

dispatchCommand :: String -> UI ()
dispatchCommand l =
    case shift l of
        ([], _) -> return ()
        (cmdStr, arg) ->
            case lookup cmdStr commandTable of
            Just cmd -> cmd arg
            Nothing  -> liftIO $ ioError $ userError $  ("unknown command: " ++ l)

----------------------------------------------------------------------------

defineObject :: Command
defineObject src =
    do sys <- get
       obj <- Sys.parseCDT sys src
       sys' <- Sys.addCDT sys obj
       put sys'
       let lr = case CDT.objectType obj of
                     LeftObject  -> "left"
                     RightObject -> "right"
           msg = concat [lr, " object ", showFunctNameWithVariance obj, " is defined"]
       liftIO $ putStrLn $ msg

cmdLeft, cmdRight :: Command
cmdLeft  s = defineObject ("left " ++ s)
cmdRight s = defineObject ("right " ++ s)

cmdShow :: Command
cmdShow arg =
    case shift arg of
    ("object", arg') ->
        do sys <- get
           let name    = strip arg'
               objects = Sys.objects sys
           liftIO $ putStrLn $
                case find (\x -> CDT.functName x == name) objects of
                Just obj -> showObjectInfo obj
                Nothing  -> "unknown object: " ++ name
    ("aexp", arg') -> do -- XXX
      sys <- get
      case Sys.parseExp sys (strip arg') of
        Left err -> fail err
        Right (_, e :! t) -> liftIO $ do
          putStrLn $ show e
          putStrLn $ "    : " ++ show t
    _ -> do
      sys <- get
      case Sys.parseExp sys (strip arg) of
        Left err -> fail err
        Right (_, e :! t) -> liftIO $ do
          putStrLn $ show $ AExp.skelton e
          putStrLn $ "    : " ++ show t

cmdLet :: Command
cmdLet arg = do
  sys <- get
  case Sys.parseDef sys (strip arg) of
    Left err -> fail err
    Right def@(name, args, e, FType _ args' t) -> do
      sys' <- Sys.letExp sys def
      put sys'
      if null args
        then liftIO $ do
          putStrLn $ name ++ " = " ++ show (AExp.skelton e)
          putStrLn $ "    : " ++ show t
        else liftIO $ do
          let lhs = name ++ "(" ++ intercalate "," args ++ ")"
          putStrLn $ lhs ++ " = " ++ show (AExp.skelton e)
          let upper = intercalate "  " $ [p ++ ": " ++ show t | (p,t) <- zip args args']
              lower = lhs ++ ": " ++ show t
              s = upper ++ "\n" ++
                  replicate (max (length upper) (length lower)) '-' ++ "\n" ++
                  lower
          putStrLn $ s
          -- putStrLn $ "    : " ++ intercalate ", " (map show args') ++ " => " ++ show t

cmdSimp :: Command
cmdSimp arg =
  case shift arg of
    ("full", arg') ->
        doSimp True (strip arg')
    _ ->
        doSimp False (strip arg)
  where
    doSimp full str = do
      sys <- get
      unless (any isTerminalObject (Sys.objects sys))
             (fail "No terminal object is defined.")
      case Sys.parseExp sys str of
        Left err -> fail err
        Right (_, e :! t) -> do
          unless (AExp.isElement e) (fail "not a element")
          let traces = Sys.simp sys full (AExp.skelton e)
              loop ((step,(depth,exp,cexp)) : xs) = do
                let line = show step
                         ++ (if depth==0 then "" else "[" ++ show depth ++ "]")
                         ++ ":" ++ show (Simp.decompile exp) ++ "*" ++ show (Simp.decompile cexp)
                when (Sys.trace sys) $ liftIO $ putStrLn line
                if null xs
                  then do
                    let it = Simp.decompile cexp
                    liftIO $ putStrLn (show it)
                    liftIO $ putStrLn ("    : " ++ show t)
                    put sys{ Sys.lastExp = Just it }
                  else
                    loop xs
          loop (zip [(0::Int)..] traces)

cmdLoad :: Command
cmdLoad s =
    do h <- liftIO $ openFile filename ReadMode
       contents <- liftIO $ hGetContents h
       let src  = unlines (map removeComment (lines contents))
           cmds = split src
       forM_ cmds $ \cmd -> do
         liftIO $ (putStr . unlines . map ("> "++) . lines $ cmd)
         dispatchCommand cmd
    where filename = -- FIXME
              let s' = strip s in
                  case s' of
                  '"':_ -> read s'
                  _     -> s'
          removeComment []      = []
          removeComment ('#':_) = []
          removeComment (x:xs)  = x : removeComment xs
          split :: String -> [String]
          split s = map (strip . reverse) (f s [])
              where f (';':xs) tmp = tmp : (f xs [])
                    f (x:xs) tmp   = f xs (x:tmp)
                    f [] tmp       = [tmp]

cmdEdit :: Command
cmdEdit _ =
    do s <- editLoop
       dispatchCommand s
    where editLoop =
              do l <- readLine "| "
                 case dropWhile isSpace (reverse l) of
                     ';':s -> return (reverse s)
                     _ -> do s <- editLoop
                             return $ l ++ "\n" ++ s

cmdQuit :: Command
cmdQuit _ = liftIO $ exitWith ExitSuccess

cmdHelp :: Command
cmdHelp _ = liftIO $ mapM_ putStrLn l
    where l = [ "  exit                        exit the interpreter"
              , "  quit                        ditto"
              , "  bye                         ditto"
              , "  edit                        enter editing mode"
              , "  simp [full] <exp>           evaluate expression"
              , "  show <exp>                  print type of expression"
              , "  show object <functor>       print information of functor"
              , "  load <filename>             load from file"
              , "  set trace [on|off]          enable/disable trace"
              , "  reset                       remove all definitions"
              ]

cmdSet :: Command
cmdSet arg =
    case shift arg of
    (flag, a) ->
        case shift a of
        ([], _) ->
            case flag of
            "trace" ->
                do sys <- get
                   liftIO $ putStrLn $
                            "trace=" ++ (if Sys.trace sys then "on" else "off")
            _ ->
                liftIO $ putStrLn $ "unknown flag:" ++ flag
        (value, _) ->
            case flag of
            "trace" ->
                case value of
                "on"  ->
                    do sys <- get
                       put (sys{ Sys.trace = True })
                "off" ->
                    do sys <- get
                       put (sys{ Sys.trace = False })
                _ ->
                    liftIO $ putStrLn ("unknown value:" ++ value)
            _ ->
                liftIO $ putStrLn ("unknown flag:" ++ flag)

cmdReset :: Command
cmdReset _ = put initialState

----------------------------------------------------------------------------

data Flag
    = Help
    | Version
    | Interactive
    -- | Load String
    | Trace String
    deriving Eq

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]    (NoArg Help)            "show help"
    , Option ['v'] ["version"] (NoArg Version)         "show version number"
    , Option ['i'] ["interactive"] (NoArg Interactive) "force interactive mode"
    -- , Option ['l'] ["load"]    (ReqArg Load "FILE") "load FILE"
    , Option ['t'] ["trace"]    (OptArg (Trace . fromMaybe "on") "[on|off]")
             "enable/disable trace"
    ]

main :: IO ()
#if defined(USE_READLINE_PACKAGE)
main = bracket SLE.initialise (const SLE.restore) (const main_)
#elif defined(USE_HASKELINE_PACKAGE)
main = runInputT defaultSettings main_
#else
main = 
    do bracket (do x <- hGetBuffering stdout
                   hSetBuffering stdout NoBuffering
                   return x)
               (hSetBuffering stdout)
               (const main_)
#endif

main_ =
    do args <- liftIO $ getArgs
       case getOpt Permute options args of
         (o,_,[])
           | Help `elem` o    -> liftIO $ putStrLn (usageInfo header options)
           | Version `elem` o -> liftIO $ putStrLn versionStr
         (o,n,[]) ->
             do liftIO $ putStr banner
                evalStateT (do mapM_ processOpt o
                               mapM_ cmdLoad n
                               if null n || Interactive `elem` o
                                   then mainLoop
                                   else return ())
                           initialState
         (_,_,errs) ->
             liftIO $ ioError $ userError $ concat errs ++ usageInfo header options

version :: [Int]
version = [0,0,6]

versionStr :: String
versionStr = intercalate "." $ map show $ version

header :: String
header = "Usage: cpl [OPTION...] files..."

banner :: String
banner =
    "Categorical Programming Language (Haskell version)\n" ++
    "version " ++ versionStr ++ "\n" ++
    "\n" ++
    "Type help for help\n" ++
    "\n"

processOpt :: Flag -> UI ()
processOpt (Trace s) =
    do sys <- get
       val <- case s of
              "on"  -> return True
              "off" -> return False
              _     -> fail "invalid option"
       put sys{ Sys.trace = val }
       return ()
processOpt _ = return ()

mainLoop :: UI ()
mainLoop = do
  l <- readLine "cpl> "
#if defined(USE_HASKELINE_PACKAGE)
  handle h (dispatchCommand l)
#else
  dispatchCommand l `catchError`  h
#endif
  mainLoop
  where
    h :: IOError -> UI ()
    h = liftIO . putStrLn . show

----------------------------------------------------------------------------
