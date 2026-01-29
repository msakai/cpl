{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if defined(USE_WASM_BACKEND)
{-# LANGUAGE JavaScriptFFI #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Masahiro Sakai 2004,2009,2014
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
import Paths_CPL

import Data.Maybe
import Data.List
import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.Version
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict -- haskeline's MonadException requries strict version
import System.Console.GetOpt
#if defined(USE_WASM_BACKEND)
import GHC.Wasm.Prim (JSString (..), toJSString, fromJSString, JSException (..), JSVal)
import Control.Exception (evaluate, try, SomeException)
#elif defined(USE_READLINE_PACKAGE)
import qualified System.Console.SimpleLineEditor as SLE
import Control.Exception (bracket)
#elif defined(USE_HASKELINE_PACKAGE)
import qualified System.Console.Haskeline as Haskeline
#else
import Control.Exception (bracket)
#endif

----------------------------------------------------------------------------

#if defined(USE_WASM_BACKEND)

-- JavaScript FFI imports for WebAssembly backend
foreign import javascript "terminal_readLine($1)"
  js_readLine :: JSString -> IO JSString

foreign import javascript unsafe "terminal_printLine($1)"
  js_printLine :: JSString -> IO ()

foreign import javascript unsafe "terminal_initialize()"
  js_initialize :: IO ()

foreign import javascript "terminal_loadFile($1)"
  js_loadFile :: JSString -> IO JSString

foreign import javascript "$1.toString()"
  js_toString :: JSVal -> IO JSString

-- Export main function for JavaScript to call as hs_start
foreign export javascript "hs_start" main :: IO ()

type Console = IO

runConsole :: Console a -> IO a
runConsole m = js_initialize >> m

readLine' :: String -> Console String
readLine' prompt = do
  result <- js_readLine (toJSString prompt)
  return $ fromJSString result

printLine' :: String -> Console ()
printLine' str = js_printLine (toJSString str)

#elif defined(USE_HASKELINE_PACKAGE)

type Console = Haskeline.InputT IO

runConsole :: Console a -> IO a
runConsole m = Haskeline.runInputT Haskeline.defaultSettings m

readLine' :: String -> Console String
readLine' prompt = liftM (fromMaybe "") $ Haskeline.getInputLine prompt

printLine' :: String -> Console ()
printLine' s = liftIO $ putStrLn $ s

#elif defined(USE_READLINE_PACKAGE)

type Console = IO

runConsole :: Console a -> IO a
runConsole m = bracket SLE.initialise (const SLE.restore) (const m)

readLine' :: String -> Console String
readLine' prompt = liftM (fromMaybe "") $ SLE.getLineEdited prompt

printLine' :: String -> Console ()
printLine' s = liftIO $ putStrLn $ s

#else

type Console = IO

runConsole :: Console a -> IO a
runConsole m = bracket initialie (hSetBuffering stdout) (const m)
  where
    initialie = do
      x <- hGetBuffering stdout
      hSetBuffering stdout NoBuffering
      return x

readLine' :: String -> Console String
readLine' prompt = putStr prompt >> getLine

printLine' :: String -> Console ()
printLine' s = liftIO $ putStrLn $ s

#endif

----------------------------------------------------------------------------

type UIState = Sys.System

initialState :: UIState
initialState = Sys.emptySystem

----------------------------------------------------------------------------

type UI a = ExceptT String (StateT UIState Console) a

readLine :: String -> UI String
readLine prompt = lift $ lift $ readLine' prompt

printLine :: String -> UI ()
printLine s = lift $ lift $ printLine' s

printLines :: [String] -> UI ()
printLines = mapM_ printLine

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
            Nothing  -> throwError ("unknown command: " ++ l)

----------------------------------------------------------------------------

defineObject :: Command
defineObject src = do
  sys <- get
  case Sys.parseCDT sys src of
    Left err -> throwError err
    Right obj -> do
      case Sys.addCDT sys obj of
        Left err -> throwError err
        Right sys' -> do
          put sys'
          let lr = case CDT.objectType obj of
                      LeftObject  -> "left"
                      RightObject -> "right"
              msg = concat [lr, " object ", showFunctNameWithVariance obj, " is defined"]
          printLine msg

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
           case find (\x -> CDT.functName x == name) objects of
             Just obj -> printLines $ lines $ showObjectInfo obj
             Nothing  -> throwError $ "unknown object: " ++ name
    ("function", arg') ->
        do sys <- get
           let name = strip arg'
           case Map.lookup name (Sys.varTable sys) of
             Just (args, _e, FType _ args' t) ->
               if null args
                 then printLines [name ++ ": " ++ show t]
                 else do
                   let lhs = name ++ "(" ++ intercalate "," args ++ ")"
                       upper = intercalate "  " $ [p ++ ": " ++ show pt | (p,pt) <- zip args args']
                       lower = lhs ++ ": " ++ show t
                   printLines
                     [ upper
                     , replicate (max (length upper) (length lower)) '-'
                     , lower
                     ]
             Nothing  -> throwError $ "unknown function: " ++ name
    ("aexp", arg') -> do -- XXX
      sys <- get
      case Sys.parseExp sys (strip arg') of
        Left err -> printLines $ lines $ err
        Right (_, e :! t) ->
          printLines $ [show e, "    : " ++ show t]
    _ -> do
      sys <- get
      case Sys.parseExp sys (strip arg) of
        Left err -> throwError $ err
        Right (_, e :! t) ->
          printLines $ [show $ AExp.skelton e, "    : " ++ show t]

cmdLet :: Command
cmdLet arg = do
  sys <- get
  case Sys.parseDef sys (strip arg) of
    Left err -> throwError err
    Right def@(name, args, e, FType _ args' t) -> do
      case Sys.letExp sys def of
        Left err -> throwError err
        Right sys' -> do
          put sys'
          if null args
            then printLines [name ++ " = " ++ show (AExp.skelton e), "    : " ++ show t]
            else do
              let lhs = name ++ "(" ++ intercalate "," args ++ ")"
                  upper = intercalate "  " $ [p ++ ": " ++ show t | (p,t) <- zip args args']
                  lower = lhs ++ ": " ++ show t
              printLines
                [ upper
                , replicate (max (length upper) (length lower)) '-'
                , lower
                -- , "    : " ++ intercalate ", " (map show args') ++ " => " ++ show t
                ]

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
      if not (any isTerminalObject (Sys.objects sys)) then do
        throwError "No terminal object is defined."
      else
        case Sys.parseExp sys str of
          Left err -> throwError err
          Right (_, e :! t) -> do
            if not (AExp.isElement e) then throwError "not a element"
            else do
              let traces = Sys.simp sys full (AExp.skelton e)
                  loop ((step,(depth,exp,cexp)) : xs) = do
                    let line = show step
                             ++ (if depth==0 then "" else "[" ++ show depth ++ "]")
                             ++ ":" ++ show (Simp.decompile exp) ++ "*" ++ show (Simp.decompile cexp)
                    when (Sys.trace sys) $ printLine line
                    if null xs
                      then do
                        let it = Simp.decompile cexp
                        printLines [show it, "    : " ++ show t]
                        put sys{ Sys.lastExp = Just it }
                      else
                        loop xs
              loop (zip [(0::Int)..] traces)

cmdLoad :: Command
#if defined(USE_WASM_BACKEND)
cmdLoad s =
    do let filename = let s' = strip s in
                          case s' of
                          '"':_ -> read s'
                          _     -> s'
       result <- liftIO $ try $ evaluate =<< (fromJSString <$> js_loadFile (toJSString filename))
       case result of
         Left (JSException val) -> do
           s2 <- liftIO $ js_toString val
           throwError (fromJSString s2)
         Right contents -> do
           let src  = unlines (map removeComment (lines contents))
               cmds = split src
           forM_ cmds $ \cmd -> do
             printLines ["> " ++ l | l <- lines cmd]
             dispatchCommand cmd
    where removeComment []      = []
          removeComment ('#':_) = []
          removeComment (x:xs)  = x : removeComment xs
          split :: String -> [String]
          split s = map (strip . reverse) (f s [])
              where f (';':xs) tmp = tmp : (f xs [])
                    f (x:xs) tmp   = f xs (x:tmp)
                    f [] tmp       = [tmp]
#else
cmdLoad s =
    do contents <- liftIO $ readFile filename
       let src  = unlines (map removeComment (lines contents))
           cmds = split src
       forM_ cmds $ \cmd -> do
         printLines ["> " ++ l | l <- lines cmd]
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
#endif

cmdEdit :: Command
cmdEdit _ = loop >>= dispatchCommand
  where
    loop = do
      l <- readLine "| "
      case dropWhile isSpace (reverse l) of
        ';':s -> return (reverse s)
        _ -> do
          s <- loop
          return $ l ++ "\n" ++ s

cmdQuit :: Command
cmdQuit _ = liftIO $ exitWith ExitSuccess

cmdHelp :: Command
cmdHelp _ = printLines
              [ "  exit                        exit the interpreter"
              , "  quit                        ditto"
              , "  bye                         ditto"
              , "  edit                        enter editing mode"
              , "  simp [full] <exp>           evaluate expression"
              , "  show <exp>                  print type of expression"
              , "  show function <name>        print information of function"
              , "  show object <functor>       print information of functor"
#if defined(USE_WASM_BACKEND)
              , "  load <filename>             load built-in sample file"
              , "  load                        open file picker"
#else
              , "  load <filename>             load from file"
#endif
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
                   printLine $ "trace=" ++ (if Sys.trace sys then "on" else "off")
            _ ->
                throwError $ "unknown flag: " ++ flag
        (value, _) ->
            case flag of
            "trace" ->
                case value of
                "on"  -> modify (\sys -> sys{ Sys.trace = True })
                "off" -> modify (\sys -> sys{ Sys.trace = False })
                _ -> throwError  $ "unknown value: " ++ value
            _ ->
                throwError $ "unknown flag: " ++ flag

cmdReset :: Command
cmdReset _ = put initialState

----------------------------------------------------------------------------

data Flag
    = Help
    | ShowVersion
    | Interactive
    -- | Load String
    | Trace String
    deriving Eq

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]    (NoArg Help)            "show help"
    , Option ['v'] ["version"] (NoArg ShowVersion)     "show version number"
    , Option ['i'] ["interactive"] (NoArg Interactive) "force interactive mode"
    -- , Option ['l'] ["load"]    (ReqArg Load "FILE") "load FILE"
    , Option ['t'] ["trace"]    (OptArg (Trace . fromMaybe "on") "[on|off]")
             "enable/disable trace"
    ]

main :: IO ()
main =
    do args <- getArgs
       case getOpt Permute options args of
         (opts,_,[])
           | Help `elem` opts -> putStrLn (usageInfo header options)
           | ShowVersion `elem` opts -> putStrLn $ showVersion version
         (opts,files,[]) ->
           runConsole $ flip evalStateT initialState $ do
             ret <- runExceptT $ do
               printLines banner
               mapM_ processOpt opts
               mapM_ cmdLoad files
               when (null files || Interactive `elem` opts) $ mainLoop
             case ret of
               Left err -> lift $ mapM_ printLine' (lines err)
               Right () -> return ()
         (_,_,errs) -> do
           forM_ errs $ \err -> hPutStr stderr err
           hPutStrLn stderr $ usageInfo header options
           exitFailure             

header :: String
header = "Usage: cpl [OPTION...] files..."

banner :: [String]
banner =
  [ "Categorical Programming Language (Haskell version)"
  , "version " ++ showVersion version
  , ""
  , "Type help for help"
  , ""
  ]

processOpt :: Flag -> UI ()
processOpt (Trace s) =
  case s of
    "on"  -> modify (\sys -> sys{ Sys.trace = True })
    "off" -> modify (\sys -> sys{ Sys.trace = False })
    _     -> throwError "invalid option"
processOpt _ = return ()

mainLoop :: UI ()
mainLoop = forever $ do
  l <- readLine "cpl> "
  dispatchCommand l `catchError` (\err -> printLines $ lines $ err)

----------------------------------------------------------------------------
