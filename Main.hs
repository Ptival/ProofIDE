module Main where

import Data.Aeson
import Data.ByteString.Lazy.UTF8

import GHCJS.Foreign
import GHCJS.Types

import Language
import Lexer
import Parser

handleWithIO :: (String -> IO String) -> JSRef a -> IO ()
handleWithIO f r = do
  input <- getProp "x" r
  output <- f . fromJSString $ input
  setProp "y" (toJSString output) r

handleWithPure :: (String -> String) -> JSRef a -> IO ()
handleWithPure f = handleWithIO $ return . f

typeCheck :: JSRef a -> IO ()
typeCheck = handleWithPure process

parseTerm :: JSRef a -> IO ()
parseTerm = handleWithPure $ pprint . Parser.parseTerm

typeCheckDebug :: JSRef a -> IO ()
typeCheckDebug = handleWithIO $ \input -> do
  let term = Parser.parseTerm input
  putStrLn $ "Parsed term: " ++ pprint term
  let work = inferWork term
  putStrLn $ "Work: " ++ show work
  let res = tcTraceResult work
  putStrLn $ "Result (show): " ++ show res
  putStrLn $ "Result (pprint): " ++ pprint res
  let resStr = toString . encode $ res
  return resStr

foreign import javascript unsafe "window[$1] = $2"
  js_assignCallback :: JSString -> JSFun b -> IO ()

foreign import javascript unsafe "window['haskellReady'] = true"
  js_ready :: IO ()

register :: String -> (JSRef a -> IO ()) -> IO ()
register s f =
  syncCallback1 AlwaysRetain False f >>= js_assignCallback (toJSString s)

main = do
  let a = AlwaysRetain
  let j = js_assignCallback
  register "typeCheck" typeCheck
  register "typeCheckDebug" typeCheckDebug
  register "parseTerm" Main.parseTerm
  js_ready
