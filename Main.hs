{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy.UTF8
import Data.JSString (JSString, unpack, pack)
import Data.JSString.Text

import GHCJS.Foreign
import GHCJS.Foreign.Callback (Callback, OnBlocked(ContinueAsync), syncCallback1')
import GHCJS.Marshal.Pure
--import GHCJS.Prim
import GHCJS.Types (JSVal)

--import JavaScript.Object

import Language
import Lexer
import Parser

handleWithIO :: (String -> IO String) -> JSVal -> IO JSVal
handleWithIO f v = do
  putStrLn $ "Starting"
  let input = unpack $ pFromJSVal v
  putStrLn $ "Input: " ++ input
  output <- f input
  putStrLn $ "Output: " ++ output
  return $ pToJSVal (pack output)

-- handleWithPure :: (String -> String) -> JSVal -> IO JSVal
-- handleWithPure f v = do
--   handleWithIO (\s -> return (f s)) v

-- typeCheck :: JSVal -> IO JSVal
-- typeCheck = handleWithPure process

-- parseTerm :: JSVal -> IO JSVal
-- parseTerm = handleWithPure $ pprint . Parser.parseTerm

typeCheckDebug :: JSVal -> IO JSVal
typeCheckDebug = handleWithIO $ \ input -> do
  let term = Parser.parseTerm input
  putStrLn $ "Parsed term: " ++ pprint term
  let work = inferWork term
  putStrLn $ "Work: " ++ show work
  let res = tcTraceResult work
  putStrLn $ "Result (show): " ++ show res
  putStrLn $ "Result (pprint): " ++ pprint res
  let resStr = toString . encode $ res
  return resStr

foreign import javascript unsafe
  "window[$1] = $2"
  js_setCallback :: JSString -> Callback a -> IO ()

foreign import javascript unsafe
  "window['haskellReady'] = true"
  js_ready :: IO ()

register :: JSString -> (JSVal -> IO JSVal) -> IO ()
register s f =
  syncCallback1' f >>= js_setCallback s

main = do
  --register "typeCheck" typeCheck
  register "typeCheckDebug" typeCheckDebug
  --register "parseTerm" Main.parseTerm
  js_ready
