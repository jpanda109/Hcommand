module Hcommand
  ( basic
  , Context
  , parseArgs
  , getInt
  , getBool
  , getString
  , head'
  ) where

import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe

head' :: [a] -> a
head' [] = error "yo"
head' (x:_) = x

basic :: IO ()
basic = putStrLn "hello"

data Context = Context { ctxArgs :: Args
                       }

type Args = Map.Map String ArgVal

data ArgVal = ArgInteger Integer
            | ArgString String
            | ArgBool Bool

parseArgs :: [String] -> Context
parseArgs tokens =
  let args = Map.fromList (List.map (\s -> (s, ArgString s)) tokens)
  in
    Context {ctxArgs=args}

getInt :: String -> Context -> Integer
getInt arg ctx =
  getFromContext (\v -> case v of
                          ArgInteger s -> Just s
                          _ -> Nothing) arg ctx

getBool :: String -> Context -> Bool
getBool arg ctx =
  getFromContext (\v -> case v of
                          ArgBool s -> Just s
                          _ -> Nothing) arg ctx

getString :: String -> Context -> String
getString arg ctx =
  getFromContext (\v -> case v of
                          ArgString s -> Just s
                          _ -> Nothing) arg ctx

getFromContext :: (ArgVal -> Maybe v) -> String -> Context -> v
getFromContext f arg ctx =
  Maybe.fromMaybe (error "parsing error") $
  Map.lookup arg (ctxArgs ctx)
  >>= f
