{-# LANGUAGE FlexibleInstances #-}
module Hcommand
  ( parseArgs
  , Context
  , ReadArg (readArg)
  , getArg
  , run
  ) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Spec as Spec

data Flag = Arg String | NoArg

data Context = Context { ctx_spec :: Spec.Spec
                       , ctx_anons :: [String]
                       , ctx_flags :: Map.Map String Flag
                       }

class ReadArg a where
  readArg :: String -> a

instance ReadArg String where
  readArg s = read $ show s :: String

run :: Spec.Spec -> [String] -> (Context -> IO ()) -> IO ()
run spec args action =
  let ctx = parseArgs spec args
  in
    action ctx

parseArgs :: Spec.Spec -> [String] -> Context
parseArgs specs = parseArgs' Context {ctx_spec=specs, ctx_anons=[], ctx_flags=Map.empty}

parseArgs' :: Context -> [String] -> Context
parseArgs' ctx [] = ctx
parseArgs' ctx [arg] =
  let newAnons = arg : ctx_anons ctx
  in
    ctx {ctx_anons=newAnons}
parseArgs' ctx (arg1 : arg2 : rest) =
  if "-" `List.isPrefixOf` arg1
  then
    let spec = Maybe.fromMaybe (error "invalid flag") (Map.lookup arg1 (Spec.flags $ ctx_spec ctx))
    in
      case spec of
        Spec.Flag ->
          let newFlags = Map.insert arg1 (Arg arg2) (ctx_flags ctx)
              newCtx = ctx {ctx_flags=newFlags}
          in parseArgs' newCtx rest
        Spec.NoArg ->
          let newFlags = Map.insert arg1 NoArg (ctx_flags ctx)
              newCtx = ctx {ctx_flags=newFlags}
          in
            parseArgs' newCtx (arg2 : rest)
        Spec.Anon -> error "invalid flag"
  else
    let newAnons = arg1 : ctx_anons ctx
    in
      ctx {ctx_anons=newAnons}


getArg :: (ReadArg r) => String -> Context -> r
getArg arg ctx =
  if "-" `List.isPrefixOf` arg
  then
    let val = Maybe.fromMaybe (error "No flag with this name") (Map.lookup arg (ctx_flags ctx))
    in
      case val of
        NoArg -> readArg $ show True
        Arg s -> readArg s
  else
    let i = Maybe.fromMaybe (error "No flag with this name") (Spec.anonIndex arg $ ctx_spec ctx)
    in
      readArg $ ((Spec.anons $ ctx_spec ctx) !! i)
