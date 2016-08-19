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

import qualified Spec

data Flag = Arg String | NoArg

data Context = Context { ctxSpec :: Spec.Spec
                       , ctxAnons :: [String]
                       , ctxFlags :: Map.Map String Flag
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
parseArgs specs args =
  let ctx = parseArgs' Context {ctxSpec=specs, ctxAnons=[], ctxFlags=Map.empty} args
  in
    ctx {ctxAnons=List.reverse $ ctxAnons ctx}

parseArgs' :: Context -> [String] -> Context
parseArgs' ctx [] = ctx
parseArgs' ctx [arg] =
  let newAnons = arg : ctxAnons ctx
  in
    ctx {ctxAnons=newAnons}
parseArgs' ctx (arg1 : arg2 : rest) =
  if "-" `List.isPrefixOf` arg1
  then
    let spec = Maybe.fromMaybe (error "invalid flag") (Map.lookup arg1 (Spec.flags $ ctxSpec ctx))
    in
      case spec of
        Spec.Flag ->
          let newFlags = Map.insert arg1 (Arg arg2) (ctxFlags ctx)
              newCtx = ctx {ctxFlags=newFlags}
          in parseArgs' newCtx rest
        Spec.NoArg ->
          let newFlags = Map.insert arg1 NoArg (ctxFlags ctx)
              newCtx = ctx {ctxFlags=newFlags}
          in
            parseArgs' newCtx (arg2 : rest)
        Spec.Anon -> error "invalid flag"
  else
    let newAnons = arg1 : ctxAnons ctx
        newCtx = ctx {ctxAnons=newAnons}
    in
      parseArgs' newCtx (arg2 : rest)

getArg :: (ReadArg r) => String -> Context -> r
getArg arg ctx =
  if "-" `List.isPrefixOf` arg
  then
    let val = Maybe.fromMaybe (error "No flag with this name") (Map.lookup arg (ctxFlags ctx))
    in
      case val of
        NoArg -> readArg $ show True
        Arg s -> readArg s
  else
    let i = Maybe.fromMaybe (error "No flag with this name") (Spec.anonIndex arg $ ctxSpec ctx)
    in
      readArg $ ctxAnons ctx !! i
