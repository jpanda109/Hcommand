module Hcommand
  ( parseArgs
  ) where

import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe

data ArgSpec = FlagSpec | NoArgSpec | AnonSpec
data Flag = Arg String | NoArg

type ArgSpecMap = Map.Map String ArgSpec

data Context = Context { anons :: [String]
                       , flags :: Map.Map String Flag}

parseArgs :: ArgSpecMap -> [String] -> Context
parseArgs = parseArgs' Context {anons=[], flags=Map.empty}

parseArgs' :: Context -> ArgSpecMap -> [String] -> Context
parseArgs' ctx _ [] = ctx
parseArgs' ctx _ [arg] =
  let newAnons = arg : anons ctx
  in
    Context {anons=newAnons, flags=flags ctx}
parseArgs' ctx specs (arg1 : arg2 : rest) =
  if "-" `isPrefixOf` arg1
  then
    let spec = Maybe.fromMaybe (error "invalid flag") (Map.lookup arg1 specs)
    in
      case spec of
        FlagSpec ->
          let newFlags = Map.insert arg1 (Arg arg2) (flags ctx)
              newCtx = Context {anons=anons ctx, flags=newFlags}
          in parseArgs' newCtx specs rest
        NoArgSpec ->
          let newFlags = Map.insert arg1 NoArg (flags ctx)
              newCtx = Context {anons=anons ctx, flags=newFlags}
          in
            parseArgs' newCtx specs (arg2 : rest)
        AnonSpec -> error "invalid flag"
  else
    let newAnons = arg1 : anons ctx
    in
      Context {anons=newAnons, flags=flags ctx}
