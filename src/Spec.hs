module Spec
  ( Spec
  , Arg (Flag, NoArg, Anon)
  , anons
  , flags
  , fromList
  , anonIndex
  ) where

import qualified Data.Map as Map
import qualified Data.List as List

data Arg = Flag| NoArg | Anon

data Spec = Spec { anons :: [String]
                 , flags :: Map.Map String Arg
                 }

empty :: Spec
empty = Spec { anons=[], flags=Map.empty }

fromList :: [(String, Arg)] -> Spec
fromList argList =
  let spec =
        List.foldl (\spec (name, arg) ->
                      case arg of
                        Flag -> spec { flags=Map.insert name arg $ flags spec }
                        NoArg -> spec { flags=Map.insert name arg $ flags spec }
                        Anon -> spec { anons=name : anons spec }) empty argList
  in
    spec { anons=List.reverse $ anons spec }

anonIndex :: String -> Spec -> Maybe Int
anonIndex arg spec = List.elemIndex arg (anons spec)
