What I'd ideally like this to look like  
doesn't necessarily have to be bind operator, but any monadic looking operator could probably work.

```
run :: [([String] -> IO ())] -> [String] -> IO ()
fromList :: [String] -> Context
flag :: String -> FlagType -> Context -> (Read v => Context -> v -> IO ())

let f args =
  flag "-a" required (fromList args) >>= (ctx, a :: String) ->
  flag "-b" required ctx >>= (ctx, b :: String) -> 
  flag "-c" required ctx >>= (ctx, c :: Int) ->
  putStrLn a >>= putStrLn b >>= putStrLn $ show c
in
let commandGroup =
  group  [ ("c1", f) ]
in
run commandGroup prog.Args

```
