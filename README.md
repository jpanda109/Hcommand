What I'd ideally like this to look like  
doesn't necessarily have to be bind operator, but any monadic looking operator could probably work.

```
run :: [Command] -> [String] -> IO ()
fromList :: [String] -> Context
group :: [(String, Command)] -> Command
flag :: String -> Context -> (Read v => Context -> v -> IO ()) -> Command

let f =
  flag "-a" required ctx >>= (ctx, a :: String) ->
  flag "-b" required ctx >>= (ctx, b :: String) -> 
  flag "-c" required ctx >>= (ctx, c :: Int) ->
  putStrLn a >>= putStrLn b >>= putStrLn $ show c
in
let commandGroup =
  group  [ ("c1", f) ]
in
run commandGroup (fromList prog.Args)

```
