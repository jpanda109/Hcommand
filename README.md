What I'd ideally like this to look like__
(essentially same as ocaml's Core.Command)

```

let main =
  let command =
    basic
      "summary"
      (empty
       +> flag "-flag" ["-f"] (required string) "documentation"
       +> anon "ANON" (string)
       )
      \someFlag someAnon () ->
      putStrLn someFlag >>= putStrLn someAnon
  in
  run command
```

What I'll likely settle with
```
data Args = SomeFlag | SomeAnon
let main =
  let command =
        basic
          "summary"
          [ flag SomeFlag "-flag" ["-f"] required "documentation"
          , anon SomeAnon "ANON"]
      ctx = run command
      someFlag = getArg ctx SomeFlag :: String
      someAnon = getArg ctx SomeAnon :: String
  in
  putStrLn someFlag >>= putStrLn someAnon
  
```
