// Learn more about F# at http://fsharp.org

open System
open FSharpPlus

open FSharp.Parsec
open FSharp.Parsec.Parser

[<EntryPoint>]
let main argv =

  let f1 =
    monad.strict {
      let! _ = anyItem
      let! b = anyItem
      let! c = anyItem
      return b+c
    }

  let f2 = pString "foo" <|> pString "bar" <|> pString "baz"
    
  let f3 =
    monad.strict {
      let! x = f2
      let! y = f1
      return x+y
    }

  let a = apply f3 "bazxyzabc" // bazyz

  printfn "%A" a
  0 // return an integer exit code
