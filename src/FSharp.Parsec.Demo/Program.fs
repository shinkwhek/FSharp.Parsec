// Learn more about F# at http://fsharp.org

open System
open FSharpPlus

open FSharp.Parsec
open FSharp.Parsec.Parser

[<EntryPoint>]
let main argv =

  let f =
    monad.strict {
      let! a = anyItem
      let! b = anyItem
      return a+b
    }

  let f2 = pString "foo" <|> pString "bar" <|> pString "baz"
    

  let a = apply f2 "bazx"

  printfn "%A" a
  0 // return an integer exit code
