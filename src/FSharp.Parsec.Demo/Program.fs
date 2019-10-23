// Learn more about F# at http://fsharp.org

open System
open FSharpPlus

open FSharp.Parsec

[<EntryPoint>]
let main argv =

  let f =
    monad.strict {
      let! a = Parser.anyItem
      let! b = Parser.anyItem
      return a+b
    }

  let a = Parser.apply f "abc"

  printfn "%A" a
  0 // return an integer exit code
