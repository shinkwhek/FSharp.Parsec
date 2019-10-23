module FSharp.Parsec.Test


module ``Parsec - test `` =
  open NUnit.Framework
  open FsUnit
  open FSharpPlus
  open FSharp.Parsec

  [<Test>]
  let ``item x2: abc -> (ab, c)`` () =
    let f =
      monad.strict {
        let! a = Parser.anyItem
        let! b = Parser.anyItem
        return a + b
      }
    "abc"
    |> Parser.apply f
    |> should equal (Ok ("ab", "c"))
