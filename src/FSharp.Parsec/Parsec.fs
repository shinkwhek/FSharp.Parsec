﻿namespace FSharp.Parsec

open FSharpPlus
open FSharpPlus.Data

type Parser =
  Ps of ( string -> Result<(string * string) option, string> )
  with
    static member inline Return a =
      Ps <| (fun cs -> (a, cs) |> Some |> Ok )
    static member inline ReturnFrom a = a

    static member (>>=) (Ps p, f) =
      let bind = 
        function
        | Ok (Some (o, cs')) ->
          let (Ps g) = f o
          g cs'
        | e -> e
      Ps (p >> bind)

    static member get_Empty () =
      Ps <| (fun _ -> Ok None)

    static member (<|>) (Ps p, Ps q) =
      Ps <| (fun cs ->
              let p = p cs
              let q = q cs
              match p,q with
              | Ok (Some a), _ -> Ok (Some a)
              | _, Ok b -> Ok b
              | _, Error _ -> Error "no match in <|>")
    
module Parser =

  let apply (Ps p) = p

  let anyItem =
    Ps <| (fun cs ->
            if cs.Length = 0
            then Error "empty input"
            else
              Ok <| Some ( cs.Substring(0,1), cs.Substring(1) ) )

  let satisfy f =
    monad.strict {
      let! c = anyItem
      if f c
      then return c
      else return! Parser.get_Empty()
    }

  let pChar c =
    satisfy (fun x -> c = x)

  let rec pString s =
    match s with
    | "" -> Parser.Return ""
    | cs ->
      let c = cs.Substring(0,1)
      let cs = cs.Substring(1)
      monad.strict {
        let! c = pChar c
        let! cs = pString cs
        return c + cs
      }

  let oneOf cs =
    satisfy (fun c -> String.forall (fun x -> c.[0] = x) cs)
