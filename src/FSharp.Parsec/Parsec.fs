namespace FSharp.Parsec

open FSharpPlus
open FSharpPlus.Data

type Parser<'a> =
  Ps of ( string -> Result<('a * string), string> )
  with
    static member inline Return a =
      Ps <| (fun cs -> Ok (a, cs))

    static member (>>=) (Ps p, f) =
      let bind = 
        function
        | Ok (o, cs') ->
          let (Ps g) = f o
          g cs'
        | e -> e
      Ps (p >> bind)
    
module Parser =

  let apply (Ps p) = p

  let anyItem =
    Ps <| (fun cs ->
            if cs.Length = 0
            then Error "empty input"
            else
              Ok ( cs.Substring(0,1), cs.Substring(1) ) )
