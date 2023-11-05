
open FSharp.Quotations


[<AbstractClass; Sealed>]
type X () =
    static member Y ([<ReflectedDefinition(includeValue = true)>] f: Expr<int -> int>) = f


module Callable =

    let f (e: int) = 2 * e

let x = X.Y (Callable.f)

printfn "%A" x