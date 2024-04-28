
namespace Parser


[<AutoOpen>]
module internal Extensions =

    open FParsec
    open FsToolkit.ErrorHandling


    // Within a parser computation expression, allows us to work with both
    // the standard parser type and a result type. Should the result type
    // contain an error, this is assumed to be a fatal parser error.
    type ParserCombinator with
        member _.Bind2 (x1, x2, f) =
            tuple2 x1 x2 >>= f

        member _.Bind3 (x1, x2, x3, f) =
            tuple3 x1 x2 x3 >>= f

        member _.Bind4 (x1, x2, x3, x4, f) =
            tuple4 x1 x2 x3 x4 >>= f

        member _.Bind5 (x1, x2, x3, x4, x5, f) =
            tuple5 x1 x2 x3 x4 x5 >>= f


        member _.Source (x: Parser<'TValue, 'TState>) =
            x

        member _.Source (x: Result<'T, string>) =
            x |> Result.either preturn failFatally
