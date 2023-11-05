
namespace Parser


[<RequireQualifiedAccess>]
module Code =

    open System
    open System.Text.RegularExpressions
    open FParsec
    open FSharp.Core

    
    let private reComment =
        Regex(@"^\s*#.*$", RegexOptions.Multiline ||| RegexOptions.NonBacktracking)

    let private parseElements =
        parse {
            do! spaces

            // We use 'skip' as we only care about the closing user state.
            do! skipManyTill Element.parse eof
        }

    let parseString str =
        let str' =
            // Remove all user comments from the code.
            reComment.Replace (str, String.Empty)

        let parseResult =
            runParserOnString parseElements ParserState.Empty "code" str'

        match parseResult with
        | ParserResult.Failure (errMsg, _, _) ->
            Error errMsg
        | ParserResult.Success (_, (ParserState mappings), _) ->
            Ok (mappings.Values |> List.ofSeq)