
namespace Parser


[<AutoOpen>]
module internal Common =

    open System
    open FParsec
    // Needed to replace Error and Ok as exposed by FParsec.
    open FSharp.Core
    open FsToolkit.ErrorHandling
    open Specification.Variables
    open Parser.Attributes


    type private Locator () =
        interface ILocator


    let internal WHITESPACE =
        spaces1

    let inline internal (>.>) (p1: Parser<unit, _>) (p2: Parser<unit, _>) =
        tuple2 p1 p2 |>> ignore

    let inline internal (>|+>) (p1: Parser<unit, _>) (p2: Parser<unit, _>) =
        tuple3 p1 WHITESPACE p2 |>> ignore

    let inline internal (.>|+>) p1 p2 =
        p1 .>> WHITESPACE .>> p2

    let inline internal (>|+>.) p1 p2 =
        p1 >>. WHITESPACE >>. p2

    let inline internal (.>|+>.) p1 p2 =
        p1 .>> WHITESPACE .>>. p2

    let inline internal (>|*>) (p1: Parser<unit, _>) (p2: Parser<unit, _>) =
        tuple3 p1 spaces p2 |>> ignore

    let inline internal (.>|*>) p1 p2 =
        p1 .>> spaces .>> p2

    let inline internal (>|*>.) p1 p2 =
        p1 >>. spaces >>. p2

    let inline internal (.>|*>.) p1 p2 =
        p1 .>> spaces .>>. p2

    /// Ensures that the string must be followed by either whitespace of EOF.
    let skipString' str: Parser<unit, ParserState> =
        skipString str >.> followedByL (WHITESPACE <|> eof) "Whitespace or EOF expected."

    
    [<Keyword>]
    let internal ACCRUAL =
        skipString' "ACCRUAL"

    [<Keyword>]
    let internal AS =
        skipString' "AS"

    [<Keyword>]
    let internal BEGIN =
        skipString' "BEGIN"

    [<Keyword>]
    let internal BOOLEAN =
        skipString' "BOOLEAN"

    [<Keyword>]
    let internal DATE =
        skipString' "DATE"

    [<Keyword>]
    let internal DEFINE =
        skipString' "DEFINE"

    [<Keyword>]
    let internal END =
        skipString' "END"

    [<Keyword>]
    let internal ENUM =
        skipString' "ENUM"

    [<Keyword>]
    let internal EXTENDS =
        skipString' "EXTENDS"

    [<Keyword>]
    let internal INTEGER =
        skipString' "INTEGER"

    [<Keyword>]
    let internal IS =
        skipString' "IS"

    [<Keyword>]
    let internal OF =
        skipString' "OF"

    [<Keyword>]
    let internal PRODUCT =
        skipString' "PRODUCT"

    [<Keyword>]
    let internal REAL =
        skipString' "REAL"

    [<Keyword>]
    let internal ROLLBACK =
        skipString' "ROLLBACK"

    [<Keyword>]
    let internal STRING =
        skipString' "STRING"

    [<Keyword>]
    let internal STRUCTURE =
        skipString' "STRUCTURE"

    [<Keyword>]
    let internal USING =
        skipString' "USING"


    let private ANY_KEYWORD_IDENTIFIER =
        let keywordParsers =
            KeywordAttribute.PropertiesMarkedAsKeyword
            // We pass null as the property is static.
            |> Seq.map (fun pi -> pi.Name, pi.GetValue(null) :?> Parser<unit, ParserState>)
            |> Seq.map (fun (kid, p) -> p >>% kid)
            |> Seq.toList

        // Choice appears to re-evaluate the underlying sequence everytime it is used.
        // Have saved parsers to an intermediate varible in order to mitigate this.
        choice keywordParsers

    let private ANY_CALLABLE_IDENTIFER =
        let callableParsers =
            CallableAttribute.CallableIdentifiers
            |> Seq.map (fun cid -> skipString' cid >>% cid)
            |> Seq.toList

        choice callableParsers


    /// Ensures that a name does not correspond to a keyword or callable.
    let internal ELEMENT_NAME: Parser<string, ParserState> =
        let expected =
            regexL "[a-zA-Z_][a-zA-Z0-9_]*" "Element name not properly formatted."
            .>> followedByL WHITESPACE "Whitespace expected."

        notFollowedByL ANY_KEYWORD_IDENTIFIER "keyword"
        >.> notFollowedByL ANY_CALLABLE_IDENTIFER "callable"
        >>. expected

    let internal QUOTED_TEXT: Parser<string, ParserState> =
        (between (pchar '"') (pchar '"') (regex "[^\"]*"))
        .>> followedBy WHITESPACE
        <?> "Quoted text expected."

    let internal TYPE =
        let primitives = [
            BOOLEAN >>% VariableType.Boolean
            DATE >>% VariableType.Date
            INTEGER >>% VariableType.Integer
            REAL >>% VariableType.Real
            STRING >>% VariableType.String
        ]

        // We need the parser state so that we can add in user defined enumerations.
        fun (ParserState parsedElements) ->
            let userTypes =
                parsedElements
                |> Map.toSeq
                |> Seq.choose (fun (name, element) ->
                    option {
                        let! varType =
                            match element with
                            | ParsedElement.IntegerEnumeration ie ->
                                Some (VariableType.IntegerEnumeration ie)
                            | ParsedElement.StringEnumeration se ->
                                Some (VariableType.StringEnumeration se)
                            | _ -> None

                        // Includes non-consumed subsequent whitespace.
                        return skipString' name >>% varType
                    })

            let parseTypesSeq =            
                primitives
                |> Seq.append userTypes
                |> Seq.toList
            
            choiceL parseTypesSeq "Unknown variable type."