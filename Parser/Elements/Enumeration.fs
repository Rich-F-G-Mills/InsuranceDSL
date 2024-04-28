
namespace Parser


[<RequireQualifiedAccess>]
module internal Enumeration =
    
    open FParsec
    open FSharp.Core
    open FsToolkit.ErrorHandling
    open Specification
    open Common


    [<RequireQualifiedAccess; NoComparison; NoEquality>]
    type private UnderlyingType =
        | Integer
        | String


    let private ENUM_TYPE =
        let choices =
            [
                INTEGER >>% UnderlyingType.Integer
                STRING >>% UnderlyingType.String
            ]

        let parseTypeKeyword =
            choiceL choices "Unexpected block type."

        // Enumeration is assumed to be of type integer unless otherwise specified.
        (OF >|+>. parseTypeKeyword .>> WHITESPACE) <|>% UnderlyingType.Integer

    let private ENUM_NAME parserState =
        parse {
            let! name =
                ELEMENT_NAME .>> WHITESPACE

            do! parserState
                // Note that we aren't just checking enumerations, we're checking ALL elements!
                |> ParserState.tryFindElement name
                |> Result.requireNone (sprintf "An element already exists with the name '%s'." name)
            
            return name
        }

    
    [<RequireQualifiedAccess>]
    module private IntegerEnum =
        let private findByName parserState name: Parser<_, ParserState> =
            parserState
            |> ParserState.tryFindElement name
            |> function
                | Some (ParsedElement.IntegerEnumeration underlying) ->
                    preturn underlying
                | _ ->
                    failFatally (sprintf "Unable to locate integer enumeration '%s'." name)

        let private INHERITED_ENUMS parserState =
            let INHERITED_ENUM_NAME =
                ELEMENT_NAME >>= findByName parserState

            let SINGLE_INHERITANCE =
                EXTENDS >|+>. INHERITED_ENUM_NAME .>> WHITESPACE

            many SINGLE_INHERITANCE

        let private ENUM_LEVEL =
            ELEMENT_NAME .>|+> AS .>|+>. pint32 .>> WHITESPACE

        let private ENUM_LEVELS =
            // The only thing we should see after all the levels is 'END ENUM'. 
            many1Till ENUM_LEVEL (followedBy END)

        let parse parserState name =
            parse {
                let! inheritedEnums =
                    INHERITED_ENUMS parserState

                do! inheritedEnums
                    |> List.tryFindDuplicateBy _.Name
                    |> Option.either
                        (fun dup -> Error (sprintf "Integer enumeration '%s' has been inherited multiple times." dup))
                        Ok
  
                let! parsedLevels =
                    ENUM_LEVELS

                do! parsedLevels
                    |> List.tryFindDuplicateBy fst
                    |> Option.either
                        (fun dup -> Error (sprintf "Enumeration level '%s' is not uniquely defined." dup))
                        Ok

                let inheritedLevels =
                    inheritedEnums
                    |> List.collect (fun pe -> pe.Levels |> Map.toList)

                let combinedLevels =
                    parsedLevels
                    |> List.append inheritedLevels
                    // Ensures that the most recent definition of a level takes priority.
                    // Could use Map.ofSeq but the behaviour is undocumented and
                    // should not be relied upon.
                    |> List.fold (fun m (alias, value) ->
                        m |> Map.add alias value) Map.empty

                let (newEnum: Enumeration.Integer) =
                    { Name = name; Levels = combinedLevels }

                let! newParserState =
                    parserState
                    |> ParserState.addParsedIntegerEnumeration newEnum

                do! setUserState newParserState
            }


    [<RequireQualifiedAccess>]
    module private StringEnum =
        let private findByName parserState name: Parser<_, ParserState> =
            parserState
            |> ParserState.tryFindElement name
            |> function
                | Some (ParsedElement.StringEnumeration underlying) ->
                    preturn underlying
                | _ ->
                    failFatally (sprintf "Unable to locate string enumeration '%s'." name)

        let private INHERITED_ENUMS parserState =
            let INHERITED_ENUM_NAME =
                ELEMENT_NAME >>= findByName parserState

            let SINGLE_INHERITANCE =
                EXTENDS >|+>. INHERITED_ENUM_NAME .>> WHITESPACE

            many SINGLE_INHERITANCE

        let private ENUM_LEVEL =
            ELEMENT_NAME .>|+> AS .>|+>. QUOTED_TEXT .>> WHITESPACE

        let private ENUM_LEVELS =
            // The only thing we should see after all the levels is 
            many1Till ENUM_LEVEL (followedBy END)

        let internal parse parserState name =
            parse {
                let! inheritedEnums =
                    INHERITED_ENUMS parserState

                do! inheritedEnums
                    |> List.tryFindDuplicateBy _.Name
                    |> Option.either
                        (fun dup -> Error (sprintf "String enumeration '%s' has been inherited multiple times." dup))
                        Ok
  
                let! parsedLevels =
                    ENUM_LEVELS

                do! parsedLevels
                    |> List.hasDuplicatesBy fst
                    |> Result.requireFalse (sprintf "Not all enumeration aliases within '%s' are uniquely defined." name)

                let inheritedLevels =
                    inheritedEnums
                    |> List.collect (fun pe -> pe.Levels |> Map.toList)

                let combinedLevels =
                    parsedLevels
                    |> List.append inheritedLevels
                    |> List.fold (fun m (alias, value) ->
                        m |> Map.add alias value) Map.empty

                let (newEnum: Enumeration.String) =
                    { Name = name; Levels = combinedLevels }

                let! newParserState =
                    parserState
                    |> ParserState.addParsedStringEnumeration newEnum

                do! setUserState newParserState
            }


    let internal parse =
        parse {
            let! parserState = getUserState
            and! underlyingType = ENUM_TYPE
            let! name = ENUM_NAME parserState

            return!
                match underlyingType with
                | UnderlyingType.Integer -> IntegerEnum.parse parserState name
                | UnderlyingType.String -> StringEnum.parse parserState name
        }
