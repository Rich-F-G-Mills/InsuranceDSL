
namespace Parser


[<RequireQualifiedAccess>]
module internal Structure =
    
    open FParsec
    open FSharp.Core
    open FsToolkit.ErrorHandling
    open Specification.Variables
    open Common


    let internal findByName parserState name =
        parserState
        |> ParserState.tryFindElement name
        |> function
            | Some (ParsedElement.Structure underlying) ->
                preturn underlying
            | _ ->
                failFatally (sprintf "Unable to locate structure '%s'." name)

    let private STRUCTURE_NAME parserState =
        parse {
            let! name =
                ELEMENT_NAME .>> WHITESPACE

            do! parserState
                |> ParserState.tryFindElement name
                |> Result.requireNone (sprintf "An element already exists with the name '%s'." name)
            
            return name
        }

    let private INHERITED_STRUCTURES parserState =
        let INHERITED_STRUCTURE_NAME =
            ELEMENT_NAME >>= findByName parserState

        let SINGLE_INHERITANCE =
            EXTENDS >|+>. INHERITED_STRUCTURE_NAME .>> WHITESPACE

        manyTill SINGLE_INHERITANCE (notFollowedBy EXTENDS)

    let private STRUCTURE_VARIABLE parserState =
        parse {
            let! inputName =
                ELEMENT_NAME .>|+> IS .>> WHITESPACE

            and! inputType =
                TYPE parserState .>> WHITESPACE

            let newInputVariable =
                match inputType with
                | VariableType.Boolean ->
                    InputVariable.Boolean { Name = inputName }
                | VariableType.Integer ->
                    InputVariable.Integer { Name = inputName }
                | VariableType.Real ->
                    InputVariable.Real { Name = inputName }
                | VariableType.String ->
                    InputVariable.String { Name = inputName }
                | VariableType.Date ->
                    InputVariable.Date { Name = inputName }
                | VariableType.IntegerEnumeration ie ->
                    InputVariable.IntegerEnumeration { Name = inputName; Definition = ie }
                | VariableType.StringEnumeration se ->
                    InputVariable.StringEnumeration { Name = inputName; Definition = se }

            return (inputName, newInputVariable)
        }

    let private STRUCTURE_VARIABLES parserState =       
        many1Till (STRUCTURE_VARIABLE parserState) (followedBy END)

    let internal parse =
        parse {
            let! parserState =
                getUserState

            let! name =
                STRUCTURE_NAME parserState
            and! inheritedStructures =
                INHERITED_STRUCTURES parserState

            do! inheritedStructures
                |> List.tryFindDuplicateBy _.Name
                |> Option.either
                    (fun dup -> Error (sprintf "Structure '%s' has been inherited multiple times." dup))
                    Ok

            let! parsedInputVariables =
                STRUCTURE_VARIABLES parserState

            do! parsedInputVariables
                |> List.tryFindDuplicateBy fst
                |> Option.either
                    (fun dup -> Error (sprintf "Structure input '%s' is not uniquely defined." dup))
                    Ok

            let inheritedInputVariables =
                inheritedStructures
                |> List.collect (fun ps -> ps.Inputs |> Map.toList)

            let combinedInputVariables =
                parsedInputVariables
                |> List.append inheritedInputVariables
                |> List.fold (fun m (alias, value) ->
                    m |> Map.add alias value) Map.empty

            let newStructure =
                { Name = name; Inputs = combinedInputVariables }

            let! newParserState =
                parserState
                |> ParserState.addParsedStructure newStructure

            do! setUserState newParserState
        }