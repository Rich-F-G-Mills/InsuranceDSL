
namespace Parser


[<AutoOpen>]
module State =

    open Specification
    open Specification.Variables
    open Specification.Definitions

       
    [<NoComparison>]
    type public ParsedStructure =
        {
            Name: string
            Inputs: Map<string, InputVariable>
        }

    [<NoComparison>]
    type public ParsedProduct =
        {
            Name: string
            Structure: ParsedStructure
            DefinedVariables: Map<string, DefinedVariable>
        }

    [<RequireQualifiedAccess; NoComparison>]
    type public ParsedElement =
        | IntegerEnumeration of Enumeration.Integer
        | StringEnumeration of Enumeration.String
        | Structure of ParsedStructure
        | Product of ParsedProduct


    [<NoComparison>]
    type internal ParserState =
        internal | ParserState of Map<string, ParsedElement>

    [<RequireQualifiedAccess>]
    module internal ParserState =
        let Empty =
            ParserState Map.empty

        let tryFindElement name (ParserState parsedElements) =
            parsedElements
            |> Map.tryFind name

        let private addParsedElement<'TElement> (elementType, nameExtractor: 'TElement -> string, mapper: 'TElement -> ParsedElement) (element: 'TElement) (ParserState parsedElements) =
            let elementName = nameExtractor element

            if parsedElements.ContainsKey elementName then
                Error (sprintf "Cannot add %s '%s' as an element already exists with that name." elementType elementName)
            else            
                let newParserState =
                    ParserState (parsedElements |> Map.add elementName (mapper element))
                
                Ok newParserState

        // Originally these were defined as 'static members' which are re-evaluated on call.
        // Tried using 'static member vals' which are auto-implemented properties. However,
        // was causing issues with 'addParsedElement'.
        let addParsedIntegerEnumeration =
            addParsedElement<Enumeration.Integer> ("integer enumeration", _.Name, ParsedElement.IntegerEnumeration)

        let addParsedStringEnumeration =
            addParsedElement<Enumeration.String> ("string enumeration", _.Name, ParsedElement.StringEnumeration)

        let addParsedStructure =
            addParsedElement<ParsedStructure> ("structure", _.Name, ParsedElement.Structure)

        let addParsedProduct =
            addParsedElement<ParsedProduct> ("product", _.Name, ParsedElement.Product)
