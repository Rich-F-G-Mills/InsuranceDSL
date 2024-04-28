
namespace Parser


[<RequireQualifiedAccess>]
module internal Product =
    
    open FParsec
    open FSharp.Core
    open FsToolkit.ErrorHandling
    open Specification.Variables
    open Specification.Definitions
    open Common
    open Parser.Expression


    [<NoComparison; NoEquality>]
    type private NonProcessedVariable =
        {
            Name: string
            Variable: Variable
            Content: string
        }


    let private findByName parserState name =
        parserState
        |> ParserState.tryFindElement name
        |> function
            | Some (ParsedElement.Product underlying) ->
                preturn underlying
            | _ ->
                failFatally (sprintf "Unable to locate product '%s'." name)

    let private PRODUCT_NAME parserState =
        parse {
            let! name = ELEMENT_NAME

            do! parserState
                |> ParserState.tryFindElement name
                |> Result.requireNone (sprintf "An element already exists with the name '%s'." name)
            
            return name
        }

    let private STRUCTURE_NAME parserState =
        ELEMENT_NAME >>= Structure.findByName parserState

    let private VARIABLE_MODE =
        let choices =
            [
                ACCRUAL >>% VariableMode.Accrual
                ROLLBACK >>% VariableMode.Rollback
            ]

        let modes =
            choice choices

        // We need 'attempt' so that we can backtrack over the whitespace if needed.
        attempt (WHITESPACE >>. modes)
        <|>% VariableMode.Singleton

    let private PRODUCT_VARIABLE parserState =
        parse {
            let! variableName =
                DEFINE >|+>. ELEMENT_NAME .>|+> AS .>> WHITESPACE

            and! variableType =
                TYPE parserState

            and! variableMode =
                VARIABLE_MODE

            and! _ =
                spaces >.> skipChar '=' >.> spaces

            and! variableContent =
                many1Chars (noneOf ";")
                .>> skipChar ';'
                .>> WHITESPACE

            let newVariable =
                match variableMode with
                | VariableMode.Accrual ->
                    match variableType with
                    | VariableType.Boolean ->
                        Variable.BooleanAccrual { Name = variableName }
                    | VariableType.Integer ->
                        Variable.IntegerAccrual { Name = variableName }
                    | VariableType.Real ->
                        Variable.RealAccrual { Name = variableName }
                    | VariableType.String ->
                        Variable.StringAccrual { Name = variableName }
                    | VariableType.Date ->
                        Variable.DateAccrual { Name = variableName }
                    | VariableType.IntegerEnumeration ie ->
                        Variable.IntegerEnumerationAccrual { Name = variableName; Definition = ie }
                    | VariableType.StringEnumeration se ->
                        Variable.StringEnumerationAccrual { Name = variableName; Definition = se }

                | VariableMode.Rollback ->
                    match variableType with
                    | VariableType.Boolean ->
                        Variable.BooleanRollback { Name = variableName }
                    | VariableType.Integer ->
                        Variable.IntegerRollback { Name = variableName }
                    | VariableType.Real ->
                        Variable.RealRollback { Name = variableName }
                    | VariableType.String ->
                        Variable.StringRollback { Name = variableName }
                    | VariableType.Date ->
                        Variable.DateRollback { Name = variableName }
                    | VariableType.IntegerEnumeration ie ->
                        Variable.IntegerEnumerationRollback { Name = variableName; Definition = ie }
                    | VariableType.StringEnumeration se ->
                        Variable.StringEnumerationRollback { Name = variableName; Definition = se }

                | VariableMode.Singleton ->
                    match variableType with
                    | VariableType.Boolean ->
                        Variable.BooleanSingleton { Name = variableName }
                    | VariableType.Integer ->
                        Variable.IntegerSingleton { Name = variableName }
                    | VariableType.Real ->
                        Variable.RealSingleton { Name = variableName }
                    | VariableType.String ->
                        Variable.StringSingleton { Name = variableName }
                    | VariableType.Date ->
                        Variable.DateSingleton { Name = variableName }
                    | VariableType.IntegerEnumeration ie ->
                        Variable.IntegerEnumerationSingleton { Name = variableName; Definition = ie }
                    | VariableType.StringEnumeration se ->
                        Variable.StringEnumerationSingleton { Name = variableName; Definition = se }

                | VariableMode.Input ->
                    failwith "Unexpected error... Input variables should not be defined in this way."

            return { Name = variableName; Variable = newVariable; Content = variableContent }
        }

    let private PRODUCT_VARIABLES parserState =                   
        many1Till (PRODUCT_VARIABLE parserState) (followedBy END)

    let private applyParser (parser: Parser<_, unit>) var (npv: NonProcessedVariable) mapper
            : Result<string * DefinedVariable, string> =     
        // Make sure that all of the variable content is used up.
        // No need to trim opening spaces as that will have already been done.
        let parser' =
            parser .>|*> eof

        match runParserOnString parser' () npv.Name npv.Content with
        | ParserResult.Success (expr, _, _) -> 
            let definedVariable =
                mapper (var, expr)

            Ok (npv.Name, definedVariable)

        | ParserResult.Failure (err, _, _) ->
            Error err

    let internal parse =
        parse {
            let! parserState = getUserState

            let! productName =
                PRODUCT_NAME parserState .>> WHITESPACE

            and! productStructure =
                USING
                >|+> STRUCTURE
                >|+>. STRUCTURE_NAME parserState
                .>> WHITESPACE
                
            and! parsedProductVariables =
                PRODUCT_VARIABLES parserState            

            do! parsedProductVariables
                |> List.tryFindDuplicateBy _.Name
                |> Option.either
                    (fun dup -> Error (sprintf "Product variable '%s' is not uniquely defined." dup))
                    Ok

            do! parsedProductVariables
                |> Seq.map _.Name
                |> Seq.tryFind productStructure.Inputs.ContainsKey
                |> Option.either
                    (fun dup -> Error (sprintf "Product '%s' already has variable '%s' defined as an input." productName dup))
                    Ok

            let inputVariables =
                productStructure.Inputs
                |> Map.map (fun _ -> Variable.fromUnderlying)
                |> Map.toSeq

            let productVariables =
                parsedProductVariables
                |> Seq.map (fun npv -> npv.Name, npv.Variable)

            let combinedVariables =
                inputVariables
                |> Seq.append productVariables
                |> Map.ofSeq

            let expressionParser =
                new ExpressionParser (combinedVariables)

            let expressionParser' =
                expressionParser :> IExpressionParser

            do expressionParser.Initialize ()

            let! processedProductVariables =
                parsedProductVariables
                |> Seq.map (fun npv ->
                    match npv.Variable with
                    | Variable.BooleanAccrual v ->
                        applyParser expressionParser'.BOOLEAN_ACCRUAL_EXPRESSION v npv DefinedVariable.buildFrom
                    | Variable.IntegerAccrual v ->
                        applyParser expressionParser'.INTEGER_ACCRUAL_EXPRESSION v npv DefinedVariable.buildFrom
                    | Variable.RealAccrual v ->
                        applyParser expressionParser'.REAL_ACCRUAL_EXPRESSION v npv DefinedVariable.buildFrom
                    | Variable.StringAccrual v ->
                        applyParser expressionParser'.STRING_ACCRUAL_EXPRESSION v npv DefinedVariable.buildFrom
                    | Variable.DateAccrual v ->
                        applyParser expressionParser'.DATE_ACCRUAL_EXPRESSION v npv DefinedVariable.buildFrom)
                |> Seq.sequenceResultM
                |> Result.map Map.ofSeq

            let (newProduct: ParsedProduct) =
                {
                    Name = productName;
                    Structure = productStructure;
                    DefinedVariables = processedProductVariables
                }

            let! newParserState =
                parserState
                |> ParserState.addParsedProduct newProduct

            do! setUserState newParserState
        }