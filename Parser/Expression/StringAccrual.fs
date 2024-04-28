
namespace Parser.Expression

open Parser
open Parser.Attributes

open FParsec
open FSharp.Core
open FsToolkit.ErrorHandling
open Specification.Expressions
open Specification.Variables


[<RequireQualifiedAccess>]
module StringAccrualExpression =

    let internal parse (expressionParser: IExpressionParser, callableFactory: CallableFactory) =

        let (EXPRESSION, EXPRESSION_REF) =
            createParserForwardedToRef<StringAccrualExpression, unit> ()

        let LITERAL =
            STRING_LITERAL |>> StringAccrualExpression.Constant

        let VARIABLE =
            parse {
                match! expressionParser.VARIABLE with
                | Variable.StringAccrual v ->
                    return! NON_FUTURE_RELATIVE_OFFSET |>> (fun o -> StringAccrualExpression.AccrualVariable (v, o))
                | Variable.StringRollback v ->
                    return! RELATIVE_OFFSET |>> (fun o -> StringAccrualExpression.RollbackVariable (v, o))                    
                | Variable.StringSingleton v ->
                    return (StringAccrualExpression.SingletonVariable v)
                | Variable.StringInput v ->
                    return (StringAccrualExpression.InputVariable v)
                | UnderlyingVariable v ->
                    return! failFatally (sprintf "Variable '%s' has unexpected type '%A'." v.Name v.Type)
            }

        let PARENTHESES =
            MAKE_PARENTHESES EXPRESSION      

        let CONDITIONAL =
            let folder (condition, ifTrue) ifFalse =
                StringAccrualExpression.Conditional (condition, ifTrue, ifFalse)

            MAKE_CONDITIONAL (expressionParser.BOOLEAN_ACCRUAL_EXPRESSION, EXPRESSION, folder)        

        do EXPRESSION_REF.Value <-
            let choices =
                [
                    CONDITIONAL
                    LITERAL                    
                    VARIABLE
                    PARENTHESES
                ]

            choice choices

        EXPRESSION
