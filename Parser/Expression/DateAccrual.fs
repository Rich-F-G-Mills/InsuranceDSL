
namespace Parser.Expression

open Parser
open Parser.Attributes

open FParsec
open FSharp.Core
open FsToolkit.ErrorHandling
open Specification.Expressions
open Specification.Variables


[<RequireQualifiedAccess>]
module DateAccrualExpression =

    let internal parse (expressionParser: IExpressionParser, callableFactory: CallableFactory) =

        let (EXPRESSION, EXPRESSION_REF) =
            createParserForwardedToRef<DateAccrualExpression, unit> ()

        let LITERAL =
            DATE_LITERAL |>> DateAccrualExpression.Constant

        let VARIABLE =
            parse {
                match! expressionParser.VARIABLE with
                | Variable.DateAccrual v ->
                    return! NON_FUTURE_RELATIVE_OFFSET |>> (fun o -> DateAccrualExpression.AccrualVariable (v, o))
                | Variable.DateRollback v ->
                    return! RELATIVE_OFFSET |>> (fun o -> DateAccrualExpression.RollbackVariable (v, o))                    
                | Variable.DateSingleton v ->
                    return (DateAccrualExpression.SingletonVariable v)
                | Variable.DateInput v ->
                    return (DateAccrualExpression.InputVariable v)
                | UnderlyingVariable v ->
                    return! failFatally (sprintf "Variable '%s' has unexpected type '%A'." v.Name v.Type)
            }

        let PARENTHESES =
            MAKE_PARENTHESES EXPRESSION      

        let CONDITIONAL =
            let folder (condition, ifTrue) ifFalse =
                DateAccrualExpression.Conditional (condition, ifTrue, ifFalse)

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
