
namespace Parser.Expression

open Parser
open Parser.Attributes

open FParsec
open FSharp.Core
open FsToolkit.ErrorHandling
open Specification.Expressions
open Specification.Variables


[<RequireQualifiedAccess>]
module BooleanAccrualExpression =

    module private Callables =
        type private Locator () =
            interface ILocator

        [<Callable>]
        let internal AND exprs =
            BooleanAccrualExpression.And exprs

        [<Callable>]
        let internal OR exprs =
            BooleanAccrualExpression.Or exprs

        [<Callable>]
        let internal NOT expr =
            BooleanAccrualExpression.Not expr


    let internal parse (expressionParser: IExpressionParser, callableFactory: CallableFactory) =

        let (EXPRESSION, EXPRESSION_REF) =
            createParserForwardedToRef<BooleanAccrualExpression, unit> ()

        let LITERAL =
            BOOLEAN_LITERAL |>> BooleanAccrualExpression.Constant

        let VARIABLE =
            parse {
                match! expressionParser.VARIABLE with
                | Variable.BooleanAccrual v ->
                    return! NON_FUTURE_RELATIVE_OFFSET |>> (fun o -> BooleanAccrualExpression.AccrualVariable (v, o))
                | Variable.BooleanRollback v ->
                    return! RELATIVE_OFFSET |>> (fun o -> BooleanAccrualExpression.RollbackVariable (v, o))                    
                | Variable.BooleanSingleton v ->
                    return (BooleanAccrualExpression.SingletonVariable v)
                | Variable.BooleanInput v ->
                    return (BooleanAccrualExpression.InputVariable v)
                | UnderlyingVariable v ->
                    return! failFatally (sprintf "Variable '%s' has unexpected type '%A'." v.Name v.Type)
            }

        let CALLABLE =
            let callableMappings =
                [
                    callableFactory.MakeCallableFlexible Callables.AND
                    callableFactory.MakeCallableFlexible Callables.OR
                    callableFactory.MakeCallable1 Callables.NOT
                ]
                |> Map.ofSeq
                
            CALLABLE_FROM_MAPPINGS callableMappings

        let PARENTHESES =
            MAKE_PARENTHESES EXPRESSION

        let ELEMENT =
            let choices =
                [
                    LITERAL
                    VARIABLE
                    CALLABLE
                    PARENTHESES
                ]

            choice choices

        let EQUATABLE_EXPRESSION =
            let makeChoice p mapper : Parser<BooleanAccrualExpression, unit> =
                // We use attempt to that we can completely backtrack as necessary.
                attempt (pipe5 p spaces BINARY_EQUATABLE_OP spaces p (fun lhs _ op _ rhs ->
                    BooleanAccrualExpression.BinaryEquatable (op, mapper (lhs, rhs))))

            let choices =
                [   
                    // We cannot use a boolean expression here or we will end up in an infinite loop.
                    makeChoice ELEMENT EquatableAccrualExpressionPair.Boolean 
                    // We want to see if we can work with integers before progressing to reals.
                    makeChoice expressionParser.INTEGER_ACCRUAL_EXPRESSION EquatableAccrualExpressionPair.Integer
                    makeChoice expressionParser.REAL_ACCRUAL_EXPRESSION EquatableAccrualExpressionPair.Real
                    makeChoice expressionParser.STRING_ACCRUAL_EXPRESSION EquatableAccrualExpressionPair.String
                    makeChoice expressionParser.DATE_ACCRUAL_EXPRESSION EquatableAccrualExpressionPair.Date
                    makeChoice expressionParser.INTEGER_ENUMERATION_ACCRUAL_EXPRESSION EquatableAccrualExpressionPair.IntegerEnumeration
                    makeChoice expressionParser.STRING_ENUMERATION_ACCRUAL_EXPRESSION EquatableAccrualExpressionPair.StringEnumeration
                ]

            choice choices

        let COMPARABLE_EXPRESSION =
            let makeChoice p mapper : Parser<BooleanAccrualExpression, unit> =
                attempt (pipe5 p spaces BINARY_COMPARABLE_OP spaces p (fun lhs _ op _ rhs ->
                    BooleanAccrualExpression.BinaryComparable (op, mapper (lhs, rhs))))

            let choices =
                [                   
                    makeChoice expressionParser.INTEGER_ACCRUAL_EXPRESSION ComparableAccrualExpressionPair.Integer
                    makeChoice expressionParser.REAL_ACCRUAL_EXPRESSION ComparableAccrualExpressionPair.Real
                    makeChoice expressionParser.DATE_ACCRUAL_EXPRESSION ComparableAccrualExpressionPair.Date
                ]

            choice choices

        let CONDITIONAL =
            let folder (condition, ifTrue) ifFalse =
                BooleanAccrualExpression.Conditional (condition, ifTrue, ifFalse)

            MAKE_CONDITIONAL (EXPRESSION, EXPRESSION, folder)  

        do EXPRESSION_REF.Value <-
            let choices =
                [      
                    CONDITIONAL
                    EQUATABLE_EXPRESSION
                    COMPARABLE_EXPRESSION
                    // This needs to come after the equatable and comparable expressions
                    // above which permit back-tracking.
                    ELEMENT
                ]

            // Non-boolean expression consume closing spaces... Same is done here for consistency.
            choice choices .>> spaces

        EXPRESSION
