
namespace Parser.Expression
    
open Parser

open FSharp.Linq.RuntimeHelpers
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FParsec
open Specification.Expressions


[<Sealed>]
type internal CallableFactory (expressionParser: IExpressionParser) =

    // Cannot use explicit generic type parameter with a let binding here.
    member private _.parserForType<'T> () =
        let t = typeof<'T>

        if t = typeof<BooleanAccrualExpression> then
            <@@ expressionParser.BOOLEAN_ACCRUAL_EXPRESSION @@>
        elif t = typeof<IntegerAccrualExpression> then
            <@@ expressionParser.INTEGER_ACCRUAL_EXPRESSION @@>
        elif t = typeof<RealAccrualExpression> then
            <@@ expressionParser.REAL_ACCRUAL_EXPRESSION @@>
        else
            failwith (sprintf "Unable to process callable with argument type '%s'." t.Name)


    member this.MakeCallable1 ([<ReflectedDefinition>] callable: Expr<'T1 -> 'U>) =
        match callable with
        | Lambdas (_, Call(_, mi, _)) ->
            let p1 =
                this.parserForType<'T1> ()

            let expr =
                // Given that 'callable' is a typed expression, we want to reflect this
                // underlying type information when inserting by using a single '%'.
                // If we use '%%', we would have to add a type annotation in order
                // for 'EvaluateQuotation' to work.
                <@@ CALLABLE_1 (mi.Name, %%p1) |>> %callable @@>

            let parser =
                LeafExpressionConverter.EvaluateQuotation expr

            mi.Name, parser :?> Parser<'U, unit>

        | _ ->
            failwith "Unable to construct callable."

    member this.MakeCallable2 ([<ReflectedDefinition>] callable: Expr<('T1 * 'T2) -> 'U>) =
        match callable with
        | Lambdas (_, Call(_, mi, _)) ->
            let p1 =
                this.parserForType<'T1> ()

            let p2 =
                this.parserForType<'T2> ()

            let expr =
                <@@ CALLABLE_2 (mi.Name, %%p1, %%p2) |>> %callable @@>

            let parser =
                LeafExpressionConverter.EvaluateQuotation expr

            mi.Name, parser :?> Parser<'U, unit>

        | _ ->
            failwith "Unable to construct callable."

    member this.MakeCallableFlexible ([<ReflectedDefinition>] callable: Expr<'T1 list -> 'U>) =
        match callable with
        | Lambdas (_, Call(_, mi, _)) ->
            let p1 =
                this.parserForType<'T1> ()

            let expr =
                <@@ CALLABLE_FLEXIBLE (mi.Name, %%p1) |>> %callable @@>

            let parser =
                LeafExpressionConverter.EvaluateQuotation expr

            mi.Name, parser :?> Parser<'U, unit>

        | _ ->
            failwith "Unable to construct callable."