
namespace Parser.Expression
    

open FParsec
open Specification.Expressions


[<Sealed>]
type internal ExpressionParser (variables) as this =

    // Without this 'ceremony', we end up with circular references between (for example)
    // the integer accrual expression parser and real accrual expression parser.
    // In effect, we need this in order to delay binding.
    let (BOOLEAN_ACCRUAL_EXPRESSION', BOOLEAN_ACCRUAL_EXPRESSION_REF) =
        createParserForwardedToRef<BooleanAccrualExpression, unit> ()

    let (INTEGER_ACCRUAL_EXPRESSION', INTEGER_ACCRUAL_EXPRESSION_REF) =
        createParserForwardedToRef<IntegerAccrualExpression, unit> ()

    let (REAL_ACCRUAL_EXPRESSION', REAL_ACCRUAL_EXPRESSION_REF) =
        createParserForwardedToRef<RealAccrualExpression, unit> ()


    member _.Initialize () =
        let callableFactory =
            new CallableFactory (this)


        do BOOLEAN_ACCRUAL_EXPRESSION_REF.Value <-
            BooleanAccrualExpression.parse (this, callableFactory)

        do INTEGER_ACCRUAL_EXPRESSION_REF.Value <-
            IntegerAccrualExpression.parse (this, callableFactory)

        do REAL_ACCRUAL_EXPRESSION_REF.Value <-
            RealAccrualExpression.parse (this, callableFactory)



    interface IExpressionParser with

        // These will NOT ensure that all content has been used up!
        // This is because, depending on the context, the required expression
        // ending can be different.
        member val BOOLEAN_ACCRUAL_EXPRESSION =
            BOOLEAN_ACCRUAL_EXPRESSION'

        member val INTEGER_ACCRUAL_EXPRESSION =
            INTEGER_ACCRUAL_EXPRESSION'

        member val REAL_ACCRUAL_EXPRESSION =
            REAL_ACCRUAL_EXPRESSION'

        member val STRING_ACCRUAL_EXPRESSION =
            fail "TEST"

        member val DATE_ACCRUAL_EXPRESSION =
            fail "TEST"

        member val INTEGER_ENUMERATION_ACCRUAL_EXPRESSION =
            fail "TEST"

        member val STRING_ENUMERATION_ACCRUAL_EXPRESSION =
            fail "TEST"
            

        member val VARIABLE =                    
            variables
            |> Map.toSeq
            |> Seq.map (fun (name, var) -> skipString name >>% var)
            |> choice
            .>> followedByL IDENTIFIER_BOUNDARY "defined variable"

        