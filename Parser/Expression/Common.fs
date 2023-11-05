
#nowarn "1189"

namespace Parser.Expression
    

[<AutoOpen>]
module Common =

    open Parser

    open FParsec
    open Specification.Variables
    open Specification.Expressions


    [<RequireQualifiedAccess; NoComparison>]
    type internal FactorOp =
        | Multiply
        | Divide

    [<RequireQualifiedAccess; NoComparison>]
    type internal TermOp =
        | Add
        | Subtract


    let inline internal (|UnderlyingVariable|)<'T when 'T: (member UnderlyingVariable: IVariable)> (v: 'T) =
        v.UnderlyingVariable


    type internal IExpressionParser =
        abstract BOOLEAN_ACCRUAL_EXPRESSION: Parser<BooleanAccrualExpression, unit> with get
        abstract INTEGER_ACCRUAL_EXPRESSION: Parser<IntegerAccrualExpression, unit> with get
        abstract REAL_ACCRUAL_EXPRESSION: Parser<RealAccrualExpression, unit> with get
        abstract STRING_ACCRUAL_EXPRESSION: Parser<StringAccrualExpression, unit> with get
        abstract DATE_ACCRUAL_EXPRESSION: Parser<DateAccrualExpression, unit> with get
        abstract INTEGER_ENUMERATION_ACCRUAL_EXPRESSION: Parser<IntegerEnumerationAccrualExpression_, unit> with get
        abstract STRING_ENUMERATION_ACCRUAL_EXPRESSION: Parser<StringEnumerationAccrualExpression_, unit> with get

        abstract VARIABLE: Parser<Variable, unit> with get
         

    let internal IDENTIFIER_BOUNDARY: Parser<_, unit> =
        let boundaries =
            [
                skipAnyOf "+-*/(),.<>=!"
                WHITESPACE
                eof
            ]

        choice boundaries

    let inline internal IDENTIFIER str =
        skipString str >.> followedBy IDENTIFIER_BOUNDARY


    let inline internal CALLABLE_1 (str, p1): Parser<_, unit> =
        IDENTIFIER str >|*> skipChar '(' >|*>. p1 .>|*> skipChar ')'

    let inline internal CALLABLE_2 (str, p1, p2): Parser<_, unit> =
        IDENTIFIER str >|*> skipChar '(' >|*>. p1 .>|*> skipChar ',' .>|*>. p2 .>|*> skipChar ')'

    let inline internal CALLABLE_FLEXIBLE (str, p1): Parser<_, unit> =
        IDENTIFIER str >|*> skipChar '(' >|*>. sepBy p1 (skipChar ',' >.> spaces) .>> skipChar ')'


    let private LOOKAHEAD_CALLABLE_IDENTIFIER =
        lookAhead (regex "[A-Z][A-Z_]*" .>|*> skipChar '(')

    let internal CALLABLE_FROM_MAPPINGS (callables: Map<string, Parser<'T, unit>>) =
        parse {
            // Check to see if we have something that looks like a callable.
            let! callableId =
                LOOKAHEAD_CALLABLE_IDENTIFIER

            // If we do... Apply the relevant parser.
            if callables.ContainsKey callableId then
                return! callables[callableId]
            else
                return! failFatally (sprintf "Unrecognised callable '%s'." callableId)
        }                


    let internal FACTOR_OP: Parser<_, unit> =
        (skipChar '*' >>% FactorOp.Multiply)
        <|> (skipChar '/' >>% FactorOp.Divide)

    let internal TERM_OP: Parser<_, unit> =
        (skipChar '+' >>% TermOp.Add)
        <|> (skipChar '-' >>% TermOp.Subtract)

    let internal BINARY_EQUATABLE_OP: Parser<_, unit> =
        (skipString "==" >>% BinaryEquatableOp.Equal)
        <|> (skipString "!=" >>% BinaryEquatableOp.NotEqual)
        <?> "equatable operation"

    let internal BINARY_COMPARABLE_OP: Parser<_, unit> =
        let choices =
            [
                // Put the longer strings to match first!
                skipString "<=" >>% BinaryComparableOp.LessThanOrEqual
                skipString ">=" >>% BinaryComparableOp.GreaterThanOrEqual
                skipString "<" >>% BinaryComparableOp.LessThan                
                skipString ">" >>% BinaryComparableOp.GreaterThan                
            ]
        
        choiceL choices "comparative operation"


    let internal NON_FUTURE_RELATIVE_OFFSET: Parser<_, unit> =
        let choices =
            [
                IDENTIFIER "PREVIOUS" >>% NonFutureRelativeOffset.Previous
                IDENTIFIER "CURRENT" >>% NonFutureRelativeOffset.Current
            ]

        let offsets =
            choiceL choices "Offset of either 'PREVIOUS' or 'CURRENT'."

        (skipChar '.' >>. offsets .>> followedBy IDENTIFIER_BOUNDARY)
        <|>% NonFutureRelativeOffset.Current

    let internal RELATIVE_OFFSET: Parser<_, unit> =
        let choices =
            [
                IDENTIFIER "PREVIOUS" >>% RelativeOffset.Previous
                IDENTIFIER "CURRENT" >>% RelativeOffset.Current
                IDENTIFIER "NEXT" >>% RelativeOffset.Next
            ]

        let offsets =
            choiceL choices "Offset of either 'PREVIOUS', 'CURRENT' or 'NEXT'."

        (skipChar '.' >>. offsets .>> followedBy IDENTIFIER_BOUNDARY)
        <|>% RelativeOffset.Current

    let internal NON_PAST_RELATIVE_OFFSET: Parser<_, unit> =
        let choices =
            [
                IDENTIFIER "CURRENT" >>% NonPastRelativeOffset.Current
                IDENTIFIER "NEXT" >>% NonPastRelativeOffset.Next
            ]

        let offsets =
            choiceL choices "Offset of either 'CURRENT' or 'NEXT'."

        (skipChar '.' >>. offsets .>> followedBy IDENTIFIER_BOUNDARY)
        <|>% NonPastRelativeOffset.Current


    let internal MAKE_TERM (factor, folder) =
        // Consume as much closing whitespace as we can.
        factor .>|*>. many (FACTOR_OP .>|*>. factor .>> spaces)
        // Using fold makes this left-associative.
        |>> (fun (f1, fs) -> List.fold folder f1 fs)

    let internal MAKE_TERM_SEQUENCE (term, folder) =
        term .>|*>. many (TERM_OP .>|*>. term .>> spaces)
        // Using fold makes this left-associative.
        |>> (fun (t1, ts) -> List.fold folder t1 ts)

    let internal MAKE_PARENTHESES expr =
        // There could be spaces between opening bracket and start of expression.
        between (skipChar '(' >.> spaces) (skipChar ')') expr
        .>> followedBy IDENTIFIER_BOUNDARY

    let internal MAKE_CONDITIONAL (booleanExpr, expr, folder): Parser<_, unit> =
        let conditionalStep str =
            // Assume no trailing whitespace after applying expression parsers.
            skipString str >|+>. booleanExpr .>> skipString "THEN" .>|+>. expr

        let OPENING_IF =
            conditionalStep "IF"

        let ELSE_IFS =
            many (conditionalStep "ELIF")

        let CLOSING_ELSE =
            skipString "ELSE" >|+>. expr
            
        parse {
            // Check to see if it's worth going down this route.
            // Otherwise, we could be tricked by (for example) "= IFSOME.NEXT".
            do! lookAhead (skipString "IF" >.> spaces1)

            let! opening = OPENING_IF
            and! middle = ELSE_IFS
            and! closing = CLOSING_ELSE

            return
                // Using foldBack makes this right-associative.
                List.foldBack folder (opening :: middle) closing
        }


    let INTEGER_CONSTANT =
        pint32 .>> followedBy IDENTIFIER_BOUNDARY

    let REAL_CONSTANT =
        pfloat .>> followedBy IDENTIFIER_BOUNDARY

    let BOOLEAN_CONSTANT =
        let choices =
            [
                skipString "TRUE" >>% true
                skipString "FALSE" >>% false
            ]

        choiceL choices "boolean constant"
        .>> followedBy IDENTIFIER_BOUNDARY
