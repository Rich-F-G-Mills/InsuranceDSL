
namespace Parser.Expression

open Parser
open Parser.Attributes

open FParsec
open FSharp.Core
open FsToolkit.ErrorHandling
open Specification.Expressions
open Specification.Variables


[<RequireQualifiedAccess>]
module IntegerAccrualExpression =

    module private Callables =
        type private Locator () =
            interface ILocator

        [<Callable>]
        let internal ABS expr =
            IntegerAccrualExpression.UnaryArithmeticOp (UnaryArithmeticOp.Abs, expr)

        [<Callable>]
        let internal INT expr =
            IntegerAccrualExpression.FromReal expr

        [<Callable>]
        let internal MOD (expr1, expr2) =
            IntegerAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Modulus, expr1, expr2)


    let internal parse (expressionParser: IExpressionParser, callableFactory: CallableFactory) =

        let (EXPRESSION, EXPRESSION_REF) =
            createParserForwardedToRef<IntegerAccrualExpression, unit> ()

        let CONSTANT =
            INTEGER_CONSTANT |>> IntegerAccrualExpression.Constant

        let VARIABLE =
            parse {
                match! expressionParser.VARIABLE with
                | Variable.IntegerAccrual v ->
                    return! NON_FUTURE_RELATIVE_OFFSET |>> (fun o -> IntegerAccrualExpression.AccrualVariable (v, o))
                | Variable.IntegerRollback v ->
                    return! RELATIVE_OFFSET |>> (fun o -> IntegerAccrualExpression.RollbackVariable (v, o))                    
                | Variable.IntegerSingleton v ->
                    return (IntegerAccrualExpression.SingletonVariable v)
                | Variable.IntegerInput v ->
                    return (IntegerAccrualExpression.InputVariable v)
                | UnderlyingVariable v ->
                    return! failFatally (sprintf "Variable '%s' has unexpected type '%A'." v.Name v.Type)
            }

        let CALLABLE =
            let callableMappings =
                [
                    callableFactory.MakeCallable1 Callables.ABS
                    callableFactory.MakeCallable2 Callables.MOD
                    callableFactory.MakeCallable1 Callables.INT
                ]
                |> Map.ofSeq
                
            CALLABLE_FROM_MAPPINGS callableMappings

        let PARENTHESES =
            MAKE_PARENTHESES EXPRESSION   

        let FACTOR =
            let choices =
                [
                    CONSTANT                    
                    VARIABLE
                    CALLABLE
                    PARENTHESES
                ]

            choice choices

        let TERM =
            let folder f1 (op, f2) =
                match op with
                | FactorOp.Multiply ->
                    IntegerAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Multiply, f1, f2) 
                | FactorOp.Divide ->
                    IntegerAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Divide, f1, f2)
            
            MAKE_TERM (FACTOR, folder)

        let TERM_SEQUENCE =            
            let folder f1 (op, f2) =
                match op with
                | TermOp.Add ->
                    IntegerAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Add, f1, f2) 
                | TermOp.Subtract ->
                    IntegerAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Subtract, f1, f2)
            
            MAKE_TERM_SEQUENCE (TERM, folder)          

        let CONDITIONAL =
            let folder (condition, ifTrue) ifFalse =
                IntegerAccrualExpression.Conditional (condition, ifTrue, ifFalse)

            MAKE_CONDITIONAL (expressionParser.BOOLEAN_ACCRUAL_EXPRESSION, EXPRESSION, folder)        

        do EXPRESSION_REF.Value <-
            CONDITIONAL <|> TERM_SEQUENCE

        EXPRESSION
