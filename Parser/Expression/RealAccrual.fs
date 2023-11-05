
namespace Parser.Expression

open Parser
open Parser.Attributes

open FParsec
open FSharp.Core
open FsToolkit.ErrorHandling
open Specification.Expressions
open Specification.Variables


[<RequireQualifiedAccess>]
module RealAccrualExpression =

    module private Callables =
        type private Locator () =
            interface ILocator

        [<Callable>]
        let internal ABS expr =
            RealAccrualExpression.UnaryArithmeticOp (UnaryArithmeticOp.Abs, expr)

        [<Callable>]
        let internal REAL expr =
            RealAccrualExpression.FromInteger expr

        [<Callable>]
        let internal MOD (expr1, expr2) =
            RealAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Modulus, expr1, expr2)


    let internal parse (expressionParser: IExpressionParser, callableFactory: CallableFactory) =

        let (EXPRESSION, EXPRESSION_REF) =
            createParserForwardedToRef<RealAccrualExpression, unit> ()

        let CONSTANT =
            REAL_CONSTANT |>> RealAccrualExpression.Constant

        let VARIABLE =
            parse {
                match! expressionParser.VARIABLE with
                | Variable.RealAccrual v ->
                    return! NON_FUTURE_RELATIVE_OFFSET |>> (fun o -> RealAccrualExpression.AccrualVariable (v, o))
                | Variable.RealRollback v ->
                    return! RELATIVE_OFFSET |>> (fun o -> RealAccrualExpression.RollbackVariable (v, o))                    
                | Variable.RealSingleton v ->
                    return (RealAccrualExpression.SingletonVariable v)
                | Variable.RealInput v ->
                    return (RealAccrualExpression.InputVariable v)
                | UnderlyingVariable v ->
                    return! failFatally (sprintf "Variable '%s' has unexpected type '%A'." v.Name v.Type)
            }

        let CALLABLE =
            let callableMappings =
                [
                    callableFactory.MakeCallable1 Callables.ABS
                    callableFactory.MakeCallable2 Callables.MOD
                    callableFactory.MakeCallable1 Callables.REAL
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
                    RealAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Multiply, f1, f2) 
                | FactorOp.Divide ->
                    RealAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Divide, f1, f2)
            
            MAKE_TERM (FACTOR, folder)

        let TERM_SEQUENCE =            
            let folder f1 (op, f2) =
                match op with
                | TermOp.Add ->
                    RealAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Add, f1, f2) 
                | TermOp.Subtract ->
                    RealAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Subtract, f1, f2)
            
            MAKE_TERM_SEQUENCE (TERM, folder)          

        let CONDITIONAL =
            let folder (condition, ifTrue) ifFalse =
                RealAccrualExpression.Conditional (condition, ifTrue, ifFalse)

            MAKE_CONDITIONAL (expressionParser.BOOLEAN_ACCRUAL_EXPRESSION, EXPRESSION, folder)        

        do EXPRESSION_REF.Value <-
            CONDITIONAL <|> TERM_SEQUENCE

        EXPRESSION
