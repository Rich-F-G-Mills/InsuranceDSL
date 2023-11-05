
namespace ExpressionBuilder

open System
open Specification.Variables
open Specification.Expressions


[<AutoOpen>]
module rec Accrual_Integer =

    type IntegerAccrualExpression with
        
        member this.IntExpr () =
            this


        // Cannot use inline SRTP approach as type resolution ignores extension methods (RFC FS-1043).
        member this.Add (x: IntegerAccrualExpression) =
            IntegerAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Add, this, x)

        member this.Add (x: IntegerAccrualVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerRollbackVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerSingletonVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerInputVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: Int32) =
            this.Add(x.IntExpr())


        member this.Subtract (x: IntegerAccrualExpression) =
            IntegerAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Subtract, this, x)

        member this.Subtract (x: IntegerAccrualVariable) =
            this.Subtract(x.IntExpr())

        member this.Subtract (x: IntegerRollbackVariable) =
            this.Subtract(x.IntExpr())

        member this.Subtract (x: IntegerSingletonVariable) =
            this.Subtract(x.IntExpr())

        member this.Subtract (x: IntegerInputVariable) =
            this.Subtract(x.IntExpr())

        member this.Subtract (x: Int32) =
            this.Subtract(x.IntExpr())


        member this.Multiply (x: IntegerAccrualExpression) =
            IntegerAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Multiply, this, x)

        member this.Multiply (x: IntegerAccrualVariable) =
            this.Multiply(x.IntExpr())

        member this.Multiply (x: IntegerRollbackVariable) =
            this.Multiply(x.IntExpr())

        member this.Multiply (x: IntegerSingletonVariable) =
            this.Multiply(x.IntExpr())

        member this.Multiply (x: IntegerInputVariable) =
            this.Multiply(x.IntExpr())

        member this.Multiply (x: Int32) =
            this.Multiply(x.IntExpr())


        member this.Divide (x: IntegerAccrualExpression) =
            IntegerAccrualExpression.BinaryArithmeticOp (BinaryArithmeticOp.Divide, this, x)

        member this.Divide (x: IntegerAccrualVariable) =
            this.Divide(x.IntExpr())

        member this.Divide (x: IntegerRollbackVariable) =
            this.Divide(x.IntExpr())

        member this.Divide (x: IntegerSingletonVariable) =
            this.Divide(x.IntExpr())

        member this.Divide (x: IntegerInputVariable) =
            this.Divide(x.IntExpr())

        member this.Divide (x: Int32) =
            this.Divide(x.IntExpr())


    type IntegerAccrualVariable with
        
        member this.IntExpr () =
            this.Current ()


        member this.Current () =
            IntegerAccrualExpression.Current (IntegerTimeDependentVariable.Accrual this)

        member this.Previous () =
            IntegerAccrualExpression.Previous (IntegerTimeDependentVariable.Accrual this)


        member this.Add (x: IntegerAccrualExpression) =
            this.IntExpr().Add(x)

        member this.Add (x: IntegerAccrualVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerRollbackVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerSingletonVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerInputVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: Int32) =
            this.Add(x.IntExpr())


        member this.Subtract (x: IntegerAccrualExpression) =
            this.IntExpr().Subtract(x)

        member this.Subtract (x: IntegerAccrualVariable) =
            this.Subtract(x.IntExpr())

        member this.Subtract (x: IntegerRollbackVariable) =
            this.Subtract(x.IntExpr())

        member this.Subtract (x: IntegerSingletonVariable) =
            this.Subtract(x.IntExpr())

        member this.Subtract (x: IntegerInputVariable) =
            this.Subtract(x.IntExpr())

        member this.Subtract (x: Int32) =
            this.Subtract(x.IntExpr())


        member this.Multiply (x: IntegerAccrualExpression) =
            this.IntExpr().Multiply(x)

        member this.Multiply (x: IntegerAccrualVariable) =
            this.Multiply(x.IntExpr())

        member this.Multiply (x: IntegerRollbackVariable) =
            this.Multiply(x.IntExpr())

        member this.Multiply (x: IntegerSingletonVariable) =
            this.Multiply(x.IntExpr())

        member this.Multiply (x: IntegerInputVariable) =
            this.Multiply(x.IntExpr())

        member this.Multiply (x: Int32) =
            this.Multiply(x.IntExpr())


        member this.Divide (x: IntegerAccrualExpression) =
            this.IntExpr().Divide(x)

        member this.Divide (x: IntegerAccrualVariable) =
            this.Divide(x.IntExpr())

        member this.Divide (x: IntegerRollbackVariable) =
            this.Divide(x.IntExpr())

        member this.Divide (x: IntegerSingletonVariable) =
            this.Divide(x.IntExpr())

        member this.Divide (x: IntegerInputVariable) =
            this.Divide(x.IntExpr())

        member this.Divide (x: Int32) =
            this.Divide(x.IntExpr())


    type IntegerRollbackVariable with

        member this.IntExpr () =
            this.Current()


        member this.Next () =
            IntegerAccrualExpression.Next this

        member this.Current () =
            IntegerAccrualExpression.Current (IntegerTimeDependentVariable.Rollback this)

        member this.Previous () =
            IntegerAccrualExpression.Previous (IntegerTimeDependentVariable.Rollback this)


        member this.Add (x: IntegerAccrualExpression) =
            this.IntExpr().Add(x)

        member this.Add (x: IntegerAccrualVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerRollbackVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerSingletonVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerInputVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: Int32) =
            this.Add(x.IntExpr())


    type IntegerSingletonVariable with

        member this.IntExpr () =
            IntegerAccrualExpression.Singleton this


        member this.Add (x: IntegerAccrualExpression) =
            this.IntExpr().Add(x)

        member this.Add (x: IntegerAccrualVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerRollbackVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerSingletonVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerInputVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: Int32) =
            this.Add(x.IntExpr())


    type IntegerInputVariable with

        member this.IntExpr () =
            IntegerAccrualExpression.Input this


        member this.Add (x: IntegerAccrualExpression) =
            this.IntExpr().Add(x)

        member this.Add (x: IntegerAccrualVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerRollbackVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerSingletonVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerInputVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: Int32) =
            this.Add(x.IntExpr())


    type Int32 with

        member this.IntExpr () =
            IntegerAccrualExpression.Constant this


        member this.Add (x: IntegerAccrualExpression) =
            this.IntExpr().Add(x)

        member this.Add (x: IntegerAccrualVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerRollbackVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerSingletonVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: IntegerInputVariable) =
            this.Add(x.IntExpr())

        member this.Add (x: Int32) =
            this.Add(x.IntExpr())
