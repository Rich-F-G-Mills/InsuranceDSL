
namespace Specification

module rec Dependencies =

    open FsToolkit.ErrorHandling
    open Variables
    open Expressions
    open Definitions


    [<RequireQualifiedAccess>]
    type AccrualVariableDependency =
        | AccrualTarget of AccrualVariable * NonFutureRelativeOffset
        | RollbackTarget of RollbackVariable * RelativeOffset
        | SingletonTarget of SingletonVariable
        | InputTarget of InputVariable

    [<RequireQualifiedAccess>]
    type RollbackVariableDependency =
        | AccrualTarget of AccrualVariable * RelativeOffset
        | RollbackTarget of RollbackVariable * NonPastRelativeOffset
        | SingletonTarget of SingletonVariable
        | InputTarget of InputVariable

    [<RequireQualifiedAccess>]
    type SingletonVariableDependency =
        | AccrualTarget of AccrualVariable * AggregationType
        | RollbackTarget of RollbackVariable * AggregationType
        | SingletonTarget of SingletonVariable
        | InputTarget of InputVariable

    type AccrualVariableDependencyPairing =
        AccrualVariable * AccrualVariableDependency Set            

    type RollbackVariableDependencyPairing =
        RollbackVariable * RollbackVariableDependency Set

    type SingletonVariableDependencyPairing =
        SingletonVariable * SingletonVariableDependency Set

    // Without this, we cannot mark the constructors as internal due to external
    // assemblies not being able to pattern match.
    [<RequireQualifiedAccess>]
    module VariableDependencyPairing =
        let (|Accrual|Rollback|Singleton|) = function
            | VariableDependencyPairing_.Accrual p -> Accrual p
            | VariableDependencyPairing_.Rollback p -> Rollback p
            | VariableDependencyPairing_.Singleton p -> Singleton p

    [<RequireQualifiedAccess>]
    type VariableDependencyPairing_ =
        private
        | Accrual of AccrualVariableDependencyPairing
        | Rollback of RollbackVariableDependencyPairing
        | Singleton of SingletonVariableDependencyPairing

        static member buildFrom (definedVariable: DefinedVariable) =
            match definedVariable with
            | DefinedVariable.BooleanAccrual (v, e) -> 
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.IntegerAccrual (v, e) -> 
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.RealAccrual (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.StringAccrual (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.DateAccrual (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.IntegerEnumerationAccrual (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.StringEnumerationAccrual (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.BooleanRollback (v, e) -> 
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.IntegerRollback (v, e) -> 
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.RealRollback (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.StringRollback (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.DateRollback (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.IntegerEnumerationRollback (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.StringEnumerationRollback (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.BooleanSingleton (v, e) -> 
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.IntegerSingleton (v, e) -> 
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.RealSingleton (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.StringSingleton (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.DateSingleton (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.IntegerEnumerationSingleton (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)
            | DefinedVariable.StringEnumerationSingleton (v, e) ->
                VariableDependencyPairing_.buildFrom (v, e)


        // If for no other reason than to keep IntelliSense under control, these are called as necessary
        // by the dispatcher above.
        // These previously returned the Result type in order to capture instance where
        // the definition of an enumeration variable differed to its corresponding expression.
        // Frankly, there is no excuse for this to happen and hence this will now raise an exception.
        static member private buildFrom (v: BooleanAccrualVariable, expr: BooleanAccrualExpression) =
            VariableDependencyPairing_.Accrual (AccrualVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: IntegerAccrualVariable, expr: IntegerAccrualExpression) =
            VariableDependencyPairing_.Accrual (AccrualVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: RealAccrualVariable, expr: RealAccrualExpression) =
            VariableDependencyPairing_.Accrual (AccrualVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: StringAccrualVariable, expr: StringAccrualExpression) =
            VariableDependencyPairing_.Accrual (AccrualVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: DateAccrualVariable, expr: DateAccrualExpression) =
            VariableDependencyPairing_.Accrual (AccrualVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: IntegerEnumerationAccrualVariable, expr: IntegerEnumerationAccrualExpression_) =
            let lhsDef = v.Definition
            let (IntegerEnumerationAccrualExpression (_, rhsDef)) = expr

            if lhsDef = rhsDef then
                VariableDependencyPairing_.Accrual (AccrualVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))
            else
                failwith $"Cannot assign integer accrual expression with definition {rhsDef} to variable with definition {lhsDef}."
            
        static member private buildFrom (v: StringEnumerationAccrualVariable, expr: StringEnumerationAccrualExpression_) =           
            let lhsDef = v.Definition
            let (StringEnumerationAccrualExpression (_, rhsDef)) = expr

            if lhsDef = rhsDef then
                VariableDependencyPairing_.Accrual (AccrualVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))  
            else
                failwith $"Cannot assign string accrual expression with definition {rhsDef} to variable with definition {lhsDef}."

        static member private buildFrom (v: BooleanRollbackVariable, expr: BooleanRollbackExpression) =
            VariableDependencyPairing_.Rollback (RollbackVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))
        
        static member private buildFrom (v: IntegerRollbackVariable, expr: IntegerRollbackExpression) =
            VariableDependencyPairing_.Rollback (RollbackVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: RealRollbackVariable, expr: RealRollbackExpression) =
            VariableDependencyPairing_.Rollback (RollbackVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: StringRollbackVariable, expr: StringRollbackExpression) =
            VariableDependencyPairing_.Rollback (RollbackVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: DateRollbackVariable, expr: DateRollbackExpression) =
            VariableDependencyPairing_.Rollback (RollbackVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: IntegerEnumerationRollbackVariable, expr: IntegerEnumerationRollbackExpression_) =
            let lhsDef = v.Definition
            let (IntegerEnumerationRollbackExpression (_, rhsDef)) = expr

            if lhsDef = rhsDef then
                VariableDependencyPairing_.Rollback (RollbackVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))
            else
                failwith $"Cannot assign integer accrual expression with definition {rhsDef} to variable with definition {lhsDef}."

        static member private buildFrom (v: StringEnumerationRollbackVariable, expr: StringEnumerationRollbackExpression_) =
            let lhsDef = v.Definition
            let (StringEnumerationRollbackExpression (_, rhsDef)) = expr

            if lhsDef = rhsDef then
                VariableDependencyPairing_.Rollback (RollbackVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))
            else
                failwith $"Cannot assign string accrual expression with definition {rhsDef} to variable with definition {lhsDef}."

        static member private buildFrom (v: BooleanSingletonVariable, expr: BooleanSingletonExpression) =
            VariableDependencyPairing_.Singleton (SingletonVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: IntegerSingletonVariable, expr: IntegerSingletonExpression) =
            VariableDependencyPairing_.Singleton (SingletonVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: RealSingletonVariable, expr: RealSingletonExpression) =
            VariableDependencyPairing_.Singleton (SingletonVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: StringSingletonVariable, expr: StringSingletonExpression) =
            VariableDependencyPairing_.Singleton (SingletonVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: DateSingletonVariable, expr: DateSingletonExpression) =
            VariableDependencyPairing_.Singleton (SingletonVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))

        static member private buildFrom (v: IntegerEnumerationSingletonVariable, expr: IntegerEnumerationSingletonExpression_) =
            let lhsDef = v.Definition
            let (IntegerEnumerationSingletonExpression (_, rhsDef)) = expr

            if lhsDef = rhsDef then
                VariableDependencyPairing_.Singleton (SingletonVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))
            else
                failwith $"Cannot assign string accrual expression with definition {rhsDef} to variable with definition {lhsDef}."

        static member private buildFrom (v: StringEnumerationSingletonVariable, expr: StringEnumerationSingletonExpression_) =
            let lhsDef = v.Definition
            let (StringEnumerationSingletonExpression (_, rhsDef)) = expr
            
            if lhsDef = rhsDef then
                VariableDependencyPairing_.Singleton (SingletonVariable.fromUnderlying v, Set (VariableDependencyPairing_.processExpr expr))
            else
                failwith $"Cannot assign string accrual expression with definition {rhsDef} to variable with definition {lhsDef}."


        static member private processExpr (expr: BooleanAccrualExpression) =
            match expr with
            | BooleanAccrualExpression.Constant _ ->
                Seq.empty
            | BooleanAccrualExpression.AccrualVariable (v, o) ->
                seq { yield AccrualVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | BooleanAccrualExpression.RollbackVariable (v, o) ->
                seq { yield AccrualVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | BooleanAccrualExpression.SingletonVariable v ->
                seq { yield AccrualVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | BooleanAccrualExpression.InputVariable v ->
                seq { yield AccrualVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | BooleanAccrualExpression.BinaryComparable (_, pair) ->
                VariableDependencyPairing_.processExpr pair  
            | BooleanAccrualExpression.BinaryEquatable (_, pair) ->
                VariableDependencyPairing_.processExpr pair
            | BooleanAccrualExpression.And bs ->
                bs |> Seq.collect VariableDependencyPairing_.processExpr
            | BooleanAccrualExpression.Or bs ->
                bs |> Seq.collect VariableDependencyPairing_.processExpr
            | BooleanAccrualExpression.Not b ->
                VariableDependencyPairing_.processExpr b
            | BooleanAccrualExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }

        static member private processExpr (expr: ComparableAccrualExpressionPair) =
            match expr with
            | ComparableAccrualExpressionPair.Integer (i1, i2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr i1
                    yield! VariableDependencyPairing_.processExpr i2
                }                
            | ComparableAccrualExpressionPair.Real (r1, r2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr r1
                    yield! VariableDependencyPairing_.processExpr r2
                }
            | ComparableAccrualExpressionPair.Date (d1, d2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d1
                    yield! VariableDependencyPairing_.processExpr d2
                }       

        static member private processExpr (expr: EquatableAccrualExpressionPair) =
            match expr with
            | EquatableAccrualExpressionPair.Boolean (b1, b2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b1
                    yield! VariableDependencyPairing_.processExpr b2
                }   
            | EquatableAccrualExpressionPair.Integer (i1, i2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr i1
                    yield! VariableDependencyPairing_.processExpr i2
                }                
            | EquatableAccrualExpressionPair.Real (r1, r2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr r1
                    yield! VariableDependencyPairing_.processExpr r2
                }
            | EquatableAccrualExpressionPair.Date (d1, d2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d1
                    yield! VariableDependencyPairing_.processExpr d2
                }      
            | EquatableAccrualExpressionPair.String (s1, s2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr s1
                    yield! VariableDependencyPairing_.processExpr s2
                }  
            | EquatableAccrualExpressionPair.IntegerEnumeration (ie1, ie2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr ie1
                    yield! VariableDependencyPairing_.processExpr ie2
                }  
            | EquatableAccrualExpressionPair.StringEnumeration (se1, se2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr se1
                    yield! VariableDependencyPairing_.processExpr se2
                }  

        static member private processExpr (expr: IntegerAccrualExpression) =
            match expr with
            | IntegerAccrualExpression.Constant _ ->
                Seq.empty
            | IntegerAccrualExpression.AccrualVariable (v, o) ->
                seq { yield AccrualVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | IntegerAccrualExpression.RollbackVariable (v, o) ->
                seq { yield AccrualVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | IntegerAccrualExpression.SingletonVariable v ->
                seq { yield AccrualVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | IntegerAccrualExpression.InputVariable v ->
                seq { yield AccrualVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | IntegerAccrualExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }                    
            | IntegerAccrualExpression.BinaryArithmeticOp (_, x, y) ->
                seq {                        
                    yield! VariableDependencyPairing_.processExpr x
                    yield! VariableDependencyPairing_.processExpr y
                } 
            | IntegerAccrualExpression.UnaryArithmeticOp (_, x) ->
                VariableDependencyPairing_.processExpr x
            | IntegerAccrualExpression.FromReal r ->
                VariableDependencyPairing_.processExpr r
            | IntegerAccrualExpression.StringLength s ->
                VariableDependencyPairing_.processExpr s

        static member private processExpr (expr: RealAccrualExpression) =
            match expr with
            | RealAccrualExpression.Constant _ ->
                Seq.empty
            | RealAccrualExpression.AccrualVariable (v, o) ->
                seq { yield AccrualVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | RealAccrualExpression.RollbackVariable (v, o) ->
                seq { yield AccrualVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | RealAccrualExpression.SingletonVariable v ->
                seq { yield AccrualVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | RealAccrualExpression.InputVariable v ->
                seq { yield AccrualVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | RealAccrualExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }                    
            | RealAccrualExpression.BinaryArithmeticOp (_, x, y) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr x
                    yield! VariableDependencyPairing_.processExpr y
                } 
            | RealAccrualExpression.UnaryArithmeticOp (_, x) ->
                VariableDependencyPairing_.processExpr x
            | RealAccrualExpression.FromInteger i ->
                VariableDependencyPairing_.processExpr i

        static member private processExpr (expr: StringAccrualExpression) =
            match expr with
            | StringAccrualExpression.Constant _ ->
                Seq.empty
            | StringAccrualExpression.AccrualVariable (v, o) ->
                seq { yield AccrualVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | StringAccrualExpression.RollbackVariable (v, o) ->
                seq { yield AccrualVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | StringAccrualExpression.SingletonVariable v ->
                seq { yield AccrualVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | StringAccrualExpression.InputVariable v ->
                seq { yield AccrualVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | StringAccrualExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }
                
        static member private processExpr (expr: DateAccrualExpression) =
            match expr with
            | DateAccrualExpression.Constant _ ->
                Seq.empty
            | DateAccrualExpression.AccrualVariable (v, o) ->
                seq { yield AccrualVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | DateAccrualExpression.RollbackVariable (v, o) ->
                seq { yield AccrualVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | DateAccrualExpression.SingletonVariable v ->
                seq { yield AccrualVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | DateAccrualExpression.InputVariable v ->
                seq { yield AccrualVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | DateAccrualExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }
            | DateAccrualExpression.Offset (d, i, _) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d
                    yield! VariableDependencyPairing_.processExpr i                   
                }
            | DateAccrualExpression.DiffDays (d1, d2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d1
                    yield! VariableDependencyPairing_.processExpr d2  
                }
                
        static member private processExpr (expr: IntegerEnumerationAccrualExpression_) =
            let (IntegerEnumerationAccrualExpression (expr', _)) = expr

            match expr' with
            | IntegerEnumerationAccrualExpressionWithoutDefinition.Constant _ ->
                Seq.empty
            | IntegerEnumerationAccrualExpressionWithoutDefinition.AccrualVariable (v, o) ->
                seq { yield AccrualVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | IntegerEnumerationAccrualExpressionWithoutDefinition.RollbackVariable (v, o) ->
                seq { yield AccrualVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | IntegerEnumerationAccrualExpressionWithoutDefinition.SingletonVariable v ->
                seq { yield AccrualVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | IntegerEnumerationAccrualExpressionWithoutDefinition.InputVariable v ->
                seq { yield AccrualVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | IntegerEnumerationAccrualExpressionWithoutDefinition.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }

        static member private processExpr (expr: StringEnumerationAccrualExpression_) =
            let (StringEnumerationAccrualExpression (expr', _)) = expr

            match expr' with
            | StringEnumerationAccrualExpressionWithoutDefinition.Constant _ ->
                Seq.empty
            | StringEnumerationAccrualExpressionWithoutDefinition.AccrualVariable (v, o) ->
                seq { yield AccrualVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | StringEnumerationAccrualExpressionWithoutDefinition.RollbackVariable (v, o) ->
                seq { yield AccrualVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | StringEnumerationAccrualExpressionWithoutDefinition.SingletonVariable v ->
                seq { yield AccrualVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | StringEnumerationAccrualExpressionWithoutDefinition.InputVariable v ->
                seq { yield AccrualVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | StringEnumerationAccrualExpressionWithoutDefinition.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }    


        static member private processExpr (expr: BooleanRollbackExpression) =
            match expr with
            | BooleanRollbackExpression.Constant _ ->
                Seq.empty
            | BooleanRollbackExpression.AccrualVariable (v, o) ->
                seq { yield RollbackVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | BooleanRollbackExpression.RollbackVariable (v, o) ->
                seq { yield RollbackVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | BooleanRollbackExpression.SingletonVariable v ->
                seq { yield RollbackVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | BooleanRollbackExpression.InputVariable v ->
                seq { yield RollbackVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | BooleanRollbackExpression.BinaryComparable (_, pair) ->
                VariableDependencyPairing_.processExpr pair
            | BooleanRollbackExpression.BinaryEquatable (_, pair) ->
                VariableDependencyPairing_.processExpr pair
            | BooleanRollbackExpression.And bs ->
                bs |> Seq.collect VariableDependencyPairing_.processExpr
            | BooleanRollbackExpression.Or bs ->
                bs |> Seq.collect VariableDependencyPairing_.processExpr
            | BooleanRollbackExpression.Not b ->
                VariableDependencyPairing_.processExpr b
            | BooleanRollbackExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }

        static member private processExpr (expr: ComparableRollbackExpressionPair) =
            match expr with
            | ComparableRollbackExpressionPair.Integer (i1, i2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr i1
                    yield! VariableDependencyPairing_.processExpr i2
                }                
            | ComparableRollbackExpressionPair.Real (r1, r2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr r1
                    yield! VariableDependencyPairing_.processExpr r2
                }
            | ComparableRollbackExpressionPair.Date (d1, d2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d1
                    yield! VariableDependencyPairing_.processExpr d2
                }

        static member private processExpr (expr: EquatableRollbackExpressionPair) =
            match expr with
            | EquatableRollbackExpressionPair.Boolean (b1, b2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b1
                    yield! VariableDependencyPairing_.processExpr b2
                }   
            | EquatableRollbackExpressionPair.Integer (i1, i2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr i1
                    yield! VariableDependencyPairing_.processExpr i2
                }                
            | EquatableRollbackExpressionPair.Real (r1, r2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr r1
                    yield! VariableDependencyPairing_.processExpr r2
                }
            | EquatableRollbackExpressionPair.Date (d1, d2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d1
                    yield! VariableDependencyPairing_.processExpr d2
                }      
            | EquatableRollbackExpressionPair.String (s1, s2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr s1
                    yield! VariableDependencyPairing_.processExpr s2
                }
            | EquatableRollbackExpressionPair.IntegerEnumeration (ie1, ie2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr ie1
                    yield! VariableDependencyPairing_.processExpr ie2
                }
            | EquatableRollbackExpressionPair.StringEnumeration (se1, se2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr se1
                    yield! VariableDependencyPairing_.processExpr se2
                }

        static member private processExpr (expr: IntegerRollbackExpression) =
            match expr with
            | IntegerRollbackExpression.Constant _ ->
                Seq.empty
            | IntegerRollbackExpression.AccrualVariable (v, o) ->
                seq { yield RollbackVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | IntegerRollbackExpression.RollbackVariable (v, o) ->
                seq { yield RollbackVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | IntegerRollbackExpression.SingletonVariable v ->
                seq { yield RollbackVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | IntegerRollbackExpression.InputVariable v ->
                seq { yield RollbackVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | IntegerRollbackExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }                    
            | IntegerRollbackExpression.BinaryArithmeticOp (_, x, y) ->
                seq {                        
                    yield! VariableDependencyPairing_.processExpr x
                    yield! VariableDependencyPairing_.processExpr y
                } 
            | IntegerRollbackExpression.UnaryArithmeticOp (_, x) ->
                VariableDependencyPairing_.processExpr x
            | IntegerRollbackExpression.FromReal r ->
                VariableDependencyPairing_.processExpr r
            | IntegerRollbackExpression.StringLength s ->
                VariableDependencyPairing_.processExpr s

        static member private processExpr (expr: RealRollbackExpression) =
            match expr with
            | RealRollbackExpression.Constant _ ->
                Seq.empty
            | RealRollbackExpression.AccrualVariable (v, o) ->
                seq { yield RollbackVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | RealRollbackExpression.RollbackVariable (v, o) ->
                seq { yield RollbackVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | RealRollbackExpression.SingletonVariable v ->
                seq { yield RollbackVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | RealRollbackExpression.InputVariable v ->
                seq { yield RollbackVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | RealRollbackExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }                    
            | RealRollbackExpression.BinaryArithmeticOp (_, x, y) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr x
                    yield! VariableDependencyPairing_.processExpr y
                } 
            | RealRollbackExpression.UnaryArithmeticOp (_, x) ->
                VariableDependencyPairing_.processExpr x
            | RealRollbackExpression.FromInteger i ->
                VariableDependencyPairing_.processExpr i

        static member private processExpr (expr: StringRollbackExpression) =
            match expr with
            | StringRollbackExpression.Constant _ ->
                Seq.empty
            | StringRollbackExpression.AccrualVariable (v, o) ->
                seq { yield RollbackVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | StringRollbackExpression.RollbackVariable (v, o) ->
                seq { yield RollbackVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | StringRollbackExpression.SingletonVariable v ->
                seq { yield RollbackVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | StringRollbackExpression.InputVariable v ->
                seq { yield RollbackVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | StringRollbackExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }     

        static member private processExpr (expr: DateRollbackExpression) =
            match expr with
            | DateRollbackExpression.Constant _ ->
                Seq.empty
            | DateRollbackExpression.AccrualVariable (v, o) ->
                seq { yield RollbackVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | DateRollbackExpression.RollbackVariable (v, o) ->
                seq { yield RollbackVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | DateRollbackExpression.SingletonVariable v ->
                seq { yield RollbackVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | DateRollbackExpression.InputVariable v ->
                seq { yield RollbackVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | DateRollbackExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }
            | DateRollbackExpression.Offset (d, i, _) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d
                    yield! VariableDependencyPairing_.processExpr i                   
                }
            | DateRollbackExpression.DiffDays (d1, d2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d1
                    yield! VariableDependencyPairing_.processExpr d2  
                }
                
        static member private processExpr (expr: IntegerEnumerationRollbackExpression_) =
            let (IntegerEnumerationRollbackExpression (expr', _)) = expr

            match expr' with
            | IntegerEnumerationRollbackExpressionWithoutDefinition.Constant _ ->
                Seq.empty
            | IntegerEnumerationRollbackExpressionWithoutDefinition.AccrualVariable (v, o) ->
                seq { yield RollbackVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | IntegerEnumerationRollbackExpressionWithoutDefinition.RollbackVariable (v, o) ->
                seq { yield RollbackVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | IntegerEnumerationRollbackExpressionWithoutDefinition.SingletonVariable v ->
                seq { yield RollbackVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | IntegerEnumerationRollbackExpressionWithoutDefinition.InputVariable v ->
                seq { yield RollbackVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | IntegerEnumerationRollbackExpressionWithoutDefinition.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }

        static member private processExpr (expr: StringEnumerationRollbackExpression_) =
            let (StringEnumerationRollbackExpression (expr', _)) = expr

            match expr' with
            | StringEnumerationRollbackExpressionWithoutDefinition.Constant _ ->
                Seq.empty
            | StringEnumerationRollbackExpressionWithoutDefinition.AccrualVariable (v, o) ->
                seq { yield RollbackVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | StringEnumerationRollbackExpressionWithoutDefinition.RollbackVariable (v, o) ->
                seq { yield RollbackVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | StringEnumerationRollbackExpressionWithoutDefinition.SingletonVariable v ->
                seq { yield RollbackVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | StringEnumerationRollbackExpressionWithoutDefinition.InputVariable v ->
                seq { yield RollbackVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | StringEnumerationRollbackExpressionWithoutDefinition.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }    


        static member private processExpr (expr: BooleanSingletonExpression) =
            match expr with
            | BooleanSingletonExpression.Constant _ ->
                Seq.empty
            | BooleanSingletonExpression.AccrualVariable (v, o) ->
                seq { yield SingletonVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, o) }
            | BooleanSingletonExpression.RollbackVariable (v, o) ->
                seq { yield SingletonVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, o) }
            | BooleanSingletonExpression.SingletonVariable v ->
                seq { yield SingletonVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | BooleanSingletonExpression.InputVariable v ->
                seq { yield SingletonVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | BooleanSingletonExpression.BinaryComparable (_, pair) ->
                VariableDependencyPairing_.processExpr pair
            | BooleanSingletonExpression.BinaryEquatable (_, pair) ->
                VariableDependencyPairing_.processExpr pair
            | BooleanSingletonExpression.And bs ->
                bs |> Seq.collect VariableDependencyPairing_.processExpr
            | BooleanSingletonExpression.Or bs ->
                bs |> Seq.collect VariableDependencyPairing_.processExpr
            | BooleanSingletonExpression.Not b ->
                VariableDependencyPairing_.processExpr b
            | BooleanSingletonExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }

        static member private processExpr (expr: ComparableSingletonExpressionPair) =
            match expr with
            | ComparableSingletonExpressionPair.Integer (i1, i2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr i1
                    yield! VariableDependencyPairing_.processExpr i2
                }                
            | ComparableSingletonExpressionPair.Real (r1, r2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr r1
                    yield! VariableDependencyPairing_.processExpr r2
                }
            | ComparableSingletonExpressionPair.Date (d1, d2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d1
                    yield! VariableDependencyPairing_.processExpr d2
                }

        static member private processExpr (expr: EquatableSingletonExpressionPair) =
            match expr with
            | EquatableSingletonExpressionPair.Boolean (b1, b2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b1
                    yield! VariableDependencyPairing_.processExpr b2
                }   
            | EquatableSingletonExpressionPair.Integer (i1, i2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr i1
                    yield! VariableDependencyPairing_.processExpr i2
                }                
            | EquatableSingletonExpressionPair.Real (r1, r2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr r1
                    yield! VariableDependencyPairing_.processExpr r2
                }
            | EquatableSingletonExpressionPair.Date (d1, d2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d1
                    yield! VariableDependencyPairing_.processExpr d2
                }      
            | EquatableSingletonExpressionPair.String (s1, s2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr s1
                    yield! VariableDependencyPairing_.processExpr s2
                }                
            | EquatableSingletonExpressionPair.IntegerEnumeration (ie1, ie2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr ie1
                    yield! VariableDependencyPairing_.processExpr ie2
                }
            | EquatableSingletonExpressionPair.StringEnumeration (se1, se2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr se1
                    yield! VariableDependencyPairing_.processExpr se2
                }

        static member private processExpr (expr: IntegerSingletonExpression) =
            match expr with
            | IntegerSingletonExpression.Constant _ ->
                Seq.empty
            | IntegerSingletonExpression.AccrualVariable (v, a) ->
                seq { yield SingletonVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, a) }
            | IntegerSingletonExpression.RollbackVariable (v, a) ->
                seq { yield SingletonVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, a) }
            | IntegerSingletonExpression.SingletonVariable v ->
                seq { yield SingletonVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | IntegerSingletonExpression.InputVariable v ->
                seq { yield SingletonVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | IntegerSingletonExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }                    
            | IntegerSingletonExpression.BinaryArithmeticOp (_, x, y) ->
                seq {                        
                    yield! VariableDependencyPairing_.processExpr x
                    yield! VariableDependencyPairing_.processExpr y
                } 
            | IntegerSingletonExpression.UnaryArithmeticOp (_, x) ->
                VariableDependencyPairing_.processExpr x
            | IntegerSingletonExpression.FromReal r ->
                VariableDependencyPairing_.processExpr r
            | IntegerSingletonExpression.StringLength s ->
                VariableDependencyPairing_.processExpr s

        static member private processExpr (expr: RealSingletonExpression) =
            match expr with
            | RealSingletonExpression.Constant _ ->
                Seq.empty
            | RealSingletonExpression.AccrualVariable (v, a) ->
                seq { yield SingletonVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, a) }
            | RealSingletonExpression.RollbackVariable (v, a) ->
                seq { yield SingletonVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, a) }
            | RealSingletonExpression.SingletonVariable v ->
                seq { yield SingletonVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | RealSingletonExpression.InputVariable v ->
                seq { yield SingletonVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | RealSingletonExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }                    
            | RealSingletonExpression.BinaryArithmeticOp (_, x, y) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr x
                    yield! VariableDependencyPairing_.processExpr y
                } 
            | RealSingletonExpression.UnaryArithmeticOp (_, x) ->
                VariableDependencyPairing_.processExpr x
            | RealSingletonExpression.FromInteger i ->
                VariableDependencyPairing_.processExpr i

        static member private processExpr (expr: StringSingletonExpression) =
            match expr with
            | StringSingletonExpression.Constant _ ->
                Seq.empty
            | StringSingletonExpression.SingletonVariable v ->
                seq { yield SingletonVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | StringSingletonExpression.InputVariable v ->
                seq { yield SingletonVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | StringSingletonExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }

        static member private processExpr (expr: DateSingletonExpression) =
            match expr with
            | DateSingletonExpression.Constant _ ->
                Seq.empty
            | DateSingletonExpression.AccrualVariable (v, a) ->
                seq { yield SingletonVariableDependency.AccrualTarget (AccrualVariable.fromUnderlying v, a) }
            | DateSingletonExpression.RollbackVariable (v, a) ->
                seq { yield SingletonVariableDependency.RollbackTarget (RollbackVariable.fromUnderlying v, a) }
            | DateSingletonExpression.SingletonVariable v ->
                seq { yield SingletonVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | DateSingletonExpression.InputVariable v ->
                seq { yield SingletonVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | DateSingletonExpression.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }
            | DateSingletonExpression.Offset (d, i, _) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d
                    yield! VariableDependencyPairing_.processExpr i                   
                }
            | DateSingletonExpression.DiffDays (d1, d2) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr d1
                    yield! VariableDependencyPairing_.processExpr d2  
                }
                
        static member private processExpr (expr: IntegerEnumerationSingletonExpression_) =
            let (IntegerEnumerationSingletonExpression (expr', _)) = expr

            match expr' with
            | IntegerEnumerationSingletonExpressionWithoutDefinition.Constant _ ->
                Seq.empty
            | IntegerEnumerationSingletonExpressionWithoutDefinition.SingletonVariable v ->
                seq { yield SingletonVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | IntegerEnumerationSingletonExpressionWithoutDefinition.InputVariable v ->
                seq { yield SingletonVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | IntegerEnumerationSingletonExpressionWithoutDefinition.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }

        static member private processExpr (expr: StringEnumerationSingletonExpression_) =
            let (StringEnumerationSingletonExpression (expr', _)) = expr

            match expr' with
            | StringEnumerationSingletonExpressionWithoutDefinition.Constant _ ->
                Seq.empty
            | StringEnumerationSingletonExpressionWithoutDefinition.SingletonVariable v ->
                seq { yield SingletonVariableDependency.SingletonTarget (SingletonVariable.fromUnderlying v) }
            | StringEnumerationSingletonExpressionWithoutDefinition.InputVariable v ->
                seq { yield SingletonVariableDependency.InputTarget (InputVariable.fromUnderlying v) }
            | StringEnumerationSingletonExpressionWithoutDefinition.Conditional (b, t, f) ->
                seq {
                    yield! VariableDependencyPairing_.processExpr b
                    yield! VariableDependencyPairing_.processExpr t
                    yield! VariableDependencyPairing_.processExpr f
                }
