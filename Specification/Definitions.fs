
namespace Specification

module Definitions =

    open Variables
    open Expressions


    // This would not (of course) include input variables which don't have expression based definitions.
    [<RequireQualifiedAccess; NoComparison>]
    type DefinedVariable =
        | BooleanAccrual of BooleanAccrualVariable * BooleanAccrualExpression
        | IntegerAccrual of IntegerAccrualVariable * IntegerAccrualExpression
        | RealAccrual of RealAccrualVariable * RealAccrualExpression
        | StringAccrual of StringAccrualVariable * StringAccrualExpression
        | DateAccrual of DateAccrualVariable * DateAccrualExpression
        | IntegerEnumerationAccrual of IntegerEnumerationAccrualVariable * IntegerEnumerationAccrualExpression_
        | StringEnumerationAccrual of StringEnumerationAccrualVariable * StringEnumerationAccrualExpression_

        | BooleanRollback of BooleanRollbackVariable * BooleanRollbackExpression
        | IntegerRollback of IntegerRollbackVariable * IntegerRollbackExpression
        | RealRollback of RealRollbackVariable * RealRollbackExpression
        | StringRollback of StringRollbackVariable * StringRollbackExpression
        | DateRollback of DateRollbackVariable * DateRollbackExpression
        | IntegerEnumerationRollback of IntegerEnumerationRollbackVariable * IntegerEnumerationRollbackExpression_
        | StringEnumerationRollback of StringEnumerationRollbackVariable * StringEnumerationRollbackExpression_

        | BooleanSingleton of BooleanSingletonVariable * BooleanSingletonExpression
        | IntegerSingleton of IntegerSingletonVariable * IntegerSingletonExpression
        | RealSingleton of RealSingletonVariable * RealSingletonExpression
        | StringSingleton of StringSingletonVariable * StringSingletonExpression
        | DateSingleton of DateSingletonVariable * DateSingletonExpression
        | IntegerEnumerationSingleton of IntegerEnumerationSingletonVariable * IntegerEnumerationSingletonExpression_
        | StringEnumerationSingleton of StringEnumerationSingletonVariable * StringEnumerationSingletonExpression_


        // These were previously returning a Result type... However, the caller MUST know
        // in advance if there is going to be an issue.
        // Hence, this will now raise an exception.
        static member buildFrom (v: BooleanAccrualVariable, e: BooleanAccrualExpression) =
            DefinedVariable.BooleanAccrual (v, e)

        static member buildFrom (v: IntegerAccrualVariable, e: IntegerAccrualExpression) =
            DefinedVariable.IntegerAccrual (v, e)

        static member buildFrom (v: RealAccrualVariable, e: RealAccrualExpression) =
            DefinedVariable.RealAccrual (v, e)

        static member buildFrom (v: StringAccrualVariable, e: StringAccrualExpression) =
            DefinedVariable.StringAccrual (v, e)

        static member buildFrom (v: DateAccrualVariable, e: DateAccrualExpression) =
            DefinedVariable.DateAccrual (v, e)

        static member buildFrom (v: IntegerEnumerationAccrualVariable, e: IntegerEnumerationAccrualExpression_) =
            let vDef = v.Definition
            let (IntegerEnumerationAccrualExpression (_, eDef)) = e
            
            if vDef = eDef then
                DefinedVariable.IntegerEnumerationAccrual (v, e)
            else
                failwith (sprintf "Mismatch in enumeration definition for '%s' between variable and expression." v.Name)

        static member buildFrom (v: StringEnumerationAccrualVariable, e: StringEnumerationAccrualExpression_) =
            let vDef = v.Definition
            let (StringEnumerationAccrualExpression (_, eDef)) = e
            
            if vDef = eDef then
                DefinedVariable.StringEnumerationAccrual (v, e)
            else
                failwith (sprintf "Mismatch in enumeration definition for '%s' between variable and expression." v.Name)


        static member buildFrom (v: BooleanRollbackVariable, e: BooleanRollbackExpression) =
            DefinedVariable.BooleanRollback (v, e)

        static member buildFrom (v: IntegerRollbackVariable, e: IntegerRollbackExpression) =
            DefinedVariable.IntegerRollback (v, e)

        static member buildFrom (v: RealRollbackVariable, e: RealRollbackExpression) =
            DefinedVariable.RealRollback (v, e)

        static member buildFrom (v: StringRollbackVariable, e: StringRollbackExpression) =
            DefinedVariable.StringRollback (v, e)

        static member buildFrom (v: DateRollbackVariable, e: DateRollbackExpression) =
            DefinedVariable.DateRollback (v, e)

        static member buildFrom (v: IntegerEnumerationRollbackVariable, e: IntegerEnumerationRollbackExpression_) =
            let vDef = v.Definition
            let (IntegerEnumerationRollbackExpression (_, eDef)) = e
            
            if vDef = eDef then
                DefinedVariable.IntegerEnumerationRollback (v, e)
            else
                failwith (sprintf "Mismatch in enumeration definition for '%s' between variable and expression." v.Name)

        static member buildFrom (v: StringEnumerationRollbackVariable, e: StringEnumerationRollbackExpression_) =
            let vDef = v.Definition
            let (StringEnumerationRollbackExpression (_, eDef)) = e
            
            if vDef = eDef then
                DefinedVariable.StringEnumerationRollback (v, e)
            else
                failwith (sprintf "Mismatch in enumeration definition for '%s' between variable and expression." v.Name)


        static member buildFrom (v: BooleanSingletonVariable, e: BooleanSingletonExpression) =
            DefinedVariable.BooleanSingleton (v, e)

        static member buildFrom (v: IntegerSingletonVariable, e: IntegerSingletonExpression) =
            DefinedVariable.IntegerSingleton (v, e)

        static member buildFrom (v: RealSingletonVariable, e: RealSingletonExpression) =
            DefinedVariable.RealSingleton (v, e)

        static member buildFrom (v: StringSingletonVariable, e: StringSingletonExpression) =
            DefinedVariable.StringSingleton (v, e)

        static member buildFrom (v: DateSingletonVariable, e: DateSingletonExpression) =
            DefinedVariable.DateSingleton (v, e)

        static member buildFrom (v: IntegerEnumerationSingletonVariable, e: IntegerEnumerationSingletonExpression_) =
            let vDef = v.Definition
            let (IntegerEnumerationSingletonExpression (_, eDef)) = e
            
            if vDef = eDef then
                DefinedVariable.IntegerEnumerationSingleton (v, e)
            else
                failwith (sprintf "Mismatch in enumeration definition for '%s' between variable and expression." v.Name)

        static member buildFrom (v: StringEnumerationSingletonVariable, e: StringEnumerationSingletonExpression_) =
            let vDef = v.Definition
            let (StringEnumerationSingletonExpression (_, eDef)) = e
            
            if vDef = eDef then
                DefinedVariable.StringEnumerationSingleton (v, e)
            else
                failwith (sprintf "Mismatch in enumeration definition for '%s' between variable and expression." v.Name)


        member this.Variable =
            match this with
            | BooleanAccrual (v, _) -> Variable.BooleanAccrual v
            | IntegerAccrual (v, _) -> Variable.IntegerAccrual v
            | RealAccrual (v, _) -> Variable.RealAccrual v
            | StringAccrual (v, _) -> Variable.StringAccrual v
            | DateAccrual (v, _) -> Variable.DateAccrual v
            | IntegerEnumerationAccrual (v, _) -> Variable.IntegerEnumerationAccrual v
            | StringEnumerationAccrual (v, _) -> Variable.StringEnumerationAccrual v

            | BooleanRollback (v, _) -> Variable.BooleanRollback v
            | IntegerRollback (v, _) -> Variable.IntegerRollback v
            | RealRollback (v, _) -> Variable.RealRollback v
            | StringRollback (v, _) -> Variable.StringRollback v
            | DateRollback (v, _) -> Variable.DateRollback v
            | IntegerEnumerationRollback (v, _) -> Variable.IntegerEnumerationRollback v
            | StringEnumerationRollback (v, _) -> Variable.StringEnumerationRollback v

            | BooleanSingleton (v, _) -> Variable.BooleanSingleton v
            | IntegerSingleton (v, _) -> Variable.IntegerSingleton v
            | RealSingleton (v, _) -> Variable.RealSingleton v
            | StringSingleton (v, _) -> Variable.StringSingleton v
            | DateSingleton (v, _) -> Variable.DateSingleton v
            | IntegerEnumerationSingleton (v, _) -> Variable.IntegerEnumerationSingleton v
            | StringEnumerationSingleton (v, _) -> Variable.StringEnumerationSingleton v


        member this.Expression =
            match this with
            | BooleanAccrual (_, e) -> Expression.BooleanAccrual e
            | IntegerAccrual (_, e) -> Expression.IntegerAccrual e
            | RealAccrual (_, e) -> Expression.RealAccrual e
            | StringAccrual (_, e) -> Expression.StringAccrual e
            | DateAccrual (_, e) -> Expression.DateAccrual e
            | IntegerEnumerationAccrual (_, e) -> Expression.IntegerEnumerationAccrual e
            | StringEnumerationAccrual (_, e) -> Expression.StringEnumerationAccrual e

            | BooleanRollback (_, e) -> Expression.BooleanRollback e
            | IntegerRollback (_, e) -> Expression.IntegerRollback e
            | RealRollback (_, e) -> Expression.RealRollback e
            | StringRollback (_, e) -> Expression.StringRollback e
            | DateRollback (_, e) -> Expression.DateRollback e
            | IntegerEnumerationRollback (_, e) -> Expression.IntegerEnumerationRollback e
            | StringEnumerationRollback (_, e) -> Expression.StringEnumerationRollback e

            | BooleanSingleton (_, e) -> Expression.BooleanSingleton e
            | IntegerSingleton (_, e) -> Expression.IntegerSingleton e
            | RealSingleton (_, e) -> Expression.RealSingleton e
            | StringSingleton (_, e) -> Expression.StringSingleton e
            | DateSingleton (_, e) -> Expression.DateSingleton e
            | IntegerEnumerationSingleton (_, e) -> Expression.IntegerEnumerationSingleton e
            | StringEnumerationSingleton (_, e) -> Expression.StringEnumerationSingleton e
