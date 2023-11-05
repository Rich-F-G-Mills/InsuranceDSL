
#nowarn "1189"

namespace Specification

module Variables =

    [<RequireQualifiedAccess; NoComparison>]
    type VariableType =
        | Boolean
        | Integer
        | Real
        | String
        | Date
        | IntegerEnumeration of Enumeration.Integer
        | StringEnumeration of Enumeration.String


    [<RequireQualifiedAccess; NoComparison>]
    type VariableMode =
        | Accrual
        | Rollback
        | Singleton
        | Input


    type IVariable =
        abstract Name: string with get
        abstract Type: VariableType with get
        abstract Mode: VariableMode with get

    type IEquatableVariable =
        inherit IVariable

    type IComparableVariable =
        inherit IEquatableVariable

    type IBooleanVariable =
        inherit IEquatableVariable

    type IIntegerVariable =
        inherit IComparableVariable

    type IRealVariable =
        inherit IComparableVariable

    type IStringVariable =
        inherit IEquatableVariable

    type IDateVariable =
        inherit IComparableVariable

    type IIntegerEnumerationVariable =
        inherit IEquatableVariable

        abstract Definition: Enumeration.Integer with get

    type IStringEnumerationVariable =
        inherit IEquatableVariable

        abstract Definition: Enumeration.String with get

    type IAccrualVariable =
        inherit IVariable

    type IRollbackVariable =
        inherit IVariable

    type ISingletonVariable =
        inherit IVariable

    type IInputVariable =
        inherit IVariable

    
    let inline private (|ToVariable|)<'T when 'T :> IVariable> (v: 'T) =
        v :> IVariable


    let inline private (|ToAccrualVariable|)<'T when 'T :> IAccrualVariable> (v: 'T) =
        v :> IAccrualVariable

    let inline private (|ToRollbackVariable|)<'T when 'T :> IRollbackVariable> (v: 'T) =
        v :> IRollbackVariable

    let inline private (|ToSingletonVariable|)<'T when 'T :> ISingletonVariable> (v: 'T) =
        v :> ISingletonVariable

    let inline private (|ToInputVariable|)<'T when 'T :> IInputVariable> (v: 'T) =
        v :> IInputVariable


    let inline private (|ToBooleanVariable|)<'T when 'T :> IBooleanVariable> (v: 'T) =
        v :> IBooleanVariable

    let inline private (|ToIntegerVariable|)<'T when 'T :> IIntegerVariable> (v: 'T) =
        v :> IIntegerVariable

    let inline private (|ToRealVariable|)<'T when 'T :> IRealVariable> (v: 'T) =
        v :> IRealVariable

    let inline private (|ToStringVariable|)<'T when 'T :> IStringVariable> (v: 'T) =
        v :> IStringVariable

    let inline private (|ToDateVariable|)<'T when 'T :> IDateVariable> (v: 'T) =
        v :> IDateVariable

    let inline private (|ToIntegerEnumerationVariable|)<'T when 'T :> IIntegerEnumerationVariable> (v: 'T) =
        v :> IIntegerEnumerationVariable

    let inline private (|ToStringEnumerationVariable|)<'T when 'T :> IStringEnumerationVariable> (v: 'T) =
        v :> IStringEnumerationVariable


    type BooleanAccrualVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Boolean
            member _.Mode = VariableMode.Accrual

        interface IBooleanVariable
        interface IAccrualVariable

    type BooleanRollbackVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Boolean
            member _.Mode = VariableMode.Rollback

        interface IBooleanVariable
        interface IRollbackVariable

    type BooleanSingletonVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Boolean
            member _.Mode = VariableMode.Singleton

        interface IBooleanVariable
        interface ISingletonVariable

    type BooleanInputVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Boolean
            member _.Mode = VariableMode.Input

        interface IBooleanVariable
        interface IInputVariable


    type IntegerAccrualVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Integer
            member _.Mode = VariableMode.Accrual

        interface IIntegerVariable
        interface IAccrualVariable

    type IntegerRollbackVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Integer
            member _.Mode = VariableMode.Rollback

        interface IIntegerVariable
        interface IRollbackVariable

    type IntegerSingletonVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Integer
            member _.Mode = VariableMode.Singleton

        interface IIntegerVariable
        interface ISingletonVariable

    type IntegerInputVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Integer
            member _.Mode = VariableMode.Input

        interface IIntegerVariable
        interface IInputVariable


    type RealAccrualVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Real
            member _.Mode = VariableMode.Accrual

        interface IRealVariable
        interface IAccrualVariable

    type RealRollbackVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Real
            member _.Mode = VariableMode.Rollback

        interface IRealVariable
        interface IRollbackVariable

    type RealSingletonVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Real
            member _.Mode = VariableMode.Singleton

        interface IRealVariable
        interface ISingletonVariable

    type RealInputVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Real
            member _.Mode = VariableMode.Input

        interface IRealVariable
        interface IInputVariable


    type StringAccrualVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.String
            member _.Mode = VariableMode.Accrual

        interface IStringVariable
        interface IAccrualVariable

    type StringRollbackVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.String
            member _.Mode = VariableMode.Rollback

        interface IStringVariable
        interface IRollbackVariable

    type StringSingletonVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.String
            member _.Mode = VariableMode.Singleton

        interface IStringVariable
        interface ISingletonVariable

    type StringInputVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.String
            member _.Mode = VariableMode.Input

        interface IStringVariable
        interface IInputVariable

 
    type DateAccrualVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Date
            member _.Mode = VariableMode.Accrual

        interface IDateVariable
        interface IAccrualVariable

    type DateRollbackVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Date
            member _.Mode = VariableMode.Rollback

        interface IDateVariable
        interface IRollbackVariable

    type DateSingletonVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Date
            member _.Mode = VariableMode.Singleton

        interface IDateVariable
        interface ISingletonVariable

    type DateInputVariable =
        {
            Name: string
        }

        interface IVariable with
            member this.Name = this.Name
            member _.Type = VariableType.Date
            member _.Mode = VariableMode.Input

        interface IDateVariable
        interface IInputVariable


    type IntegerEnumerationAccrualVariable =
        {
            Name: string
            Definition: Enumeration.Integer
        }

        interface IVariable with
            member this.Name = this.Name
            member this.Type = VariableType.IntegerEnumeration (this.Definition)
            member _.Mode = VariableMode.Accrual

        interface IIntegerEnumerationVariable with
            member this.Definition = this.Definition

        interface IAccrualVariable

    type IntegerEnumerationRollbackVariable =
        {
            Name: string
            Definition: Enumeration.Integer
        }

        interface IVariable with
            member this.Name = this.Name
            member this.Type = VariableType.IntegerEnumeration (this.Definition)
            member _.Mode = VariableMode.Rollback

        interface IIntegerEnumerationVariable with
            member this.Definition = this.Definition

        interface IRollbackVariable

    type IntegerEnumerationSingletonVariable =
        {
            Name: string
            Definition: Enumeration.Integer
        }

        interface IVariable with
            member this.Name = this.Name
            member this.Type = VariableType.IntegerEnumeration (this.Definition)
            member _.Mode = VariableMode.Singleton

        interface IIntegerEnumerationVariable with
            member this.Definition = this.Definition

        interface ISingletonVariable

    type IntegerEnumerationInputVariable =
        {
            Name: string
            Definition: Enumeration.Integer
        }

        interface IVariable with
            member this.Name = this.Name
            member this.Type = VariableType.IntegerEnumeration (this.Definition)
            member _.Mode = VariableMode.Input

        interface IIntegerEnumerationVariable with
            member this.Definition = this.Definition

        interface IInputVariable


    type StringEnumerationAccrualVariable =
        {
            Name: string
            Definition: Enumeration.String
        }

        interface IVariable with
            member this.Name = this.Name
            member this.Type = VariableType.StringEnumeration (this.Definition)
            member _.Mode = VariableMode.Accrual

        interface IStringEnumerationVariable with
            member this.Definition = this.Definition

        interface IAccrualVariable

    type StringEnumerationRollbackVariable =
        {
            Name: string
            Definition: Enumeration.String
        }

        interface IVariable with
            member this.Name = this.Name
            member this.Type = VariableType.StringEnumeration (this.Definition)
            member _.Mode = VariableMode.Rollback

        interface IStringEnumerationVariable with
            member this.Definition = this.Definition

        interface IRollbackVariable

    type StringEnumerationSingletonVariable =
        {
            Name: string
            Definition: Enumeration.String
        }

        interface IVariable with
            member this.Name = this.Name
            member this.Type = VariableType.StringEnumeration (this.Definition)
            member _.Mode = VariableMode.Singleton

        interface IStringEnumerationVariable with
            member this.Definition = this.Definition

        interface ISingletonVariable

    type StringEnumerationInputVariable =
        {
            Name: string
            Definition: Enumeration.String
        }

        interface IVariable with
            member this.Name = this.Name
            member this.Type = VariableType.StringEnumeration (this.Definition)
            member _.Mode = VariableMode.Input

        interface IStringEnumerationVariable with
            member this.Definition = this.Definition

        interface IInputVariable


    [<RequireQualifiedAccess>]
    type AccrualVariable =
        | Boolean of BooleanAccrualVariable
        | Integer of IntegerAccrualVariable
        | Real of RealAccrualVariable
        | String of StringAccrualVariable
        | Date of DateAccrualVariable
        | IntegerEnumeration of IntegerEnumerationAccrualVariable
        | StringEnumeration of StringEnumerationAccrualVariable

        // These are used as part of the dependency pairing breakdown logic.
        static member fromUnderlying (v: BooleanAccrualVariable) =
            Boolean v

        static member fromUnderlying (v: IntegerAccrualVariable) =
            Integer v

        static member fromUnderlying (v: RealAccrualVariable) =
            Real v

        static member fromUnderlying (v: StringAccrualVariable) =
            String v

        static member fromUnderlying (v: DateAccrualVariable) =
            Date v

        static member fromUnderlying (v: IntegerEnumerationAccrualVariable) =
            IntegerEnumeration v

        static member fromUnderlying (v: StringEnumerationAccrualVariable) =
            StringEnumeration v

        member this.UnderlyingVariable =
            match this with
            | Boolean (ToAccrualVariable v)
            | Integer (ToAccrualVariable v)
            | Real (ToAccrualVariable v)
            | String (ToAccrualVariable v)
            | Date (ToAccrualVariable v)
            | IntegerEnumeration (ToAccrualVariable v)
            | StringEnumeration (ToAccrualVariable v)
                -> v         

    [<RequireQualifiedAccess>]
    type RollbackVariable =
        | Boolean of BooleanRollbackVariable
        | Integer of IntegerRollbackVariable
        | Real of RealRollbackVariable
        | String of StringRollbackVariable
        | Date of DateRollbackVariable
        | IntegerEnumeration of IntegerEnumerationRollbackVariable
        | StringEnumeration of StringEnumerationRollbackVariable

        static member fromUnderlying (v: BooleanRollbackVariable) =
            Boolean v

        static member fromUnderlying (v: IntegerRollbackVariable) =
            Integer v

        static member fromUnderlying (v: RealRollbackVariable) =
            Real v

        static member fromUnderlying (v: StringRollbackVariable) =
            String v

        static member fromUnderlying (v: DateRollbackVariable) =
            Date v

        static member fromUnderlying (v: IntegerEnumerationRollbackVariable) =
            IntegerEnumeration v

        static member fromUnderlying (v: StringEnumerationRollbackVariable) =
            StringEnumeration v

        member this.UnderlyingVariable =
            match this with
            | Boolean (ToRollbackVariable v)
            | Integer (ToRollbackVariable v)
            | Real (ToRollbackVariable v)
            | String (ToRollbackVariable v)
            | Date (ToRollbackVariable v)
            | IntegerEnumeration (ToRollbackVariable v)
            | StringEnumeration (ToRollbackVariable v)
                -> v

    [<RequireQualifiedAccess>]
    type SingletonVariable =
        | Boolean of BooleanSingletonVariable
        | Integer of IntegerSingletonVariable
        | Real of RealSingletonVariable
        | String of StringSingletonVariable
        | Date of DateSingletonVariable
        | IntegerEnumeration of IntegerEnumerationSingletonVariable
        | StringEnumeration of StringEnumerationSingletonVariable

        static member fromUnderlying (v: BooleanSingletonVariable) =
            Boolean v

        static member fromUnderlying (v: IntegerSingletonVariable) =
            Integer v

        static member fromUnderlying (v: RealSingletonVariable) =
            Real v

        static member fromUnderlying (v: StringSingletonVariable) =
            String v

        static member fromUnderlying (v: DateSingletonVariable) =
            Date v

        static member fromUnderlying (v: IntegerEnumerationSingletonVariable) =
            IntegerEnumeration v

        static member fromUnderlying (v: StringEnumerationSingletonVariable) =
            StringEnumeration v

        member this.UnderlyingVariable =
            match this with
            | Boolean (ToSingletonVariable v)
            | Integer (ToSingletonVariable v)
            | Real (ToSingletonVariable v)
            | String (ToSingletonVariable v)
            | Date (ToSingletonVariable v)
            | IntegerEnumeration (ToSingletonVariable v)
            | StringEnumeration (ToSingletonVariable v)
                -> v           

    [<RequireQualifiedAccess>]
    type InputVariable =
        | Boolean of BooleanInputVariable
        | Integer of IntegerInputVariable
        | Real of RealInputVariable
        | String of StringInputVariable
        | Date of DateInputVariable
        | IntegerEnumeration of IntegerEnumerationInputVariable
        | StringEnumeration of StringEnumerationInputVariable

        static member fromUnderlying (v: BooleanInputVariable) =
            Boolean v

        static member fromUnderlying (v: IntegerInputVariable) =
            Integer v

        static member fromUnderlying (v: RealInputVariable) =
            Real v

        static member fromUnderlying (v: StringInputVariable) =
            String v

        static member fromUnderlying (v: DateInputVariable) =
            Date v

        static member fromUnderlying (v: IntegerEnumerationInputVariable) =
            IntegerEnumeration v

        static member fromUnderlying (v: StringEnumerationInputVariable) =
            StringEnumeration v

        member this.UnderlyingVariable =
            match this with
            | Boolean (ToInputVariable v)
            | Integer (ToInputVariable v)
            | Real (ToInputVariable v)
            | String (ToInputVariable v)
            | Date (ToInputVariable v)
            | IntegerEnumeration (ToInputVariable v)
            | StringEnumeration (ToInputVariable v)
                -> v  


    [<RequireQualifiedAccess>]
    type BooleanVariable =
        | Accrual of BooleanAccrualVariable
        | Rollback of BooleanRollbackVariable
        | Singleton of BooleanSingletonVariable
        | Input of BooleanInputVariable

        member this.UnderlyingVariable =
            match this with
            | Accrual (ToBooleanVariable v)
            | Rollback (ToBooleanVariable v)
            | Singleton (ToBooleanVariable v)
            | Input (ToBooleanVariable v)
                -> v

    [<RequireQualifiedAccess>]
    type IntegerVariable =
        | Accrual of IntegerAccrualVariable
        | Rollback of IntegerRollbackVariable
        | Singleton of IntegerSingletonVariable
        | Input of IntegerInputVariable

        member this.UnderlyingVariable =
            match this with
            | Accrual (ToIntegerVariable v)
            | Rollback (ToIntegerVariable v)
            | Singleton (ToIntegerVariable v)
            | Input (ToIntegerVariable v)
                -> v

    [<RequireQualifiedAccess>]
    type RealVariable =
        | Accrual of RealAccrualVariable
        | Rollback of RealRollbackVariable
        | Singleton of RealSingletonVariable
        | Input of RealInputVariable

        member this.UnderlyingVariable =
            match this with
            | Accrual (ToRealVariable v)
            | Rollback (ToRealVariable v)
            | Singleton (ToRealVariable v)
            | Input (ToRealVariable v)
                -> v

    [<RequireQualifiedAccess>]
    type StringVariable =
        | Accrual of StringAccrualVariable
        | Rollback of StringRollbackVariable
        | Singleton of StringSingletonVariable
        | Input of StringInputVariable

        member this.UnderlyingVariable =
            match this with
            | Accrual (ToStringVariable v)
            | Rollback (ToStringVariable v)
            | Singleton (ToStringVariable v)
            | Input (ToStringVariable v)
                -> v

    [<RequireQualifiedAccess>]
    type DateVariable =
        | Accrual of DateAccrualVariable
        | Rollback of DateRollbackVariable
        | Singleton of DateSingletonVariable
        | Input of DateInputVariable

        member this.UnderlyingVariable =
            match this with
            | Accrual (ToDateVariable v)
            | Rollback (ToDateVariable v)
            | Singleton (ToDateVariable v)
            | Input (ToDateVariable v)
                -> v

    [<RequireQualifiedAccess>]
    type IntegerEnumerationVariable =
        | Accrual of IntegerEnumerationAccrualVariable
        | Rollback of IntegerEnumerationRollbackVariable
        | Singleton of IntegerEnumerationSingletonVariable
        | Input of IntegerEnumerationInputVariable

        member this.UnderlyingVariable =
            match this with
            | Accrual (ToIntegerEnumerationVariable v)
            | Rollback (ToIntegerEnumerationVariable v)
            | Singleton (ToIntegerEnumerationVariable v)
            | Input (ToIntegerEnumerationVariable v)
                -> v

    [<RequireQualifiedAccess>]
    type StringEnumerationVariable =
        | Accrual of StringEnumerationAccrualVariable
        | Rollback of StringEnumerationRollbackVariable
        | Singleton of StringEnumerationSingletonVariable
        | Input of StringEnumerationInputVariable

        member this.UnderlyingVariable =
            match this with
            | Accrual (ToStringEnumerationVariable v)
            | Rollback (ToStringEnumerationVariable v)
            | Singleton (ToStringEnumerationVariable v)
            | Input (ToStringEnumerationVariable v)
                -> v


    [<RequireQualifiedAccess>]
    type VariableByMode =
        | Accrual of AccrualVariable
        | Rollback of RollbackVariable
        | Singleton of SingletonVariable
        | Input of InputVariable

        // Used as part of the dependency logic within the common implementation block.
        static member fromUnderlying (v: AccrualVariable) =
            Accrual v

        static member fromUnderlying (v: RollbackVariable) =
            Rollback v

        static member fromUnderlying (v: SingletonVariable) =
            Singleton v

        static member fromUnderlying (v: InputVariable) =
            Input v

        member this.UnderlyingVariable =
            match this with
            | Accrual v -> v.UnderlyingVariable :> IVariable
            | Rollback v -> v.UnderlyingVariable :> IVariable
            | Singleton v -> v.UnderlyingVariable :> IVariable
            | Input v -> v.UnderlyingVariable :> IVariable


    [<RequireQualifiedAccess>]
    type Variable =
        | BooleanAccrual of BooleanAccrualVariable
        | IntegerAccrual of IntegerAccrualVariable
        | RealAccrual of RealAccrualVariable
        | StringAccrual of StringAccrualVariable
        | DateAccrual of DateAccrualVariable
        | IntegerEnumerationAccrual of IntegerEnumerationAccrualVariable
        | StringEnumerationAccrual of StringEnumerationAccrualVariable

        | BooleanRollback of BooleanRollbackVariable
        | IntegerRollback of IntegerRollbackVariable
        | RealRollback of RealRollbackVariable
        | StringRollback of StringRollbackVariable
        | DateRollback of DateRollbackVariable
        | IntegerEnumerationRollback of IntegerEnumerationRollbackVariable
        | StringEnumerationRollback of StringEnumerationRollbackVariable

        | BooleanSingleton of BooleanSingletonVariable
        | IntegerSingleton of IntegerSingletonVariable
        | RealSingleton of RealSingletonVariable
        | StringSingleton of StringSingletonVariable
        | DateSingleton of DateSingletonVariable
        | IntegerEnumerationSingleton of IntegerEnumerationSingletonVariable
        | StringEnumerationSingleton of StringEnumerationSingletonVariable

        | BooleanInput of BooleanInputVariable
        | IntegerInput of IntegerInputVariable
        | RealInput of RealInputVariable
        | StringInput of StringInputVariable
        | DateInput of DateInputVariable
        | IntegerEnumerationInput of IntegerEnumerationInputVariable
        | StringEnumerationInput of StringEnumerationInputVariable

        member this.UnderlyingVariable =
            match this with
            | BooleanAccrual (ToVariable v)
            | IntegerAccrual (ToVariable v)
            | RealAccrual (ToVariable v)
            | StringAccrual (ToVariable v)
            | DateAccrual (ToVariable v)
            | IntegerEnumerationAccrual (ToVariable v)
            | StringEnumerationAccrual (ToVariable v)
            | BooleanRollback (ToVariable v)
            | IntegerRollback (ToVariable v)
            | RealRollback (ToVariable v)
            | StringRollback (ToVariable v)
            | DateRollback (ToVariable v)
            | IntegerEnumerationRollback (ToVariable v)
            | StringEnumerationRollback (ToVariable v)
            | BooleanSingleton (ToVariable v)
            | IntegerSingleton (ToVariable v)
            | RealSingleton (ToVariable v)
            | StringSingleton (ToVariable v)
            | DateSingleton (ToVariable v)
            | IntegerEnumerationSingleton (ToVariable v)
            | StringEnumerationSingleton (ToVariable v)
            | BooleanInput (ToVariable v)
            | IntegerInput (ToVariable v)
            | RealInput (ToVariable v)
            | StringInput (ToVariable v)
            | DateInput (ToVariable v)
            | IntegerEnumerationInput (ToVariable v)
            | StringEnumerationInput (ToVariable v)
                -> v

        static member fromUnderlying (v: InputVariable) =
            match v with
            | InputVariable.Boolean v' -> BooleanInput v'
            | InputVariable.Integer v' -> IntegerInput v'
            | InputVariable.Real v' -> RealInput v'
            | InputVariable.String v' -> StringInput v'
            | InputVariable.Date v' -> DateInput v'
            | InputVariable.IntegerEnumeration v' -> IntegerEnumerationInput v'
            | InputVariable.StringEnumeration v' -> StringEnumerationInput v'
