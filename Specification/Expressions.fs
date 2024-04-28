
#nowarn "1189"

namespace Specification

module rec Expressions =

    open Variables


    type IExpression =
        abstract Type: VariableType with get
        abstract Mode: VariableMode with get

    type IBooleanExpression =
        inherit IExpression

    type IIntegerExpression =
        inherit IExpression

    type IRealExpression =
        inherit IExpression

    type IStringExpression =
        inherit IExpression

    type IDateExpression =
        inherit IExpression

    type IIntegerEnumerationExpression =
        inherit IExpression

        abstract Definition: Enumeration.Integer with get

    type IStringEnumerationExpression =
        inherit IExpression

        abstract Definition: Enumeration.String with get

    type IAccrualExpression =
        inherit IExpression

    type IRollbackExpression =
        inherit IExpression

    type ISingletonExpression =
        inherit IExpression

    type IInputExpression =
        inherit IExpression


    let inline private (|ToExpression|)<'T when 'T :> IExpression> (v: 'T) =
        v :> IExpression


    let inline private (|ToAccrualExpression|)<'T when 'T :> IAccrualExpression> (v: 'T) =
        v :> IAccrualExpression

    let inline private (|ToRollbackExpression|)<'T when 'T :> IRollbackExpression> (v: 'T) =
        v :> IRollbackExpression

    let inline private (|ToSingletonExpression|)<'T when 'T :> ISingletonExpression> (v: 'T) =
        v :> ISingletonExpression


    [<RequireQualifiedAccessAttribute>]
    type RelativeOffset =
        | Previous
        | Current
        | Next

    [<RequireQualifiedAccess>]
    type NonFutureRelativeOffset =
        | Previous
        | Current

    [<RequireQualifiedAccess>]
    type NonPastRelativeOffset =
        | Next
        | Current


    [<RequireQualifiedAccess; NoComparison>]
    type UnaryArithmeticOp =
        | Abs

    [<RequireQualifiedAccess; NoComparison>]
    type BinaryArithmeticOp =
        | Add
        | Subtract
        | Multiply
        | Divide
        | Modulus

    [<RequireQualifiedAccess; NoComparison>]
    type BinaryComparableOp =
        | LessThan
        | LessThanOrEqual
        | GreaterThanOrEqual
        | GreaterThan

    [<RequireQualifiedAccess; NoComparison>]
    type BinaryEquatableOp =
        | Equal
        | NotEqual

    [<RequireQualifiedAccess; NoComparison>]
    type PeriodLength =
        | Days
        | Months
        | Years


    // A date constant of '0' corresponds to the end of the following year.
    let YearOrigin = 1899


    [<RequireQualifiedAccess; NoComparison>]
    type ComparableAccrualExpressionPair =
        | Integer of IntegerAccrualExpression * IntegerAccrualExpression
        | Real of RealAccrualExpression * RealAccrualExpression
        | Date of DateAccrualExpression * DateAccrualExpression

    [<RequireQualifiedAccess; NoComparison>]
    type EquatableAccrualExpressionPair =
        | Boolean of BooleanAccrualExpression * BooleanAccrualExpression
        | Integer of IntegerAccrualExpression * IntegerAccrualExpression
        | Real of RealAccrualExpression * RealAccrualExpression
        | Date of DateAccrualExpression * DateAccrualExpression
        | String of StringAccrualExpression * StringAccrualExpression
        | IntegerEnumeration of IntegerEnumerationAccrualExpression_ * IntegerEnumerationAccrualExpression_
        | StringEnumeration of StringEnumerationAccrualExpression_ * StringEnumerationAccrualExpression_

    [<RequireQualifiedAccess; NoComparison>]
    type BooleanAccrualExpression =
        | Constant of bool
        | AccrualVariable of BooleanAccrualVariable * NonFutureRelativeOffset
        | RollbackVariable of BooleanRollbackVariable * RelativeOffset
        | SingletonVariable of BooleanSingletonVariable
        | InputVariable of BooleanInputVariable
        | BinaryComparable of BinaryComparableOp * ComparableAccrualExpressionPair
        | BinaryEquatable of BinaryEquatableOp * EquatableAccrualExpressionPair
        | And of BooleanAccrualExpression list
        | Or of BooleanAccrualExpression list
        | Not of BooleanAccrualExpression
        | Conditional of BooleanAccrualExpression * True: BooleanAccrualExpression * False: BooleanAccrualExpression

        interface IExpression with
            member _.Type = VariableType.Boolean
            member _.Mode = VariableMode.Accrual

        interface IBooleanExpression
        interface IAccrualExpression

    [<RequireQualifiedAccess; NoComparison>]
    type IntegerAccrualExpression =
        | Constant of int
        | AccrualVariable of IntegerAccrualVariable * NonFutureRelativeOffset
        | RollbackVariable of IntegerRollbackVariable * RelativeOffset
        | SingletonVariable of IntegerSingletonVariable
        | InputVariable of IntegerInputVariable
        | Conditional of BooleanAccrualExpression * True: IntegerAccrualExpression * False: IntegerAccrualExpression
        | BinaryArithmeticOp of BinaryArithmeticOp * IntegerAccrualExpression * IntegerAccrualExpression
        | UnaryArithmeticOp of UnaryArithmeticOp * IntegerAccrualExpression
        | FromReal of RealAccrualExpression
        | StringLength of StringAccrualExpression

        interface IExpression with
            member _.Type = VariableType.Integer
            member _.Mode = VariableMode.Accrual

        interface IIntegerExpression
        interface IAccrualExpression

    [<RequireQualifiedAccess; NoComparison>]
    type RealAccrualExpression =
        | Constant of double
        | AccrualVariable of RealAccrualVariable * NonFutureRelativeOffset
        | RollbackVariable of RealRollbackVariable * RelativeOffset
        | SingletonVariable of RealSingletonVariable
        | InputVariable of RealInputVariable
        | Conditional of BooleanAccrualExpression * True: RealAccrualExpression * False: RealAccrualExpression
        | BinaryArithmeticOp of BinaryArithmeticOp * RealAccrualExpression * RealAccrualExpression
        | UnaryArithmeticOp of UnaryArithmeticOp * RealAccrualExpression
        | FromInteger of IntegerAccrualExpression

        interface IExpression with
            member _.Type = VariableType.Real
            member _.Mode = VariableMode.Accrual

        interface IRealExpression
        interface IAccrualExpression

    [<RequireQualifiedAccess; NoComparison>]
    type StringAccrualExpression =
        | Constant of string
        | AccrualVariable of StringAccrualVariable * NonFutureRelativeOffset
        | RollbackVariable of StringRollbackVariable * RelativeOffset
        | SingletonVariable of StringSingletonVariable
        | InputVariable of StringInputVariable
        | Conditional of BooleanAccrualExpression * True: StringAccrualExpression * False: StringAccrualExpression

        interface IExpression with
            member _.Type = VariableType.String
            member _.Mode = VariableMode.Accrual

        interface IStringExpression
        interface IAccrualExpression

    [<RequireQualifiedAccess; NoComparison>]
    type DateAccrualExpression =
        | Constant of int
        | AccrualVariable of DateAccrualVariable * NonFutureRelativeOffset
        | RollbackVariable of DateRollbackVariable * RelativeOffset
        | SingletonVariable of DateSingletonVariable
        | InputVariable of DateInputVariable
        | Conditional of BooleanAccrualExpression * True: DateAccrualExpression * False: DateAccrualExpression
        | Offset of Origin: DateAccrualExpression * Amount: IntegerAccrualExpression * Period: PeriodLength
        | DiffDays of From: DateAccrualExpression * To: DateAccrualExpression

        interface IExpression with
            member _.Type = VariableType.Date
            member _.Mode = VariableMode.Accrual

        interface IDateExpression
        interface IAccrualExpression

    [<RequireQualifiedAccess; NoComparison>]
    type IntegerEnumerationAccrualExpressionWithoutDefinition =
        | Constant of Level: string * Definition: Enumeration.Integer
        | AccrualVariable of IntegerEnumerationAccrualVariable * NonFutureRelativeOffset
        | RollbackVariable of IntegerEnumerationRollbackVariable * RelativeOffset
        | SingletonVariable of IntegerEnumerationSingletonVariable
        | InputVariable of IntegerEnumerationInputVariable
        | Conditional of BooleanAccrualExpression * True: IntegerEnumerationAccrualExpression_ * False: IntegerEnumerationAccrualExpression_

    let (|IntegerEnumerationAccrualExpression|) = function
        | IntegerEnumerationAccrualExpression_ (expr, d) -> (expr, d)            

    [<NoComparison>]
    type IntegerEnumerationAccrualExpression_ =
        private | IntegerEnumerationAccrualExpression_ of
            Expression: IntegerEnumerationAccrualExpressionWithoutDefinition
                * Definition: Enumeration.Integer

        member private this.Definition_ =
            match this with (IntegerEnumerationAccrualExpression_ (_, d)) -> d

        interface IExpression with
            member this.Type = VariableType.IntegerEnumeration this.Definition_
            member _.Mode = VariableMode.Accrual

        interface IIntegerEnumerationExpression with
            member this.Definition = this.Definition_

        interface IAccrualExpression

        // Rather than use a result type, there is no reason why the developer cannot
        // ensure that we do NOT mix enumeration definitions. Hence, an exception will
        // occcur if one is not careful.
        static member createFrom (expr: IntegerEnumerationAccrualExpressionWithoutDefinition) =
            match expr with
            | IntegerEnumerationAccrualExpressionWithoutDefinition.Constant (_, d)
            | IntegerEnumerationAccrualExpressionWithoutDefinition.AccrualVariable ({ Definition = d}, _)
            | IntegerEnumerationAccrualExpressionWithoutDefinition.RollbackVariable ({ Definition = d }, _)
            | IntegerEnumerationAccrualExpressionWithoutDefinition.SingletonVariable { Definition = d }
            | IntegerEnumerationAccrualExpressionWithoutDefinition.InputVariable { Definition = d } ->
                IntegerEnumerationAccrualExpression_ (expr, d)
            | IntegerEnumerationAccrualExpressionWithoutDefinition.Conditional (
                    _, IntegerEnumerationAccrualExpression_ (_, d1), IntegerEnumerationAccrualExpression_ (_, d2)) ->
                if d1 = d2 then
                    IntegerEnumerationAccrualExpression_ (expr, d1)
                else
                    failwith $"Different integer enumeration definitions used: {d1} and {d2}."

    [<RequireQualifiedAccess; NoComparison>]
    type StringEnumerationAccrualExpressionWithoutDefinition =
        | Constant of Level: string * Definition: Enumeration.String
        | AccrualVariable of StringEnumerationAccrualVariable * NonFutureRelativeOffset
        | RollbackVariable of StringEnumerationRollbackVariable * RelativeOffset
        | SingletonVariable of StringEnumerationSingletonVariable
        | InputVariable of StringEnumerationInputVariable
        | Conditional of BooleanAccrualExpression * True: StringEnumerationAccrualExpression_ * False: StringEnumerationAccrualExpression_

    let (|StringEnumerationAccrualExpression|) = function
        | StringEnumerationAccrualExpression_ (expr, d) -> (expr, d)

    [<NoComparison>]
    type StringEnumerationAccrualExpression_ =
        private | StringEnumerationAccrualExpression_ of
            Expression: StringEnumerationAccrualExpressionWithoutDefinition
                * Definition: Enumeration.String

        member private this.Definition_ =
            match this with (StringEnumerationAccrualExpression_ (_, d)) -> d

        interface IExpression with
            member this.Type = VariableType.StringEnumeration this.Definition_
            member _.Mode = VariableMode.Accrual

        interface IStringEnumerationExpression with
            member this.Definition = this.Definition_

        interface IAccrualExpression

        static member createFrom (expr: StringEnumerationAccrualExpressionWithoutDefinition) =
            match expr with
            | StringEnumerationAccrualExpressionWithoutDefinition.Constant (_, d)
            | StringEnumerationAccrualExpressionWithoutDefinition.AccrualVariable ({ Definition = d}, _)
            | StringEnumerationAccrualExpressionWithoutDefinition.RollbackVariable ({ Definition = d }, _)
            | StringEnumerationAccrualExpressionWithoutDefinition.SingletonVariable { Definition = d }
            | StringEnumerationAccrualExpressionWithoutDefinition.InputVariable { Definition = d } ->
                StringEnumerationAccrualExpression_ (expr, d)
            | StringEnumerationAccrualExpressionWithoutDefinition.Conditional (
                    _, StringEnumerationAccrualExpression_ (_, d1), StringEnumerationAccrualExpression_ (_, d2)) ->
                if d1 = d2 then
                    StringEnumerationAccrualExpression_ (expr, d1)
                else
                    failwith $"Different string enumeration definitions used: {d1} and {d2}."


    [<RequireQualifiedAccess; NoComparison>]
    type ComparableRollbackExpressionPair =
        | Integer of IntegerRollbackExpression * IntegerRollbackExpression
        | Real of RealRollbackExpression * RealRollbackExpression
        | Date of DateRollbackExpression * DateRollbackExpression

    [<RequireQualifiedAccess; NoComparison>]
    type EquatableRollbackExpressionPair =
        | Boolean of BooleanRollbackExpression * BooleanRollbackExpression
        | Integer of IntegerRollbackExpression * IntegerRollbackExpression
        | Real of RealRollbackExpression * RealRollbackExpression
        | Date of DateRollbackExpression * DateRollbackExpression
        | String of StringRollbackExpression * StringRollbackExpression
        | IntegerEnumeration of IntegerEnumerationRollbackExpression_ * IntegerEnumerationRollbackExpression_
        | StringEnumeration of StringEnumerationRollbackExpression_ * StringEnumerationRollbackExpression_

    [<RequireQualifiedAccess; NoComparison>]
    type BooleanRollbackExpression =
        | Constant of bool
        | AccrualVariable of BooleanAccrualVariable * RelativeOffset
        | RollbackVariable of BooleanRollbackVariable * NonPastRelativeOffset
        | SingletonVariable of BooleanSingletonVariable
        | InputVariable of BooleanInputVariable
        | BinaryComparable of BinaryComparableOp * ComparableRollbackExpressionPair
        | BinaryEquatable of BinaryEquatableOp * EquatableRollbackExpressionPair
        | And of BooleanRollbackExpression list
        | Or of BooleanRollbackExpression list
        | Not of BooleanRollbackExpression
        | Conditional of BooleanRollbackExpression * True: BooleanRollbackExpression * False: BooleanRollbackExpression

        interface IExpression with
            member _.Type = VariableType.Boolean
            member _.Mode = VariableMode.Rollback

        interface IBooleanExpression
        interface IRollbackExpression

    [<RequireQualifiedAccess; NoComparison>]
    type IntegerRollbackExpression =
        | Constant of int
        | AccrualVariable of IntegerAccrualVariable * RelativeOffset
        | RollbackVariable of IntegerRollbackVariable * NonPastRelativeOffset
        | SingletonVariable of IntegerSingletonVariable
        | InputVariable of IntegerInputVariable
        | Conditional of BooleanRollbackExpression * True: IntegerRollbackExpression * False: IntegerRollbackExpression
        | BinaryArithmeticOp of BinaryArithmeticOp * IntegerRollbackExpression * IntegerRollbackExpression
        | UnaryArithmeticOp of UnaryArithmeticOp * IntegerRollbackExpression
        | FromReal of RealRollbackExpression
        | StringLength of StringRollbackExpression

        interface IExpression with
            member _.Type = VariableType.Integer
            member _.Mode = VariableMode.Rollback

        interface IIntegerExpression
        interface IRollbackExpression

    [<RequireQualifiedAccess; NoComparison>]
    type RealRollbackExpression =
        | Constant of double
        | AccrualVariable of RealAccrualVariable * RelativeOffset
        | RollbackVariable of RealRollbackVariable * NonPastRelativeOffset
        | SingletonVariable of RealSingletonVariable
        | InputVariable of RealInputVariable
        | Conditional of BooleanRollbackExpression * True: RealRollbackExpression * False: RealRollbackExpression
        | BinaryArithmeticOp of BinaryArithmeticOp * RealRollbackExpression * RealRollbackExpression
        | UnaryArithmeticOp of UnaryArithmeticOp * RealRollbackExpression
        | FromInteger of IntegerRollbackExpression

        interface IExpression with
            member _.Type = VariableType.Real
            member _.Mode = VariableMode.Rollback

        interface IRealExpression
        interface IRollbackExpression

    [<RequireQualifiedAccess; NoComparison>]
    type StringRollbackExpression =
        | Constant of string
        | AccrualVariable of StringAccrualVariable * RelativeOffset
        | RollbackVariable of StringRollbackVariable * NonPastRelativeOffset
        | SingletonVariable of StringSingletonVariable
        | InputVariable of StringInputVariable
        | Conditional of BooleanRollbackExpression * True: StringRollbackExpression * False: StringRollbackExpression

        interface IExpression with
            member _.Type = VariableType.String
            member _.Mode = VariableMode.Rollback

        interface IStringExpression
        interface IRollbackExpression

    [<RequireQualifiedAccess; NoComparison>]
    type DateRollbackExpression =
        | Constant of int
        | AccrualVariable of DateAccrualVariable * RelativeOffset
        | RollbackVariable of DateRollbackVariable * NonPastRelativeOffset
        | SingletonVariable of DateSingletonVariable
        | InputVariable of DateInputVariable
        | Conditional of BooleanRollbackExpression * True: DateRollbackExpression * False: DateRollbackExpression
        | Offset of Origin: DateRollbackExpression * Amount: IntegerRollbackExpression * Period: PeriodLength
        | DiffDays of From: DateRollbackExpression * To: DateRollbackExpression

        interface IExpression with
            member _.Type = VariableType.Date
            member _.Mode = VariableMode.Rollback

        interface IDateExpression
        interface IRollbackExpression
    
    [<RequireQualifiedAccess; NoComparison>]
    type IntegerEnumerationRollbackExpressionWithoutDefinition =
        | Constant of Level: string * Definition: Enumeration.Integer
        | AccrualVariable of IntegerEnumerationAccrualVariable * RelativeOffset
        | RollbackVariable of IntegerEnumerationRollbackVariable * NonPastRelativeOffset
        | SingletonVariable of IntegerEnumerationSingletonVariable
        | InputVariable of IntegerEnumerationInputVariable
        | Conditional of BooleanRollbackExpression * True: IntegerEnumerationRollbackExpression_ * False: IntegerEnumerationRollbackExpression_

    let (|IntegerEnumerationRollbackExpression|) = function
        | IntegerEnumerationRollbackExpression_ (expr, d) -> (expr, d)

    [<NoComparison>]
    type IntegerEnumerationRollbackExpression_ =
        private | IntegerEnumerationRollbackExpression_ of
            Expression: IntegerEnumerationRollbackExpressionWithoutDefinition
                * Definition: Enumeration.Integer

        member private this.Definition_ =
            match this with (IntegerEnumerationRollbackExpression_ (_, d)) -> d

        interface IExpression with
            member this.Type = VariableType.IntegerEnumeration this.Definition_
            member _.Mode = VariableMode.Rollback

        interface IIntegerEnumerationExpression with
            member this.Definition = this.Definition_

        interface IRollbackExpression

        static member createFrom (expr: IntegerEnumerationRollbackExpressionWithoutDefinition) =
            match expr with
            | IntegerEnumerationRollbackExpressionWithoutDefinition.Constant (_, d)
            | IntegerEnumerationRollbackExpressionWithoutDefinition.AccrualVariable ({ Definition = d}, _)
            | IntegerEnumerationRollbackExpressionWithoutDefinition.RollbackVariable ({ Definition = d }, _)
            | IntegerEnumerationRollbackExpressionWithoutDefinition.SingletonVariable { Definition = d }
            | IntegerEnumerationRollbackExpressionWithoutDefinition.InputVariable { Definition = d } ->
                IntegerEnumerationRollbackExpression_ (expr, d)
            | IntegerEnumerationRollbackExpressionWithoutDefinition.Conditional (
                    _, IntegerEnumerationRollbackExpression_ (_, d1), IntegerEnumerationRollbackExpression_ (_, d2)) ->
                if d1 = d2 then
                    IntegerEnumerationRollbackExpression_ (expr, d1)
                else
                    failwith $"Different integer enumeration definitions used: {d1} and {d2}."

    [<RequireQualifiedAccess; NoComparison>]
    type StringEnumerationRollbackExpressionWithoutDefinition =
        | Constant of Level: string * Definition: Enumeration.String
        | AccrualVariable of StringEnumerationAccrualVariable * RelativeOffset
        | RollbackVariable of StringEnumerationRollbackVariable * NonPastRelativeOffset
        | SingletonVariable of StringEnumerationSingletonVariable
        | InputVariable of StringEnumerationInputVariable
        | Conditional of BooleanRollbackExpression * True: StringEnumerationRollbackExpression_ * False: StringEnumerationRollbackExpression_

    let (|StringEnumerationRollbackExpression|) = function
        | StringEnumerationRollbackExpression_ (expr, d) -> (expr, d)

    [<NoComparison>]
    type StringEnumerationRollbackExpression_ =
        private | StringEnumerationRollbackExpression_ of
            Expression: StringEnumerationRollbackExpressionWithoutDefinition
                * Definition: Enumeration.String

        member private this.Definition_ =
            match this with (StringEnumerationRollbackExpression_ (_, d)) -> d

        interface IExpression with
            member this.Type = VariableType.StringEnumeration this.Definition_
            member _.Mode = VariableMode.Rollback

        interface IStringEnumerationExpression with
            member this.Definition = this.Definition_

        interface IRollbackExpression

        static member createFrom (expr: StringEnumerationRollbackExpressionWithoutDefinition) =
            match expr with
            | StringEnumerationRollbackExpressionWithoutDefinition.Constant (_, d)
            | StringEnumerationRollbackExpressionWithoutDefinition.AccrualVariable ({ Definition = d}, _)
            | StringEnumerationRollbackExpressionWithoutDefinition.RollbackVariable ({ Definition = d }, _)
            | StringEnumerationRollbackExpressionWithoutDefinition.SingletonVariable { Definition = d }
            | StringEnumerationRollbackExpressionWithoutDefinition.InputVariable { Definition = d } ->
                StringEnumerationRollbackExpression_ (expr, d)
            | StringEnumerationRollbackExpressionWithoutDefinition.Conditional (
                    _, StringEnumerationRollbackExpression_ (_, d1), StringEnumerationRollbackExpression_ (_, d2)) ->
                if d1 = d2 then
                    StringEnumerationRollbackExpression_ (expr, d1)
                else
                    failwith $"Different integer enumeration definitions used: {d1} and {d2}."

    [<RequireQualifiedAccess>]
    type AggregationType =
        | Min
        | Max

    [<RequireQualifiedAccess; NoComparison>]
    type ComparableSingletonExpressionPair =
        | Integer of IntegerSingletonExpression * IntegerSingletonExpression
        | Real of RealSingletonExpression * RealSingletonExpression
        | Date of DateSingletonExpression * DateSingletonExpression

    // Unlike numerics, we cannot mix and match non-numeric types.
    [<RequireQualifiedAccess; NoComparison>]
    type EquatableSingletonExpressionPair =
        | Boolean of BooleanSingletonExpression * BooleanSingletonExpression
        | Integer of IntegerSingletonExpression * IntegerSingletonExpression
        | Real of RealSingletonExpression * RealSingletonExpression
        | Date of DateSingletonExpression * DateSingletonExpression
        | String of StringSingletonExpression * StringSingletonExpression
        | IntegerEnumeration of IntegerEnumerationSingletonExpression_ * IntegerEnumerationSingletonExpression_
        | StringEnumeration of StringEnumerationSingletonExpression_ * StringEnumerationSingletonExpression_

    [<RequireQualifiedAccess; NoComparison>]
    type BooleanSingletonExpression =
        | Constant of bool
        | AccrualVariable of BooleanAccrualVariable * AggregationType
        | RollbackVariable of BooleanRollbackVariable * AggregationType
        | SingletonVariable of BooleanSingletonVariable
        | InputVariable of BooleanInputVariable
        | BinaryComparable of BinaryComparableOp * ComparableSingletonExpressionPair
        | BinaryEquatable of BinaryEquatableOp * EquatableSingletonExpressionPair
        | And of BooleanSingletonExpression list
        | Or of BooleanSingletonExpression list
        | Not of BooleanSingletonExpression
        | Conditional of BooleanSingletonExpression * True: BooleanSingletonExpression * False: BooleanSingletonExpression

        interface IExpression with
            member _.Type = VariableType.Boolean
            member _.Mode = VariableMode.Singleton

        interface IBooleanExpression
        interface ISingletonExpression

    [<RequireQualifiedAccess; NoComparison>]
    type IntegerSingletonExpression =
        | Constant of int
        | AccrualVariable of IntegerAccrualVariable * AggregationType
        | RollbackVariable of IntegerRollbackVariable * AggregationType
        | SingletonVariable of IntegerSingletonVariable
        | InputVariable of IntegerInputVariable
        | Conditional of BooleanSingletonExpression * IntegerSingletonExpression * False: IntegerSingletonExpression
        | BinaryArithmeticOp of BinaryArithmeticOp * IntegerSingletonExpression * IntegerSingletonExpression
        | UnaryArithmeticOp of UnaryArithmeticOp * IntegerSingletonExpression
        | FromReal of RealSingletonExpression
        | StringLength of StringSingletonExpression

        interface IExpression with
            member _.Type = VariableType.Integer
            member _.Mode = VariableMode.Singleton

        interface IIntegerExpression
        interface ISingletonExpression

    [<RequireQualifiedAccess; NoComparison>]
    type RealSingletonExpression =
        | Constant of double
        | AccrualVariable of RealAccrualVariable * AggregationType
        | RollbackVariable of RealRollbackVariable * AggregationType
        | SingletonVariable of RealSingletonVariable
        | InputVariable of RealInputVariable
        | Conditional of BooleanSingletonExpression * True: RealSingletonExpression * False: RealSingletonExpression
        | BinaryArithmeticOp of BinaryArithmeticOp * RealSingletonExpression * RealSingletonExpression
        | UnaryArithmeticOp of UnaryArithmeticOp * RealSingletonExpression
        | FromInteger of IntegerSingletonExpression

        interface IExpression with
            member _.Type = VariableType.Real
            member _.Mode = VariableMode.Singleton

        interface IRealExpression
        interface ISingletonExpression

    [<RequireQualifiedAccess; NoComparison>]
    type StringSingletonExpression =
        | Constant of string
        | SingletonVariable of StringSingletonVariable
        | InputVariable of StringInputVariable
        | Conditional of BooleanSingletonExpression * True: StringSingletonExpression * False: StringSingletonExpression

        interface IExpression with
            member _.Type = VariableType.String
            member _.Mode = VariableMode.Singleton

        interface IStringExpression
        interface ISingletonExpression

    [<RequireQualifiedAccess; NoComparison>]
    type DateSingletonExpression =
        | Constant of string
        | AccrualVariable of DateAccrualVariable * AggregationType
        | RollbackVariable of DateRollbackVariable * AggregationType
        | SingletonVariable of DateSingletonVariable
        | InputVariable of DateInputVariable
        | Conditional of BooleanSingletonExpression * True: DateSingletonExpression * False: DateSingletonExpression
        | Offset of Origin: DateSingletonExpression * Amount: IntegerSingletonExpression * Period: PeriodLength
        | DiffDays of From: DateSingletonExpression * To: DateSingletonExpression

        interface IExpression with
            member _.Type = VariableType.Date
            member _.Mode = VariableMode.Singleton

        interface IDateExpression
        interface ISingletonExpression

    [<RequireQualifiedAccess; NoComparison>]
    type IntegerEnumerationSingletonExpressionWithoutDefinition =
        | Constant of Level: string * Definition: Enumeration.Integer
        | SingletonVariable of IntegerEnumerationSingletonVariable
        | InputVariable of IntegerEnumerationInputVariable
        | Conditional of BooleanSingletonExpression * True: IntegerEnumerationSingletonExpression_ * False: IntegerEnumerationSingletonExpression_

    let (|IntegerEnumerationSingletonExpression|) = function
        | IntegerEnumerationSingletonExpression_ (expr, d) -> (expr, d)

    [<NoComparison>]
    type IntegerEnumerationSingletonExpression_ =
        private | IntegerEnumerationSingletonExpression_ of
            Expression: IntegerEnumerationSingletonExpressionWithoutDefinition
                * Definition: Enumeration.Integer

        member private this.Definition_ =
            match this with (IntegerEnumerationSingletonExpression_ (_, d)) -> d

        interface IExpression with
            member this.Type = VariableType.IntegerEnumeration this.Definition_
            member _.Mode = VariableMode.Singleton

        interface IIntegerEnumerationExpression with
            member this.Definition = this.Definition_

        interface ISingletonExpression

        static member createFrom (expr: IntegerEnumerationSingletonExpressionWithoutDefinition) =
            match expr with
            | IntegerEnumerationSingletonExpressionWithoutDefinition.Constant (_, d)
            | IntegerEnumerationSingletonExpressionWithoutDefinition.SingletonVariable { Definition = d }
            | IntegerEnumerationSingletonExpressionWithoutDefinition.InputVariable { Definition = d } ->
                IntegerEnumerationSingletonExpression_ (expr, d)
            | IntegerEnumerationSingletonExpressionWithoutDefinition.Conditional (
                    _, IntegerEnumerationSingletonExpression_ (_, d1), IntegerEnumerationSingletonExpression_ (_, d2)) ->
                if d1 = d2 then
                    IntegerEnumerationSingletonExpression_ (expr, d1)
                else
                    failwith $"Different integer enumeration definitions used: {d1} and {d2}."

    [<RequireQualifiedAccess; NoComparison>]
    type StringEnumerationSingletonExpressionWithoutDefinition =
        | Constant of Level: string * Definition: Enumeration.String
        | SingletonVariable of StringEnumerationSingletonVariable
        | InputVariable of StringEnumerationInputVariable
        | Conditional of BooleanSingletonExpression * True: StringEnumerationSingletonExpression_ * False: StringEnumerationSingletonExpression_

    let (|StringEnumerationSingletonExpression|) = function
        | StringEnumerationSingletonExpression_ (expr, d) -> (expr, d)

    [<NoComparison>]
    type StringEnumerationSingletonExpression_ =
        private | StringEnumerationSingletonExpression_ of
            Expression: StringEnumerationSingletonExpressionWithoutDefinition
                * Definition: Enumeration.String

        member private this.Definition_ =
            match this with (StringEnumerationSingletonExpression_ (_, d)) -> d

        interface IExpression with
            member this.Type = VariableType.StringEnumeration this.Definition_
            member _.Mode = VariableMode.Singleton

        interface IStringEnumerationExpression with
            member this.Definition = this.Definition_

        interface ISingletonExpression

        static member createFrom (expr: StringEnumerationSingletonExpressionWithoutDefinition) =
            match expr with
            | StringEnumerationSingletonExpressionWithoutDefinition.Constant (_, d)
            | StringEnumerationSingletonExpressionWithoutDefinition.SingletonVariable { Definition = d }
            | StringEnumerationSingletonExpressionWithoutDefinition.InputVariable { Definition = d } ->
                StringEnumerationSingletonExpression_ (expr, d)
            | StringEnumerationSingletonExpressionWithoutDefinition.Conditional (
                    _, StringEnumerationSingletonExpression_ (_, d1), StringEnumerationSingletonExpression_ (_, d2)) ->
                if d1 = d2 then
                    StringEnumerationSingletonExpression_ (expr, d1)
                else
                    failwith $"Different string enumeration definitions used: {d1} and {d2}."


    [<RequireQualifiedAccess; NoComparison>]
    type AccrualExpression =
        | BooleanAccrual of BooleanAccrualExpression
        | IntegerAccrual of IntegerAccrualExpression
        | RealAccrual of RealAccrualExpression
        | StringAccrual of StringAccrualExpression
        | DateAccrual of DateAccrualExpression
        | IntegerEnumerationAccrual of IntegerEnumerationAccrualExpression_
        | StringEnumerationAccrual of StringEnumerationAccrualExpression_

        member this.UnderlyingExpression =
            match this with
            | BooleanAccrual (ToAccrualExpression e)
            | IntegerAccrual (ToAccrualExpression e)
            | RealAccrual (ToAccrualExpression e)
            | StringAccrual (ToAccrualExpression e)
            | DateAccrual (ToAccrualExpression e)
            | IntegerEnumerationAccrual (ToAccrualExpression e)
            | StringEnumerationAccrual (ToAccrualExpression e)
                -> e            

    [<RequireQualifiedAccess; NoComparison>]
    type RollbackExpression =
        | BooleanRollback of BooleanRollbackExpression
        | IntegerRollback of IntegerRollbackExpression
        | RealRollback of RealRollbackExpression
        | StringRollback of StringRollbackExpression
        | DateRollback of DateRollbackExpression
        | IntegerEnumerationRollback of IntegerEnumerationRollbackExpression_
        | StringEnumerationRollback of StringEnumerationRollbackExpression_

        member this.UnderlyingExpression =
            match this with
            | BooleanRollback (ToRollbackExpression e)
            | IntegerRollback (ToRollbackExpression e)
            | RealRollback (ToRollbackExpression e)
            | StringRollback (ToRollbackExpression e)
            | DateRollback (ToRollbackExpression e)
            | IntegerEnumerationRollback (ToRollbackExpression e)
            | StringEnumerationRollback (ToRollbackExpression e)
                -> e

    [<RequireQualifiedAccess; NoComparison>]
    type SingletonExpression =
        | BooleanSingleton of BooleanSingletonExpression
        | IntegerSingleton of IntegerSingletonExpression
        | RealSingleton of RealSingletonExpression
        | StringSingleton of StringSingletonExpression
        | DateSingleton of DateSingletonExpression
        | IntegerEnumerationSingleton of IntegerEnumerationSingletonExpression_
        | StringEnumerationSingleton of StringEnumerationSingletonExpression_

        member this.UnderlyingExpression =
            match this with
            | BooleanSingleton (ToSingletonExpression e)
            | IntegerSingleton (ToSingletonExpression e)
            | RealSingleton (ToSingletonExpression e)
            | StringSingleton (ToSingletonExpression e)
            | DateSingleton (ToSingletonExpression e)
            | IntegerEnumerationSingleton (ToSingletonExpression e)
            | StringEnumerationSingleton (ToSingletonExpression e)
                -> e

    [<RequireQualifiedAccess; NoComparison>]
    type Expression =
        | BooleanAccrual of BooleanAccrualExpression
        | IntegerAccrual of IntegerAccrualExpression
        | RealAccrual of RealAccrualExpression
        | StringAccrual of StringAccrualExpression
        | DateAccrual of DateAccrualExpression
        | IntegerEnumerationAccrual of IntegerEnumerationAccrualExpression_
        | StringEnumerationAccrual of StringEnumerationAccrualExpression_

        | BooleanRollback of BooleanRollbackExpression
        | IntegerRollback of IntegerRollbackExpression
        | RealRollback of RealRollbackExpression
        | StringRollback of StringRollbackExpression
        | DateRollback of DateRollbackExpression
        | IntegerEnumerationRollback of IntegerEnumerationRollbackExpression_
        | StringEnumerationRollback of StringEnumerationRollbackExpression_

        | BooleanSingleton of BooleanSingletonExpression
        | IntegerSingleton of IntegerSingletonExpression
        | RealSingleton of RealSingletonExpression
        | StringSingleton of StringSingletonExpression
        | DateSingleton of DateSingletonExpression
        | IntegerEnumerationSingleton of IntegerEnumerationSingletonExpression_
        | StringEnumerationSingleton of StringEnumerationSingletonExpression_

        member this.UnderlyingExpression =
            match this with
            | BooleanAccrual (ToExpression e)
            | IntegerAccrual (ToExpression e)
            | RealAccrual (ToExpression e)
            | StringAccrual (ToExpression e)
            | DateAccrual (ToExpression e)
            | IntegerEnumerationAccrual (ToExpression e)
            | StringEnumerationAccrual (ToExpression e)
            | BooleanRollback (ToExpression e)
            | IntegerRollback (ToExpression e)
            | RealRollback (ToExpression e)
            | StringRollback (ToExpression e)
            | DateRollback (ToExpression e)
            | IntegerEnumerationRollback (ToExpression e)
            | StringEnumerationRollback (ToExpression e)
            | BooleanSingleton (ToExpression e)
            | IntegerSingleton (ToExpression e)
            | RealSingleton (ToExpression e)
            | StringSingleton (ToExpression e)
            | DateSingleton (ToExpression e)
            | IntegerEnumerationSingleton (ToExpression e)
            | StringEnumerationSingleton (ToExpression e) ->
                e
