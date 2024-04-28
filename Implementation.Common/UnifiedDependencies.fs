
namespace Implementation.Common

(*
Here we convert the dependency relations as specified within the specification
into a more unified structure that is easier to work with. That is, we are
reducing a large number of distinct types into a much smaller set.
*)

module (*internal*) UnifiedDependencies =

    open FsToolkit.ErrorHandling
    open Specification
    open Specification.Variables
    open Specification.Expressions
    open Specification.Dependencies


    [<RequireQualifiedAccess>]
    type TimeOffset =
        | Previous
        | Current
        | Next
        | NonTimeDependent
        | Aggregate of Expressions.AggregationType

        static member fromUnderlying (rt: NonFutureRelativeOffset) =
            match rt with
            | NonFutureRelativeOffset.Previous -> TimeOffset.Previous
            | NonFutureRelativeOffset.Current -> TimeOffset.Current

        static member fromUnderlying (rt: NonPastRelativeOffset) =
            match rt with
            | NonPastRelativeOffset.Current -> TimeOffset.Current
            | NonPastRelativeOffset.Next -> TimeOffset.Next

        static member fromUnderlying (rt: RelativeOffset) =
            match rt with
            | RelativeOffset.Previous -> TimeOffset.Previous
            | RelativeOffset.Current -> TimeOffset.Current
            | RelativeOffset.Next -> TimeOffset.Next

    // We work with VariableByMode as we are more interested in the modality of a variable
    // rather than its underlying type.
    type UnifiedVariableDependencyPairings =
        Map<VariableByMode, Set<VariableByMode * TimeOffset>>

    module UnifiedVariableDependencyPairings =
        let createFrom: _ -> UnifiedVariableDependencyPairings =
            let typeMapper = function
                | VariableDependencyPairing.Accrual (d, ps) ->
                    let ps' =
                        ps
                        |> Set.map (function 
                            | AccrualVariableDependency.AccrualTarget (p, rt) ->
                                VariableByMode.fromUnderlying p, TimeOffset.fromUnderlying rt
                            | AccrualVariableDependency.RollbackTarget (p, rt) ->
                                VariableByMode.fromUnderlying p, TimeOffset.fromUnderlying rt
                            | AccrualVariableDependency.SingletonTarget p ->
                                VariableByMode.fromUnderlying p, TimeOffset.NonTimeDependent
                            | AccrualVariableDependency.InputTarget p ->
                                VariableByMode.fromUnderlying p, TimeOffset.NonTimeDependent)

                    VariableByMode.fromUnderlying d, ps'

                | VariableDependencyPairing.Rollback (d, ps) ->
                    let ps' =
                        ps
                        |> Set.map (function
                            | RollbackVariableDependency.AccrualTarget (p, rt) ->
                                VariableByMode.fromUnderlying p, TimeOffset.fromUnderlying rt
                            | RollbackVariableDependency.RollbackTarget (p, rt) ->
                                VariableByMode.fromUnderlying p, TimeOffset.fromUnderlying rt
                            | RollbackVariableDependency.SingletonTarget p ->
                                VariableByMode.fromUnderlying p, TimeOffset.NonTimeDependent
                            | RollbackVariableDependency.InputTarget p ->
                                VariableByMode.fromUnderlying p, TimeOffset.NonTimeDependent)

                    VariableByMode.fromUnderlying d, ps'

                | VariableDependencyPairing.Singleton (d, ps) ->
                    let ps' =
                        ps
                        |> Set.map (function
                            | SingletonVariableDependency.AccrualTarget (p, a) ->
                                VariableByMode.fromUnderlying p, TimeOffset.Aggregate a
                            | SingletonVariableDependency.RollbackTarget (p, a) ->
                                VariableByMode.fromUnderlying p, TimeOffset.Aggregate a 
                            | SingletonVariableDependency.SingletonTarget p ->
                                VariableByMode.fromUnderlying p, TimeOffset.NonTimeDependent
                            | SingletonVariableDependency.InputTarget p ->
                                VariableByMode.fromUnderlying p, TimeOffset.NonTimeDependent)

                    VariableByMode.fromUnderlying d, ps'

            Map.ofList << List.map typeMapper
            

    // The pairings are considered complete if a dependency definition exists for all
    // non-input precedents that appear at least once. We only allow this type to be 
    // instantiated provided the underlying dependencies are complete.
    type CompleteUnifiedVariableDependencyPairings =
        internal | CompleteUnifiedVariableDependencyPairings_ of UnifiedVariableDependencyPairings

        static member createFrom (pairings: UnifiedVariableDependencyPairings) =
            result {
                do! pairings
                    |> Map.keys
                    |> Seq.filter (function | VariableByMode.Input _ -> true | _ -> false)
                    |> Result.requireEmpty "Input variables cannot be specified as dependent variables."

                do! pairings
                    |> Map.values
                    |> Seq.concat
                    |> Seq.choose (function | VariableByMode.Input _, _ -> None | p, _ -> Some p)
                    |> Seq.forall (fun p -> pairings |> Map.containsKey p)
                    |> Result.requireTrue "Dependencies have not been specified for all VariableByMode precedents."
                
                return CompleteUnifiedVariableDependencyPairings_ pairings
            }