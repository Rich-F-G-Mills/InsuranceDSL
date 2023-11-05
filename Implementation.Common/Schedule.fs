
namespace Implementation.Common

module Schedule =

    open System.Collections.Generic
    open System.Linq
    open FsToolkit.ErrorHandling
    open Specification.Variables
    open Specification.Dependencies
    open UnifiedDependencies
    open Chains


    type internal CalculationStage =
        // We use an array (as opposed to a set) because we care about the order.
        | AccrualStage
        | RollbackStage
        | SingletonStage

    type CalculationGroup =
        | AccrualGroup of AccrualVariable array
        | RollbackGroup of RollbackVariable array
        | SingletonGroup of SingletonVariable array

    let private getStageTuple =
        let accrualTuple =
            CalculationStage.RollbackStage,
                function | VariableByMode.Accrual _ -> true | _ -> false
         
        let rollbackTuple =
            CalculationStage.SingletonStage,
                function | VariableByMode.Rollback _ -> true | _ -> false

        let singletonTuple =
            CalculationStage.AccrualStage,
                function | VariableByMode.Singleton _ -> true | _ -> false

        function
        | AccrualStage -> accrualTuple
        | RollbackStage -> rollbackTuple
        | SingletonStage -> singletonTuple

    let private createGroup =
        let preamble =
            List.rev >> List.toArray

        let accrualMapper =
            preamble
            >> Array.map (function | VariableByMode.Accrual v -> v | _ -> failwith "Unexpected group member.")
            >> AccrualGroup

        let rollbackMapper =
            preamble
            >> Array.map (function | VariableByMode.Rollback v -> v | _ -> failwith "Unexpected group member.")
            >> RollbackGroup

        let singletonMapper =
            preamble
            >> Array.map (function | VariableByMode.Singleton v -> v | _ -> failwith "Unexpected group member.")
            >> SingletonGroup

        function
        | AccrualStage -> accrualMapper
        | RollbackStage -> rollbackMapper
        | SingletonStage -> singletonMapper

    let internal determineCalculationGroups (ValidatedPrecedenceChains_ chains) =
        let prunedChains =
            chains
            // Remove any precedents which are input variables.
            |> Map.map (fun _ ->
                Map.filter (fun p _ ->
                    match p with | VariableByMode.Input _ -> false | _ -> true))
            // We need to prune the chain. Specifically, ignore incidences where
            // accrual variables are referring to prior timesteps (of any accrual variable)
            // and likewise for rollback variables.
            |> Map.map (function
                | VariableByMode.Accrual _ ->
                    Map.map (fun p rt ->
                        match p, rt with
                        | VariableByMode.Accrual _, RequiredTiming.RelativeOffset ros ->
                            ros
                            |> Set.exists (fun ro -> ro = 0)

                        | _ -> true)

                | VariableByMode.Rollback _ ->
                    Map.map (fun p rt ->
                        match p, rt with
                        | VariableByMode.Rollback _, RequiredTiming.RelativeOffset ros ->
                            ros
                            |> Set.exists (fun ro -> ro = 0)

                        | _ -> true)
                        
                | VariableByMode.Singleton _ ->
                    Map.map (fun _ _ -> true)

                | VariableByMode.Input _ ->
                    failwith "Unexpected dependent variable.")
            |> Map.map (fun _ ->
                Map.filter (fun _ -> id) >> Map.keys >> HashSet)
                
        let priorGroups = new List<_>()
        
        let allocated = new HashSet<_>()
        
        let rec outer stage remaining depth =
            let nextStage, filter = getStageTuple stage

            let createGroup' = createGroup stage

            let rec inner groupInProgress (remaining: Map<_, HashSet<_>>) =
                if remaining.Count = 0 then
                    let newGroup =
                        createGroup' groupInProgress

                    do priorGroups.Add newGroup

                    priorGroups

                else
                    remaining
                    |> Map.iter (fun d chain ->
                        if filter d then
                            do ignore <| chain.RemoveWhere (fun p -> allocated.Contains p))

                    remaining
                    |> Map.tryFindKey (fun d chain ->
                        (filter d) && (chain.Count = 0))
                    |> function
                        | Some d ->
                            do ignore <| allocated.Add d

                            inner (d::groupInProgress) (remaining |> Map.remove d)

                        | None ->                             
                            // Don't add an empty group!
                            if groupInProgress.Length > 0 then
                                let newGroup =
                                    createGroup' groupInProgress

                                do priorGroups.Add newGroup

                            elif priorGroups.Count = 0 then
                                if depth = 2 then
                                    failwith "Unable to determine ordering."

                            else
                                match priorGroups.Last(), stage with
                                | AccrualGroup _, AccrualStage
                                | RollbackGroup _, RollbackStage
                                | SingletonGroup _, SingletonStage ->
                                    failwith "Unable to determine ordering."
                                | _ -> ()
                            
                            outer nextStage remaining (depth + 1)

            inner List.empty remaining

        outer CalculationStage.SingletonStage prunedChains 0
                

    let buildCalculationSchedule (pairings: VariableDependencyPairing_ list) =
        result {
            // There are a number of disparate types underlying the dependency
            // list above. Here, we convert it into a (more) unified set of types.
            let pairings =
                UnifiedVariableDependencyPairings.createFrom pairings

            // Any (non-input) precedents must also appear as the dependent variable in a pairing.
            let! completePairings =
                CompleteUnifiedVariableDependencyPairings.createFrom pairings

            let precedenceChains =
                Chains.buildPrecedenceChains completePairings

            // Ensure that the resulting chain is valid. For example, that accrual
            // variables don't refer to future editions of themselves.
            let! validatedPrecedenceChains =
                Chains.ValidatedPrecedenceChains.createFrom precedenceChains

            let calculationGroups =
                determineCalculationGroups validatedPrecedenceChains

            return (validatedPrecedenceChains, calculationGroups)
        }

