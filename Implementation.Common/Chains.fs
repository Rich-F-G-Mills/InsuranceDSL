
namespace Implementation.Common

(*
A chain represents a (sufficiently) complete view of all precedents leading into
a given variable. This allows us to identify invalid links and/or circular relations.

Here we raise an exception in the case of an unexpected situation as they should NOT happen!
... as opposed to working with the result type.
*)

module (*internal*) Chains =

    open System.Collections.Generic
    open FsToolkit.ErrorHandling
    open Specification.Variables
    open UnifiedDependencies

    
    [<RequireQualifiedAccess>]
    type RequiredTiming =
        | RelativeOffset of int Set
        | AllTime           // Relevant for aggregations.
        | NoTiming          // Relevant for inputs and singletons.
    
    type internal PrecedenceChainElement =
        {
            mutable RequiredTimings: RequiredTiming
            mutable Expanded: bool
        }

    type internal PrecedenceChain =
        {
            mutable CountNotExpanded: int
            // An immutable Map could have been used... However, a mutable Dictionary
            // should be more performant due to reduced allocations.
            Precedents: Dictionary<VariableByMode, PrecedenceChainElement>
        }

    type PrecedenceChains =
        Map<VariableByMode, Map<VariableByMode, RequiredTiming>>          

    (*
    For any variable, this function aims to determine ALL underlying precedents and
    the temporal depths at which they are encountered.

    This is done by looking at the precedents as currently identified and, for those
    not marked as 'expanded', adding the precedents of that respective variable and so on.

    Once all precedents are marked as 'expanded', this indicates that all
    have been identified for a given dependent variable.
    *)
    let buildPrecedenceChains (CompleteUnifiedVariableDependencyPairings_ pairings): PrecedenceChains =
        // This is the initial chain of precedents that we will expand.
        let chains =
            pairings
            |> Map.map (fun d ps ->
                // This will be populated via side-effects.
                let chain = new Dictionary<_, _>()

                do ps |> Set.iter (fun (p, o) ->
                    let state =
                        match d, p, o with
                        | _, VariableByMode.Input _, TimeOffset.NonTimeDependent ->
                            // An input cannot have any precedents and is therefore
                            // already considered expanded.
                            { RequiredTimings = RequiredTiming.NoTiming; Expanded = true }

                        | _, VariableByMode.Singleton _, TimeOffset.NonTimeDependent ->
                            { RequiredTimings = RequiredTiming.NoTiming; Expanded = false }


                        // If a singleton has an aggregated precedent, then we'll need the
                        // full history of that precedent.
                        | VariableByMode.Singleton _, _, TimeOffset.Aggregate _ ->
                            { RequiredTimings = RequiredTiming.AllTime; Expanded = false }

                        // If a time-dependent variable links to one of another type...
                        // We'll need its entire history.
                        | VariableByMode.Accrual _, VariableByMode.Rollback _, _
                        | VariableByMode.Rollback _, VariableByMode.Accrual _, _ ->
                            { RequiredTimings = RequiredTiming.AllTime; Expanded = false }

                        | VariableByMode.Accrual _, VariableByMode.Accrual _, TimeOffset.Previous ->
                            // If we're a precedent of the dependent variable, then
                            // we're (effectively) already expanded.
                            { RequiredTimings = RequiredTiming.RelativeOffset (Set.singleton -1); Expanded = p = d }

                        | VariableByMode.Accrual _, VariableByMode.Accrual _, TimeOffset.Current
                        | VariableByMode.Rollback _, VariableByMode.Rollback _, TimeOffset.Current ->
                            { RequiredTimings = RequiredTiming.RelativeOffset (Set.singleton 0); Expanded = p = d }

                        | VariableByMode.Rollback _, VariableByMode.Rollback _, TimeOffset.Next ->
                            { RequiredTimings = RequiredTiming.RelativeOffset (Set.singleton 1); Expanded = p = d }                        
                        
                        | _ ->
                            failwith "Unexpected precedent."   
                    
                    do  match chain.TryGetValue p with
                        | true, state' ->
                            // If we're already tracking this precedent, combine the
                            // relative depths that we're aware of.
                            do state'.RequiredTimings <-
                                match state.RequiredTimings, state'.RequiredTimings with
                                | RequiredTiming.RelativeOffset ros, RequiredTiming.RelativeOffset ros' ->
                                    ros
                                    |> Set.union ros'
                                    |> RequiredTiming.RelativeOffset
                                | a, b when a = b -> a
                                | _ -> failwith "Unexpected precedent."

                        | false, _ ->
                            // Otherwise, if this is the first instance of this precedent,
                            // simply add it to our chain.
                            do chain.Add (p, state))
            
                // Return the completed chain for this dependent variable.
                chain)
            |> Map.map (fun _ chain ->
                let count =
                    chain
                    |> Seq.sumBy (fun (FromKVP (_, { Expanded = e })) ->
                        if e then 0 else 1)

                { CountNotExpanded = count; Precedents = chain })

        // Create an empty dictionary to represent input variables that don't have precedents.
        let noPrecedents =
            new Dictionary<_, _>()

        let rec loop (pending: Map<VariableByMode, PrecedenceChain>) =
            if pending |> Map.isEmpty then
                // We're finished!
                chains
                |> Map.map (fun _ chain ->
                    chain.Precedents
                    |> Seq.map (function | FromKVP (p, { RequiredTimings = rs }) -> (p, rs))
                    |> Map.ofSeq)
                
            else
                // We assume that the dependent variable with the least number ofprecedents
                // that have yet to be expanded as being a more preferable starting position.
                let (d, chain) =
                    pending
                    |> Map.toSeq
                    |> Seq.minBy (fun (_, { CountNotExpanded = c }) -> c)

                // Assign to prevent repeated member access.
                let precedents = chain.Precedents

                let (p, state) =
                    precedents
                    // If we're in this chain, then we're guaranteed (!) to find a precedent
                    // that has yet to be expanded; hence not using tryPick.
                    |> Seq.pick (function
                        | FromKVP (p, ({ Expanded = false } as state)) -> Some (p, state)
                        | _ -> None) 

                // As a consequence of the search above, this CANNOT already be expanded.
                // Mark as expanded and reduce the corresponding count.
                do state.Expanded <- true
                do chain.CountNotExpanded <- chain.CountNotExpanded - 1

                // Get the precedents for the selected precedent. If the precedent is an
                // input, there is nothing else to do!... Don't even search for it.
                let ps' =
                    match p with
                    // Inputs do not have precedents.
                    | VariableByMode.Input _ -> noPrecedents
                    | _ -> chains[p].Precedents                        

                // For each precedent of the variable (itself a precedent) that we've just expanded...
                for (FromKVP (p', state')) in ps' do
                    // Determing the relative depth of the precedents depending on those of
                    // the variable that we've just expanded.
                    let newRelTimings' =
                        match state.RequiredTimings, state'.RequiredTimings with
                            | RequiredTiming.RelativeOffset ros, RequiredTiming.RelativeOffset ros' ->
                                ros
                                |> Seq.allPairs ros'
                                |> Seq.map (fun (ro, ro') -> ro + ro')
                                |> Set
                                |> RequiredTiming.RelativeOffset                            
                            | RequiredTiming.AllTime, RequiredTiming.RelativeOffset _
                            | _, RequiredTiming.AllTime ->
                                RequiredTiming.AllTime
                            | _, RequiredTiming.NoTiming ->
                                RequiredTiming.NoTiming
                            | _ ->
                                failwith "Unexpected precedent."                        

                    // Are we already tracking this precedent?
                    match precedents.TryGetValue p' with
                    | true, state'' ->
                        // If so, merge its relative depths with what we're already aware of.
                        do state''.RequiredTimings <-
                            match state''.RequiredTimings, newRelTimings' with
                            | RequiredTiming.RelativeOffset ros'', RequiredTiming.RelativeOffset ros' ->
                                ros''
                                |> Set.union ros'
                                |> RequiredTiming.RelativeOffset
                            | a, b when a = b -> a
                            | _ -> failwith "Unexpected precedent."

                        // Now we need to merge expansion statuses.
                        // A change is only needed if we're bringing in an expanded
                        // precedent that we don't already have expanded ourselves.
                        if state'.Expanded && not state''.Expanded then
                            do state''.Expanded <- true
                            do chain.CountNotExpanded <- chain.CountNotExpanded - 1
                            
                    | false, _ ->
                        // If not, add it in at the required depth.
                        // Note that what we're adding in needs to be a clone!
                        // Here, it is a achieved by the object expression.
                        if d = p' then
                            // If we're adding a new precedent which happens to be the dependent variable,
                            // mark it as already expanded.
                            do precedents.Add (p', { Expanded = true; RequiredTimings = newRelTimings' })

                            // No need to modifify the expanded count as this is already expanded (and new).
                        else
                            // Otherwise, bring over its expansion status as-is with the new depths.
                            do precedents.Add (p', { state' with RequiredTimings = newRelTimings' })
                                
                            // Increment our non-expanded count as necessary.
                            if not state'.Expanded then
                                do chain.CountNotExpanded <- chain.CountNotExpanded + 1    
                      
                if chain.CountNotExpanded = 0 then
                    // If we have no unexpanded precedents left, then we can remove
                    // the head of the pending list!
                    loop (pending |> Map.remove d)

                else
                    loop pending

        chains
        |> Map.toSeq
        // We don't need to process any variables where all precedents have
        // already been fully expanded.
        |> Seq.choose (fun (d, chain) -> if chain.CountNotExpanded = 0 then Some d else None)
        |> Seq.fold (fun pending d -> pending |> Map.remove d) chains
        |> loop


    type ValidatedPrecedenceChains =
        internal | ValidatedPrecedenceChains_ of PrecedenceChains

        static member createFrom precedenceChain =
            result {
                (*
                Using the chain above, there are a few checks that we need to run.
                1. If an accrual variable ultimately links back to itself, the maximum
                    relative depth MUST be negative.
                2. If a rollback variable ultimately links back to itself, the minimum
                    relative depth MUST be positive.
                3. A singleton variable CANNOT ultimately refer to itself.
                *)
                do! precedenceChain
                    // For each dependent variable, find all precedents for that same variable.
                    |> Map.map (fun d -> Map.tryFind d)
                    // Find and unwrap the option which is gauaranteed to be 'Some'.
                    |> Map.filter (fun _ -> Option.isSome)
                    |> Map.map (fun _ -> Option.get)
                    |> Map.toSeq
                    |> Seq.map (function
                        // Check 1.
                        | VariableByMode.Accrual d, RequiredTiming.RelativeOffset ros ->
                            ros
                            |> Set.forall (fun ro -> ro < 0)
                            |> Result.requireTrue (sprintf "Invalid cycle detected for accrual variable %A." d)

                        // Check 2.
                        | VariableByMode.Rollback d, RequiredTiming.RelativeOffset ros ->
                            ros
                            |> Set.forall (fun ro -> ro > 0)
                            |> Result.requireTrue (sprintf "Invalid cycle detected for rollback variable %A." d)

                        // Check 3.
                        | VariableByMode.Singleton d, _ ->
                            // The fact that a singleton refers to enough is bad! Irrespective of the
                            // specific form of relationship.
                            Error (sprintf "Invalid cycle detected for singleton variable %A." d)

                        | VariableByMode.Input d, _ ->
                            failwith (sprintf "Unexpected error: Input variable %A defined as dependent variable." d)
                        
                        | _ ->
                            failwith "Unexpected precedent.")
                    |> Seq.sequenceResultM
                    |> Result.ignore

                return ValidatedPrecedenceChains_ precedenceChain
            }