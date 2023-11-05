
namespace Common

[<AutoOpen>]
module Attributes =
    
    open System
    open FSharp.Reflection


    [<AbstractClass>]
    type MapFromAttributeBase<'T when 'T:comparison> (source: 'T) =
        inherit Attribute ()

        member val Source = source

    [<Sealed>]
    type MapFromStringAttribute (source: string) =
        inherit MapFromAttributeBase<string> (source)


    let getMappingsForType<'TSource, 'TTarget when 'TSource:comparison and 'TTarget:comparison> =
        let anyDuplicated arr =
            let distinct =
                Array.distinct arr

            distinct.Length < arr.Length

        let mappings =
            FSharpType.GetUnionCases typeof<'TTarget>
            |> Array.choose (fun c ->
                c.GetCustomAttributes typeof<MapFromAttributeBase<'TSource>>
                |> Array.map (fun a -> a :?> MapFromAttributeBase<'TSource>)
                |> Array.tryExactlyOne
                |> Option.map (fun a -> a.Source, FSharpValue.MakeUnion (c, Array.empty) :?> 'TTarget))

        let sources =
            mappings |> Array.map fst

        let targets =
            mappings |> Array.map snd

        if anyDuplicated sources then
            failwith (sprintf "Not all sources of type '%s' are distinct for target of type '%s'." typeof<'TSource>.Name typeof<'TTarget>.Name)
        elif anyDuplicated targets then
            failwith (sprintf "Not all targets of type '%s' are distinct." typeof<'TTarget>.Name)
        else
            mappings

    let private makeStrict<'T1, 'T2 when 'T1:comparison> (mapper: 'T1 -> 'T2 option) =
        fun from ->
            match mapper from with
            | Some ``to`` -> ``to``
            | None -> failwith (sprintf "Unable to map '%A' to target type '%s'." from typeof<'T2>.Name)

    let createMapperFromNativeToType<'TSource, 'TTarget when 'TSource:comparison and 'TTarget:comparison> =
        let cases =
            getMappingsForType<'TSource, 'TTarget>
            |> Map.ofArray

        fun source ->
            Map.tryFind source cases

    let createStrictMapperFromNativeToType<'TSource, 'TTarget when 'TSource:comparison and 'TTarget:comparison> =
        makeStrict createMapperFromNativeToType<'TSource, 'TTarget>

    let createMapperToNativeFromType<'TSource, 'TTarget when 'TSource:comparison and 'TTarget:comparison> =
        let cases =
            getMappingsForType<'TSource, 'TTarget>
            |> Seq.map (fun (s, t) -> t, s)
            |> Map.ofSeq

        fun target ->            
            Map.tryFind target cases

    let createStrictMapperToNativeFromType<'TSource, 'TTarget when 'TSource:comparison and 'TTarget:comparison> =
        makeStrict createMapperToNativeFromType<'TSource, 'TTarget>