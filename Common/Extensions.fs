
namespace Common

[<AutoOpen>]
module Extensions =

    [<RequireQualifiedAccess>]
    module Seq =
        let inline tee ([<InlineIfLambda>] f) xs =
            seq {
                for x in xs do
                    do f x
                    yield x
            }


    [<RequireQualifiedAccess>]
    module List =       
        let inline tryFindDuplicateBy ([<InlineIfLambda>] mapper) =
            List.countBy mapper
            >> List.tryFind (fun (_, count) -> count > 1)
            >> Option.map fst

        let inline hasDuplicatesBy ([<InlineIfLambda>] mapper) =
            tryFindDuplicateBy mapper >> Option.isSome