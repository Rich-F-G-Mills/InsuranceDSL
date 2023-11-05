
namespace Implementation.Common

[<AutoOpen>]
module internal Common =

    open System.Collections.Generic

    
    let (|FromKVP|) (kvp: KeyValuePair<_, _>) =
        (kvp.Key, kvp.Value)
