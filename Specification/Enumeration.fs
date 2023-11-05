
namespace Specification

[<RequireQualifiedAccess>]
module Enumeration =

    // TODO - Add contraint to value type.
    type Definition<'TEnum> =
        {
            Name: string
            Levels: Map<string, 'TEnum>
        }

    type Integer = Definition<int>

    type String = Definition<string>
