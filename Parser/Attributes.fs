
namespace Parser


module internal rec Attributes =

    open System
    open System.Reflection


    let private markedLocations =
        Assembly.GetExecutingAssembly ()
        |> _.DefinedTypes
        |> Seq.filter (fun ti ->
            // We don't want to include the interface itself.
            ti.IsClass && ti.IsAssignableTo (typeof<ILocator>))
        |> Seq.toList


    // A mechanism used to locate modules containing keywords and/or callables.
    type internal ILocator =
        interface end
                

    [<Sealed>]
    [<AttributeUsage(validOn = AttributeTargets.Property)>]
    type internal KeywordAttribute () =
        inherit System.Attribute ()

        static let propertyHasKeywordAttribute (pi: PropertyInfo) =
            Attribute.GetCustomAttribute(pi, typeof<KeywordAttribute>)
            |> (not << isNull)

        // We cannot extract the property value at this stage as they won't have been
        // instantiated via the start-up code.
        // Instead, return the property info for later use.
        static member PropertiesMarkedAsKeyword =
            markedLocations
            |> Seq.map _.DeclaringType
            |> Seq.map _.GetTypeInfo()
            |> Seq.collect _.DeclaredProperties
            |> Seq.filter propertyHasKeywordAttribute
            |> Seq.toList


    [<Sealed>]
    [<AttributeUsage(validOn = AttributeTargets.Method)>]
    type internal CallableAttribute () =
        inherit System.Attribute ()

        static let methodHasCallableAttribute (mi: MethodInfo) =
            Attribute.GetCustomAttribute(mi, typeof<CallableAttribute>)
            |> (not << isNull)

        static member val CallableIdentifiers =
            markedLocations
            |> Seq.map _.DeclaringType
            |> Seq.map _.GetTypeInfo()
            |> Seq.collect _.DeclaredMethods
            |> Seq.filter methodHasCallableAttribute
            |> Seq.map _.Name
            |> Seq.toList
