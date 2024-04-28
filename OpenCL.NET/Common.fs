
namespace OpenCL.NET

[<AutoOpen>]
module Common =

    open System
    open System.Runtime.InteropServices
    open System.Text
    open FsToolkit.ErrorHandling
    open OpenCL.NET.Interop


    let private (|OfNativeCLFailure|) =
        CLFailure.ofNativeStrict

    
    type internal IDisposableGCHandle =
        inherit IDisposable
        abstract member AddrOfPinnedObject: unit -> IntPtr


    type internal ResultBuilder with
        member _.Source (result: NativeCLResult) =
            match result with
                | NativeCLResult.Success -> Ok ()
                | OfNativeCLFailure failure -> Error failure

        member _.Source ((result, p1): NativeCLResult * 'T1) =
            match result with
                | NativeCLResult.Success -> Ok p1
                | OfNativeCLFailure failure -> Error failure

        member _.Source ((p1, result): 'T1 * NativeCLResult) =
            match result with
                | NativeCLResult.Success -> Ok p1
                | OfNativeCLFailure failure -> Error failure

        member _.Source ((result, p1, p2): NativeCLResult * 'T1 * 'T2) =
            match result with
                | NativeCLResult.Success -> Ok (p1, p2)
                | OfNativeCLFailure failure -> Error failure


    let internal extractString (extractor: byte array * UIntPtr -> NativeCLResult * UIntPtr) =
        result {
            let! requiredSize =
                extractor (null, 0un)

            if requiredSize = 0un then
                return String.Empty

            else
                let buffer =
                    Array.zeroCreate (int requiredSize)

                let! _ =
                    extractor (buffer, requiredSize)

                let bufferSpan =
                    // Remove terminating 0.
                    (ReadOnlySpan buffer).Slice (0, (int requiredSize) - 1)
                        
                return Encoding.ASCII.GetString bufferSpan
        }     


    let private getDisposablePinnedGCHandle (obj: obj) =
        let handle =
            GCHandle.Alloc (obj, GCHandleType.Pinned)

        { new IDisposableGCHandle with
            member _.Dispose () =
                do handle.Free ()

            member _.AddrOfPinnedObject () =
                handle.AddrOfPinnedObject () }


    let internal castFromBytesToValue<'TValue> =
        let valueType =
            typeof<'TValue>

        let outputType =
            if valueType.IsEnum then
                Enum.GetUnderlyingType valueType
            else
                valueType

        let size = Marshal.SizeOf outputType

        fun (extractor: (byte array * UIntPtr) -> NativeCLResult * UIntPtr) ->
            result {
                let buffer =
                    Array.zeroCreate size

                use handle =
                    getDisposablePinnedGCHandle buffer

                let! _ =
                    extractor (buffer, unativeint size)
                
                return Marshal.PtrToStructure (handle.AddrOfPinnedObject (), outputType) :?> 'TValue
            }

    let internal castFromBytesToValueArray<'TValue when 'TValue: struct and 'TValue: (new: unit -> 'TValue) and 'TValue :> ValueType>
        (extractor: (byte array * UIntPtr) -> NativeCLResult * UIntPtr) =
            result {
                let! totalArraySize =
                    extractor (null, 0un)

                let buffer =
                    Array.zeroCreate (int totalArraySize)

                let! _ =
                    extractor (buffer, totalArraySize)
                
                let (converted: Span<'TValue>) =
                    MemoryMarshal.Cast(buffer.AsSpan ())

                return converted.ToArray ()
            }


    [<RequireQualifiedAccess>]
    module InformationMapper =
        type internal Extractor<'TTarget, 'TNative> =
            'TTarget * 'TNative * UIntPtr * byte array -> NativeCLResult * UIntPtr

        type ValueTypeInfo<'TNative, 'TReturn, 'TTransformed> =
            internal | ValueTypeInfo of NativeRef: 'TNative * Transformer: ('TReturn -> 'TTransformed)

        type StringInfo<'TNative, 'TTransformed> =
            internal | StringInfo of NativeRef: 'TNative * Transformer: (string -> 'TTransformed)

        type ValueTypeInfoArray<'TNative, 'TReturn, 'TTransformed> =
            internal | ValueTypeInfoArray of NativeRef: 'TNative * Transformer: ('TReturn -> 'TTransformed)

    [<Sealed>]
    type InformationMapper<'TTarget, 'TNative>
            internal (extractor: InformationMapper.Extractor<'TTarget, 'TNative>) =
        (*
        The issue is that we need to call a different function depending on whether
        the return type is a string or something else.
        We cannot cast the string returned by 'extractString' to 'TReturn, even though
        we can check via conditional that the types are the same. This would require the use of
        Unsafe.As which adds the 'reference type' constraint to 'TReturn which isn't ideal!
        Instead, we leverage the multiple dispatch approach offered via static class members.
        *)
        
        member _.get (target, requiredInfo) =
            let (InformationMapper.StringInfo (nativeRef, transformer)) = requiredInfo
        
            let extractor' (buffer, size) =
                extractor (target, nativeRef, size, buffer)

            extractString extractor'
            |> Result.map transformer

        member _.get (target, requiredInfo) =
            let (InformationMapper.ValueTypeInfo (nativeRef, transformer)) = requiredInfo
        
            let extractor' (buffer, size) =
                extractor (target, nativeRef, size, buffer)

            castFromBytesToValue<'TReturn> extractor'
            |> Result.map transformer

        member _.get (target, requiredInfo) =
            let (InformationMapper.ValueTypeInfoArray (nativeRef, transformer)) = requiredInfo
        
            let extractor' (buffer, size) =
                extractor (target, nativeRef, size, buffer)

            castFromBytesToValueArray<'TReturn> extractor'
            |> Result.map (Array.map transformer)
   