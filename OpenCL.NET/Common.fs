
namespace OpenCL.NET

[<AutoOpen>]
module internal Common =

    open System
    open System.Runtime.InteropServices
    open System.Text
    open FsToolkit.ErrorHandling
    open OpenCL.NET.Interop


    let private (|OfNativeCLFailure|) =
        CLFailure.ofNativeStrict

    
    type internal IDisposableGCHandle =
        inherit IDisposable
        abstract member AddrOfPinnedObject: unit -> nativeint


    type internal ResultBuilder with
        member _.Source ((result, p1): NativeCLResult * 'T1) =
            match result with
                | NativeCLResult.Success -> Ok p1
                | OfNativeCLFailure failure -> Error failure

        member _.Source ((result, p1, p2): NativeCLResult * 'T1 * 'T2) =
            match result with
                | NativeCLResult.Success -> Ok (p1, p2)
                | OfNativeCLFailure failure -> Error failure


    let extractString (extractor: byte array * unativeint -> NativeCLResult * unativeint) =
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


    let castFromBytes<'TValue, 'TAux> =
        let valueType =
            typeof<'TValue>

        let outputType =
            if valueType.IsEnum then
                Enum.GetUnderlyingType valueType
            else
                valueType

        let size = Marshal.SizeOf outputType

        fun (extractor: (byte array * unativeint) -> NativeCLResult * 'TAux) ->
            result {
                let buffer =
                    Array.zeroCreate size

                use handle =
                    getDisposablePinnedGCHandle buffer

                let! _ =
                    extractor (buffer, unativeint size)
                
                return Marshal.PtrToStructure (handle.AddrOfPinnedObject (), outputType) :?> 'TValue
            }
   