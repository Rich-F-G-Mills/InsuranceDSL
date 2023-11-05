
namespace OpenCL.NET

open System
open System.Text.RegularExpressions
open FsToolkit.ErrorHandling
open Common


module Devices =

    type DeviceId =
        internal DeviceId of nativeint

    type private NativeDeviceType =
        Interop.Devices.DeviceType

    [<Sealed>]
    type private MapFromDeviceTypeAttribute (deviceType) =
        inherit MapFromAttributeBase<NativeDeviceType> (deviceType)

    [<RequireQualifiedAccess>]
    type DeviceType =
        /// <summary>
        /// An OpenCL device that is the host processor. The host processor runs the OpenCL implementations and is a single or multi-core CPU.
        /// </summary>
        | [<MapFromDeviceType(NativeDeviceType.Cpu)>] Cpu

        /// <summary>
        /// An OpenCL device that is a GPU. By this we mean that the device can also be used to accelerate a 3D API such as OpenGL or DirectX.
        /// </summary>
        | [<MapFromDeviceType(NativeDeviceType.Gpu)>] Gpu

        /// <summary>
        /// Dedicated OpenCL accelerators (for example the IBM CELL Blade). These devices communicate with the host processor using a peripheral interconnect such as PCIe.
        /// </summary>
        | [<MapFromDeviceType(NativeDeviceType.Accelerator)>] Accelerator

        /// <summary>
        /// The default OpenCL device in the system. The default device cannot be a <c>CL_DEVICE_TYPE_CUSTOM</c> device.
        /// </summary>
        | [<MapFromDeviceType(NativeDeviceType.Default)>] Default

        /// <summary>
        /// Dedicated accelerators that do not support programs written in OpenCL C.
        /// </summary>
        | [<MapFromDeviceType(NativeDeviceType.Custom)>] Custom

        /// <summary>
        /// All OpenCL devices available in the system except <c>CL_DEVICE_TYPE_CUSTOM</c> devices.
        /// </summary>
        | [<MapFromDeviceType(NativeDeviceType.All)>] All        

        static member val internal toNativeStrict =
            createStrictMapperToNativeFromType<NativeDeviceType, DeviceType>

    let getDeviceIds (Platforms.PlatformId pid) deviceType =
        result {
            let nativeDeviceType =
                DeviceType.toNativeStrict deviceType

            let! found =
                Interop.Devices.GetDeviceIds (pid, nativeDeviceType, 0u, null)

            let deviceIds =
                Array.zeroCreate (int found)

            let! _ =
                Interop.Devices.GetDeviceIds (pid, nativeDeviceType, found, deviceIds)

            return
                deviceIds |> Array.map DeviceId
        }


    type private NativeDeviceGlobalMemoryCacheType =
        Interop.Devices.DeviceGlobalMemoryCacheType

    [<Sealed>]
    type private MapFromDeviceGlobalMemoryCacheTypeAttribute (cacheType) =
        inherit MapFromAttributeBase<NativeDeviceGlobalMemoryCacheType> (cacheType)

    [<RequireQualifiedAccess>]
    type DeviceGlobalMemoryCacheType =
        | [<MapFromDeviceGlobalMemoryCacheType(NativeDeviceGlobalMemoryCacheType.None)>] None
        | [<MapFromDeviceGlobalMemoryCacheType(NativeDeviceGlobalMemoryCacheType.ReadOnlyCache)>] ReadOnlyCache
        | [<MapFromDeviceGlobalMemoryCacheType(NativeDeviceGlobalMemoryCacheType.ReadWriteCache)>] ReadWriteCache

        static member val internal fromNativeStrict =
            createStrictMapperFromNativeToType<NativeDeviceGlobalMemoryCacheType, DeviceGlobalMemoryCacheType>
   
   [<RequireQualifiedAccess>]
    type DeviceProfile =
        | [<MapFromString("FULL_PROFILE")>] Full
        | [<MapFromString("EMBEDDED_PROFILE")>] Embedded

        static member val internal fromString =
            createStrictMapperFromNativeToType<string, DeviceProfile>

    type DeviceVersion =
        { 
            Major: int
            Minor: int
            Information: string
        }

        static member private re: Regex =
            new Regex (@"^OpenCL\s([0-9]+)\.([0-9]+)\s(.*)$")

        static member fromString versionStr =
            let versionMatch =
                DeviceVersion.re.Match versionStr

            versionMatch.Groups
            |> Seq.map (fun g -> g.Value)
            |> Seq.toList
            |> function
                | _::major::minor::information::[] -> { Major = int major; Minor = int minor; Information = information.Trim() }
                | _ -> failwith (sprintf "Unable to process version string '%s'." versionStr)


    [<RequireQualifiedAccess>]
    module DeviceInformation =

        type private NativeDeviceInformation =
            Interop.Devices.DeviceInformation

        type NonStringElement<'TReturn, 'TTransformed> =
            internal | NonStringElement of NativeName: NativeDeviceInformation * Transformer: ('TReturn -> 'TTransformed)

        type StringElement<'TTransformed> =
            internal | StringElement of NativeName: NativeDeviceInformation * Transformer: (string -> 'TTransformed)


        let private splitSpaceDelimitedString (s: string) =
            s.Split (" ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList


        let (AddressBits: NonStringElement<uint32, _>) =
            NonStringElement (NativeDeviceInformation.AddressBits, id)

        let (CompilerAvailable: NonStringElement<bool, _>) =
            NonStringElement (NativeDeviceInformation.CompilerAvailable, id)

        let Extensions =
            StringElement (NativeDeviceInformation.Extensions, splitSpaceDelimitedString)

        let GlobalMemoryCacheType =
            NonStringElement (NativeDeviceInformation.GlobalMemoryCacheType, Devices.DeviceGlobalMemoryCacheType.fromNativeStrict)

        let Name =
            StringElement (NativeDeviceInformation.Name, id)

        let Profile =
            StringElement (NativeDeviceInformation.Profile, DeviceProfile.fromString)

        let Version =
            StringElement (NativeDeviceInformation.Version, DeviceVersion.fromString)


// Best we can do to get a static class.
[<Sealed; AbstractClass>]
type Devices () =
    (*
    The issue is that we need to call a different function depending on whether
    the return type is a string or something else.
    We cannot cast the string returned by 'extractString' to 'TReturn, even though
    we can check via conditional that the types are the same. This would require the use of
    Unsafe.As which adds the 'reference type' constraint to 'TReturn which isn't ideal!
    Instead, we leverage the multiple dispatch approach offered via static class members.
    *)
    
    static member getDeviceInformation (Devices.DeviceId did, element: DeviceInformation.StringElement<'TTransformed>) =
        let (DeviceInformation.StringElement (nativeName, transformer)) = element
        
        let extractor (buffer, size) =
            Interop.Devices.GetDeviceInformation (did, nativeName, size, buffer)

        extractString extractor
        |> Result.map transformer

    static member getDeviceInformation (Devices.DeviceId did, element: DeviceInformation.NonStringElement<'TReturn, 'TTransformed>) =
        let (DeviceInformation.NonStringElement (nativeName, transformer)) = element
        
        let extractor (buffer, size) =
            Interop.Devices.GetDeviceInformation (did, nativeName, size, buffer)

        castFromBytes<'TReturn, _> extractor
        |> Result.map transformer 