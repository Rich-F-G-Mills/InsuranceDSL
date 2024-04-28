
namespace OpenCL.NET

open System
open System.Text.RegularExpressions
open FsToolkit.ErrorHandling
open Common


module Devices =

    type DeviceId =
        internal DeviceId of IntPtr


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
        
    [<RequireQualifiedAccess>]
    module DeviceType =
        let internal toNativeStrict =
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

    [<RequireQualifiedAccess>]
    module DeviceGlobalMemoryCacheType =
        let internal fromNativeStrict =
            createStrictMapperFromNativeToType<NativeDeviceGlobalMemoryCacheType, DeviceGlobalMemoryCacheType>
   

    [<RequireQualifiedAccess>]
    type DeviceProfile =
        | [<MapFromString("FULL_PROFILE")>] Full
        | [<MapFromString("EMBEDDED_PROFILE")>] Embedded

    [<RequireQualifiedAccess>]
    module DeviceProfile =
        let internal fromString =
            createStrictMapperFromNativeToType<string, DeviceProfile>


    type DeviceVersion =
        { 
            Major: int
            Minor: int
            Information: string
        }

        static member val private re: Regex =
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

        let private splitSpaceDelimitedString (s: string) =
            s.Split (" ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList


        type private NativeDeviceInformation =
            Interop.Devices.DeviceInformation


        let AddressBits: InformationMapper.ValueTypeInfo<_, uint32, _> =
            InformationMapper.ValueTypeInfo (NativeDeviceInformation.AddressBits, id)

        let CompilerAvailable: InformationMapper.ValueTypeInfo<_, bool, _> =
            InformationMapper.ValueTypeInfo (NativeDeviceInformation.CompilerAvailable, id)

        let Extensions =
            InformationMapper.StringInfo (NativeDeviceInformation.Extensions, splitSpaceDelimitedString)

        let GlobalMemoryCacheType =
            InformationMapper.ValueTypeInfo (NativeDeviceInformation.GlobalMemoryCacheType, DeviceGlobalMemoryCacheType.fromNativeStrict)

        let Name =
            InformationMapper.StringInfo (NativeDeviceInformation.Name, id)

        let Profile =
            InformationMapper.StringInfo (NativeDeviceInformation.Profile, DeviceProfile.fromString)

        let Version =
            InformationMapper.StringInfo (NativeDeviceInformation.Version, DeviceVersion.fromString)


    let DeviceInformation =
        let mapper (DeviceId did, nativeRef, size, buffer) =
            Interop.Devices.GetDeviceInformation (did, nativeRef, size, buffer)

        new InformationMapper<_, _>(mapper)
