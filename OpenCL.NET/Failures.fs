
namespace OpenCL.NET

open Common
open OpenCL.NET.Interop


[<Sealed>]
type private MapFromNativeCLResultAttribute (nativeResult) =
    inherit MapFromAttributeBase<NativeCLResult> (nativeResult)


type CLFailure =
    /// <summary>
    /// No device that matched the specified device type could be found.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.DeviceNotFound)>] DeviceNotFound

    /// <summary>
    /// The device is currently not available.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.DeviceNotAvailable)>] DeviceNotAvailable

    /// <summary>
    /// The compiler is not available for the current platform.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.CompilerNotAvailable)>] CompilerNotAvailable

    /// <summary>
    /// There was a failure to allocate memory for buffer object. 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.MemObjectAllocationFailure)>] MemObjectAllocationFailure

    /// <summary>
    /// There was a failure to allocate resources required by the OpenCL implementation on the on the device.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.OutOfResources)>] OutOfResources

    /// <summary>
    /// There was a failure to allocate resources required by the OpenCL implementation on the host.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.OutOfHostMemory)>] OutOfHostMemory

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.ProfilingInformationNotAvailable)>] ProfilingInformationNotAvailable

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.MemCopyOverlap)>] MemCopyOverlap

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.ImageFormatMismatch)>] ImageFormatMismatch

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.ImageFormatNotSupported)>] ImageFormatNotSupported

    /// <summary>
    /// If there is a failure to build the program executable.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.BuildProgramFailure)>] BuildProgramFailure

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.MapFailure)>] MapFailure

    /// <summary>
    /// One or more of the provided arguments has an invalid value.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidValue)>] InvalidValue

    /// <summary>
    /// The specified device type is not a valid device type.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidDeviceType)>] InvalidDeviceType

    /// <summary>
    /// The specified platform is not a valid platform.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidPlatform)>] InvalidPlatform

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidDevice)>] InvalidDevice

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidContext)>] InvalidContext

    /// <summary>
    /// The specified queue properties are not valid.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidQueueProperties)>] InvalidQueueProperties

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidCommandQueue)>] InvalidCommandQueue

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidHostPtr)>] InvalidHostPtr

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidMemObject)>] InvalidMemObject

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidImageFormatDescriptor)>] InvalidImageFormatDescriptor

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidImageSize)>] InvalidImageSize

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidSampler)>] InvalidSampler

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidBinary)>] InvalidBinary

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidBuildOptions)>] InvalidBuildOptions

    /// <summary>
    /// The program is not a valid program object.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidProgram)>] InvalidProgram

    /// <summary>
    /// There is no successfully built executable program.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidProgramExecutable)>] InvalidProgramExecutable

    /// <summary>
    /// The specified kernel name was not found in the program.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidKernelName)>] InvalidKernelName

    /// <summary>
    /// The function definition for the __kernel function such as the number of arguments or the argument types are not the same for all devices for which the program executable has been built.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidKernelDefinition)>] InvalidKernelDefinition

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidKernel)>] InvalidKernel

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidArgIndex)>] InvalidArgIndex

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidArgValue)>] InvalidArgValue

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidArgSize)>] InvalidArgSize

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidKernelArgs)>] InvalidKernelArgs

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidWorkDimension)>] InvalidWorkDimension

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidWorkGroupSize)>] InvalidWorkGroupSize

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidWorkItemSize)>] InvalidWorkItemSize

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidGlobalOffset)>] InvalidGlobalOffset

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidEventWaitList)>] InvalidEventWaitList

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidEvent)>] InvalidEvent

    /// <summary>
    /// The operation performed is invalid.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidOperation)>] InvalidOperation

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidGLObject)>] InvalidGLObject

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidBufferSize)>] InvalidBufferSize

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidMipLevel)>] InvalidMipLevel

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidGlobalWorkSize)>] InvalidGlobalWorkSize

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidProperty)>] InvalidProperty

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidImageDescriptor)>] InvalidImageDescriptor

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidCompilerOptions)>] InvalidCompilerOptions

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidLinkerOptions)>] InvalidLinkerOptions

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidDevicePartitionCount)>] InvalidDevicePartitionCount

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidPipeSize)>] InvalidPipeSize

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidDeviceQueue)>] InvalidDeviceQueue

    /// <summary>
    /// If the cl_khr_icd extension is enabled and no platforms are found.
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.PlatformNotFoundKhr)>] PlatformNotFoundKhr

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.DevicePartitionFailedExt)>] DevicePartitionFailedExt

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidPartitionCoundExt)>] InvalidPartitionCoundExt

    /// <summary>
    /// 
    /// </summary>
    | [<MapFromNativeCLResult(NativeCLResult.InvalidPartitionNameExt)>] InvalidPartitionNameExt

[<RequireQualifiedAccess>]
module CLFailure =
    let internal ofNativeStrict =
        createStrictMapperFromNativeToType<NativeCLResult, CLFailure>