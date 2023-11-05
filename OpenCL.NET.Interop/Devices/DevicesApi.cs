
namespace OpenCL.NET.Interop
{
    using System.Runtime.InteropServices;

    using static Common;

    public static partial class Devices
    {
        /// <summary>
        /// Obtain the list of devices available on a platform.<br/>
        /// Refer to: <see href="https://registry.khronos.org/OpenCL/sdk/2.0/docs/man/xhtml/clGetDeviceIDs.html"/>
        /// </summary>
        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clGetDeviceIDs")]
        public static extern NativeCLResult GetDeviceIds(
            [In] IntPtr platform,
            [In] [MarshalAs(UnmanagedType.U8)] DeviceType deviceType,
            [In] [MarshalAs(UnmanagedType.U4)] uint numberOfEntries,
            [Out] [MarshalAs(UnmanagedType.LPArray)] IntPtr[] devices,
            [Out] [MarshalAs(UnmanagedType.U4)] out uint numberOfDevicesReturned
        );

        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clGetDeviceInfo")]
        public static extern NativeCLResult GetDeviceInformation(
            [In] IntPtr device,
            [In] [MarshalAs(UnmanagedType.U4)] DeviceInformation parameterName,
            [In] UIntPtr parameterValueSize,
            [Out] byte[] parameterValue,
            [Out] out UIntPtr parameterValueSizeReturned
        );

        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clCreateSubDevices")]
        public static extern NativeCLResult CreateSubDevices(
            [In] IntPtr inDevice,
            [In] [MarshalAs(UnmanagedType.LPArray)] IntPtr[] properties,
            [In] [MarshalAs(UnmanagedType.U4)] uint numberOfDevices,
            [Out] [MarshalAs(UnmanagedType.LPArray)] IntPtr[] outDevices,
            [Out] [MarshalAs(UnmanagedType.U4)] out uint numberOfDevicesReturned
        );

        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clRetainDevice")]
        public static extern NativeCLResult RetainDevice(
            [In] IntPtr device
        );

        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clReleaseDevice")]
        public static extern NativeCLResult ReleaseDevice(
            [In] IntPtr device
        );

        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clSetDefaultDeviceCommandQueue")]
        public static extern NativeCLResult SetDefaultDeviceCommandQueue(
            [In] IntPtr context,
            [In] IntPtr device,
            [In] IntPtr commandQueue
        );

        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clGetDeviceAndHostTimer")]
        public static extern NativeCLResult GetDeviceAndHostTimer(
            [In] IntPtr device,
            [In] IntPtr deviceTimestamp,
            [In] IntPtr hostTimestamp
        );

        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clGetHostTimer")]
        public static extern NativeCLResult GetHostTimer(
            [In] IntPtr device,
            [In] IntPtr hostTimestamp
        );
    }
}