
namespace OpenCL.NET.Interop
{
    using System.Runtime.InteropServices;
    using static Common;

    public static partial class Platforms
    {
        /// <summary>
        /// Obtain the list of platforms available.<br/>
        /// Refer to: <see href="https://registry.khronos.org/OpenCL/sdk/1.0/docs/man/xhtml/clGetPlatformIDs.html"/>
        /// </summary>
        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clGetPlatformIDs")]
        public static extern NativeCLResult GetPlatformIds(
            [In][MarshalAs(UnmanagedType.U4)] int numberOfEntries,
            [Out][MarshalAs(UnmanagedType.LPArray)] IntPtr[] platforms,
            [Out][MarshalAs(UnmanagedType.U4)] out int numberOfPlatforms
        );

        /// <summary>
        /// Get specific information about the OpenCL platform.<br/>
        /// Refer to: <see href="https://registry.khronos.org/OpenCL/sdk/1.0/docs/man/xhtml/clGetPlatformInfo.html"/>
        /// </summary>
        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clGetPlatformInfo")]
        public static extern NativeCLResult GetPlatformInformation(
            [In] IntPtr platform,
            [In][MarshalAs(UnmanagedType.U4)] PlatformInformation parameterName,
            [In] UIntPtr parameterValueSize,
            [Out][MarshalAs(UnmanagedType.LPArray)] byte[] parameterValue,
            [Out] out UIntPtr parameterValueSizeReturned
        );
    }
}