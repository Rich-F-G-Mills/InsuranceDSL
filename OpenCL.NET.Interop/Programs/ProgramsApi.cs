﻿
namespace OpenCL.NET.Interop
{
    using System.Runtime.InteropServices;
    using static Common;

    public static partial class Programs
    {
        /// <summary>
        /// Creates a program object for a context, and loads the source code specified by the text strings in the <see cref="strings"/> array into the program object.
        /// </summary>
        /// <param name="context">Must be a valid OpenCL context.</param>
        /// <param name="count">The number of source code strings that are provided.</param>
        /// <param name="strings">An array of <see cref="count"/> pointers to optionally null-terminated character strings that make up the source code.</param>
        /// <param name="lengths">
        /// An array with the number of chars in each string (the string length). If an element in <see cref="lengths"/> is zero, its accompanying string is null-terminated. If lengths is <c>null</c>, all strings in the strings argument are
        /// considered null-terminated. Any length value passed in that is greater than zero excludes the null terminator in its count.
        /// </param>
        /// <param name="errorCode">Returns an appropriate error code. If errorCode is <c>null</c>, no error code is returned.</param>
        /// <returns>
        /// Returns a valid non-zero program object and <see cref="errorCode"/> is set to <c>Result.Success</c> if the program object is created successfully. Otherwise, it returns a <c>null</c> value with one of the following error values
        /// returned in <see cref="errorCode"/>:
        /// 
        /// <c>Result.InvalidContext</c> if <see cref="context"/> is not a valid context.
        /// 
        /// <c>Result.InvalidValue</c> if <see cref="count"/> is zero or if strings or any entry in strings is <c>null</c>.
        /// 
        /// <c>Result.OutOfResources</c> if there is a failure to allocate resources required by the OpenCL implementation on the device.
        /// 
        /// <c>Result.OutOfHostMemory</c> if there is a failure to allocate resources required by the OpenCL implementation on the host.
        /// </returns>
        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clCreateProgramWithSource")]
        public static extern IntPtr CreateProgramWithSource(
            [In] IntPtr context,
            [In][MarshalAs(UnmanagedType.U4)] uint count,
            [In][MarshalAs(UnmanagedType.LPArray)] IntPtr[] strings,
            [In][MarshalAs(UnmanagedType.LPArray)] UIntPtr[] lengths,
            [Out][MarshalAs(UnmanagedType.I4)] out NativeCLResult errorCode
        );

        /// <summary>
        /// Decrements the program reference count.
        /// </summary>
        /// <param name="program">The program to release.</param>
        /// <returns>
        /// Returns <c>Result.Success</c> if the function is executed successfully. Otherwise, it returns one of the following errors:
        /// 
        /// <c>Result.InvalidProgram</c> if <see cref="program"/> is not a valid program object.
        /// 
        /// <c>Result.OutOfResources</c> if there is a failure to allocate resources required by the OpenCL implementation on the device.
        /// 
        /// <c>Result.OutOfHostMemory</c> if there is a failure to allocate resources required by the OpenCL implementation on the host.
        /// </returns>
        [DllImport(OPENCL_DLL_NAME, EntryPoint = "clReleaseProgram")]
        public static extern NativeCLResult ReleaseProgram(
            [In] IntPtr program
        );

        /// <summary>
        /// Returns information about the program object.
        /// </summary>
        /// <param name="program">Specifies the program object being queried.</param>
        /// <param name="parameterName">Specifies the information to query.</param>
        /// <param name="parameterValueSize">Used to specify the size in bytes of memory pointed to by <see cref="parameterValue"/>. This size must be greater or equal to the size of the return type.</param>
        /// <param name="parameterValue">A pointer to memory where the appropriate result being queried is returned. If <see cref="parameterValue"/> is <c>null</c>, it is ignored.</param>
        /// <param name="parameterValueSizeReturned">The actual size in bytes of data copied to <see cref="parameterValue"/>. If <see cref="parameterValueSizeReturned"/> is <c>null</c>, it is ignored.</param>
        /// <returns>
        /// Returns <c>Result.Success</c> if the function is executed successfully. Otherwise, it returns the following:
        /// 
        /// <c>Result.InvalidValue</c> if <see cref="parameterName"/> is not valid, or if size in bytes specified by <see cref="parameterValueSize"/> is less than the size of return type and <see cref="parameterValue"/> is not <c>null</c>.
        /// 
        /// <c>Result.InvalidProgram</c> if <see cref="program"/> is not a valid program object.
        /// 
        /// <c>Result.InvalidProgramExecutable</c> if <see cref="parameterName"/> is <c>ProgramInformation.NumberOfKernels</c> or <c>ProgramInformation.KernelNames</c> and a successful program executable has not been built for at least one
        /// device in the list of devices associated with <see cref="program"/>.
        /// 
        /// <c>Result.OutOfResources</c> if there is a failure to allocate resources required by the OpenCL implementation on the device.
        /// 
        /// <c>Result.OutOfHostMemory</c> if there is a failure to allocate resources required by the OpenCL implementation on the host.
        /// </returns>
        [DllImport("OpenCL", EntryPoint = "clGetProgramInfo")]
        public static extern NativeCLResult GetProgramInformation(
            [In] IntPtr program,
            [In][MarshalAs(UnmanagedType.U4)] ProgramInformation parameterName,
            [In] UIntPtr parameterValueSize,
            [Out] byte[] parameterValue,
            [Out] out UIntPtr parameterValueSizeReturned
        );

        /// <summary>
        /// Builds (compiles and links) a program executable from the program source or binary.
        /// </summary>
        /// <param name="program">The program object.</param>
        /// <param name="numberOfDevices">The number of devices listed in <see cref="deviceList"/>.</param>
        /// <param name="deviceList">
        /// A pointer to a list of devices associated with <see cref="program"/>. If <see cref="deviceList"/> is a <c>null</c> value, the program executable is built for all devices associated with <see cref="program"/> for which a source
        /// or binary has been loaded. If <see cref="deviceList"/> is a non-<c>null</c> value, the program executable is built for devices specified in this list for which a source or binary has been loaded.
        /// </param>
        /// <param name="options">A pointer to a null-terminated string of characters that describes the build options to be used for building the program executable. Certain options are ignored when program is created with IL.</param>
        /// <param name="notificationCallback">
        /// A function pointer to a notification routine. The notification routine is a callback function that an application can register and which will be called when the program executable has been built (successfully or unsuccessfully).
        /// If <see cref="notificationCallback"/> is not <c>null</c>, <see cref="BuildProgram"/> does not need to wait for the build to complete and can return immediately once the build operation can begin. The build operation can begin if
        /// the context, program whose sources are being compiled and linked, list of devices and build options specified are all valid and appropriate host and device resources needed to perform the build are available. If
        /// <see cref="notificationCallback"/> is <c>null</c>, <see cref="BuildProgram"/> does not return until the build has completed. This callback function may be called asynchronously by the OpenCL implementation. It is the
        /// application’s responsibility to ensure that the callback function is thread-safe.
        /// </param>
        /// <param name="userData">Passed as an argument when <see cref="notificationCallback"/> is called. <see cref="userData"/> can be <c>null</c>.</param>
        /// <returns>Returns <c>Result.Success</c> if the function is executed successfully. Otherwise, it returns an error.</returns>
        [DllImport("OpenCL", EntryPoint = "clBuildProgram")]
        public static extern NativeCLResult BuildProgram(
            [In] IntPtr program,
            [In][MarshalAs(UnmanagedType.U4)] uint numberOfDevices,
            [In][MarshalAs(UnmanagedType.LPArray)] IntPtr[] deviceList,
            [In][MarshalAs(UnmanagedType.LPStr)] string options,
            [In] IntPtr notificationCallback,
            [In] IntPtr userData
        );
    }
}