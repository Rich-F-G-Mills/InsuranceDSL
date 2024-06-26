﻿
namespace OpenCL.NET.Interop
{
    public static partial class Contexts
    {
        // Type alias defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L59
        // Levels defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L468-L474
        public enum ContextInformation : UInt32
        {
            /// <summary>
            /// The context reference count. The reference count returned should be considered immediately stale. It is unsuitable for general use in applications. This feature is provided for identifying memory leaks.
            /// </summary>
            ReferenceCount = 0x1080,

            /// <summary>
            /// The list of devices in the context.
            /// </summary>
            Devices = 0x1081,

            /// <summary>
            /// The properties argument specified in <see cref="CreateContext"/> or <see cref="CreateContextFromType"/>.
            /// </summary>
            Properties = 0x1082,

            /// <summary>
            /// The number of devices in context.
            /// </summary>
            NumberOfDevices = 0x1083
        }
    }
}