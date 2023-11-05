
namespace OpenCL.NET.Interop
{
    public static partial class Devices
    {
        // Type alias defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L48C29-L48C56
        // Levels defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L457-L458

        /// <summary>
        /// Represents an enumeration for the different execution capabilities of devices.
        /// </summary>
        [Flags]
        public enum DeviceExecutionCapabilities : UInt64
        {
            /// <summary>
            /// The OpenCL device can execute OpenCL kernels.
            /// </summary>
            Kernel = 1 << 0,

            /// <summary>
            /// The OpenCL device can execute native kernels.
            /// </summary>
            NativeKernel = 1 << 1
        }
    }
}