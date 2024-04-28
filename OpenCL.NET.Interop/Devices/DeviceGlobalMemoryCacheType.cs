
namespace OpenCL.NET.Interop
{ 
    public static partial class Devices
    {
        // Type alias defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L46C29-L46C53
        // Levels defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L448-L450

        /// <summary>
        /// Describes the type of global memory cache supported.
        /// </summary>
        public enum DeviceGlobalMemoryCacheType : UInt32
        {
            None = 0x0,
            ReadOnlyCache = 0x1,
            ReadWriteCache = 0x2
        }
    }
}