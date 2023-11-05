
namespace OpenCL.NET.Interop
{
    public static partial class Devices
    {
        // Type alias defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L42C9-L42C29
        // Levels defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L307-L431

        [Flags]
        public enum DeviceType : UInt64
        {
            Default = 1 << 0,
            Cpu = 1 << 1,
            Gpu = 1 << 2,
            Accelerator = 1 << 3,
            Custom = 1 << 4,
            All = 0xFFFFFFFF
        }
    }
}