
namespace OpenCL.NET.Interop
{
    public static partial class Devices
    {
        // Type alias defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L55
        // Levels defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L495-L500

        /// <summary>
        /// Describes the execution capabilities of the device.
        /// </summary>
        [Flags]
        public enum DeviceAffinityDomain : UInt64
        {
            /// <summary>
            /// Split the device into sub-devices comprised of compute units that share a NUMA node.
            /// </summary>
            Numa = 1 << 0,

            /// <summary>
            /// Split the device into sub-devices comprised of compute units that share a level 4 data cache.
            /// </summary>
            Level4Cache = 1 << 1,

            /// <summary>
            /// Split the device into sub-devices comprised of compute units that share a level 3 data cache.
            /// </summary>
            Level3Cache = 1 << 2,

            /// <summary>
            /// Split the device into sub-devices comprised of compute units that share a level 2 data cache.
            /// </summary>
            Level2Cache = 1 << 3,

            /// <summary>
            /// Split the device into sub-devices comprised of compute units that share a level 1 data cache.
            /// </summary>
            Level1Cache = 1 << 4,

            /// <summary>
            /// Split the device along the next partitionable affinity domain. The implementation shall find the first level along which the device or sub-device may be further subdivided in the order NUMA, L4, L3, L2, L1, and partition the
            /// device into sub-devices comprised of compute units that share memory subsystems at this level.
            /// </summary>
            NextPartitionable = 1 << 5
        }
    }
}