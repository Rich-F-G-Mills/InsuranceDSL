
namespace OpenCL.NET.Interop
{
    public static partial class Devices
    {
        // Type alias defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L54C29-L54C57
        // Levels defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L485-L488

        /// <summary>
        /// Represents an enumeration for the different device partition properties.
        /// </summary>
        public enum DevicePartitionProperty : UInt32
        {
            /// <summary>
            /// Partitions the device equally among sub-devices.
            /// </summary>
            PartitionEqually = 0x1086,

            /// <summary>
            /// Partitions the device among sub-devices by counts.
            /// </summary>
            PartitionByCounts = 0x1087,

            /// <summary>
            /// Marks the end of the <see cref="PartitionByCounts"/> list.
            /// </summary>
            PartitionByCountsListEnd = 0x0,

            /// <summary>
            /// Partitions the device among sub-devices along a cache line.
            /// </summary>
            PartitionByAffinityDomain = 0x1088
        }
    }
}