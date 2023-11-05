
namespace OpenCL.NET.Interop
{
    public static partial class Devices
    {
        // Type alias defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L45C29-L45C48
        // Levels defined: https://github.com/KhronosGroup/OpenCL-Headers/blob/2368105c0531069fe927989505de7d125ec58c55/CL/cl.h#L434-L445

        /// <summary>
        /// Represents an enumeration for the different floating point configurations of devices.
        /// </summary>
        [Flags]
        public enum DeviceFloatingPointConfiguration : UInt64
        {
            /// <summary>
            /// Denorms are supported.
            /// </summary>
            Denorm = 1 << 0,

            /// <summary>
            /// Inifinity (INF) and Not-a-Number's (NaNs) are supported.
            /// </summary>
            InfinityAndNotANumber = 1 << 1,

            /// <summary>
            /// Round to nearest even rounding mode is supported.
            /// </summary>
            RoundToNearest = 1 << 2,

            /// <summary>
            /// Round to zero rounding mode supported.
            /// </summary>
            RoundToZero = 1 << 3,

            /// <summary>
            /// Round to +ve and -ve infinity rounding modes supported.
            /// </summary>
            RoundToInfinity = 1 << 4,

            /// <summary>
            /// IEEE754-2008 fused multiply-add is supported.
            /// </summary>
            FusedMultiplyAdd = 1 << 5,

            /// <summary>
            /// Basic floating-point operations (such as addition, subtraction, multiplication) are implemented in software.
            /// </summary>
            SoftwareFloat = 1 << 6,

            /// <summary>
            /// Divide and sqrt are correctly rounded as defined by the IEEE754 specification.
            /// </summary>
            CorrectlyRoundedDivideSquareRoot = 1 << 7
        }
    }
}