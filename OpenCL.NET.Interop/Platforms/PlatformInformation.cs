
namespace OpenCL.NET.Interop
{
    public static partial class Platforms
    {
        // https://github.com/OpenCL/GEGL-OpenCL/blob/d5e8cd3f96c050faa96f8cded31c919780ce6f0a/gegl/opencl/cl.h#L156
        public enum PlatformInformation : UInt32
        {
            Profile = 0x0900,
            Version = 0x0901,
            Name = 0x0902,
            Vendor = 0x0903,
            Extensions = 0x0904
        }
    }
}