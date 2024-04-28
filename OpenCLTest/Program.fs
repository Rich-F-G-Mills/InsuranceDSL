
open System
open System.IO
open System.Text

open FsToolkit.ErrorHandling

open OpenCL.NET



result {
    let assembly =
        System.Reflection.Assembly.GetCallingAssembly()

    use stream =
        assembly.GetManifestResourceStream ($"{assembly.GetName().Name}.program.cl")

    use streamReader =
        new StreamReader (stream)

    let clProgram =
        streamReader.ReadToEnd ()

    do printfn "Program: %s" clProgram

    let! platformIds =
        Platforms.getPlatformIds ()

    let! deviceIds =
        Devices.getDeviceIds platformIds[0] Devices.DeviceType.Gpu

    use! context =
        Contexts.createContext deviceIds

    use! program =
        Programs.createProgramWithSource context clProgram

    let! devices =
        Programs.ProgramInformation.get (program, Programs.Information.Devices)

    do printfn "Ref count: %A" devices

    return 0
}
|> Result.teeError (printfn "OpenCL Error: %A")
|> ignore
