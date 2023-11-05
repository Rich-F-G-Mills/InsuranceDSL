
open System
open System.Text

open FsToolkit.ErrorHandling

open OpenCL.NET



result {
    let! platforms =
        Platforms.getPlatformIds ()

    let! platformInfo =
        platforms
        |> Seq.map Platforms.getPlatformInformation
        |> Seq.sequenceResultM
        |> Result.map Seq.toArray

    printfn "Platform Info: %A" platformInfo

    let! deviceIds =
        Devices.getDeviceIds platforms[0] Devices.DeviceType.All

    let! deviceInfo =
        deviceIds
        |> Seq.map (fun did -> Devices.getDeviceInformation (did, DeviceInformation.Version))
        |> Seq.sequenceResultM
        |> Result.map Seq.toArray

    printfn "Device Info: %A" deviceInfo
} |> ignore
