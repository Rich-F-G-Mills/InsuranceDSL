
namespace OpenCL.NET

open System
open FsToolkit.ErrorHandling
open Common


module rec Contexts =

    type ContextHandle =
        internal | ContextHandle of IntPtr

        interface IDisposable with
            member this.Dispose () =
                do releaseContext this |> ignore


    let createContext (deviceIds: Devices.DeviceId array) =
        result {
            let deviceIds' =
                deviceIds
                |> Array.map (fun (Devices.DeviceId did) -> did)

            let! context =
                Interop.Contexts.CreateContext
                    (IntPtr.Zero, uint deviceIds'.Length, deviceIds', IntPtr.Zero, IntPtr.Zero)

            return (ContextHandle context)
        }

    let releaseContext (ContextHandle context) =
        result {
            return! Interop.Contexts.ReleaseContext (context)
        }


    type private NativeContextInformation =
        Interop.Contexts.ContextInformation

    type ContextInformation =
        {
            ReferenceCount: int
            NumberOfDevices: int
            Devices: Devices.DeviceId array        
        }

    let private getContextInformationElement<'TValue> (ContextHandle context', nativeName) =
        let extractor (buffer, size) =
            Interop.Contexts.GetContextInformation (context', nativeName, size, buffer)

        castFromBytesToValue<'TValue> extractor

    let getContextInformation (context) =
        result {
            let! referenceCount =
                getContextInformationElement<uint> (context, NativeContextInformation.ReferenceCount)
                |> Result.map int

            let! numDevices =
                getContextInformationElement<uint> (context, NativeContextInformation.NumberOfDevices)
                |> Result.map int

            let! devices =
                getContextInformationElement<IntPtr []> (context, NativeContextInformation.Devices)
                |> Result.map (Array.map Devices.DeviceId)

            return {
                ReferenceCount = referenceCount
                NumberOfDevices = numDevices
                Devices = devices
            }
        }

