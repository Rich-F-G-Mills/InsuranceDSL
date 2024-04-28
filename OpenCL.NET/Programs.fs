
namespace OpenCL.NET

open System
open System.Runtime.InteropServices
open FsToolkit.ErrorHandling
open Common


module rec Programs =

    type ProgramHandle =
        internal | ProgramHandle of IntPtr

        interface IDisposable with
            member this.Dispose () =
                do releaseProgram this |> ignore


    let createProgramWithSource (Contexts.ContextHandle context) sourceCode =
        result {
            let sourceCode' =
                Marshal.StringToHGlobalAnsi sourceCode

            let sourceCodeLen =
                sourceCode |> String.length |> unativeint

            let! program =
                Interop.Programs.CreateProgramWithSource (context, 1u, [|sourceCode'|], [|sourceCodeLen|])

            return (ProgramHandle program)
        }

    let releaseProgram (ProgramHandle program) =
        result {
            return! Interop.Programs.ReleaseProgram (program)
        }

    let buildProgram (ProgramHandle program) =
        result {

            return! Interop.Programs.BuildProgram (program, 0u, null, null, IntPtr.Zero, IntPtr.Zero)
        }

        
    [<RequireQualifiedAccess>]
    module Information =

        type private NativeProgramInformation =
            Interop.Programs.ProgramInformation


        let ReferenceCount: InformationMapper.ValueTypeInfo<_, uint32, _> =
            InformationMapper.ValueTypeInfo (NativeProgramInformation.ReferenceCount, id)

        let Context =
            InformationMapper.ValueTypeInfo (NativeProgramInformation.Context, Contexts.ContextHandle)

        let NumberOfDevices: InformationMapper.ValueTypeInfo<_, uint32, _> =
            InformationMapper.ValueTypeInfo (NativeProgramInformation.NumberOfDevices, id) 

        let Devices =
            InformationMapper.ValueTypeInfoArray (NativeProgramInformation.Devices, OpenCL.NET.Devices.DeviceId) 

    let ProgramInformation =
        let mapper (ProgramHandle program, nativeRef, size, buffer) =
            Interop.Programs.GetProgramInformation (program, nativeRef, size, buffer)

        new InformationMapper<_, _> (mapper)
    
    