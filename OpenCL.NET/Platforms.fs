
namespace OpenCL.NET


module Platforms =
    
    open System
    open System.Text.RegularExpressions
    open FsToolkit.ErrorHandling
    open Common


    type PlatformId =
        internal PlatformId of nativeint

    let getPlatformIds () =
        result {
            let! found =
                Interop.Platforms.GetPlatformIds (0, null)

            let platformIds =
                Array.zeroCreate (int found)

            let! _ =
                Interop.Platforms.GetPlatformIds (found, platformIds)

            return
                platformIds |> Array.map PlatformId
        }


    [<RequireQualifiedAccess>]
    type PlatformProfile =
        | [<MapFromString("FULL_PROFILE")>] Full
        | [<MapFromString("EMBEDDED_PROFILE")>] Embedded

        static member val internal ofNativeStrict =
            createStrictMapperFromNativeToType<string, PlatformProfile>

    type PlatformVersion =
        { 
            Major: int
            Minor: int
            Information: string
        }

    type PlatformInformation =
        {
            Profile: PlatformProfile
            Version: PlatformVersion
            Name: string
            Vendor: string
            Extensions: string list
        }


    let private reVersion =
        new Regex (@"^OpenCL\s([0-9]+)\.([0-9]+)\s(.*)$")

    let getPlatformInformation (PlatformId platformId) =       
        result {
            let getInfoElementString name =
                extractString (fun (buffer, size) ->
                    Interop.Platforms.GetPlatformInformation (platformId, name, size, buffer))

            let! profile =
                getInfoElementString Interop.Platforms.PlatformInformation.Profile
                |> Result.map PlatformProfile.ofNativeStrict

            and! versionRaw =
                getInfoElementString Interop.Platforms.PlatformInformation.Version
                
            let versionMatch =
                reVersion.Match versionRaw

            let version =
                versionMatch.Groups
                |> Seq.map (fun g -> g.Value)
                |> Seq.toList
                |> function
                    | _::major::minor::information::[] -> { Major = int major; Minor = int minor; Information = information.Trim() }
                    | _ -> failwith (sprintf "Unable to process version string '%s'." versionRaw)

            let! name =
                getInfoElementString Interop.Platforms.PlatformInformation.Name

            and! vendor =
                getInfoElementString Interop.Platforms.PlatformInformation.Vendor

            and! extensions =
                getInfoElementString Interop.Platforms.PlatformInformation.Extensions
                |> Result.map (fun s -> s.Split (" ", StringSplitOptions.RemoveEmptyEntries))
                |> Result.map Array.toList

            return {
                Profile = profile
                Version = version
                Name = name
                Vendor = vendor
                Extensions = extensions                
            }
        }