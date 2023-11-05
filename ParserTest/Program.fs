﻿
open System
open System.IO
open System.Reflection

open FsToolkit.ErrorHandling
open Parser






[<EntryPoint>]
let main args =
    result {
        let content =
            File.ReadAllText "ProductSpec.txt"

        let! codeElements =
            Code.parseString content
            |> Result.teeError (printfn "FAILED: %A")

        let products =
            codeElements
            |> List.choose (function | ParsedElement.Product p -> Some p | _ -> None)
            
        do  products
            |> List.iter (fun p ->
                printfn "Product: '%s' ----->>>" p.Name

                p.DefinedVariables
                |> Map.values
                |> Seq.map (fun dv -> dv.Variable, dv.Expression)
                |> Seq.iter (fun (v, e) -> printfn "     Variable '%s' = %A" v.UnderlyingVariable.Name e)
            )

        return 0
    }
    |> Result.defaultValue -1