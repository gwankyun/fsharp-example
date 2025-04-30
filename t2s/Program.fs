// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO
open Common
open extra

let t2s path =
    let entryList path =
        Directory.traverse path

    let dirEntryList, fileEntryList =
        entryList path

    for i in dirEntryList do
        printfn $"d: %A{i}"

    for p in fileEntryList do
        let i = p |> Entry.path
        Process.start "opencc" [
            "-i"; i;
            "-c"; @"C:\local\vcpkg\installed\x64-windows\share\opencc\t2s.json";
            "-o"; i] |> ignore

[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        printfn "path: %s" path
        match Directory.exists path with
        | false -> printfn "path not exists"
        | true ->
            t2s path
    | _ -> printfn "Usage: t2s <path>"
    0 // return an integer exit code
