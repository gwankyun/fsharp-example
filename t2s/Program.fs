// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO
open System.Text.Json
open System.Text
open Common
open extra

type Config = {
   Config: string 
}

let t2s path (config: Config) =
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
            "-c"; config.Config;
            "-o"; i] |> ignore

[<EntryPoint>]
let main argv =
    let current = Directory.current
    printfn $"current: %s{current}"

    let config = Path.join current "t2s.json"
    let configContext = File.readAllTextEncoding config Encoding.UTF8

    let result =
        JsonSerializer.Deserialize<Config> configContext
    printfn $"t2s: %s{result.Config}"

    match argv with
    | [| path |] ->
        printfn "path: %s" path
        match Directory.exists path with
        | false -> printfn "path not exists"
        | true ->
            t2s path result
    | _ -> printfn "Usage: t2s <path>"
    0 // return an integer exit code
