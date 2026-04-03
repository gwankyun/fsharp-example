module DirStatus
open System.IO
open Status
open Serilog
open FSLogger
open System.Text
open System.Text.Json
open System.Text.Json.Serialization

// 1. Either create the serializer options from the F# options...
let options =
    JsonFSharpOptions.Default()
        // Add any .WithXXX() calls here to customize the format
        .ToJsonSerializerOptions()

// 启用格式化（带缩进）
options.WriteIndented <- true

let logger = Logger.ColorConsole

module Directory =
    let exists path =
        Directory.Exists(path)

module String =
    let startsWith (sub: string) (str: string) =
        str.StartsWith(sub)

module Path =
    let relativePath relativeTo path =
        Path.GetRelativePath(relativeTo, path)

let add path destination (exclude: string list option) =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    //printfn "添加操作：从 %s 到 %s" path destination
    logger.I $"path: {path} destination: {destination}"
    if exclude.IsSome then
        logger.I $"排除：{exclude.Value}"
    let fileList =
        Status.getChildItem path true
        |> Seq.map Status.toFileSystemInfoDetail
        |> Seq.map (fun x ->
            { x with FullName = Path.relativePath path x.FullName})
    let fileList =
        match exclude with
        | Some e ->
            fileList
            |> Seq.filter (fun x ->
                let startsWith i = x.Name |> String.startsWith i |> not
                List.forall startsWith e)
        | None -> fileList
    let text =
        JsonSerializer.Serialize<Status.FileSystemInfoDetail seq>(fileList, options)
    File.WriteAllText(destination, text, Encoding.UTF8)

let compare left right destination =
    printfn $"left: {left} right: {right} destination: {destination}"
    if Directory.exists left |> not then
        failwith $"{left} not exists"
    if Directory.exists right |> not then
        failwith $"{right} not exists"
    let fromPath path =
        let content = File.ReadAllText(path, Encoding.UTF8)
        JsonSerializer.Deserialize<Status.FileSystemInfoDetail seq>(content, options)
        //|> Seq.map (fun x -> x.FullName, x)
        //|> Map.ofSeq
    let leftJson = fromPath left
    let rightJson = fromPath right
    //let diff =
    //    Status.compareObject leftJson rightJson (fun x -> x.FullName)
    //    |> Map.map (fun x ->
    //        match x with
    //        | Some le, Some ri -> { Status.Difference.FullName = }
    //    )
    ()

let export path difference destination =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    printfn $"path: {path} difference: {difference} destination: {destination}"

let merge path destination =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    if Directory.exists destination |> not then
        failwith $"{destination} not exists"
    printfn $"path: {path} destination: {destination}"
