// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO
open Common

type Entry =
    | DirEntry of path: string
    | FileEntry of path: string

module Entry =
    let path (entry: Entry) =
        match entry with
        | DirEntry path -> path
        | FileEntry path -> path

module Directory =
    /// <summary>遍历指定路径下的所有目录和文件</summary>
    /// <param name="path">要遍历的路径</param>
    /// <returns>返回一个元组，包含目录条目列表和文件条目列表</returns>
    let traverse path =
        let rec innerTraverse entryList dirResult fileResult=
            match entryList with
            | [] -> dirResult, fileResult
            | entry::rest ->
                match entry with
                | DirEntry path ->
                    let dirList =
                        Directory.enumerateDirectories path
                        |> Seq.map DirEntry
                        |> Seq.toList
                    let fileList =
                        Directory.enumerateFiles path
                        |> Seq.map FileEntry
                        |> Seq.toList
                    let concat a b = List.concat [ a; b ]
                    let rest = concat rest dirList
                    let fileResult = concat fileResult fileList
                    let dirResult = concat dirResult dirList
                    innerTraverse rest dirResult fileResult
                | FileEntry _ -> dirResult, fileResult
        match Directory.exists path with
        | false -> [], []
        | true ->
            innerTraverse [DirEntry path] [] []

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
