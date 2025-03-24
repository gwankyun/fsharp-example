// For more information see https://aka.ms/fsharp-console-apps
open System.IO

printfn "Hello from F#"

type Color =
    | Red = 1
    | Green = 2
    | Blue = 3

let red = enum<Color> 1

let green : Color = enum 2

printfn $"red: %A{red}"
printfn $"green: %A{green}"

// type Base() =
//     abstract member F : uint -> unit
//     default u.F() : unit =
//         printfn $"F Base"

// type Derived() =
//     inherit Base()
//     override u.F() = 
//         printfn $"F Derived"

// let d : Derived = Derived()

// d.F()

type Entry =
    | DirEntry of path: string
    | FileEntry of path: string

module Path =
    let getRelativePath relativeTo path =
        Path.GetRelativePath(relativeTo, path)

module Directory =
    let enumerateDirectories path =
        Directory.EnumerateDirectories(path)

    let enumerateFiles path =
        Directory.EnumerateFiles(path)

    let rec innerTraverse entryList dirResult fileResult=
        match entryList with
        | [] -> dirResult, fileResult
        | entry::rest ->
            match entry with
            | DirEntry path ->
                let dirList =
                    enumerateDirectories path
                    |> Seq.map DirEntry
                    |> Seq.toList
                let fileList =
                    enumerateFiles path
                    |> Seq.map FileEntry
                    |> Seq.toList
                let concat a b = List.concat [ a; b ]
                let rest = concat rest dirList
                let fileResult = concat fileResult fileList
                let dirResult = concat dirResult dirList
                innerTraverse rest dirResult fileResult
            | FileEntry _ -> dirResult, fileResult

    let exists path =
        Directory.Exists(path)

    /// <summary>遍历指定路径下的所有目录和文件</summary>
    /// <param name="path">要遍历的路径</param>
    /// <returns>返回一个元组，包含目录条目列表和文件条目列表</returns>
    let traverse path =
        match exists path with
        | false -> [], []
        | true ->
            innerTraverse [DirEntry(path)] [] []

let entryList path =
    Directory.traverse path

let dirEntryList, fileEntryList =
    entryList @"E:\local\fsharp-example\compare\dest"

for i in dirEntryList do
    printfn $"d: %A{i}"

for i in fileEntryList do
    printfn $"f: %A{i}"
