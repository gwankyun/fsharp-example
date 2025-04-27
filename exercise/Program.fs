// For more information see https://aka.ms/fsharp-console-apps
open System.IO
open Common
open System.Text.Json

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

module Entry =
    let path (entry: Entry) =
        match entry with
        | DirEntry path -> path
        | FileEntry path -> path

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

module Process =
    /// <summary>启动一个新的进程并执行指定的命令，同时捕获其标准输出。</summary>
    /// <param name="name">要执行的进程的可执行文件的名称。</param>
    /// <param name="args">传递给进程的参数列表。</param>
    /// <returns>进程执行后的标准输出结果。</returns>
    let start name args =
        let proc = new System.Diagnostics.Process()
        let info = proc.StartInfo
        info.FileName <- name
        for a in args do
            info.ArgumentList.Add(a)
        info.UseShellExecute <- false
        info.RedirectStandardOutput <- true

        proc.Start() |> ignore
        let result = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        result

let t2s path =
    let entryList path =
        Directory.traverse path

    let dirEntryList, fileEntryList =
        entryList path

    for i in dirEntryList do
        printfn $"d: %A{i}"

    for p in fileEntryList do
        // printfn $"f: %A{i}"
        let i = p |> Entry.path
        Process.start "opencc" [
            "-i"; i;
            "-c"; @"C:\local\vcpkg\installed\x64-windows\share\opencc\t2s.json";
            "-o"; i] |> ignore

// t2s @"c:\Users\liangjunquan\Downloads\C++\C++多線程\"

module DirCompare =
    let add path =
        let d, f = Directory.traverse path
        let relativePath p =
            Path.getRelativePath path (Entry.path p)
        let d =
            d
            |> List.map relativePath
        let f =
            f
            |> List.map relativePath
        d, f

let root = @"C:\Users\liangjunquan\AppData\Roaming\Tencent\WXWork\Update\Umi-OCR_Paddle_v2.1.5\UmiOCR-data\"
let d, f =
    Directory.traverse root

for i in d do
    let rela = Path.getRelativePath root (Entry.path i)
    printfn $"d: %A{rela}"

printfn "%A" Directory.baseDir

let config = Path.join Directory.baseDir "config.json"

type Config = {
    t2s: string
}

if File.exists config then
    printfn "config.json exists"
    let configStr = File.readAllText config
    let config =
        JsonSerializer.Deserialize<Config> configStr
    printfn $"t2s: %A{config.t2s}"
    t2s config.t2s
else
    printfn "config.json not exists"
    // File.writeAllTextEncoding
