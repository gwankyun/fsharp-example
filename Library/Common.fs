// namespace Library
module Common
open System
open System.IO
open System.Text.RegularExpressions
open System.Diagnostics
open FSharpPlus
// open System.Text.Json

// type PathInfo = string

module Type =
    type Path = string

let s: Type.Path = ""

module Path =
    let getRelativePath relateTo path =
        Path.GetRelativePath(relateTo, path)

    let join (path1: string) path2 =
        Path.Join(path1, path2)

    let join3 (path1: string) path2 path3 =
        Path.Join(path1, path2, path3)

    let joinList (pathList: string list) =
        List.reduce join pathList

    let combine (path1: string) (path2: string) =
        Path.Combine(path1, path2)

    let directoryName (path: string) =
        Path.GetDirectoryName(path)

module FileInfo =
    let ofFullName path =
        new FileInfo(path)

    let moveTo dest (src: FileInfo) =
        src.MoveTo(dest)

    let directory (file: FileInfo) =
        file.Directory

    let directoryName (file: FileInfo) =
        file.DirectoryName

    let rename name file =
        let dir = file |> directoryName
        file |> moveTo (Path.join dir name)

    let isDir (file: FileInfo) =
        // let dir = FileAttributes.Directory
        // file.Attributes &&& dir = dir
        file.Exists |> not

    let fullName (file: FileInfo) =
        file.FullName

    let copyTo dest overwrite (file: FileInfo) =
        file.CopyTo(dest, overwrite)

    let lastWriteTime (file: FileInfo) =
        file.LastWriteTime

module Directory =
    let getFileSystemEntries path searchPattern (searchOption: SearchOption) =
        Directory.GetFileSystemEntries(path, searchPattern, searchOption)

    let getAllFileSystemEntries path =
        getFileSystemEntries path "*" SearchOption.AllDirectories

    let enumerateFileSystemEntriesAll path =
        Directory.EnumerateFileSystemEntries(path, "", SearchOption.AllDirectories)

    let createDirectory path =
        Directory.CreateDirectory(path)

    let createDir path =
        createDirectory path |> ignore

    let exists path =
        Directory.Exists(path)

    let createDirectoryFor (path: string) =
        let dir =
            FileInfo.ofFullName path
            |> FileInfo.directoryName
        if exists dir |> not then
            createDir dir

    let current =
        // Environment.CurrentDirectory
        Directory.GetCurrentDirectory()

    let baseDir =
        AppContext.BaseDirectory

    let delete path recursive =
        Directory.Delete(path, recursive)

    let deleteIfExists path recursive =
        if exists path then
            delete path recursive

    let enumerateFileSystemInfos path =
        let opt = new EnumerationOptions();
        opt.RecurseSubdirectories <- true;
        let info = new DirectoryInfo(path)
        info.EnumerateFileSystemInfos("*", opt);

    let enumerateDirectories path =
        Directory.EnumerateDirectories(path)

    let enumerateFiles path =
        Directory.EnumerateFiles(path)

// module DirectoryInfo =
//     let enumerateFileSystemInfos path =
//         let opt = new EnumerationOptions();
//         opt.RecurseSubdirectories <- true;
//         let info = new DirectoryInfo(path)
//         info.EnumerateFileSystemInfos("*", opt);

module AppContext =
    let baseDir =
        AppContext.BaseDirectory

module String =
    // type _T = string
    // let split (sep: _T) (s: _T) =
    //     s.Split(sep)

    let startsWith (value: string) (str: string) =
        str.StartsWith(value)


module File =
    let copy source dest overwrite =
        File.Copy(source, dest, overwrite)

    let move source dest overwrite =
        File.Move(source, dest, overwrite)

    let exists path =
        File.Exists(path)

    let delete path =
        File.Delete(path)

    let readAlllines path =
        File.ReadAllLines(path)

    let readAlllinesEncoding path encoding =
        printfn $"[{__LINE__}] ${path}"
        if path |> FileInfo.ofFullName |> FileInfo.isDir then
            failwith $"{path} is Dir"
        File.ReadAllLines(path, encoding)

    let readAllText path =
        File.ReadAllText(path)

    let readAllTextEncoding path encoding =
        File.ReadAllText(path, encoding)

    let writeAllText path (contents: string) =
        File.WriteAllText(path, contents)

    let writeAllTextEncoding path (contents: string) encoding =
        File.WriteAllText(path, contents, encoding)

    let writeAllLines path (contents: string seq) =
        File.WriteAllLines(path, contents)

    let writeAllLinesEncoding path (contents: string seq) encoding =
        File.WriteAllLines(path, contents, encoding)

module DirectoryInfo =
    // type T = DirectoryInfo
    let create (path: DirectoryInfo) =
        if not path.Exists then
            path.Create()

    let ofFullName path =
        new DirectoryInfo(path)

    let getDirectories (dir: DirectoryInfo) =
        dir.GetDirectories()

    let getFiles (dir: DirectoryInfo) =
        dir.GetFiles()

module FileSystemInfo =
    let lastWriteTime (info: FileSystemInfo) =
        info.LastWriteTime

module Match =
    // type T = Match
    let groupsArray (m: Match) =
        m.Groups
        |> Seq.map (fun x -> x.Value)
        |> Seq.toArray

module Regex =
    let isMatch pattern (input: string) =
        Regex.IsMatch(input, pattern)

    let matchFirst pattern input =
        Regex.Match(input, pattern)

    let matchFirstGroups pattern input =
        Regex.Match(input, pattern)
        |> Match.groupsArray

    let tryMatchFirstValue pattern input =
        let mf = matchFirst pattern input
        match mf.Length with
        | 0 -> None
        | _ -> Some mf.Value

    let split pattern input =
        Regex.Split(input, pattern)

module Seq =
    let partition pred seq =
        let a = seq |> Seq.filter pred
        let b = seq |> Seq.filter (pred >> not)
        a, b

module Diagnostics =
    let startProcess name args =
        let proc = new Process()
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

module Environment =
    let getVariable variable =
        Environment.GetEnvironmentVariable(variable)

let compareWith f a b =
    compare (f a) (f b)

//let mapTuple (f: 'a -> 'b) (tuple: 'a * 'a) : 'b * 'b =
//    let x, y = tuple  // 解构输入元组为两个 'a 元素
//    (f x, f y)

module Tuple2 =
    let inline map f (a, b) = f a, f b

    let inline swap (a, b) = b, a

    let inline apply f (a, b) = f a b

module Map =
    //let kesy

    //let 

    //let keysSet (table1: Map<'k, 'v>) (table2: Map<'k, 'v>) =
    //    let toSet x = x |> Map.keys |> Set.ofSeq
    //    let keys1 = table1 |> toSet
    //    let keys2 = table2 |> toSet
    //    keys1, keys2


    /// <summary>兩個表的交集</summary>
    /// <param name="table1"></param>
    /// <param name="table2"></param>
    /// <typeparam name="'k"></typeparam>
    /// <typeparam name="'v"></typeparam>
    /// <returns></returns>
    let intersectK (table1: Map<'k, 'v>) (table2: Map<'k, 'v>) =
        let toSet x = x |> Map.keys |> Set.ofSeq
        let keys1 = table1 |> toSet
        let keys2 = table2 |> toSet
        Set.intersect keys1 keys2
        |> Set.toSeq
        |> Seq.map (fun x -> x, (table1[x], table2[x]))
        |> Map.ofSeq

    /// <summary>兩個表的交集</summary>
    /// <param name="f"></param>
    /// <param name="table1"></param>
    /// <param name="table2"></param>
    /// <typeparam name="'k"></typeparam>
    /// <typeparam name="'v"></typeparam>
    /// <returns></returns>
    let intersectWithK (f: 'k -> 'v -> 'v -> 'v option) (table1: Map<'k, 'v>) (table2: Map<'k, 'v>) =
        let toSet x = x |> Map.keys |> Set.ofSeq
        let keys1 = table1 |> toSet
        let keys2 = table2 |> toSet
        Set.intersect keys1 keys2
        |> Set.fold (fun s t ->
            let v = f t table1[t] table2[t]
            match v with
            | Some value -> s |> Map.add t value
            | None -> s) Map.empty

    // 對比
    let compare (table1: Map<'k, 'v>) (table2: Map<'k, 'v>) =
        let toSet x = x |> Map.keys |> Set.ofSeq
        let keys1, keys2 = Tuple2.map toSet (table1, table2)
        Set.union keys1 keys2
        |> Set.toSeq
        |> Seq.map (fun x ->
            let tryFind = Map.tryFind x
            let tuple = Tuple2.map tryFind (table1, table2)
            x, tuple)
        |> Map.ofSeq

    /// <summary>`table1` - `table2`，以鍵為對比</summary>
    /// <param name="table1"></param>
    /// <param name="table2"></param>
    /// <typeparam name="'k"></typeparam>
    /// <typeparam name="'v"></typeparam>
    /// <returns></returns>
    let difference (table1: Map<'k, 'v>) (table2: Map<'k, 'v>) =
        compare table1 table2
        |> Map.chooseValues (fun v ->
            match v with
            | Some v1, None -> Some v1
            | _ -> None)

    let compareWith (f: 'k -> 'v -> 'v -> 'v option) (table1: Map<'k, 'v>) (table2: Map<'k, 'v>) =
        let u = compare table1 table2
        let keys = u |> Map.keys
        Seq.fold (fun s t ->
            let v = u[t]
            let r =
                match v with
                | Some v1, Some v2 -> f t v1 v2
                | Some v1, _ -> Some v1
                | _, Some v2 -> Some v2
                | _, _ -> None
            match r with
            | Some result -> s |> Map.add t result
            | None -> s
            ) Map.empty keys

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

module extra =
    type Entry =
        | DirEntry of path: string
        | FileEntry of path: string

    module Entry =
        let path (entry: Entry) =
            match entry with
            | DirEntry path -> path
            | FileEntry path -> path

        let lastWriteTime (entry: Entry) =
            let path = path entry
            path
            |> FileInfo.ofFullName
            |> FileInfo.lastWriteTime

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
