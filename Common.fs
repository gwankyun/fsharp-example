module Common
open System
open System.IO
open System.Text.RegularExpressions
open System.Diagnostics

module Directory =
    let getFileSystemEntries path searchPattern (searchOption: SearchOption) =
        Directory.GetFileSystemEntries(path, searchPattern, searchOption)

    let getAllFileSystemEntries path =
        getFileSystemEntries path "*" SearchOption.AllDirectories

    let createDirectory path =
        Directory.CreateDirectory(path)

    let exists path =
        Directory.Exists(path)

    let current =
        // Environment.CurrentDirectory
        Directory.GetCurrentDirectory()
    
    let baseDir =
        AppContext.BaseDirectory

module AppContext =
    let baseDir =
        AppContext.BaseDirectory

module String =
    type T = string
    let split (sep: T) (s: T) =
        s.Split(sep)

    let startsWith (value: T) (str: T) =
        str.StartsWith(value)

module Path =
    let getRelativePath relateTo path =
        Path.GetRelativePath(relateTo, path)

    let join (path1: string) path2 =
        Path.Join(path1, path2)

    let join3 (path1: string) path2 path3 =
        Path.Join(path1, path2, path3)

module File =
    let copy source dest overwrite =
        File.Copy(source, dest, overwrite)

    let move source dest overwrite =
        File.Move(source, dest, overwrite)

    let exists path =
        File.Exists(path)

    let readAlllines path =
        File.ReadAllLines(path)

    let readAlllinesEncoding path encoding =
        File.ReadAllLines(path, encoding)

module FileInfo =
    type T = FileInfo
    let ofFullName path =
        new T(path)

    let moveTo dest (src: T) =
        src.MoveTo(dest)

    let directoryName (file: T) =
        file.DirectoryName

    let rename name file =
        let dir = file |> directoryName
        file |> moveTo (Path.join dir name)

    let isDir (file: T) =
        let dir = FileAttributes.Directory
        file.Attributes &&& dir = dir

    let fullName (file: T) =
        file.FullName

module DirectoryInfo =
    type T = DirectoryInfo
    let create (path: T) =
        if not path.Exists then
            path.Create()

module Match =
    type T= Match
    let groupsArray (m: T) =
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

module Map =
    let difference (table1: Map<'k, 'v>) (table2: Map<'k, 'v>) =
        let toSet x = x |> Map.keys |> Set.ofSeq
        let keys1 = table1 |> toSet
        let keys2 = table2 |> toSet
        let diff = Set.difference keys1 keys2
        table1
        |> Map.filter (fun k v -> diff |> Set.contains k)

    let intersect (table1: Map<'k, 'v>) (table2: Map<'k, 'v>) =
        let toSet x = x |> Map.keys |> Set.ofSeq
        let keys1 = table1 |> toSet
        let keys2 = table2 |> toSet
        Set.intersect keys1 keys2
        |> Set.toSeq
        |> Seq.map (fun x -> x, (table1[x], table2[x]))
        |> Map.ofSeq

    let intersectWith (f: 'k -> 'v -> 'v -> 'v option) (table1: Map<'k, 'v>) (table2: Map<'k, 'v>) =
        let toSet x = x |> Map.keys |> Set.ofSeq
        let keys1 = table1 |> toSet
        let keys2 = table2 |> toSet
        Set.intersect keys1 keys2
        |> Set.fold (fun s t ->
            let v = f t table1[t] table2[t]
            match v with
            | Some value -> s |> Map.add t value
            | None -> s) Map.empty
