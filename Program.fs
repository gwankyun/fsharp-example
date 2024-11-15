// For more information see https://aka.ms/fsharp-console-apps
open Common
open FSLogger
open Argu
open System.IO
open System.Text

let logger = Logger.ColorConsole

type CliArguments =
    | Version
    | Walk of dir: string
    | Compare of dir1: string * dir2: string
    | Add of path: string * saveFile: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version -> "版本號"
            | Walk dir -> "遍歷"
            | Compare (dir1, dir2) -> "對比"
            | Add (path, saveFile) -> "添加"

module Diff =
    let toFile path =
        let lst =
            Directory.getAllFileSystemEntries path
            |> Seq.ofArray
            |> Seq.map FileInfo.ofFullName
        let m =
            lst
            |> Seq.map (fun x -> Path.getRelativePath path x.FullName, x)
            |> Map.ofSeq
        m

    let sort (k1: FileInfo.T) (k2: FileInfo.T) =
        let level f =
            f
            |> FileInfo.fullName
            |> String.split @"\"
            |> Array.length
        compareWith level k1 k2

    let fileInfoToStr (f: FileInfo.T) =
        let t =
            if f |> FileInfo.isDir then "d" else "f"
        let lwt = f.LastWriteTime.ToFileTime()
        // let p = Path.getRelativePath path f.FullName
        $"%s{t}|%d{lwt}"

    let toSeq m=
        m
        |> Map.toSeq
        |> Seq.sortWith (fun (_, k1) (_, k2) ->
            sort k1 k2)
        |> Seq.map (fun (k, v) ->
            let f = fileInfoToStr v
            $"%s{f}|%s{k}")

let onCompare (compareArg: string * string) =
    let dir1, dir2 = compareArg
    let cp path = Path.join3 Directory.current "compare" path
    let file1 = Diff.toFile (cp dir1)
    let file2 = Diff.toFile (cp dir2)
    logger.I $"{file1}"
    logger.I $"{file2}"

    let creation =
        Map.difference file2 file1
        |> Diff.toSeq

    let deletion =
        Map.difference file1 file2
        |> Diff.toSeq
        |> Seq.rev

    let modification =
        Map.intersectWith (fun k v2 v1 ->
            let isDir = FileInfo.isDir
            match (isDir v2), (isDir v1) with
            | true, true -> None
            | false, false ->
                match v2.LastWriteTime = v1.LastWriteTime with
                | true -> None
                | false -> Some v2
            | _, _ -> Some v2) file2 file1
        |> Diff.toSeq

    logger.I $"creation: %A{creation}"
    logger.I $"deletion: %A{deletion}"
    logger.I $"modification: %A{modification}"

    let testFile =
        Directory.getAllFileSystemEntries @"e:\local\fsharp\fs_test\testdir\"
        |> Seq.ofArray
        |> Seq.map FileInfo.ofFullName
        |> Seq.sortWith Diff.sort
    
    for i in testFile do
        logger.I $"{i}"

[<EntryPoint>]
let main args =
    let file = FileInfo.ofFullName @"e:\local\fsharp-example\README.md"

    logger.I $"{file.Name}"
    logger.I $"{file.FullName}"

    let parser = ArgumentParser.Create<CliArguments>(programName = "test.exe")

    let result = parser.Parse args

    let all = result.GetAllResults()

    logger.I $"{parser.PrintUsage()}"

    let version = result.TryGetResult Version
    if version.IsSome then
        logger.I $""

    let currentDir = Directory.current
    let baseDir = AppContext.baseDir
    logger.I $"{currentDir}"
    logger.I $"{baseDir}"

    let walk = result.TryGetResult Walk
    if walk.IsSome then
        logger.I $"{walk.Value}"
        let file =
            let notStartsWith sub x =
                x
                |> FileInfo.fullName
                |> String.startsWith (Path.join walk.Value sub)
                |> not
            Directory.getAllFileSystemEntries walk.Value
            |> Seq.ofArray
            |> Seq.map FileInfo.ofFullName
            |> Seq.filter (notStartsWith "build")
            |> Seq.filter (notStartsWith ".git")
            |> Seq.filter (notStartsWith "coroutine_example-cppcheck-build-dir")
        for i in file do
            logger.I $"{i}"

    let compareArg = result.TryGetResult Compare
    if compareArg.IsSome then
        onCompare compareArg.Value

    let onAdd path =
        let file =
            Diff.toFile path
            |> Diff.toSeq
        path, file

    let ofSeq path s =
        s
        |> Seq.map (fun x -> String.split @"|" x)
        |> Seq.map (fun x -> FileInfo.ofFullName (Path.join path x[2]))

    let addArg = result.TryGetResult Add
    if addArg.IsSome then
        let p, s = addArg.Value
        let path, file = onAdd p
        // for i in file do
        //     logger.I $"{i}"
        if s <> "" then
            File.WriteAllLines(s, file, Encoding.UTF8)
        let fileInfo = ofSeq path file
        for i in fileInfo do
            logger.I $"%A{i}"
            if i.Exists || (i |> FileInfo.isDir) then
                // failwith $"%A{i}"
                logger.I $"%A{i} exists"
            else
                failwith $"%A{i}"

    exit 0
