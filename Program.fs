// For more information see https://aka.ms/fsharp-console-apps
open Common
open FSLogger
open Argu
open System.IO
open System.Text
open Expecto
open FSharpPlus

let logger = Logger.ColorConsole

type CliArguments =
    | Version
    | Walk of dir: string
    | Compare of dir1: string * dir2: string
    | Add of path: string * saveFile: string
    | Test of dir: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version -> "版本號"
            | Walk dir -> "遍歷"
            | Compare (dir1, dir2) -> "對比"
            | Add (path, saveFile) -> "添加"
            | Test dir -> "測試"

module Diff =
    /// <summary>生成指定路徑的文件狀態</summary>
    /// <param name="path">路徑</param>
    /// <returns></returns>
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
            |> String.split [@"\"]
            |> Seq.length
        compareWith level k1 k2

    let fileInfoToStr (f: FileInfo.T) =
        let t =
            if f |> FileInfo.isDir then "d" else "f"
        let lwt = f.LastWriteTime.ToFileTime()
        // let p = Path.getRelativePath path f.FullName
        $"%s{t}|%d{lwt}"

    /// <summary></summary>
    /// <param name="m"></param>
    /// <returns></returns>
    let toSeq m=
        m
        |> Map.toSeq
        |> Seq.sortWith (fun (_, k1) (_, k2) ->
            sort k1 k2)
        |> Seq.map (fun (k, v) ->
            let f = fileInfoToStr v
            $"%s{f}|%s{k}")

// let onCompare (compareArg: string * string) =
//     let dir1, dir2 = compareArg
//     let cp path = Path.join3 Directory.current "compare" path
//     let file1 = Diff.toFile (cp dir1)
//     let file2 = Diff.toFile (cp dir2)
//     logger.I $"{file1}"
//     logger.I $"{file2}"

//     let creation =
//         Map.difference file2 file1
//         |> Diff.toSeq

//     let deletion =
//         Map.difference file1 file2
//         |> Diff.toSeq
//         |> Seq.rev

//     let modification =
//         Map.intersectWith (fun k v2_ v1 ->
//             let isDir_ = FileInfo.isDir
//             match (isDir_ v2_), (isDir_ v1) with
//             | true, true -> None
//             | false, false ->
//                 match v2_.LastWriteTime = v1.LastWriteTime with
//                 | true -> None
//                 | false -> Some v2_
//             | _, _ -> Some v2_) file2 file1
//         |> Diff.toSeq

//     logger.I $"creation: %A{creation}"
//     logger.I $"deletion: %A{deletion}"
//     logger.I $"modification: %A{modification}"

//     let testFile =
//         Directory.getAllFileSystemEntries @"e:\local\fsharp\fs_test\testdir\"
//         |> Seq.ofArray
//         |> Seq.map FileInfo.ofFullName
//         |> Seq.sortWith Diff.sort

//     for i in testFile do
//         logger.I $"{i}"

let tests =
  test "A simple test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "The strings should equal"
  }

module VirtualFileInfo =
    type T =
        { RelativePath: string
          Type: string
          LastWriteTime: int64 }

        static member Default =
            { RelativePath = ""
              Type = ""
              LastWriteTime = 0L }

    let ofFileInfo path (info: FileInfo.T) =
        let rela = Path.getRelativePath path info.FullName
        let t = if FileInfo.isDir info then "d" else "f"
        let lwt = info.LastWriteTime.ToFileTime()
        { RelativePath = rela
          Type = t
          LastWriteTime = lwt }

    let toFileInfo path (info: T) =
        FileInfo.ofFullName
        <| Path.join path info.RelativePath

let rec copyDirectory (sourceDir: string) (destinationDir: string) (recursive: bool) =
    let dir = DirectoryInfo.ofFullName sourceDir

    let dirs = dir |> DirectoryInfo.getDirectories

    Directory.createDirectory destinationDir |> ignore

    for file in dir |> DirectoryInfo.getFiles do
        let targetFilePath = Path.combine destinationDir file.Name
        file |> FileInfo.copyTo targetFilePath true |> ignore

    if recursive then
        for subDir in dirs do
            let newDestDir = Path.combine destinationDir subDir.Name
            copyDirectory subDir.FullName newDestDir true

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
    // let baseDir = AppContext.baseDir
    // logger.I $"currentDir: {currentDir}"
    // logger.I $"baseDir: {baseDir}"

    // let walk = result.TryGetResult Walk
    // if walk.IsSome then
    //     logger.I $"{walk.Value}"
    //     let file =
    //         let notStartsWith sub x =
    //             x
    //             |> FileInfo.fullName
    //             |> String.startsWith (Path.join walk.Value sub)
    //             |> not
    //         Directory.getAllFileSystemEntries walk.Value
    //         |> Seq.ofArray
    //         |> Seq.map FileInfo.ofFullName
    //         |> Seq.filter (notStartsWith "build")
    //         |> Seq.filter (notStartsWith ".git")
    //         |> Seq.filter (notStartsWith "coroutine_example-cppcheck-build-dir")
    //     for i in file do
    //         logger.I $"{i}"

    // let compareArg = result.TryGetResult Compare
    // if compareArg.IsSome then
    //     onCompare compareArg.Value

    // let onAdd path =
    //     let file =
    //         Diff.toFile path
    //         |> Diff.toSeq
    //     path, file

    // let ofSeq path s =
    //     s
    //     |> Seq.map (fun x -> String.split @"|" x)
    //     |> Seq.map (fun x -> FileInfo.ofFullName (Path.join path x[2]))

    // let addArg = result.TryGetResult Add
    // if addArg.IsSome then
    //     let p, s = addArg.Value
    //     let path, file = onAdd p
    //     // for i in file do
    //     //     logger.I $"{i}"
    //     if s <> "" then
    //         File.WriteAllLines(s, file, Encoding.UTF8)
    //     let fileInfo = ofSeq path file
    //     for i in fileInfo do
    //         logger.I $"%A{i}"
    //         if i.Exists || (i |> FileInfo.isDir) then
    //             // failwith $"%A{i}"
    //             logger.I $"%A{i} exists"
    //         else
    //             failwith $"%A{i}"

    let testArgs : string array = Array.empty

    runTestsWithCLIArgs [] testArgs tests
    |> ignore

    // let addTests =
    //     test "add test" {
    //         // let subject = "Hello World"
    //         // Expect.equal subject "Hello World" "The strings should equal"
    //         // let file =
    //         //     Path.join3 currentDir "compare/add" "add.txt"
    //         //     // |> FileInfo.ofFullName
    //         // if File.exists file then
    //         //     File.Delete(file)
    //         // // if file.Exists then
    //         // //     File.Delete(file.FullName)
    //         // File.copy
    //         //     <| Path.join3 currentDir "compare" "add.txt"
    //         //     <| Path.join3 currentDir "compare" "add"
    //         //     <| true
    //         // let result = onAdd (Path.join "compare" "add")
    //         // logger.I $"%A{result}"

    //         let pathList = Path.joinList [ "1"; "2"; "3" ]
    //         Expect.equal pathList @"1\2\3" "joinList"

    //         let addPath = Path.joinList [ currentDir; "compare"; "test"; "add" ]
    //         File.copy <| Path.joinList [ currentDir; "compare"; "add.txt" ]
    //             <| Path.join addPath "add.txt"
    //             <| true
    //         let p, m = onAdd addPath
    //         logger.I $"%A{m}"
    //         Expect.equal p addPath ""
    //         // Expect.equal p addPath ""
    //     }

    // runTestsWithCLIArgs [] args addTests
    // |> ignore

    // logger.I $"{Appx.ba}"


    // let _ = (
    //     let result =
    //         Diff.toFile <| Path.joinList [ currentDir; "compare"; "test1" ]
    //     logger.I $"%A{result}"

    //     let readme =
    //         Path.joinList [ currentDir; "README.md" ] |> FileInfo.ofFullName
    //     logger.I $"{readme.DirectoryName}"
    //     logger.I $"{readme.Directory}"
    //     logger.I $"======================================"
    //     let basePath = Path.joinList [ currentDir; "compare" ]
    //     let file =
    //         Directory.getAllFileSystemEntries
    //         <| basePath
    //         |> Array.toSeq
    //         |> Seq.map FileInfo.ofFullName
    //         |> Seq.sortWith Diff.sort
    //         |> Seq.map (fun x -> {
    //             VirtualFileInfo.RelativePath = Path.getRelativePath basePath x.FullName
    //             VirtualFileInfo.LastWriteTime = x.LastWriteTime.ToFileTime()
    //             VirtualFileInfo.Type = if FileInfo.isDir x then "d" else "f"
    //         })
    //     for i in file do
    //         logger.I $"{i}"
    // )

    let testPath = result.TryGetResult Test
    if testPath.IsSome then
        let diffTests =
            test "add test" {
                let mutable testBase = testPath.Value
                if testBase = "" then
                    testBase <- Path.join currentDir "test"
                let src = Path.join testBase "src"

                // 初始初始文件
                if Directory.exists testBase |> not then
                    Directory.createDirectory testBase |> ignore

                if Directory.exists src |> not then
                    Directory.createDirectory src |> ignore

                File.writeAllTextEncoding (Path.join src "1.txt") "" Encoding.UTF8
                File.writeAllTextEncoding (Path.join src "2.txt") "" Encoding.UTF8

                let fileList = Directory.getAllFileSystemEntries src
                for i in fileList do
                    logger.I $"{i}"

                let vf =
                    fileList
                    |> Array.map FileInfo.ofFullName
                    |> Array.map (VirtualFileInfo.ofFileInfo src)

                let a = fileList |> Array.map FileInfo.ofFullName
                let e =
                    vf
                    |> Array.map (VirtualFileInfo.toFileInfo src)
                    |> Array.map FileInfo.fullName
                logger.I $"a: %A{a}"
                logger.I $"e: %A{e}"
                logger.I $"eq: %A{fileList = e}"

                Expect.equal
                    (fileList)
                    (e)
                    "VirtualFileInfo"

                let statesFile = Path.join currentDir "state1.txt"

                // 測試狀態寫入本地
                File.writeAllLinesEncoding
                    statesFile 
                    (vf
                     |> Array.map (fun x ->
                        $"%s{x.Type}|%d{x.LastWriteTime}|%s{x.RelativePath}"))
                    Encoding.UTF8

                // 讀取狀態
                let states =
                    File.readAlllinesEncoding statesFile Encoding.UTF8
                    |> Array.map (fun x -> String.split ["|"] x |> Array.ofSeq)
                    |> Array.map (fun x ->
                        { VirtualFileInfo.RelativePath = x[2]
                          VirtualFileInfo.Type = x[0]
                          VirtualFileInfo.LastWriteTime = int64 x[1] })

                Expect.equal
                    vf
                    states
                    "read = write"

                let dest = Path.join testBase "dest"
                logger.I $"dest: %s{dest}"

                if Directory.exists dest then
                    Directory.delete dest true

                Directory.createDirectory dest |> ignore

                copyDirectory src dest true
                // 新增文件
                let addFile =
                    let file = "add.txt"
                    let addFile = Path.joinList [ dest; file ]
                    File.writeAllTextEncoding addFile "" Encoding.UTF8
                    file

                // 刪除文件
                let deleteFile =
                    let file = "1.txt"
                    let deleteFile = Path.joinList [ dest; file ]
                    File.delete deleteFile
                    file

                // 修改文件
                let updateFile =
                    let file = "2.txt"
                    let updateFile = Path.joinList [ dest; file ]
                    File.writeAllTextEncoding updateFile "1" Encoding.UTF8
                    file

                let srcM = Diff.toFile src
                let destM = Diff.toFile dest

                // logger.I $"%A{srcM}"
                // logger.I $"%A{destM}"

                let u = Map.compare destM srcM
                logger.I $"u: %A{u}"

                let addFileM =
                    Map.difference destM srcM

                logger.I $"%A{addFileM}"

                let mapToList (m: Map<'k, 'v>) =
                    Map.toList >> (List.map fst) >> List.sort
                    <| m

                Expect.equal
                    (addFileM |> mapToList)
                    [addFile]
                    "addFile"

                let deleteFileM =
                    Map.difference srcM destM

                // logger.I $"%A{deleteFile}"

                Expect.equal
                    (deleteFileM |> mapToList)
                    [deleteFile]
                    "deleteFile"

                let updateFileM =
                    u
                    |> Map.filter (fun k v ->
                        match v with
                        | Some v1, Some v2 ->
                            let isDir = FileInfo.isDir
                            let v1d = isDir v1
                            let v2d = isDir v2
                            match v1d, v2d with
                            | false, false -> v1.LastWriteTime <> v2.LastWriteTime
                            | true, true -> false
                            | _, _ -> true
                        | _ -> false)

                logger.I $"updateFileM: %A{updateFileM}"

                Expect.equal
                    (updateFileM |> mapToList)
                    [updateFile]
                    "updateFile"
            }

        runTestsWithCLIArgs [] testArgs diffTests
        |> ignore

    runTestsWithCLIArgs [] testArgs CommonTest.mapTests
    |> ignore

    exit 0
