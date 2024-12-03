// For more information see https://aka.ms/fsharp-console-apps
open Common
open FSLogger
open Argu
open System.Text
open Expecto
open FSharpPlus
open State
open System.IO
// open System.Diagnostics
// open System.Runtime.CompilerServices
// open System.Runtime.InteropServices

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
            // Directory.getAllFileSystemEntries path
            Directory.enumerateFileSystemEntriesAll path
            |> Seq.toArray
            |> Seq.ofArray
            |> Seq.map FileInfo.ofFullName
        let m =
            lst
            |> Seq.map (fun x -> Path.getRelativePath path x.FullName, x)
            |> Map.ofSeq
        m

    let sort (k1: FileInfo) (k2: FileInfo) =
        let level f =
            f
            |> FileInfo.fullName
            |> String.split [@"\"]
            |> Seq.length
        compareWith level k1 k2

    let fileInfoToStr (f: FileInfo) =
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

let rec copyDirectory (sourceDir: string) (destinationDir: string) (recursive: bool) =
    let dir = DirectoryInfo.ofFullName sourceDir

    let dirs = dir |> DirectoryInfo.getDirectories

    Directory.createDir destinationDir

    for file in dir |> DirectoryInfo.getFiles do
        let targetFilePath = Path.combine destinationDir file.Name
        file |> FileInfo.copyTo targetFilePath true |> ignore

    if recursive then
        for subDir in dirs do
            let newDestDir = Path.combine destinationDir subDir.Name
            copyDirectory subDir.FullName newDestDir true

let createDirectoryFor (path: string) =
    let dir =
        FileInfo.ofFullName path
        |> FileInfo.directoryName
    // if dir <> null && (not dir.Exists) then
    //     dir.Create() |> ignore
    if Directory.exists dir |> not then
        Directory.createDir dir

module Main = 
    let writeAllText path contents =
        createDirectoryFor path
        File.writeAllTextEncoding path contents Encoding.UTF8

    let initSrc src =
        writeAllText (Path.join src @"delete.txt") ""
        writeAllText (Path.join src @"update.txt") ""
        writeAllText (Path.join src @"u\update.txt") ""

        writeAllText (Path.join src @"d\delete.txt") ""

    let changeFile dest =
        // 新增文件
        let addFile =
            let file = [ ("add.txt", "f"); (@"a\add2.txt", "f") ]
            for (i, _) in file do
                let addFile = Path.joinList [ dest; i ]
                writeAllText addFile ""
            ("a", "d")::file

        // 刪除文件
        let deleteFile =
            let file = [ ("delete.txt", "f"); (@"d\delete.txt", "f") ]
            for (i, _) in file do
                let deleteFile = Path.joinList [ dest; i ]
                File.delete deleteFile
            Directory.delete (Path.join dest "d") true
            ("d", "d")::file |> List.rev

        // 修改文件
        let updateFile =
            let file = [ ("update.txt", "f"); (@"u\update.txt", "f") ]
            for i, _ in file do
                let updateFile = Path.joinList [ dest; i ]
                writeAllText updateFile "1"
            file
        // addFile, updateFile, deleteFile
        {
            State.Difference.Creation = addFile
            State.Difference.Modification = updateFile
            State.Difference.Deletion = deleteFile
        }

    // let testSort =

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

    let testDiff testBase =
        if Directory.exists testBase then
            Directory.delete testBase true
        Directory.createDir testBase

        let src = Path.join testBase "src"
        let history = Path.join testBase "history"
        let dest = Path.join testBase "dest"
        let diff = Path.join testBase "diff"

        // 初始初始文件
        if Directory.exists testBase |> not then
            Directory.createDir testBase

        Directory.createDir src
        Directory.createDir dest

        Directory.createDir history

        Main.initSrc src

        // 測試排序
        let _ = (
            let pathList = [ @"0"; "1"; "10"; @"0\0"; @"0\1"; @"0\0\0" ]
            Expect.equal
                pathList
                (List.sortWith VirtualFileInfo.sort pathList)
                "path sort"
        )

        let srcState = State.create src

        let _ = (
            let fileList =
                // Directory.getAllFileSystemEntries src
                Directory.enumerateFileSystemEntriesAll src
                |> Seq.toArray
                // |> Seq.map Fin
                |> Array.sortWith VirtualFileInfo.sort
            // for i in fileList do
            //     logger.I $"{__LINE__} {i}"

            let e =
                srcState
                |> State.toArray
                |> Array.map (VirtualFileInfo.toFileInfo src)
                |> Array.map FileInfo.fullName
            // logger.I $"a: %A{a}"
            // logger.I $"e: %A{e}"
            // logger.I $"eq: %A{fileList = e}"

            Expect.equal
                (fileList)
                (e)
                "VirtualFileInfo"
        )

        let srcStatePath = Path.join history "src.txt"
        let destStatePath = Path.join history "dest.txt"

        // 測試狀態寫入本地
        State.write srcStatePath srcState

        // 讀取狀態
        let _ = (
            let srcStateRead = State.read srcStatePath

            Expect.equal
                srcState
                srcStateRead
                "read = write"
        )

        copyDirectory src dest true

        // 修改dest
        let diffFile = Main.changeFile dest

        // 保存dest当前狀態
        let destState = State.create dest

        let difference = State.diff destState srcState

        Expect.equal
            difference
            diffFile
            "diff"

        // State.write d
        State.write destStatePath destState

        Directory.createDir diff

        // 將變更寫入本地
        Difference.write dest diff difference

        // 讀取變更
        let d = Difference.read diff

        // 合併
        Difference.merge src diff d

        let dirEqual src dest =
            // src和dest對比
            let getResult path =
                path
                // |> Directory.getAllFileSystemEntries
                |> Directory.enumerateFileSystemEntriesAll
                |> Seq.toArray
                |> Array.sortWith VirtualFileInfo.sort
                |> Array.map (fun x ->
                    VirtualFileInfo.ofFileInfo path (FileInfo.ofFullName x))

            let resultSrc = getResult src
            let resultDest = getResult dest

            // logger.I $"[{__LINE__}] resultSrc: %A{resultSrc}"

            let dirLwt (result: VirtualFileInfo array) =
                result
                |> Array.map (fun x ->
                    match x.Type = "d" with
                    | true -> { x with LastWriteTime = "" }
                    | false -> x)
            (resultSrc |> dirLwt) = (resultDest |> dirLwt)

        let dirEq = dirEqual src dest

        logger.I $"{__LINE__} %A{dest}"
        logger.I $"{__LINE__} %A{src}"
        let difference = State.diff (State.read destStatePath) (State.read srcStatePath)
        logger.I $"{__LINE__} %A{difference}"

        Expect.equal
            dirEq true
            "check result"
        // src, dest
        srcStatePath, destStatePath

    let testPath = result.TryGetResult Test
    if testPath.IsSome then
        let diffTests =
            test "add test" {
                let mutable testBase = testPath.Value
                if testBase = "" then
                    testBase <- Path.join currentDir "test"

                let src, dest = testDiff testBase
                logger.I $"{__LINE__} %A{dest}"
                logger.I $"{__LINE__} %A{src}"
            }

        runTestsWithCLIArgs [] testArgs diffTests
        |> ignore

    runTestsWithCLIArgs [] testArgs CommonTest.mapTests
    |> ignore

    Library.Say.hello "test"

    exit 0
