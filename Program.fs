// For more information see https://aka.ms/fsharp-console-apps
open Common
open FSLogger
open Argu
open System.IO
open System.Text
open Expecto

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
            |> String.split @"\"
            |> Array.length
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

let tests =
  test "A simple test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "The strings should equal"
  }

type VirtualFileInfo =
    { RelativePath: string
      Type: string
      LastWriteTime: int64 }

    static member Default =
        { RelativePath = ""
          Type = ""
          LastWriteTime = 0L }

let rec copyDirectory (sourceDir: string) (destinationDir: string) (recursive: bool) =
    let dir = DirectoryInfo.ofFullName sourceDir

    let dirs = dir |> DirectoryInfo.getDirectories

    Directory.createDirectory destinationDir |> ignore

    for file in dir |> DirectoryInfo.getFiles do
        let targetFilePath = Path.combine destinationDir file.Name
        file |> FileInfo.copyTo targetFilePath |> ignore

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


    let _ = (
        let result =
            Diff.toFile <| Path.joinList [ currentDir; "compare"; "test1" ]
        logger.I $"%A{result}"

        let readme =
            Path.joinList [ currentDir; "README.md" ] |> FileInfo.ofFullName
        logger.I $"{readme.DirectoryName}"
        logger.I $"{readme.Directory}"
        logger.I $"======================================"
        let basePath = Path.joinList [ currentDir; "compare" ]
        let file =
            Directory.getAllFileSystemEntries
            <| basePath
            |> Array.toSeq
            |> Seq.map FileInfo.ofFullName
            |> Seq.sortWith Diff.sort
            |> Seq.map (fun x -> {
                VirtualFileInfo.RelativePath = Path.getRelativePath basePath x.FullName
                VirtualFileInfo.LastWriteTime = x.LastWriteTime.ToFileTime()
                VirtualFileInfo.Type = if FileInfo.isDir x then "d" else "f"
            })
        for i in file do
            logger.I $"{i}"
    )

    let testPath = result.TryGetResult Test
    if testPath.IsSome then
        let diffTests =
            test "add test" {
                let src = Path.join testPath.Value "test"

                let fileList = Directory.getAllFileSystemEntries src
                for i in fileList do
                    logger.I $"{i}"

                let dest = Path.join testPath.Value "dest"

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

                let u = Map.union destM srcM
                logger.I $"u: %A{u}"

                let addFileM =
                    u
                    |> Map.filter (fun k v ->
                        match v with
                        | Some _, None -> true
                        | _ -> false)

                logger.I $"%A{addFileM}"

                Expect.equal
                    (addFileM |> Map.toList |> List.map fst |> List.sort)
                    [addFile]
                    "addFile"

                let deleteFileM =
                    u
                    |> Map.filter (fun k v ->
                        match v with
                        | None, Some _ -> true
                        | _ -> false)

                // logger.I $"%A{deleteFile}"

                Expect.equal
                    (deleteFileM |> Map.toList |> List.map fst |> List.sort)
                    [deleteFile]
                    "deleteFile"

                let updateFileM =
                    u
                    |> Map.filter (fun k v ->
                        match v with
                        | Some v1, Some v2 ->
                            let v1d = FileInfo.isDir v1
                            let v2d = FileInfo.isDir v2
                            match v1d, v2d with
                            | false, false -> v1.LastWriteTime <> v2.LastWriteTime
                            | true, true -> false
                            | _, _ -> true
                        | _ -> false)

                Expect.equal
                    (updateFileM |> Map.toList |> List.map fst |> List.sort)
                    [updateFile]
                    "updateFile"
            }

        runTestsWithCLIArgs [] testArgs diffTests
        |> ignore

    exit 0
