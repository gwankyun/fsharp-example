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
          LastWriteTime: string }

        static member Default =
            { RelativePath = ""
              Type = ""
              LastWriteTime = "" }

    let ofFileInfo path (info: FileInfo.T) =
        let rela = Path.getRelativePath path info.FullName
        let t = if FileInfo.isDir info then "d" else "f"
        let lwt = info.LastWriteTime
        { RelativePath = rela
          Type = t
          LastWriteTime = lwt.ToString(@"yyyy-MM-dd HH:mm:ss:fffK") }

    let toFileInfo path (info: T) =
        FileInfo.ofFullName
        <| Path.join path info.RelativePath

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

let sort k1 k2 =
    let level f =
        f
        // |> FileInfo.fullName
        |> String.split [@"\"]
        |> Seq.length
    compareWith level k1 k2

module State =
    let write (state: VirtualFileInfo.T array) path =
        File.writeAllLinesEncoding
            path
            (state
            |> Array.map (fun x ->
                $"%s{x.Type}|%s{x.LastWriteTime}|%s{x.RelativePath}"))
            Encoding.UTF8

    let read path =
        File.readAlllinesEncoding path Encoding.UTF8
        |> Array.map (fun x -> String.split ["|"] x |> Array.ofSeq)
        |> Array.map (fun x ->
            { VirtualFileInfo.RelativePath = x[2]
              VirtualFileInfo.Type = x[0]
              VirtualFileInfo.LastWriteTime = x[1] })

    let create src =
        let fileList = Directory.getAllFileSystemEntries src
        fileList
        |> Array.sortWith sort
        |> Array.map FileInfo.ofFullName
        |> Array.map (VirtualFileInfo.ofFileInfo src)

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

                // if Directory.exists src |> not then
                Directory.createDir src
                Directory.createDir dest

                // if Directory.exists history then
                //     Directory.delete history true
                Directory.createDir history

                // File.writeAllTextEncoding (Path.join src "3/1.txt") "" Encoding.UTF8

                let writeAllText path contents =
                    createDirectoryFor path
                    File.writeAllTextEncoding path contents Encoding.UTF8

                // File.writeAllTextEncoding (Path.join src "delete.txt") "" Encoding.UTF8
                // File.writeAllTextEncoding (Path.join src "update.txt") "" Encoding.UTF8
                writeAllText (Path.join src @"delete.txt") ""
                writeAllText (Path.join src @"update.txt") ""
                writeAllText (Path.join src @"u\update.txt") ""

                writeAllText (Path.join src @"d\delete.txt") ""

                // let file = Path.joinList [ src; "3"; "1"; "1.txt" ]
                // writeAllText file ""

                // let dir = Path.joinList [ src; "4"; "3"; "2"; "1" ]
                // if Directory.exists dir then
                //     Directory.delete dir true
                // Directory.createDir dir


                // 測試排序
                let pathList = [ @"0"; "1"; "10"; @"0\0"; @"0\1"; @"0\0\0" ]
                Expect.equal
                    pathList
                    (List.sortWith sort pathList)
                    "path sort"

                let fileList =
                    Directory.getAllFileSystemEntries src
                    // |> Seq.map Fin
                    |> Array.sortWith sort
                // for i in fileList do
                //     logger.I $"{__LINE__} {i}"


                let srcState = State.create src
                    // fileList
                    // |> Array.map FileInfo.ofFullName
                    // |> Array.map (VirtualFileInfo.ofFileInfo src)

                let a = fileList |> Array.map FileInfo.ofFullName
                let e =
                    srcState
                    |> Array.map (VirtualFileInfo.toFileInfo src)
                    |> Array.map FileInfo.fullName
                // logger.I $"a: %A{a}"
                // logger.I $"e: %A{e}"
                // logger.I $"eq: %A{fileList = e}"

                Expect.equal
                    (fileList)
                    (e)
                    "VirtualFileInfo"

                let srcStatePath = Path.join history "src.txt"
                let destStatePath = Path.join history "dest.txt"

                // 測試狀態寫入本地
                State.write srcState srcStatePath


                // 讀取狀態
                let srcStateRead = State.read srcStatePath

                Expect.equal
                    srcState
                    srcStateRead
                    "read = write"

                // logger.I $"dest: %s{dest}"

                // if Directory.exists dest then
                //     Directory.delete dest true


                copyDirectory src dest true
                // 新增文件
                let addFile =
                    let file = [ "add.txt"; @"a\add2.txt" ]
                    for i in file do
                        let addFile = Path.joinList [ dest; i ]
                        writeAllText addFile ""
                    "a"::file

                // 刪除文件
                let deleteFile =
                    let file = [ "delete.txt"; @"d\delete.txt" ]
                    for i in file do
                        let deleteFile = Path.joinList [ dest; i ]
                        File.delete deleteFile
                    Directory.delete (Path.join dest "d") true
                    "d"::file

                // 修改文件
                let updateFile =
                    let file = [ "update.txt"; @"u\update.txt" ]
                    for i in file do
                        let updateFile = Path.joinList [ dest; i ]
                        writeAllText updateFile "1"
                    file


                let srcM = Diff.toFile src
                let destM = Diff.toFile dest

                // logger.I $"%A{srcM}"
                // logger.I $"%A{destM}"

                let u = Map.compare destM srcM
                // logger.I $"u: %A{u}"

                let addFileM =
                    Map.difference destM srcM

                // logger.I $"%A{addFileM}"



                let deleteFileM =
                    Map.difference srcM destM

                // logger.I $"%A{deleteFile}"
                let mapToList (m: Map<'k, 'v>) =
                    Map.toList >> (List.map fst) >> List.sort
                    <| m

                let updateFileM =
                    u
                    |> Map.chooseValues (fun v ->
                        match v with
                        | Some v1, Some v2 ->
                            let isDir = FileInfo.isDir
                            let v1d = isDir v1
                            let v2d = isDir v2
                            match v1d, v2d with
                            | false, false ->
                                Option.ofPair (v1.LastWriteTime <> v2.LastWriteTime, v1)
                            | true, true -> None
                            | _, _ -> Some v1
                        | _ -> None)

                logger.I $"[{__LINE__}] updateFileM: %A{updateFileM}"

                Expect.equal
                    (updateFileM |> mapToList |> List.sortWith sort)
                    updateFile
                    "updateFile"

                Expect.equal
                    (addFileM |> mapToList |> List.sortWith sort)
                    addFile
                    "addFile"

                Expect.equal
                    (deleteFileM |> mapToList |> List.sortWith sort)
                    deleteFile
                    "deleteFile"

                // State.write d
                State.write (State.create dest) destStatePath

                let toSeq m =
                    m
                    |> Map.toSeq
                    |> Seq.map fst
                    |> Seq.sortWith sort

                let updateFileSeq =
                    updateFileM
                    |> toSeq

                // 根據變動複製
                let addFileSeq =
                    addFileM
                    |> toSeq

                let deleteFileSeq =
                    deleteFileM
                    |> toSeq
                    |> Seq.rev // 刪除要反序

                logger.I $"[{__LINE__}] update: %A{updateFileSeq}"
                logger.I $"[{__LINE__}] add: %A{addFileSeq}"
                logger.I $"[{__LINE__}] delete: %A{deleteFileSeq}"

                // 將修改複製出來
                Directory.createDir diff

                let diffDate = Path.join diff "data"

                let copyAction i =
                    let destFi = FileInfo.ofFullName (Path.join dest i)
                    if destFi |> FileInfo.isDir then
                        Directory.createDir (Path.join diffDate i)
                    else
                        let diffDateFi = Path.join diffDate i |> FileInfo.ofFullName
                        let diffDateDir = diffDateFi |> FileInfo.directoryName
                        if Directory.exists diffDateDir |> not then
                            Directory.createDir diffDateDir
                        destFi |> FileInfo.copyTo (diffDateFi |> FileInfo.fullName) false |> ignore

                for i in updateFileSeq do
                    copyAction i

                for i in addFileSeq do
                    // logger.I $"[{__LINE__}] i: %A{i}"
                    copyAction i

                // 生成刪除表
                File.writeAllLinesEncoding (Path.join diff "deletion.txt") deleteFileSeq Encoding.UTF8

                // 根據修改同步src
                let diffFileList =
                    Directory.getAllFileSystemEntries diffDate
                    |> Seq.ofArray
                    |> Seq.map (Path.getRelativePath diffDate)
                    |> Seq.sortWith sort

                for i in diffFileList do
                    // logger.I $"[{__LINE__}] i: {i}"
                    let fp = Path.join src i
                    let dp = Path.join diffDate i
                    let fi = FileInfo.ofFullName fp
                    if dp |> FileInfo.ofFullName |> FileInfo.isDir then
                        Directory.createDir fp
                    else
                        let fd = fi.DirectoryName
                        if Directory.exists fd |> not then
                            Directory.createDir fd
                        File.copy (Path.join diffDate i) fp true

                // 同步刪除
                for i in deleteFileSeq do
                    // logger.I $"[{__LINE__}] i: {i}"
                    let path = Path.join src i
                    let info = FileInfo.ofFullName path
                    if info |> FileInfo.isDir then
                        Directory.delete path false
                    else
                        File.delete path

                // src和dest對比
                let getResult path =
                    path
                    |> Directory.getAllFileSystemEntries
                    |> Array.sortWith sort
                    |> Array.map (fun x ->
                        VirtualFileInfo.ofFileInfo path (FileInfo.ofFullName x))

                let resultSrc = getResult src
                let resultDest = getResult dest

                // logger.I $"[{__LINE__}] resultSrc: %A{resultSrc}"

                let dirLwt (result: VirtualFileInfo.T array) =
                    result
                    |> Array.map (fun x ->
                        match x.Type = "d" with
                        | true -> { x with LastWriteTime = "" }
                        | false -> x)

                Expect.equal
                    (resultSrc |> dirLwt)
                    (resultDest |> dirLwt)
                    "check result"
            }

        runTestsWithCLIArgs [] testArgs diffTests
        |> ignore

    runTestsWithCLIArgs [] testArgs CommonTest.mapTests
    |> ignore

    exit 0
