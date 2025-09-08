// For more information see https://aka.ms/fsharp-console-apps
open System.IO
open Common
open Common.extra
open System.Text.Json
open System.Text
open Expecto
open FSLogger
//open FSharp.SystemTextJson
//tem.Text.Json.Serialization
open System.Text.Json.Serialization

// 1. Either create the serializer options from the F# options...
let options =
    JsonFSharpOptions.Default()
        // Add any .WithXXX() calls here to customize the format
        .ToJsonSerializerOptions()

// 创建 JsonSerializerOptions 并配置 F# 支持
//let options = JsonSerializerOptions()
//options.Converters.Add(FSharpJsonConverter())

let logger = Logger.ColorConsole

//module File =
//    begin
//        let readLines path =
//            File.ReadLines(path, System.Text.Encoding.UTF8)
//    end

printfn "Hello from F#"

type Item = {
    Path: string
    LastWrite: int64
}

type DataType = Item list

module DateType =
    let toStringList (dt: DataType) =
        dt
        |> List.map (fun x -> x.Path)

    //let ofStringList strList
    //    |>

let relativePath parent path =
    Path.getRelativePath parent path

module DateTime =
    type T = System.DateTime
    let toFileTime (dt: T) =
        dt.ToFileTime()

    let toFileTimeUtc (dt: T) =
        dt.ToFileTimeUtc()

let writeFile name text =
    let path = Path.join Directory.baseDir name
    File.writeAllTextEncoding path text Encoding.UTF8



// 讀入

let readFile name =
    let path = Path.join Directory.baseDir name
    File.readAllTextEncoding path Encoding.UTF8

let progress path info : DataType =
    info
    |> List.map (fun x ->
        let rela = relativePath path
        { Path = x |> Entry.path |> rela
          LastWrite =
            x |> Entry.lastWriteTime |> DateTime.toFileTime }
        )

//let d, f = Directory.traverse ""

let testRoot path =
    let root = path
    let d, f =
        Directory.traverse root

    let dinfo =
        d
        |> progress root

    let finfo =
        f
        |> progress root

    let dtext = JsonSerializer.Serialize<DataType>(dinfo)
    let ftext = JsonSerializer.Serialize<DataType>(finfo)

    writeFile "dtext.json" dtext
    writeFile "ftext.json" ftext

//for p, d in dinfo do
//    printfn $"d: %A{p} %A{d}"

//for p, d in finfo do
//    printfn $"f: %A{p} %A{d}"

// 寫入本地

type Difference = {
    Creation: string list
    Modification: string list
    Deletion: string list
}

//type Entry =
//    | Dir
//    | File

type EntryDiff = {
    DirDiff: Difference
    FileDiff: Difference
}

type Operation =
    | DirCreation
    | FileCopying
    | DirDeletion
    | FileDeletion

// 创建并注册转换器
//let settings = JsonSerializer. JsonSerializerSettings()
//settings.Converters.Add(DiscriminatedUnionConverter<Operation>())

module Difference =
    type T = Difference
    let toList (dir: T) (file: T) =
        let trans op = List.map (fun x -> x, op)
        [ dir.Creation |> trans DirCreation;
          file.Creation |> trans FileCopying;
          file.Modification |> trans FileCopying;
          file.Deletion |> trans FileDeletion |> List.rev;
          dir.Deletion |> trans DirDeletion |> List.rev; ]
        |> List.concat

module EntryDiff =
    type T = EntryDiff

    let dataDir = "data"

    let save src dest (e: T) =
        if Directory.exists dest |> not then
            Directory.createDir dest
        let ls = Difference.toList e.DirDiff e.FileDiff
        for i in ls do
            match i with
            | path, FileCopying ->
                let srcPath = Path.join src path
                let destPath = Path.join3 dest dataDir path
                Directory.createDirectoryFor destPath
                File.copy srcPath destPath true
            | _ -> ()
        let text = JsonSerializer.Serialize<(string * Operation) list>(ls, options)
        File.writeAllTextEncoding (Path.join dest "info.json") text Encoding.UTF8

    let load dest =
        let text = File.readAllTextEncoding (Path.join dest "info.json") Encoding.UTF8
        let info = JsonSerializer.Deserialize<(string * Operation) list>(text, options)
        info

type Status = {
        Dir: DataType
        File: DataType
    }

module Status =
    //type Difference = {
    //    Creation: string list
    //    Modification: string list
    //    Deletion: string list
    //}

    let copyNeed (d: Difference) (f: Difference) src dest =
        // 目錄創建，淺到深
        let createDir = d.Creation
        for i in createDir do
            let path = Path.join dest i
            Directory.createDir path

        // 文件修改
        let modifyFile = f.Modification |> List.rev

        let copy fileList =
            for i in fileList do
                let srcPath = Path.join src i
                let destPath = Path.join dest i
                if Directory.exists srcPath then
                    //assert(false)
                    failwith $"%A{srcPath} is dir"
                File.copy srcPath destPath true

        copy modifyFile

        // 文件新增
        let createFile = f.Creation |> List.rev
        //printfn $"%A{createFile}"
        copy createFile
        createDir, (List.append modifyFile createFile)

    //module Difference =
    //    let writeBase path diff =
    //        let text = JsonSerializer.Serialize<DataType>(diff)
    //        File.writeAllTextEncoding path text Encoding.UTF8

    //    let readBase path =
    //        let text = File.readAllTextEncoding path Encoding.UTF8
    //        JsonSerializer.Deserialize<DataType>(text)

    //    //let getImport dir file =
    //    //    let 

    //    let write src path dir file =
    //        let d, f = copyNeed dir file src (Path.join path "data")
    //        let w data file =
    //            let file = Path.join path file
    //            File.writeAllLinesEncoding file data Encoding.UTF8
    //        w d "dir.txt"
    //        w f "file.txt"

        //let read path =
        //    let dir = readBase (Path.join path "dir.json")
        //    let file = readBase (Path.join path "file.json")
        //    dir, file

    let fromPath path : Status =
        let d, f =
            Directory.traverse path
        let d, f = Tuple2.map (progress path) (d, f)
        { Dir = d; File = f }

    let write path info =
        let text = JsonSerializer.Serialize<DataType>(info)
        File.writeAllTextEncoding path text Encoding.UTF8

    let read path =
        let text = File.readAllTextEncoding path Encoding.UTF8
        JsonSerializer.Deserialize<DataType>(text)

    //let merge (d: Difference) (f: Difference) src dest =
    let merge (entry: EntryDiff) src dest =
        let { EntryDiff.DirDiff = d; FileDiff = f } = entry
        logger.D $"d: %A{d}"
        logger.D $"f: %A{f}"
        //logger.D $"ls: %A{ls}"

        let ls = Difference.toList d f

        for i in ls do
            match i with
            | path, DirCreation ->
                let destPath = Path.join dest path
                logger.D $"DirCreation: %A{destPath}"
                Directory.createDir destPath
            | path, DirDeletion ->
                let destPath = Path.join dest path
                logger.D $"DirDeletion: %A{destPath}"
                Directory.delete destPath true
            | path, FileDeletion ->
                let destPath = Path.join dest path
                File.delete destPath
            | path, FileCopying ->
                let srcPath = Path.join src path
                let destPath = Path.join dest path
                File.copy srcPath destPath true

        //// 文件刪除，深到淺
        //let deleteFile = f.Deletion |> List.rev
        //for i in deleteFile do
        //    let path = Path.join dest i
        //    File.delete path

        //// 目錄刪除，深到淺
        //let deleteDir = d.Deletion |> List.rev
        //for i in deleteDir do
        //    let path = Path.join dest i
        //    Directory.delete path true

        //copyNeed d f src dest

    let merge2 src dest =
        let ls = EntryDiff.load src
        let src = Path.join src EntryDiff.dataDir
        for i in ls do
            match i with
            | path, DirCreation ->
                let destPath = Path.join dest path
                logger.D $"DirCreation: %A{destPath}"
                Directory.createDir destPath
            | path, DirDeletion ->
                let destPath = Path.join dest path
                logger.D $"DirDeletion: %A{destPath}"
                Directory.delete destPath true
            | path, FileDeletion ->
                let destPath = Path.join dest path
                File.delete destPath
            | path, FileCopying ->
                let srcPath = Path.join src path
                let destPath = Path.join dest path
                File.copy srcPath destPath true

    let equal src dest =
        let  { Dir = sd; File = sf }, { Dir = dd; File = df } =
            Tuple2.map fromPath (src, dest)
        let toStr x = List.map (fun y -> y.Path) x
        // 目錄衹用存在就好
        let sd, dd = Tuple2.map toStr (sd, dd)
        if (sd <> dd) then
            printfn $"sd: %A{sd}"
            printfn $"sd: %A{dd}"
            failwith $"%A{sd} %A{dd}"
        if (sf <> df) then
            failwith $"%A{sf} %A{df}"
        (sd = dd) && (sf = df)

    let compare (oldData: DataType) (newData: DataType) : Difference =
        let index (data: DataType) =
            data
            |> List.map (fun x -> x.Path)
            |> List.indexed
            |> List.map Tuple2.swap
            |> Map.ofList
        let indexOld = index oldData
        let indexNew = index newData
        let toMap (data: DataType) =
            data
            |> List.map (fun d -> d.Path, d.LastWrite)
            |> Map.ofList
        let mapOld, mapNew = Tuple2.map toMap (oldData, newData)
        let c = Map.compare mapOld mapNew
        let toList x =
            x
            |> Map.toList
            |> List.map fst
        //optionMinus a b =
        //    let da, db = Tuple2.map (Option.defaultValue 0) (a, b)
        //    a
        //    |> Option.bind (fun v -> )
        let creation =
            c
            |> Map.filter (fun _ (a, b) ->
                match a, b with
                | None, Some(_) -> true
                | _ -> false
                )
            |> toList
            |> List.sortBy (fun x -> indexNew[x])
        let deletion =
            c
            |> Map.filter (fun _ (a, b) ->
                match a, b with
                | Some(_), None -> true
                | _ -> false
                )
            |> toList
            |> List.sortBy (fun x -> indexOld[x])
        let modification =
            c
            |> Map.filter (fun _ (a, b) ->
                match a, b with
                | Some(dtA), Some(dtB) -> dtA <> dtB
                | _ -> false
                )
            |> toList
            |> List.sortBy (fun x -> indexNew[x])
        { Creation = creation
          Modification = modification
          Deletion = deletion }

//Common.


printfn $"%A{Directory.baseDir}"

let createDir path =
    if Directory.exists path then
        Directory.delete path true
    Directory.createDir path

let mapTests =
    test "map test" {
        let path = Path.join Directory.baseDir "test"
        createDir path

        let backup = Path.join Directory.baseDir "backup"
        createDir backup

        let diff = Path.join Directory.baseDir "diff"
        createDir diff

        let { Dir = d; File = f } = Status.fromPath path
        printfn $"d: %A{d}"
        printfn $"f: %A{f}"

        let status = { Dir = d; File = f }

        let toStr x = List.map (fun y -> y.Path) x

        // 添加一個目錄
        let testDirCreation path (status: Status) =
            Directory.createDir (Path.join path "d1")
            let { Dir = d1; File = f1 } = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"] "d1"
            Expect.equal mf [] "f = []"
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffDir.Creation) ["d1"] "diff.Creation"
            //{ Dir = d1; File = f1 }, diffDir, diffFile
            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testDirCreation path status
        let { Dir = d; File = f } = status

        let diffDirCreation = Path.join diff "1"
        EntryDiff.save path diffDirCreation entry

        // 同步到backup
        //Status.merge entry path backup
        Status.merge2 diffDirCreation backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加一個文件
        let testFileCreation path status =
            let newFile = Path.join path "f1.txt"
            File.writeAllText newFile ""
            //let d1, f1 = Status.fromPath path
            let { Dir = d1; File = f1 } = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"] "d1"
            Expect.equal mf ["f1.txt"] "f = []"
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Creation) ["f1.txt"] "diff.Creation"

            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testFileCreation path status

        // 同步到backup
        Status.merge entry path backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 再添加一個目錄
        let testDirCreationOther path status =
            Directory.createDir (Path.join path "d2")
            //let d1, f1 = Status.fromPath path
            let { Dir = d1; File = f1 } = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"; "d2"] "d1"
            Expect.equal mf ["f1.txt"] "f = []"
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffDir.Creation) ["d2"] "diff.Creation"
            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testDirCreationOther path status

        // 同步到backup
        Status.merge entry path backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加二級目錄
        let testDirCreationInside path status =
            let newPath = @"d1\d3"
            Directory.createDir (Path.join path newPath)
            //let d1, f1 = Status.fromPath path
            let { Dir = d1; File = f1 } = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"; "d2"; newPath] "d1"
            Expect.equal mf ["f1.txt"] "f = []"
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffDir.Creation) [newPath] "diff.Creation"
            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testDirCreationInside path status

        // 同步到backup
        Status.merge entry path backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加一個文件
        let testFileCreationOther path status =
            let newFile = Path.join path "f2.txt"
            File.writeAllText newFile ""
            //let d1, f1 = Status.fromPath path
            let { Dir = d1; File = f1 } = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Creation) ["f2.txt"] "diff.Creation"

            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testFileCreationOther path status

        // 同步到backup
        Status.merge entry path backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加深層文件
        let testFileCreationInside path status =
            let newFile = Path.join path @"d1\f3.txt"
            File.writeAllText newFile ""
            let { Dir = d1; File = f1 } = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Creation) [@"d1\f3.txt"] "diff.Creation"

            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testFileCreationInside path status

        // 同步到backup
        Status.merge entry path backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 刪除目錄
        let testDirDeletion path status =
            let deleteDir = Path.join path "d2"
            if Directory.exists deleteDir then
                Directory.delete deleteDir true
            //let d1, f1 = Status.fromPath path
            let { Dir = d1; File = f1 } = Status.fromPath path
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffDir.Deletion) [@"d2"] "diff.Creation"

            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testDirDeletion path status

        printfn $"%A{status.Dir}"
        printfn $"%A{status.File}"
        //printfn $"dd %A{dd}"

        // 同步到backup
        Status.merge entry path backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 刪除深層目錄
        let testDirDeletionInside path status =
            let deleteDir = Path.join path @"d1\d3"
            if Directory.exists deleteDir then
                Directory.delete deleteDir true
            //let d1, f1 = Status.fromPath path
            let { Dir = d1; File = f1 } = Status.fromPath path
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffDir.Deletion) [ @"d1\d3"] "diff.Deletion"

            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        logger.D $"before============================================="
        let status, entry = testDirDeletionInside path status

        // 同步到backup
        logger.D $"entry: %A{entry}"
        logger.D $"Dir: %A{entry.DirDiff}"
        logger.D $"File: %A{entry.FileDiff}"
        //Status.merge entry.Dir entry.File path backup |> ignore
        //Status.merge entry path backup |> ignore
        Status.merge entry path backup
        logger.D $"end   ============================================="

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 修改文件
        let testFileModification path status =
            let newFile = Path.join path "f1.txt"
            File.writeAllText newFile "update"
            //let d1, f1 = Status.fromPath path
            let { Dir = d1; File = f1 } = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Modification) ["f1.txt"] "diff.Modification"

            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testFileModification path status

        // 同步到backup
        Status.merge entry path backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 修改文件
        let testFileModificationInside path status =
            let file =  @"d1\f3.txt"
            let newFile = Path.join path file
            File.writeAllText newFile "update"
            //let d1, f1 = Status.fromPath path
            let { Dir = d1; File = f1 } = Status.fromPath path
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Modification) [file] "diff.Modification"

            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testFileModificationInside path status

        // 同步到backup
        Status.merge entry path backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 删除文件
        let testFileDeletion path status =
            let file =  @"f1.txt"
            let opFile = Path.join path file
            File.delete opFile
            //let d1, f1 = Status.fromPath path
            let { Dir = d1; File = f1 } = Status.fromPath path
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Deletion) [file] "diff.Modification"

            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testFileDeletion path status

        // 同步到backup
        Status.merge entry path backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 删除深層文件
        let testFileDeletionInside path status =
            let file =  @"d1\f3.txt"
            let opFile = Path.join path file
            File.delete opFile
            //let d1, f1 = Status.fromPath path
            let { Dir = d1; File = f1 } = Status.fromPath path
            let { Dir = d; File = f } = status

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Deletion) [file] "diff.Modification"

            { Dir = d1; File = f1 }, { EntryDiff.DirDiff = diffDir; FileDiff = diffFile }

        let status, entry = testFileDeletionInside path status

        // 同步到backup
        Status.merge entry path backup

        // 對比
        Expect.equal (Status.equal path backup) true ""

        (d, f) |> ignore
    }

runTestsWithCLIArgs [] Array.empty mapTests
|> ignore
