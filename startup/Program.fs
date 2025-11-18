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

let logger = Logger.ColorConsole

printfn "Hello from F#"

type Item = {
    Path: string
    LastWrite: int64
}

let inline (</>) path1 path2 = Path.join path1 path2

let inline (|?) (option: 'T option) defaultValue = 
    Option.defaultValue defaultValue option

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

type Difference = {
    Creation: string list
    Modification: string list
    Deletion: string list
}

module DateType =
    type T = Item list

    let toStringList (dt: T) =
        dt
        |> List.map (fun x -> x.Path)

    let compare (oldData: T) (newData: T) : Difference =
        let index (data: T) =
            data
            |> List.map (fun x -> x.Path)
            |> List.indexed
            |> List.map Tuple2.swap
            |> Map.ofList
        let indexOld = index oldData
        let indexNew = index newData
        let toMap (data: T) =
            data
            |> List.map (fun d -> d.Path, d.LastWrite)
            |> Map.ofList
        let mapOld, mapNew = Tuple2.map toMap (oldData, newData)
        let c = Map.compare mapOld mapNew
        let toList x =
            x
            |> Map.toList
            |> List.map fst
        let creation =
            c
            |> Map.filter (fun _ (a, b) ->
                a.IsNone && b.IsSome
                )
            |> toList
            |> List.sortBy (fun x -> indexNew[x])
        let deletion =
            c
            |> Map.filter (fun _ (a, b) ->
                a.IsSome && b.IsNone
                )
            |> toList
            |> List.sortBy (fun x -> indexOld[x])
        let modification =
            c
            |> Map.filter (fun _ (a, b) ->
                Option.map2 (<>) a b |? false
                )
            |> toList
            |> List.sortBy (fun x -> indexNew[x])
        { Creation = creation
          Modification = modification
          Deletion = deletion }

let progress path info : DateType.T =
    info
    |> List.map (fun x ->
        let rela = relativePath path
        { Path = x |> Entry.path |> rela
          LastWrite =
            x |> Entry.lastWriteTime |> DateTime.toFileTime }
        )

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

    let dtext = JsonSerializer.Serialize<DateType.T>(dinfo)
    let ftext = JsonSerializer.Serialize<DateType.T>(finfo)

    writeFile "dtext.json" dtext
    writeFile "ftext.json" ftext

type EntryDiff = {
    DirDiff: Difference
    FileDiff: Difference
}

type Operation =
    | DirCreation
    | FileCreation
    | FileModification
    | DirDeletion
    | FileDeletion

module Difference =
    type T = Difference
    let toList (dir: T) (file: T) =
        let trans op = List.map (fun x -> x, op)
        [ dir.Creation |> trans DirCreation;
          file.Creation |> trans FileCreation;
          file.Modification |> trans FileModification;
          file.Deletion |> trans FileDeletion |> List.rev;
          dir.Deletion |> trans DirDeletion |> List.rev; ]
        |> List.concat

module EntryDiff =
    type T = EntryDiff

    let dataDir = "data"
    let backupDir = "backup"

    let save src dest (e: T) =
        if Directory.exists dest |> not then
            Directory.createDir dest
        let ls = Difference.toList e.DirDiff e.FileDiff
        Directory.createDir (dest </> dataDir)
        Directory.createDir (dest </> backupDir)
        for i in ls do
            match i with
            | path, FileCreation ->
                let srcPath = src </> path
                let destPath = dest </> dataDir </> path
                Directory.createDirectoryFor destPath
                File.copy srcPath destPath true
            | path, FileModification ->
                let srcPath = src </> path
                let destPath = dest </> dataDir </> path
                Directory.createDirectoryFor destPath
                File.copy srcPath destPath true
            | _ -> ()
        let text = JsonSerializer.Serialize<(string * Operation) list>(ls, options)
        File.writeAllTextEncoding (dest </> "info.json") text Encoding.UTF8

    let load dest =
        let text = File.readAllTextEncoding (dest </> "info.json") Encoding.UTF8
        let info = JsonSerializer.Deserialize<(string * Operation) list>(text, options)
        info

type Status = {
        Dir: DateType.T
        File: DateType.T
    }

module Status =
    type T = Status

    let copyNeed (d: Difference) (f: Difference) src dest =
        // 目錄創建，淺到深
        let createDir = d.Creation
        for i in createDir do
            let path = dest </> i
            Directory.createDir path

        // 文件修改
        let modifyFile = f.Modification |> List.rev

        let copy fileList =
            for i in fileList do
                let srcPath = src </> i
                let destPath = dest </> i
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

    let fromPath path : Status =
        let d, f =
            Directory.traverse path
        let d, f = Tuple2.map (progress path) (d, f)
        { Dir = d; File = f }

    let write path info =
        let text = JsonSerializer.Serialize<DateType.T>(info)
        File.writeAllTextEncoding path text Encoding.UTF8

    let read path =
        let text = File.readAllTextEncoding path Encoding.UTF8
        JsonSerializer.Deserialize<DateType.T>(text)

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
            | path, FileCreation ->
                let srcPath = Path.join src path
                let destPath = Path.join dest path
                File.copy srcPath destPath true
            | path, FileModification ->
                let srcPath = Path.join src path
                let destPath = Path.join dest path
                File.copy srcPath destPath true

    let merge2 src dest backup =
        let ls = EntryDiff.load src
        let backupDir = src </> EntryDiff.backupDir
        let src = src </> EntryDiff.dataDir
        let backupFile path =
            if backup then
                let backupPath = backupDir </> path
                let destPath = dest </> path
                if File.exists destPath then
                    Directory.createDirectoryFor backupPath
                    File.copy destPath backupPath true
        for i in ls do
            match i with
            | path, DirCreation ->
                let destPath = dest </> path
                logger.D $"DirCreation: %A{destPath}"
                Directory.createDir destPath
            | path, DirDeletion ->
                let destPath = dest </> path
                logger.D $"DirDeletion: %A{destPath}"
                Directory.delete destPath true
            | path, FileDeletion ->
                let destPath = dest </> path
                logger.D $"FileDeletion: %A{destPath}"
                backupFile path
                File.delete destPath
            | path, FileCreation ->
                let srcPath = src </> path
                let destPath = dest </> path
                logger.D $"FileCopying: %A{destPath}"
                File.copy srcPath destPath true
            | path, FileModification ->
                let srcPath = src </> path
                let destPath = dest </> path
                backupFile path
                logger.D $"FileCopying: %A{destPath}"
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

    let compare (oldData: T) (newData: T) : EntryDiff.T =
        let { Dir = oldDir; File = oldFile } = oldData
        let { Dir = newDir; File = newFile } = newData
        { DirDiff = DateType.compare oldDir newDir;
          FileDiff = DateType.compare oldFile newFile }

printfn $"%A{Directory.baseDir}"

let createDir path =
    Directory.deleteIfExists path true
    Directory.createDir path

let mapTests =
    test "map test" {
        let path = Directory.baseDir </> "test"
        createDir path

        let backup = Directory.baseDir </> "backup"
        createDir backup

        let diff = Directory.baseDir </> "diff"
        createDir diff

        let { Dir = d; File = f } = Status.fromPath path
        printfn $"d: %A{d}"
        printfn $"f: %A{f}"

        let status = { Dir = d; File = f }

        let toStr x = List.map (fun y -> y.Path) x

        // 添加一個目錄
        let testDirCreation path (status: Status) =
            Directory.createDir (path </> "d1")
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"] "d1"
            Expect.equal mf [] "f = []"

            let diff = Status.compare status newStatus
            Expect.equal (diff.DirDiff.Creation) ["d1"] "diff.Creation"
            newStatus, diff

        let status, entry = testDirCreation path status
        let { Dir = d; File = f } = status

        let saveAndMerge name entry =
            let diffFileCreation = diff </> name
            // 導出增量包
            EntryDiff.save path diffFileCreation entry

            // 同步到backup
            Status.merge2 diffFileCreation backup true

        saveAndMerge "dir-creation" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加一個文件
        let testFileCreation path status =
            let newFile = path </> "f1.txt"
            File.writeAllText newFile ""
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"] "d1"
            Expect.equal mf ["f1.txt"] "f = []"

            let diff = Status.compare status newStatus
            let diffFile = diff.FileDiff
            Expect.equal (diffFile.Creation) ["f1.txt"] "diff.Creation"

            newStatus, diff

        let status, entry = testFileCreation path status

        saveAndMerge "file-creation" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 再添加一個目錄
        let testDirCreationOther path status =
            Directory.createDir (path </> "d2")
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"; "d2"] "d1"
            Expect.equal mf ["f1.txt"] "f = []"

            let diff = Status.compare status newStatus
            let diffDir = diff.DirDiff
            Expect.equal (diffDir.Creation) ["d2"] "diff.Creation"
            newStatus, diff

        let status, entry = testDirCreationOther path status

        saveAndMerge "dir-creation-other" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加二級目錄
        let testDirCreationInside path status =
            let newPath = @"d1\d3"
            Directory.createDir (path </> newPath)
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"; "d2"; newPath] "d1"
            Expect.equal mf ["f1.txt"] "f = []"

            let diff = Status.compare status newStatus
            let diffDir = diff.DirDiff
            Expect.equal (diffDir.Creation) [newPath] "diff.Creation"
            newStatus, diff

        let status, entry = testDirCreationInside path status

        saveAndMerge "dir-creation-inside" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加一個文件
        let testFileCreationOther path status =
            let newFile = path </> "f2.txt"
            File.writeAllText newFile ""
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)

            let diff = Status.compare status newStatus
            let diffFile = diff.FileDiff
            Expect.equal (diffFile.Creation) ["f2.txt"] "diff.Creation"

            newStatus, diff

        let status, entry = testFileCreationOther path status

        saveAndMerge "file-creation-other" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加深層文件
        let testFileCreationInside path status =
            let newFile = path </> @"d1\f3.txt"
            File.writeAllText newFile ""
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)

            let diff = Status.compare status newStatus
            let diffFile = diff.FileDiff
            Expect.equal (diffFile.Creation) [@"d1\f3.txt"] "diff.Creation"

            newStatus, diff

        let status, entry = testFileCreationInside path status

        saveAndMerge "file-creation-inside" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 刪除目錄
        let testDirDeletion path status =
            let deleteDir = path </> "d2"
            Directory.deleteIfExists deleteDir true
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path

            let diff = Status.compare status newStatus
            let diffDir = diff.DirDiff
            Expect.equal (diffDir.Deletion) [@"d2"] "diff.Creation"

            newStatus, diff

        let status, entry = testDirDeletion path status

        saveAndMerge "dir-deletion" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 刪除深層目錄
        let testDirDeletionInside path status =
            let deleteDir = path </> @"d1\d3"
            Directory.deleteIfExists deleteDir true
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path

            let diff = Status.compare status newStatus
            let diffDir = diff.DirDiff
            Expect.equal (diffDir.Deletion) [ @"d1\d3"] "diff.Deletion"

            newStatus, diff

        logger.D $"before============================================="
        let status, entry = testDirDeletionInside path status

        saveAndMerge "dir-deletion-inside" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 修改文件
        let testFileModification path status =
            let newFile = path </> "f1.txt"
            File.writeAllText newFile "update"
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)

            let diff = Status.compare status newStatus
            let diffFile = diff.FileDiff
            Expect.equal (diffFile.Modification) ["f1.txt"] "diff.Modification"

            newStatus, diff

        let status, entry = testFileModification path status

        saveAndMerge "file-modification" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 修改文件
        let testFileModificationInside path status =
            let file = @"d1\f3.txt"
            let newFile = path </> file
            File.writeAllText newFile "update"
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path

            let diff = Status.compare status newStatus
            let diffFile = diff.FileDiff
            Expect.equal (diffFile.Modification) [file] "diff.Modification"

            newStatus, diff

        let status, entry = testFileModificationInside path status

        saveAndMerge "file-modification-inside" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 删除文件
        let testFileDeletion path status =
            let file = @"f1.txt"
            let opFile = path </> file
            File.delete opFile
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path

            let diff = Status.compare status newStatus
            let diffFile = diff.FileDiff
            Expect.equal (diffFile.Deletion) [file] "diff.Modification"

            newStatus, diff

        let status, entry = testFileDeletion path status

        saveAndMerge "file-deletion" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 删除深層文件
        let testFileDeletionInside path status =
            let file = @"d1\f3.txt"
            let opFile = path </> file
            File.delete opFile
            let { Dir = d1; File = f1 } as newStatus = Status.fromPath path

            let diff = Status.compare status newStatus
            let diffFile = diff.FileDiff
            Expect.equal (diffFile.Deletion) [file] "diff.Modification"

            newStatus, diff

        let status, entry = testFileDeletionInside path status

        saveAndMerge "file-deletion-inside" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        (status, d, f) |> ignore
    }

runTestsWithCLIArgs [] Array.empty mapTests
|> ignore
