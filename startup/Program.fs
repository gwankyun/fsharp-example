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
open DirDiff



printfn "Hello from F#"



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


printfn $"%A{Directory.baseDir}"

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
            let diffFile = diff.FileDiff.Creation
            Expect.equal diffFile ["f1.txt"] "diff.Creation"

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
            let diffDir = diff.DirDiff.Creation
            Expect.equal diffDir ["d2"] "diff.Creation"
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
            let diffDir = diff.DirDiff.Creation
            Expect.equal diffDir [newPath] "diff.Creation"
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
            let diffFile = diff.FileDiff.Creation
            Expect.equal diffFile ["f2.txt"] "diff.Creation"

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
            let diffFile = diff.FileDiff.Creation
            Expect.equal diffFile [@"d1\f3.txt"] "diff.Creation"

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
            let diffDir = diff.DirDiff.Deletion
            Expect.equal diffDir [@"d2"] "diff.Creation"

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
            let diffDir = diff.DirDiff.Deletion
            Expect.equal diffDir [ @"d1\d3"] "diff.Deletion"

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
            let diffFile = diff.FileDiff.Modification
            Expect.equal diffFile ["f1.txt"] "diff.Modification"

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
            let diffFile = diff.FileDiff.Modification
            Expect.equal diffFile [file] "diff.Modification"

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
            let diffFile = diff.FileDiff.Deletion
            Expect.equal diffFile [file] "diff.Modification"

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
            let diffFile = diff.FileDiff.Deletion
            Expect.equal diffFile [file] "diff.Modification"

            newStatus, diff

        let status, entry = testFileDeletionInside path status

        saveAndMerge "file-deletion-inside" entry

        // 對比
        Expect.equal (Status.equal path backup) true ""

        (status, d, f) |> ignore
    }

runTestsWithCLIArgs [] Array.empty mapTests
|> ignore
