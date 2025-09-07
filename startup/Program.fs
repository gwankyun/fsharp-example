// For more information see https://aka.ms/fsharp-console-apps
open System.IO
open Common
open Common.extra
open System.Text.Json
open System.Text
open Expecto

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

module Status =
    type Difference = {
        Creation: string list
        Modification: string list
        Deletion: string list
    }

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

    module Difference =
        let writeBase path diff =
            let text = JsonSerializer.Serialize<DataType>(diff)
            File.writeAllTextEncoding path text Encoding.UTF8

        let readBase path =
            let text = File.readAllTextEncoding path Encoding.UTF8
            JsonSerializer.Deserialize<DataType>(text)

        //let getImport dir file =
        //    let 

        let write src path dir file =
            let d, f = copyNeed dir file src (Path.join path "data")
            let w data file =
                let file = Path.join path file
                File.writeAllLinesEncoding file data Encoding.UTF8
            w d "dir.txt"
            w f "file.txt"

        //let read path =
        //    let dir = readBase (Path.join path "dir.json")
        //    let file = readBase (Path.join path "file.json")
        //    dir, file

    let fromPath path =
        let d, f =
            Directory.traverse path
        Tuple2.map (progress path) (d, f)

    let write path info =
        let text = JsonSerializer.Serialize<DataType>(info)
        File.writeAllTextEncoding path text Encoding.UTF8

    let read path =
        let text = File.readAllTextEncoding path Encoding.UTF8
        JsonSerializer.Deserialize<DataType>(text)

    let merge (d: Difference) (f: Difference) src dest =
        // 文件刪除，深到淺
        let deleteFile = f.Deletion |> List.rev
        for i in deleteFile do
            let path = Path.join dest i
            File.delete path

        // 目錄刪除，深到淺
        let deleteDir = d.Deletion |> List.rev
        for i in deleteDir do
            let path = Path.join dest i
            Directory.delete path true

        copyNeed d f src dest

    let equal src dest =
        let (sd, sf), (dd, df) = Tuple2.map fromPath (src, dest)
        let toStr x = List.map (fun y -> y.Path) x
        // 目錄衹用存在就好
        let sd, dd = Tuple2.map toStr (sd, dd)
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

        let d, f = Status.fromPath path
        printfn $"d: %A{d}"
        printfn $"f: %A{f}"

        let toStr x = List.map (fun y -> y.Path) x

        // 添加一個目錄
        let testDirCreation path d f =
            Directory.createDir (Path.join path "d1")
            let d1, f1 = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"] "d1"
            Expect.equal mf [] "f = []"

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffDir.Creation) ["d1"] "diff.Creation"
            d1, f1, diffDir, diffFile

        let d, f, dd, df = testDirCreation path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加一個文件
        let testFileCreation path d f =
            let newFile = Path.join path "f1.txt"
            File.writeAllText newFile ""
            let d1, f1 = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"] "d1"
            Expect.equal mf ["f1.txt"] "f = []"

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Creation) ["f1.txt"] "diff.Creation"

            d1, f1, diffDir, diffFile

        let d, f, dd, df = testFileCreation path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 再添加一個目錄
        let testDirCreationOther path d f =
            Directory.createDir (Path.join path "d2")
            let d1, f1 = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"; "d2"] "d1"
            Expect.equal mf ["f1.txt"] "f = []"

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffDir.Creation) ["d2"] "diff.Creation"
            d1, f1, diffDir, diffFile

        let d, f, dd, df = testDirCreationOther path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加二級目錄
        let testDirCreationInside path d f =
            let newPath = @"d1\d3"
            Directory.createDir (Path.join path newPath)
            let d1, f1 = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"; "d2"; newPath] "d1"
            Expect.equal mf ["f1.txt"] "f = []"

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffDir.Creation) [newPath] "diff.Creation"
            d1, f1, diffDir, diffFile

        let d, f, dd, df = testDirCreationInside path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加一個文件
        let testFileCreationOther path d f =
            let newFile = Path.join path "f2.txt"
            File.writeAllText newFile ""
            let d1, f1 = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Creation) ["f2.txt"] "diff.Creation"

            //d1, f1
            d1, f1, diffDir, diffFile

        let d, f, dd, df = testFileCreationOther path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 添加深層文件
        let testFileCreationInside path d f =
            let newFile = Path.join path @"d1\f3.txt"
            File.writeAllText newFile ""
            let d1, f1 = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Creation) [@"d1\f3.txt"] "diff.Creation"

            d1, f1, diffDir, diffFile

        let d, f, dd, df = testFileCreationInside path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 刪除目錄
        let testDirDeletion path d f =
            let deleteDir = Path.join path "d2"
            if Directory.exists deleteDir then
                Directory.delete deleteDir true
            let d1, f1 = Status.fromPath path

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffDir.Deletion) [@"d2"] "diff.Creation"

            d1, f1, diffDir, diffFile

        let d, f, dd, df = testDirDeletion path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 刪除深層目錄
        let testDirDeletionInside path d f =
            let deleteDir = Path.join path @"d1\d3"
            if Directory.exists deleteDir then
                Directory.delete deleteDir true
            let d1, f1 = Status.fromPath path

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffDir.Deletion) [ @"d1\d3"] "diff.Creation"

            d1, f1, diffDir, diffFile

        let d, f, dd, df = testDirDeletionInside path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 修改文件
        let testFileModification path d f =
            let newFile = Path.join path "f1.txt"
            File.writeAllText newFile "update"
            let d1, f1 = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Modification) ["f1.txt"] "diff.Modification"

            d1, f1, diffDir, diffFile

        let d, f, dd, df = testFileModification path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 修改文件
        let testFileModificationInside path d f =
            let file =  @"d1\f3.txt"
            let newFile = Path.join path file
            File.writeAllText newFile "update"
            let d1, f1 = Status.fromPath path

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Modification) [file] "diff.Modification"

            d1, f1, diffDir, diffFile

        let d, f, dd, df = testFileModificationInside path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 删除文件
        let testFileDeletion path d f =
            let file =  @"f1.txt"
            let opFile = Path.join path file
            File.delete opFile
            let d1, f1 = Status.fromPath path

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Deletion) [file] "diff.Modification"

            d1, f1, diffDir, diffFile

        let d, f, dd, df = testFileDeletion path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        // 删除深層文件
        let testFileDeletionInside path d f =
            let file =  @"d1\f3.txt"
            let opFile = Path.join path file
            File.delete opFile
            let d1, f1 = Status.fromPath path

            let diffDir = Status.compare d d1
            let diffFile = Status.compare f f1
            Expect.equal (diffFile.Deletion) [file] "diff.Modification"

            d1, f1, diffDir, diffFile

        let d, f, dd, df = testFileDeletionInside path d f

        // 同步到backup
        Status.merge dd df path backup |> ignore

        // 對比
        Expect.equal (Status.equal path backup) true ""

        (d, f) |> ignore
    }

runTestsWithCLIArgs [] Array.empty mapTests
|> ignore
