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
          LastWrite = x |> Entry.lastWriteTime |> DateTime.toFileTime }
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
    type Result = {
        Creation: string list
        Modification: string list
        Deletion: string list
    }

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

    let compare (oldData: DataType) (newData: DataType) : Result =
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



let mapTests =
    test "map test" {
        let path = Path.join Directory.baseDir "test"
        if Directory.exists path then
            Directory.delete path true
        Directory.createDir path
        let d, f = Status.fromPath path
        printfn $"d: %A{d}"
        printfn $"f: %A{f}"
        //Expect.equal d [] "d = []"
        //Expect.equal f [] "f = []"

        let toStr x = List.map (fun y -> y.Path) x

        // 添加一個目錄
        let testDirCreation path d f =
            Directory.createDir (Path.join path "d1")
            let d1, f1 = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"] "d1"
            Expect.equal mf [] "f = []"

            let diff = Status.compare d d1
            Expect.equal (diff.Creation) ["d1"] "diff.Creation"
            d1, f1

        let d, f = testDirCreation path d f

        // 添加一個文件
        let testFileCreation path d f =
            let newFile = Path.join path "f1.txt"
            File.writeAllText newFile ""
            let d1, f1 = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"] "d1"
            Expect.equal mf ["f1.txt"] "f = []"
            d1, f1

        let d, f = testFileCreation path d f
        d |> ignore

        // 再添加一個目錄
        let testDirCreation2 path d f =
            Directory.createDir (Path.join path "d2")
            let d1, f1 = Status.fromPath path
            let md, mf = Tuple2.map toStr (d1, f1)
            Expect.equal md ["d1"; "d2"] "d1"
            Expect.equal mf ["f1.txt"] "f = []"

            let diff = Status.compare d d1
            Expect.equal (diff.Creation) ["d2"] "diff.Creation"
            d1, f1

        let d, f = testDirCreation2 path d f

        d |> ignore
    }

runTestsWithCLIArgs [] Array.empty mapTests
|> ignore
