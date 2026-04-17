module DirStatus
open System.IO
open Status
open Serilog
//open FSLogger
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open Expecto
open Common
open FSharpPlus

//let options =
//    JsonFSharpOptions.Default()
//        .ToJsonSerializerOptions()

//// 启用格式化（带缩进）
//options.WriteIndented <- true

//let logger = Logger.ColorConsole

let fromPath path =
    let content = File.ReadAllText(path, Encoding.UTF8)
    JsonSerializer.Deserialize<Status.RelativePath seq>(content, Common.options)

module Status =
    let add path (includeDir: string list) (exclude: string list) =
        //if exclude.IsSome then
        //    logger.I $"排除：{exclude.Value}"
        let fileList =
            Status.getChildItemIf path true (fun x ->
                let rela = Path.relativePath path x
                //logger.D $"rela: %s{rela}"
                //true
                //let inc = List.forall (fun i -> String.startsWith i rela) includeDir
                //let inc = List.contains rela includeDir
                let inc =
                    match includeDir |> List.isEmpty with
                    | true -> true
                    | false -> List.contains rela includeDir
                //let exc = List.forall (fun i -> not <| String.startsWith i rela) exclude
                //let exc = not <| List.contains rela exclude
                let exc =
                    match exclude |> List.isEmpty with
                    | true -> true
                    | false -> not <| List.contains rela exclude
                inc && exc
                )
            //true
            //|> Seq.map Status.toFileSystemInfoDetail
            //|> Seq.map (fun x ->
            //    { x with FullName = Path.relativePath path x.FullName})
            |> Seq.map (Status.toRelativePath path)
        //let fileList =
        //    match includeDir |> List.isEmpty with
        //    | true -> fileList
        //    | false ->
        //        fileList
        //        |> Seq.filter (fun x ->
        //            let startsWith i = x.Path |> String.startsWith i
        //            List.forall startsWith includeDir)
        //let fileList =
        //    match exclude |> List.isEmpty with
        //    | true -> fileList
        //    | false ->
        //        fileList
        //        |> Seq.filter (fun x ->
        //            let startsWith i = x.Path |> String.startsWith i |> not
        //            List.forall startsWith exclude)
        fileList

    type ItemType = Status.RelativePath

    let compare (leftJson: Status.RelativePath seq)
        (rightJson: ItemType seq) =

        let diff : Map<string, Status.Difference> =
            Status.compareObject leftJson rightJson (fun x -> x.Path)
            |> Map.filter (fun _ v ->
                match v with
                | Some le, Some ri ->
                    if le.IsContainer && ri.IsContainer then
                        false
                    else
                        //(le.PSIsContainer <> ri.PSIsContainer)
                        //|| (le.LastWriteTime <> ri.LastWriteTime)
                        //|| (le.Length <> ri.Length)
                        le <> ri
                | _ -> true
            )
            |> Map.map (fun _ v ->
                match v with
                | Some _, Some ri ->
                    { Status.Difference.Path = ri.Path;
                      Status.Difference.IsContainer = ri.IsContainer;
                      Status.Difference.Operation = Status.Modification }
                | Some le, None ->
                    { Path = le.Path;
                      IsContainer = le.IsContainer;
                      Operation = Status.Deletion }
                | None, Some ri ->
                    { Path = ri.Path;
                      IsContainer = ri.IsContainer;
                      Operation = Status.Creation }
                | _ -> failwith "fail"
            )
        let diff =
            diff
            |> Map.values
        diff

module Check =
    let dir path =
        if Directory.exists path |> not then
            failwith $"{path} not exists"

    let file path =
        if File.exists path |> not then
            failwith $"{path} not exists"

let init path destination name =
    Config.init path destination name

let appendJson p =
    match p |> String.endsWith ".json" with
    | true -> p
    | false -> p ++ ".json"

let add path (destination: string) (includeDir: string list) (exclude: string list) =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    logger.I $"path: {path} destination: {destination}"
    let destination = appendJson destination
    let fileList = Status.add path includeDir exclude
    let text =
        JsonSerializer.Serialize<Status.RelativePath seq>(fileList, Common.options)
    File.WriteAllText(destination, text, Encoding.UTF8)

let compare left right destination =
    printfn $"left: {left} right: {right} destination: {destination}"
    let left = appendJson left
    let right = appendJson right
    if File.exists left |> not then
        failwith $"{left} not exists"
    if File.exists right |> not then
        failwith $"{right} not exists"
    let leftJson = fromPath left
    let rightJson = fromPath right

    let destination = appendJson destination

    let diff = Status.compare leftJson rightJson
    logger.D $"diff: %A{diff}"
    // 寫入文件
    let text =
        JsonSerializer.Serialize<Status.Difference seq>(diff, Common.options)
    logger.D $"destination: {destination}"
    File.WriteAllText(destination, text, Encoding.UTF8)

let diffFrom path =
    let content = File.ReadAllText(path, Encoding.UTF8)
    JsonSerializer.Deserialize<Status.Difference seq>(content, Common.options)

let export path difference destination =
    let difference = appendJson difference
    logger.D $"path: {path}"
    logger.D $"difference: {difference}"
    logger.D $"destination: {destination}"
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    if File.exists difference |> not then
        failwith $"{difference} not exists"
    //printfn $"path: {path} difference: {difference} destination: {destination}"
    Directory.deleteIfExists destination true
    Directory.createDirectory destination
    let diff = diffFrom difference
    for i in diff do
        logger.I $"{i}"
        let destFile = destination +/ "data" +/ i.Path
        let createItem (i: Status.Difference) =
            if i.IsContainer then
                Directory.createDirectory destFile
            else
                let dir = Path.GetDirectoryName destFile
                Directory.createDirectory dir
                //File.Copy(path +/ i.Path, destFile)
                File.copyTo destFile true <| path +/ i.Path
        match i.Operation with
        | Status.Operation.Creation ->
            createItem i
        | Status.Operation.Modification ->
            createItem i
        | Status.Operation.Deletion -> ()
    //File.Copy(difference, destination +/ "diff.json")
    File.copyTo <| destination +/ "diff.json" <| true <| difference

let merge path destination =
    Check.dir path
    Check.dir destination
    Check.dir <| path +/ "data"
    Check.file <| path +/ "diff.json"
    let diff = diffFrom <| path +/ "diff.json"
    for i in diff do
        logger.I $"{i}"
        // 目標路徑
        let destPath = destination +/ i.Path
        // 源路徑
        let srcPath = path +/ "data" +/ i.Path
        let copyFile srcPath (destPath: string) =
            let dir = Path.GetDirectoryName destPath
            Directory.createDirectory dir
            File.Copy(srcPath, destPath)
        let creationOrModification (i: Status.Difference) =
            if i.IsContainer then
                Directory.createDirectory destPath
            else
                copyFile srcPath destPath
        match i.Operation with
        | Status.Operation.Creation ->
            Directory.deleteIfExists destPath true
            File.deleteIfExists destPath
            creationOrModification i
        | Status.Operation.Modification ->
            let destIsDir = Directory.exists destPath
            match i.IsContainer, destIsDir with
            | true, false ->
                File.delete destPath
                Directory.createDirectory destPath
            | false, false ->
                File.delete destPath
                copyFile srcPath destPath
            | false, true ->
                Directory.delete destPath true
                copyFile srcPath destPath
            | _ -> failwith $"can not"
        | Status.Operation.Deletion ->
            let destIsDir = Directory.exists destPath
            match i.IsContainer, destIsDir with
            | true, true ->
                Directory.delete destPath true
            | false, false ->
                File.deleteIfExists destPath
            | _, true ->
                Directory.delete destPath true
            | _, false ->
                File.deleteIfExists destPath
            //| _ -> failwith $"can not"

let writeAllText (path: string) (contents: string) =
    File.WriteAllText(path, contents)

let testPrepare path destination =
    if Directory.exists path |> not then
        failwith $"{path} must exists"

    let copyDirectory p d =
        Directory.deleteIfExists d true
        Directory.copyDirectory p d

    let src = destination +/ "src"
    let dest = destination +/ "dest"

    logger.D $"src: {src}"

    // 複製目錄
    copyDirectory path src

    let deleteFile = src +/ "delete.txt"
    let deleteFile2 = src +/ "Tutorial" +/ "delete.txt"

    // 先添加文件
    writeAllText deleteFile "delete"
    writeAllText deleteFile2 "delete"

    let deleteDir = src +/ "delete"
    let deleteDir2 = src +/ "Tutorial" +/ "delete"

    logger.D $"deleteDir: {deleteDir}"
    logger.D $"deleteDir2: {deleteDir2}"

    Directory.createDirectory deleteDir
    Directory.createDirectory deleteDir2

    copyDirectory src dest

    let status1 =  destination +/ "1.json"
    let status2 =  destination +/ "2.json"

    File.deleteIfExists status1
    File.deleteIfExists status2

let equal src dest =
    let srcStatus = Status.add src [] []
    let destStatus = Status.add dest [] []
    let diff = Status.compare srcStatus destStatus
    logger.D $"diff: %A{diff}"
    let mapTests =
        Expecto.Tests.test "map test" {
            Expect.equal (Seq.isEmpty diff) true ""
        }
    runTestsWithCLIArgs [] Array.empty mapTests
    |> ignore

let testUpdate path destination =
    let src = destination +/ "src"
    //let dest = destination +/ "dest"

    let deleteFile = src +/ "delete.txt"
    let deleteFile2 = src +/ "Tutorial" +/ "delete.txt"

    let deleteDir = src +/ "delete"
    let deleteDir2 = src +/ "Tutorial" +/ "delete"

    // 添加文件
    writeAllText <| src +/ "add.txt" <| "add"
    writeAllText <| src +/ "Tutorial" +/ "add.txt" <| "add"

    // 添加目錄
    Directory.createDirectory <| src +/ "add"
    Directory.createDirectory <| src +/ "Tutorial" +/ "add"

    // 修改文件
    writeAllText <| src +/ "CMakeLists.txt" <| "update"
    writeAllText <| src +/ "Tutorial" +/ "CMakeLists.txt" <| "add"

    // 刪除文件
    File.deleteIfExists deleteFile
    File.deleteIfExists deleteFile2

    Directory.deleteIfExists deleteDir true
    Directory.deleteIfExists deleteDir2 true

let test path destination =
    if Directory.exists path |> not then
        failwith $"{path} must exists"

    let src = destination +/ "src"
    let dest = destination +/ "dest"

    let statusBase = destination +/ "DirStatus" +/ "Step1" +/ "status"
    let status1 = statusBase +/ "1.json"
    let status2 = statusBase +/ "2.json"

    // 準備測試數據
    testPrepare path destination

    add src status1 [] []

    // 更新測試數據
    testUpdate path destination

    add src status2 [] []

    let diff = destination +/ "DirStatus" +/ "Step1" +/ "diff" +/ "diff.json"
    File.deleteIfExists diff

    compare status1 status2 diff

    //let exportPath = destination +/ "export" "diff"
    let exportPath =  destination +/ "DirStatus" +/ "Step1" +/ "export"

    // 導出變更包
    export src diff exportPath

    // 合併
    merge exportPath dest

    // 測試結果
    equal src dest
