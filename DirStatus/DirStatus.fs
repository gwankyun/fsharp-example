module DirStatus
open System.IO
open Status
open Serilog
open FSLogger
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open Expecto
open Common

//let options =
//    JsonFSharpOptions.Default()
//        .ToJsonSerializerOptions()

//// 启用格式化（带缩进）
//options.WriteIndented <- true

let logger = Logger.ColorConsole

let fromPath path =
    let content = File.ReadAllText(path, Encoding.UTF8)
    JsonSerializer.Deserialize<Status.RelativePath seq>(content, Common.options)

module Status =
    let add path (includeDir: string list) (exclude: string list) =
        //if exclude.IsSome then
        //    logger.I $"排除：{exclude.Value}"
        let fileList =
            Status.getChildItem path true
            //|> Seq.map Status.toFileSystemInfoDetail
            //|> Seq.map (fun x ->
            //    { x with FullName = Path.relativePath path x.FullName})
            |> Seq.map (Status.toRelativePath path)
        let fileList =
            match exclude |> List.isEmpty with
            | true ->
                fileList
                |> Seq.filter (fun x ->
                    let startsWith i = x.Path |> String.startsWith i
                    List.forall startsWith includeDir)
            | false -> fileList
        let fileList =
            match exclude |> List.isEmpty with
            | true ->
                fileList
                |> Seq.filter (fun x ->
                    let startsWith i = x.Path |> String.startsWith i |> not
                    List.forall startsWith exclude)
            | false -> fileList
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

let add path (destination: string) (includeDir: string list) (exclude: string list) =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    logger.I $"path: {path} destination: {destination}"
    let fileList = Status.add path includeDir exclude
    let text =
        JsonSerializer.Serialize<Status.RelativePath seq>(fileList, Common.options)
    File.WriteAllText(destination, text, Encoding.UTF8)

let compare left right destination =
    printfn $"left: {left} right: {right} destination: {destination}"
    if File.exists left |> not then
        failwith $"{left} not exists"
    if File.exists right |> not then
        failwith $"{right} not exists"
    let leftJson = fromPath left
    let rightJson = fromPath right

    let diff = Status.compare leftJson rightJson
    // 寫入文件
    let text =
        JsonSerializer.Serialize<Status.Difference seq>(diff, Common.options)
    File.WriteAllText(destination, text, Encoding.UTF8)

let diffFrom path =
    let content = File.ReadAllText(path, Encoding.UTF8)
    JsonSerializer.Deserialize<Status.Difference seq>(content, Common.options)

let export path difference destination =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    if File.exists difference |> not then
        failwith $"{difference} not exists"
    //printfn $"path: {path} difference: {difference} destination: {destination}"
    logger.D $"path: {path}"
    logger.D $"difference: {difference}"
    logger.D $"destination: {destination}"
    Directory.deleteIfExists destination true
    Directory.createDirectory destination
    let diff = diffFrom difference
    for i in diff do
        logger.I $"{i}"
        let destFile = destination +/ "data" +/ i.Path
        let creationOrModification (i: Status.Difference) =
            if i.IsContainer then
                Directory.createDirectory destFile
            else
                let dir = Path.GetDirectoryName destFile
                Directory.createDirectory dir
                File.Copy(path +/ i.Path, destFile)
        match i.Operation with
        | Status.Operation.Creation ->
            creationOrModification i
        | Status.Operation.Modification ->
            creationOrModification i
        | Status.Operation.Deletion -> ()
    File.Copy(difference, destination +/ "diff.json")

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
                File.delete destPath
            | _ -> failwith $"can not"

let test path destination =
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

    let writeAllText (path: string) (contents: string) =
        File.WriteAllText(path, contents)

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

    add src status1 [] []

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

    add src status2 [] []

    let diff =  destination +/ "diff.json"
    File.deleteIfExists diff

    compare status1 status2 diff

    // 導出變更包
    export src diff <| destination +/ "diff"

    // 合併
    merge <| destination +/ "diff" <| dest

    // 測試結果
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
