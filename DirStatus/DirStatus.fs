module DirStatus
open System.IO
open Status
open Serilog
open FSLogger
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open Expecto

let options =
    JsonFSharpOptions.Default()
        .ToJsonSerializerOptions()

// 启用格式化（带缩进）
options.WriteIndented <- true

let logger = Logger.ColorConsole

let inline (+/) (path1: string) path2 =
    Path.Join(path1, path2)

module Directory =
    let exists path =
        Directory.Exists(path)

    let delete path recusive =
        Directory.Delete(path, recusive)

    let deleteIfExists path recusive =
        if exists path then
            delete path recusive

    let createDirectory path =
        Directory.CreateDirectory path |> ignore

    /// 复制目录及其所有内容
    let rec copyDirectory sourceDir targetDir =
        // 确保源目录存在
        if exists sourceDir then
            // 创建目标目录（如果不存在）
            if not (exists targetDir) then
                Directory.CreateDirectory targetDir |> ignore
            
            // 复制所有文件
            Directory.GetFiles sourceDir
            |> Array.iter (fun file ->
                let fileName = Path.GetFileName file
                let targetFile = Path.Combine(targetDir, fileName)
                File.Copy(file, targetFile, true) // true 表示覆盖已存在的文件
            )
            
            // 递归复制所有子目录
            Directory.GetDirectories sourceDir
            |> Array.iter (fun dir ->
                let dirName = Path.GetFileName dir
                let targetSubDir = Path.Combine(targetDir, dirName)
                copyDirectory dir targetSubDir
            )
        else
            failwithf "源目录不存在: %s" sourceDir

module String =
    let startsWith (sub: string) (str: string) =
        str.StartsWith(sub)

module Path =
    let relativePath relativeTo path =
        Path.GetRelativePath(relativeTo, path)

    let join2 (path1: string) path2 =
        Path.Join(path1, path2)

    let join3 (path1: string) path2 path3 =
        Path.Join(path1, path2, path3)

    let join (path: string list) =
        List.reduce join2 path

module File =
    let exists path =
        File.Exists(path)

    let delete path =
        File.Delete(path)

    let deleteIfExists p =
        if exists p then
            delete p

let fromPath path =
    let content = File.ReadAllText(path, Encoding.UTF8)
    JsonSerializer.Deserialize<Status.FileSystemInfoDetail seq>(content, options)

module Status =
    let add path (exclude: string list option) =
        if exclude.IsSome then
            logger.I $"排除：{exclude.Value}"
        let fileList =
            Status.getChildItem path true
            |> Seq.map Status.toFileSystemInfoDetail
            |> Seq.map (fun x ->
                { x with FullName = Path.relativePath path x.FullName})
        let fileList =
            match exclude with
            | Some e ->
                fileList
                |> Seq.filter (fun x ->
                    let startsWith i = x.Name |> String.startsWith i |> not
                    List.forall startsWith e)
            | None -> fileList
        fileList

    let compare (leftJson: Status.FileSystemInfoDetail seq)
        (rightJson: Status.FileSystemInfoDetail seq) =

        let diff : Map<string, Status.Difference> =
            Status.compareObject leftJson rightJson (fun x -> x.FullName)
            |> Map.filter (fun k v ->
                match v with
                | Some le, Some ri ->
                    if le.PSIsContainer && ri.PSIsContainer then
                        false
                    else
                        (le.PSIsContainer <> ri.PSIsContainer)
                        || (le.LastWriteTime <> ri.LastWriteTime)
                        || (le.Length <> ri.Length)
                | _ -> true
            )
            |> Map.map (fun k v ->
                match v with
                | Some _, Some ri ->
                    { Status.Difference.FullName = ri.FullName;
                      Status.Difference.PSIsContainer = ri.PSIsContainer;
                      Status.Difference.Operation = Status.Modification }
                | Some le, None ->
                    { FullName = le.FullName;
                      PSIsContainer = le.PSIsContainer;
                      Operation = Status.Deletion }
                | None, Some ri ->
                    { FullName = ri.FullName;
                      PSIsContainer = ri.PSIsContainer;
                      Operation = Status.Creation }
                | _ -> failwith "fail"
            )
        let diff =
            diff
            |> Map.values
        diff

let add path destination (exclude: string list option) =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    logger.I $"path: {path} destination: {destination}"
    let fileList = Status.add path exclude
    let text =
        JsonSerializer.Serialize<Status.FileSystemInfoDetail seq>(fileList, options)
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
        JsonSerializer.Serialize<Status.Difference seq>(diff, options)
    File.WriteAllText(destination, text, Encoding.UTF8)

let diffFrom path =
    let content = File.ReadAllText(path, Encoding.UTF8)
    JsonSerializer.Deserialize<Status.Difference seq>(content, options)

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
        let destFile = destination +/ "data" +/ i.FullName
        let creationOrModification (i: Status.Difference) =
            if i.PSIsContainer then
                Directory.createDirectory destFile
            else
                let dir = Path.GetDirectoryName destFile
                Directory.createDirectory dir
                File.Copy(path +/ i.FullName, destFile)
        match i.Operation with
        | Status.Operation.Creation ->
            creationOrModification i
        | Status.Operation.Modification ->
            creationOrModification i
        | Status.Operation.Deletion -> ()
    File.Copy(difference, destination +/ "diff.json")

module Check =
    let dir path =
        if Directory.exists path |> not then
            failwith $"{path} not exists"

    let file path =
        if File.exists path |> not then
            failwith $"{path} not exists"

let merge path destination =
    Check.dir path
    Check.dir destination
    Check.dir <| path +/ "data"
    Check.file <| path +/ "diff.json"
    let diff = diffFrom <| path +/ "diff.json"
    for i in diff do
        logger.I $"{i}"
        // 目標路徑
        let destPath = destination +/ i.FullName
        // 源路徑
        let srcPath = path +/ "data" +/ i.FullName
        let copyFile srcPath (destPath: string) =
            let dir = Path.GetDirectoryName destPath
            Directory.createDirectory dir
            File.Copy(srcPath, destPath)
        let creationOrModification (i: Status.Difference) =
            if i.PSIsContainer then
                Directory.createDirectory destPath
            else
                copyFile srcPath destPath
        match i.Operation with
        | Status.Operation.Creation ->
            Directory.deleteIfExists destPath true
            File.deleteIfExists destPath
            creationOrModification i
        | Status.Operation.Modification ->
            //creationOrModification i
            let destIsDir = Directory.exists destPath
            match i.PSIsContainer, destIsDir with
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
            match i.PSIsContainer, destIsDir with
            | true, true ->
                Directory.delete destPath true
            | false, false ->
                File.delete destPath
            | _ -> failwith $"can not"

let test path destination =
    if Directory.exists path |> not then
        failwith $"{path} must exists"

    let copyDirectory p d =
        if Directory.exists d then
            Directory.Delete(d, true)
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

    add src status1 None

    // 添加文件
    writeAllText <| src +/ "add.txt" <| "add"
    writeAllText <| src +/ "Tutorial" +/ "add.txt" <| "add"

    // 添加目錄
    Directory.createDirectory <| src +/ "add"
    Directory.createDirectory <| Path.Join(src, "Tutorial", "add")

    // 修改文件
    writeAllText <| src +/ "CMakeLists.txt" <| "update"
    writeAllText <| src +/ "Tutorial" +/ "CMakeLists.txt" <| "add"

    // 刪除文件
    File.deleteIfExists deleteFile
    File.deleteIfExists deleteFile2

    Directory.deleteIfExists deleteDir true
    Directory.deleteIfExists deleteDir2 true

    add src status2 None

    let diff =  destination +/ "diff.json"
    File.deleteIfExists diff

    compare status1 status2 diff

    // 導出變更包
    export src diff <| destination +/ "diff"

    // 合併
    merge <| destination +/ "diff" <| dest

    // 測試結果
    let srcStatus = Status.add src None
    let destStatus = Status.add dest None
    let diff = Status.compare srcStatus destStatus
    logger.D $"diff: %A{diff}"
    let mapTests =
        Expecto.Tests.test "map test" {
            Expect.equal (diff |> Seq.isEmpty) true ""
        }
    runTestsWithCLIArgs [] Array.empty mapTests
    |> ignore
