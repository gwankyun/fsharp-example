module DirStatus
open System.IO
open Status
open Serilog
open FSLogger
open System.Text
open System.Text.Json
open System.Text.Json.Serialization

// 1. Either create the serializer options from the F# options...
let options =
    JsonFSharpOptions.Default()
        // Add any .WithXXX() calls here to customize the format
        .ToJsonSerializerOptions()

// 启用格式化（带缩进）
options.WriteIndented <- true

let logger = Logger.ColorConsole

module Directory =
    let exists path =
        Directory.Exists(path)

    let delete path recusive =
        Directory.Delete(path, recusive)

    let deleteIfExists path recusive =
        if exists path then
            delete path recusive

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

    let join (path: string list) =
        List.reduce join2 path

module File =
    let exists path =
        File.Exists(path)

let add path destination (exclude: string list option) =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    //printfn "添加操作：从 %s 到 %s" path destination
    logger.I $"path: {path} destination: {destination}"
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
    let text =
        JsonSerializer.Serialize<Status.FileSystemInfoDetail seq>(fileList, options)
    File.WriteAllText(destination, text, Encoding.UTF8)

let compare left right destination =
    printfn $"left: {left} right: {right} destination: {destination}"
    if File.exists left |> not then
        failwith $"{left} not exists"
    if File.exists right |> not then
        failwith $"{right} not exists"
    let fromPath path =
        let content = File.ReadAllText(path, Encoding.UTF8)
        JsonSerializer.Deserialize<Status.FileSystemInfoDetail seq>(content, options)
        //|> Seq.map (fun x -> x.FullName, x)
        //|> Map.ofSeq
    let leftJson = fromPath left
    let rightJson = fromPath right
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
    // 寫入文件
    let text =
        JsonSerializer.Serialize<Status.Difference seq>(diff, options)
    File.WriteAllText(destination, text, Encoding.UTF8)
    ()

let export path difference destination =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    printfn $"path: {path} difference: {difference} destination: {destination}"

let merge path destination =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    if Directory.exists destination |> not then
        failwith $"{destination} not exists"
    printfn $"path: {path} destination: {destination}"

let test path destination =
    if Directory.exists path |> not then
        failwith $"{path} must exists"

    let copyDirectory p d =
        if Directory.exists d then
            Directory.Delete(d, true)
        Directory.copyDirectory p d

    let src = Path.Combine(destination, "src")
    let dest = Path.Combine(destination, "dest")

    logger.D $"src: {src}"

    // 複製目錄
    copyDirectory path src

    let deleteFile = Path.join [src; "delete.txt"]
    let deleteFile2 = Path.join [src; "Tutorial"; "delete.txt"]

    // 先添加文件
    File.WriteAllText(deleteFile, "delete")
    File.WriteAllText(deleteFile2, "delete")

    let createDirectory path =
        Directory.CreateDirectory(path) |> ignore

    let deleteDir = Path.join [src; "delete"]
    let deleteDir2 = Path.join [src; "Tutorial"; "delete"]

    logger.D $"deleteDir: {deleteDir}"
    logger.D $"deleteDir2: {deleteDir2}"

    createDirectory deleteDir
    createDirectory deleteDir2

    copyDirectory src dest

    let status1 =  Path.Combine(destination, "1.json")
    let status2 =  Path.Combine(destination, "2.json")

    let deleteIfExists p =
        if File.Exists(p) then
            File.Delete(p)

    deleteIfExists status1
    deleteIfExists status2

    add src status1 None

    // 添加文件
    File.WriteAllText(Path.Combine(src, "add.txt"), "add")
    File.WriteAllText(Path.Combine(src, "Tutorial", "add.txt"), "add")

    // 添加目錄
    createDirectory <| Path.Join(src, "add")
    createDirectory <| Path.Join(src, "Tutorial", "add")

    // 修改文件
    File.WriteAllText(Path.Combine(src, "CMakeLists.txt"), "update")
    File.WriteAllText(Path.Combine(src, "Tutorial", "CMakeLists.txt"), "add")

    // 刪除文件
    deleteIfExists deleteFile
    deleteIfExists deleteFile2

    Directory.deleteIfExists deleteDir true
    Directory.deleteIfExists deleteDir2 true

    add src status2 None

    let diff =  Path.Combine(destination, "diff.json")
    deleteIfExists diff

    compare status1 status2 diff

    // 導出變更包

    //export src 
