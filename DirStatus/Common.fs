module Common
open System.Text.Json
open System.Text.Json.Serialization
open System.IO

let options =
    JsonFSharpOptions.Default()
        .ToJsonSerializerOptions()

// 启用格式化（带缩进）
options.WriteIndented <- true

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
            if not <| exists targetDir then
                createDirectory targetDir
            
            // 复制所有文件
            Directory.GetFiles sourceDir
            |> Array.iter (fun file ->
                let fileName = Path.GetFileName file
                let targetFile = targetDir +/ fileName
                File.Copy(file, targetFile, true) // true 表示覆盖已存在的文件
            )
            
            // 递归复制所有子目录
            Directory.GetDirectories sourceDir
            |> Array.iter (fun dir ->
                let dirName = Path.GetFileName dir
                let targetSubDir = targetDir +/ dirName
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
