module Common
open System.Text.Json.Serialization
open System.IO
open FSLogger
open System.Diagnostics

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

//module String =
//    let startsWith (sub: string) (str: string) =
//        str.StartsWith(sub)

module Path =
    let relativePath relativeTo path =
        Path.GetRelativePath(relativeTo, path)

    let join2 (path1: string) path2 =
        Path.Join(path1, path2)

    let join3 (path1: string) path2 path3 =
        Path.Join(path1, path2, path3)

    let join (path: string list) =
        List.reduce join2 path

    let isPathFullyQualified (path: string) =
        Path.IsPathFullyQualified(path)

module File =
    let exists path =
        File.Exists(path)

    let delete path =
        File.Delete(path)

    let deleteIfExists p =
        if exists p then
            delete p

    let copyFrom src overwrite dest =
        File.Copy(src, dest, overwrite)

    let copyTo dest overwrite src =
        File.Copy(src, dest, overwrite)

    let setLastWriteTimeUtc (path: string) lastWriteTimeUtc =
        File.SetLastWriteTimeUtc(path, lastWriteTimeUtc)

// AI生成：節流進度——總量 ≤50 逐項打；否則每秒最多一行（逐項打日誌在大目錄時會拖慢掃描），
// 滿 2 秒後附帶 ETA。
module Progress =
    let over (total: int) (what: string) =
        let sw = Stopwatch.StartNew()
        let mutable last = 0L
        let mutable i = 0
        let perItem = total <= 50
        fun (current: string) ->
            i <- i + 1
            let el = sw.ElapsedMilliseconds
            if perItem || el - last >= 1000 then
                let eta =
                    if el >= 2000L then
                        let remainMs = float el / float i * float (total - i)
                        $"，預計剩餘 {remainMs / 1000.0:F0}s"
                    else ""
                logger.I $"{what}: {i}/{total}（{current}）{eta}"
                last <- el
