// For more information see https://aka.ms/fsharp-console-apps
open System.IO

//printfn "Hello from F#"

/// 表示文件系统项的联合类型
type FileSystemItem =
    | FileItem of FileInfo
    | DirItem of DirectoryInfo

/// 详细的文件系统信息类型
type FileSystemInfoDetail = {
    FullName: string
    Name: string
    PSIsContainer: bool
    Length: int64
    LastWriteTime: System.DateTime
    Attributes: FileAttributes
}

module Directory =
    let exists path =
        Directory.Exists(path)

    let getFiles path =
        Directory.GetFiles(path)

    let getDirectories path =
        Directory.GetDirectories(path)

module File =
    let exists path =
        File.Exists(path)

/// 递归获取目录中的所有文件系统项
let rec getChildItem (path: string) (recurse: bool) =
    seq {
        // 检查路径是否存在
        if not (Directory.exists path || File.exists path) then
            yield! Seq.empty
        else if File.exists path then
            // 如果是文件，直接返回
            yield FileItem <| FileInfo path
        else
            // 获取当前目录中的所有文件
            for file in Directory.getFiles path do
                yield FileItem <| FileInfo file
            
            // 获取当前目录中的所有子目录
            for dir in Directory.getDirectories path do
                yield DirItem <| DirectoryInfo dir
                
                // 如果需要递归，继续处理子目录
                if recurse then
                    yield! getChildItem dir recurse
    }

/// 将 FileSystemItem 转换为详细信息
let toFileSystemInfoDetail item =
    match item with
    | FileItem fileInfo ->
        {
            FullName = fileInfo.FullName
            Name = fileInfo.Name
            PSIsContainer = false
            Length = fileInfo.Length
            LastWriteTime = fileInfo.LastWriteTime
            Attributes = fileInfo.Attributes
        }
    | DirItem dirInfo ->
        {
            FullName = dirInfo.FullName
            Name = dirInfo.Name
            PSIsContainer = true
            Length = 0L  // 目录没有长度
            LastWriteTime = dirInfo.LastWriteTime
            Attributes = dirInfo.Attributes
        }

/// 比较两个序列，返回一个 Map，其中键是通过 keySelector 提取的，
/// 值是 (左侧元素选项, 右侧元素选项) 的元组
let compareObject<'a, 'b when 'b : comparison>
    (left: seq<'a>)
    (right: seq<'a>)
    (keySelector: 'a -> 'b) : Map<'b, 'a option * 'a option> =
    // 第一步：处理左侧序列，构建初始 Map
    let leftMap = 
        left
        |> Seq.map (fun item -> keySelector item, (Some item, None))
        |> Map.ofSeq

    // 第二步：处理右侧序列，更新 Map
    right
    |> Seq.fold (fun map item ->
        let key = keySelector item
        match map.TryFind key with
        | Some (leftItem, _) ->
            // 键已存在，更新右侧元素
            Map.add key (leftItem, Some item) map
        | None ->
            // 键不存在，添加新条目
            Map.add key (None, Some item) map
    ) leftMap

exit 0
