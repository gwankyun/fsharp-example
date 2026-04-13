namespace Status
open System.IO

module Status =
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

    type RelativePath = {
        Path: string
        IsContainer: bool
        Length: int64
        LastWriteTime: System.DateTime
    }

    let toRelativePath path item =
        match item with
        | FileItem fileInfo ->
            {
                Path = Path.GetRelativePath(path, fileInfo.FullName )
                IsContainer = false
                Length = fileInfo.Length
                LastWriteTime = fileInfo.LastWriteTime
            }
        | DirItem dirInfo ->
            {
                Path = Path.GetRelativePath(path, dirInfo.FullName )
                IsContainer = true
                Length = 0L  // 目录没有长度
                LastWriteTime = dirInfo.LastWriteTime
            }

    type Operation =
    | Creation
    | Modification
    | Deletion

    type Difference = {
        Path: string
        IsContainer: bool
        Operation: Operation
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
                // 先添加当前目录本身
                yield DirItem <| DirectoryInfo path 

                // 获取当前目录中的所有文件
                for file in Directory.getFiles path do
                    yield FileItem <| FileInfo file
                
                // 获取当前目录中的所有子目录
                for dir in Directory.getDirectories path do
                    //yield DirItem <| DirectoryInfo dir
                    
                    // 如果需要递归，继续处理子目录
                    if recurse then
                        yield! getChildItem dir recurse
        }

    /// 递归获取目录中的所有文件系统项
    let rec getChildItemIf (path: string) (recurse: bool) (pred: string -> bool) =
        seq {
            // 检查路径是否存在
            if not (Directory.exists path || File.exists path) then
                yield! Seq.empty
            else if File.exists path then
                // 如果是文件，直接返回
                yield FileItem <| FileInfo path
            else
                // // 先添加当前目录本身
                // yield DirItem <| DirectoryInfo path 

                // // 获取当前目录中的所有文件
                // for file in Directory.getFiles path do
                //     yield FileItem <| FileInfo file
                
                // // 获取当前目录中的所有子目录
                // for dir in Directory.getDirectories path do
                //     //yield DirItem <| DirectoryInfo dir
                    
                //     // 如果需要递归，继续处理子目录
                //     if recurse then
                //         yield! getChildItem dir recurse
                // 只有当目录满足过滤条件时，才获取目录本身及其内容
                if pred path then
                    // 添加当前目录本身
                    yield DirItem <| DirectoryInfo path 

                    // 获取当前目录中的所有文件
                    for file in Directory.getFiles path do
                        if pred file then
                            yield FileItem <| FileInfo file
                    
                    // 获取当前目录中的所有子目录
                    for dir in Directory.getDirectories path do
                        // 如果需要递归，继续处理子目录
                        if recurse then
                            yield! getChildItemIf dir recurse pred
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
