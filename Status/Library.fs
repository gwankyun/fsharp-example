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
        /// 文件内容哈希（SHA-256 十六进制小写）。目录没有内容，用空串。
        Hash: string
    }

    /// 计算文件内容哈希（SHA-256，十六进制小写）。
    let hashFile (path: string) =
        use stream = System.IO.File.OpenRead path
        use sha = System.Security.Cryptography.SHA256.Create()
        System.Convert.ToHexString(sha.ComputeHash stream).ToLowerInvariant()

    /// 用给定的哈希解析器构造快照项。解析器接收（相对路径, FileInfo），
    /// 调用方可以借此复用缓存里已有的哈希（stat 快路径），避免重复读盘。
    let toRelativePathWith (resolveHash: string -> FileInfo -> string) path item =
        match item with
        | FileItem fileInfo ->
            let relPath = Path.GetRelativePath(path, fileInfo.FullName)
            {
                Path = relPath
                IsContainer = false
                Length = fileInfo.Length
                LastWriteTime = fileInfo.LastWriteTime
                Hash = resolveHash relPath fileInfo
            }
        | DirItem dirInfo ->
            {
                Path = Path.GetRelativePath(path, dirInfo.FullName )
                IsContainer = true
                Length = 0L  // 目录没有长度
                LastWriteTime = dirInfo.LastWriteTime
                Hash = ""
            }

    /// 默认实现：直接读取文件内容求哈希。
    let toRelativePath path item =
        toRelativePathWith (fun _ fileInfo -> hashFile fileInfo.FullName) path item

    type Operation =
    | Creation
    | Modification
    | Deletion

    type Difference = {
        Path: string
        IsContainer: bool
        Operation: Operation
        /// 目標（右側／來源）內容的 SHA-256。Deletion 沒有目標內容，用空串。
        /// export 與 merge 據此校驗拷出來的位元組是否與清單相符。
        Hash: string
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
