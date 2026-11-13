// namespace DirStatus
module Record

open System
open System.IO
open System.Text.Json
open System.Text
open Common
open System.IO.Hashing
open System.Collections.Generic

module Record =
    type FileEntry = {
        Path: string
        Size: int64
        MTimeUtc: DateTime
        Hash: string option
    }

    type Dir = {
        PathEntry: string
        // MTimeUtc: DateTime
    }

    type Entry =
        | File of FileEntry
        | Dir of Dir

    module Entry =
        let path e =
            match e with
            | Dir { PathEntry = path } -> path
            | File { Path = path } -> path

        let equal e1 e2 =
            match e1, e2 with
            | File f1, File f2 -> f1 = f2
            | Dir d1, Dir d2 -> d1 = d2
            | _ -> false

        let isDir e =
            match e with
            | Dir _ -> true
            | _ -> false

        let isFile e =
            match e with
            | File _ -> true
            | _ -> false

    /// 階段A收集的文件元數據：列舉當下就拿到 Size/MTime，階段B不必再逐檔 stat。
    [<Struct>]
    type FileMeta = {
        Path: string
        Size: int64
        MTimeUtc: DateTime
    }

    let jsonToMap p =
        let text = File.ReadAllText(p, Encoding.UTF8)
        JsonSerializer.Deserialize<Map<string, Entry>>(text, Common.options)

    let relativePath root path =
        Path.GetRelativePath(root, path)

    let computeFileXxh128 (filePath: string) : string option =
        try
            use stream = File.OpenRead(filePath)
            let xxh = XxHash128()
            xxh.Append(stream)
            let hashBytes = xxh.GetHashAndReset()
            let hexStr = BitConverter.ToString(hashBytes).Replace("-", "").ToLowerInvariant()
            Some hexStr
        with
        | _ -> None

    /// --exclude 只接受「相對於 --path 的目錄路徑」。
    /// a、a\b 都收，a/b 也當成 a\b；結尾的斜線和開頭的 .\ 只是同一條路徑的另一種寫法，吸收掉。
    /// 無法當成相對路徑解釋的（絕對路徑、磁碟代號、..、萬用字元）直接報錯，不要靜默失效。
    let normalizeExclude (e: string) : string =
        let sep = Path.DirectorySeparatorChar
        let replaced = e.Replace('/', sep)
        let stripped =
            match replaced.StartsWith("." + string sep) with
            | true -> replaced.Substring 2
            | false -> replaced
        let body = stripped.TrimEnd sep
        let hint = $"請寫相對於 --path 的目錄路徑，例如 .git 或 a{sep}b"
        if body = "" || body = "." then
            failwith $"exclude 不能是空的或 \".\"：{e}（{hint}）"
        elif Path.IsPathRooted body || body.Contains ":" then
            failwith $"exclude 不接受絕對路徑或磁碟代號：{e}（{hint}）"
        elif body.Split sep |> Array.exists ((=) "..") then
            failwith $"exclude 不能用 \"..\"：{e}"
        elif body.IndexOfAny [| '*'; '?'; '"' |] >= 0 then
            failwith $"exclude 不支援萬用字元或引號：{e}"
        else
            body

    /// --exclude 的清單 → scan 用的 keepDir，外加一個「掃完之後」的未命中報告。
    /// 大小寫用 OrdinalIgnoreCase 跟 Windows 檔案系統一致；hit 必須用同一個 comparer，
    /// 否則使用者寫 "SRC" 對上磁碟上的 "src" 會被誤報成未命中。
    let excludeToKeepDir (excludeDir: string list) : (string -> bool) * (unit -> unit) =
        let excluded =
            HashSet<string>(excludeDir |> Seq.map normalizeExclude, StringComparer.OrdinalIgnoreCase)
        let hit = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let keepDir (d: string) =
            match excluded.Contains d with
            | true ->
                hit.Add d |> ignore
                false
            | false -> true
        let report () =
            for e in excluded do
                if not <| hit.Contains e then
                    logger.W $"exclude 本次列舉未命中任何目錄：{e}"
        keepDir, report

    // AI生成：改為「先收集再處理」兩階段——第一階段只走目錄樹收集路徑（純元數據，便宜），
    // 第二階段逐文件算哈希。枚舉只發生一次、總量免費得到，掃描進度才有 i/total 可報。
    /// keepDir 收「相對於 root 的目錄路徑」，回 false 就整棵子樹不掃（連列舉都不會發生）。
    /// root 自己不受 keepDir 管：它一定會被掃，不然底下什麼都沒有。
    /// m 是緩存
    let scan (rootPath: string) (cache: Map<string, Entry>) (keepDir: string -> bool) : Entry seq =
        seq {
            let relaPath = relativePath rootPath

            // 階段A：一次列舉目錄與檔案。EnumerateFileSystemInfos 讓每個目錄只讀一次，
            // 且 Windows 上 FileInfo 的 Size/MTime 直接來自列舉緩衝區，階段B不必再逐檔 stat。
            let dirs = ResizeArray<string>()
            let files = ResizeArray<FileMeta>()
            let stack = Stack<string>()
            stack.Push(rootPath)
            while stack.Count > 0 do
                let currentDir = stack.Pop()
                try
                    dirs.Add currentDir
                    let subDirs = ResizeArray<string>()
                    for fsi in DirectoryInfo(currentDir).EnumerateFileSystemInfos() do
                        match fsi with
                        | :? DirectoryInfo as d ->
                            let p = d.FullName
                            // 在下降處就剪掉，被排除的目錄連列舉都不會發生
                            if keepDir (relaPath p) then
                                subDirs.Add p
                        | :? FileInfo as f ->
                            files.Add {
                                Path = f.FullName
                                Size = f.Length
                                MTimeUtc = f.LastWriteTimeUtc
                            }
                        | _ -> ()
                    // 反著 push，讓 Stack 彈出的順序和原本 Seq.rev 的結果一致
                    for i = subDirs.Count - 1 downto 0 do
                        stack.Push subDirs[i]
                with
                | :? UnauthorizedAccessException -> ()
                | _ -> ()

            for d in dirs do
                yield Dir {
                    PathEntry = relaPath d
                }

            // 階段B：逐文件算哈希（貴的部分），帶節流進度
            let progress = Progress.over files.Count "scan"
            for file in files do
                try
                    // key 要和 pathToMap 存的 Entry.path 一致：相對 root 的路徑
                    let rel = relaPath file.Path
                    progress rel
                    let e = Map.tryFind rel cache
                    let h =
                        match e with
                        | Some (File {
                            Size = s
                            MTimeUtc = t
                            Hash = Some ha
                            }) when (file.Size, file.MTimeUtc) = (s, t) ->
                            Some ha
                        | _ -> computeFileXxh128 file.Path
                    yield File {
                        Path = rel
                        Size = file.Size
                        MTimeUtc = file.MTimeUtc
                        Hash = h
                    }
                with
                | _ -> ()
        }

    // 複製整棵目錄樹。骨架和 scan 一樣：棧 + 下鑽前過濾。
    /// keepDir 收的是「相對於 src 的目錄路徑」，回 false 的目錄整棵子樹都不複製。
    let copyTree (src: string) (dst: string) (keepDir: string -> bool) =
        let relaPath = relativePath src
        let stack = Stack<string>()
        stack.Push src
        while stack.Count > 0 do
            let currentDir = stack.Pop()

            // 一 pop 出來就建；「父先於子」由遍歷順序保證
            Directory.CreateDirectory(Path.Combine(dst, relaPath currentDir)) |> ignore

            let subDirs =
                Directory.EnumerateDirectories(currentDir)
                |> Seq.filter (fun x -> keepDir (relaPath x))
                |> Seq.rev
                |> List.ofSeq
            for d in subDirs do stack.Push d

            for f in Directory.EnumerateFiles(currentDir) do
                let target = Path.Combine(dst, relaPath f)
                File.Copy(f, target, true)
                File.SetLastWriteTimeUtc(target, File.GetLastWriteTimeUtc f)   // File.Copy 不保留 mtime

    let add path cache keepDir =
        let s = scan path cache keepDir
        let m =
            s
            |> Seq.map (fun x -> Entry.path x, x)
            |> Map.ofSeq
        m

    let predAddFile k (l, r) =
        match l, r with
        | Some (File li), Some (File ri) when li <> ri ->
            // 都是文件且已修改
            true
        | Some (Dir _), Some (File ri) ->
            // 從目錄變成文件
            true
        | None, Some (File ri) ->
            // 新增文件
            true
        | _ -> false

    let predAddDir k (l, r) =
        match l, r with
        | None, Some (Dir _) -> true
        | Some (File _), Some (Dir _) -> true
        | _ -> false

    let predRemoveDir k (l, r) =
        match l, r with
        | Some (Dir _), None -> true
        | _ -> false

    let predRemoveFile k (l, r) =
        match l, r with
        | Some (File _), None -> true
        | _ -> false

    /// 給 Map.compareGroup 的第二個返回值用：那裡的鍵是「兩邊都有」，
    /// 值是一對裸 Entry（不是 option），有沒有改要在這裡判斷。
    /// 只看文件：Dir 的 MTimeUtc 沒有可信來源（copyTree 只補回文件的 mtime），
    /// 拿它比會讓整棵樹的目錄全部被判成「已修改」。
    let predUpdateFile k (l: Entry, r: Entry) =
        match l, r with
        | File li, File ri -> li <> ri
        | _ -> false

    //let add path =
    //    Map.empty
