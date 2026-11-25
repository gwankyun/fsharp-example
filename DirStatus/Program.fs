open Argu
open Common
open System
open System.IO
open System.Collections.Generic
open System.IO.Hashing
open System.Diagnostics
open System.Text.Json
open System.Text
open FSharpPlus
open Expecto
open Record
open AttributeCommon
open Args

// type PathAttribute() =
//     inherit AltCommandLineAttribute([|"-p"|])

// type DestAttribute() =
//     inherit AltCommandLineAttribute([|"-d"; "--dest"|])

// type PrevAttribute() =
//     inherit AltCommandLineAttribute([|"--prev"|])

// type NameAttribute() =
//     inherit AltCommandLineAttribute([|"-n"|])

// type NoPrefixAttribute() =
//     inherit CliPrefixAttribute(CliPrefix.None)

module ArgText =
    let path = "指定目錄。"
    let name = "指定配置名稱。"
    let dest = "輸出路徑"
    let test = "，僅內部使用"

type InitArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string
    | [<Name>] Name of name: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> ArgText.path
            | Destination _ -> ArgText.dest
            | Name _ -> ArgText.name



type AddArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string
    | [<Name>] Name of name: string
    | Include of includeDir: string list
    | Exclude of exclude: string list

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> ArgText.path
            | Destination _ -> ArgText.dest
            | Include _ -> "包含目錄。"
            | Exclude _ -> "排除目錄。"
            | Name _ -> ArgText.name

type XAddArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of path: string
    | [<Prev>] Previous of path: string
    | Exclude of excludeDir: string list

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "操作目錄"
            | Destination _ -> "輸出文件"
            | Previous _ -> "以前"
            | Exclude _ -> "排除目錄。"

type XCompareArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of path: string
    | Left of left: string
    | Right of right: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "操作目錄"
            | Destination _ -> "輸出文件"
            | Left _ -> "左"
            | Right _ -> "右"

type XMergeArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of path: string
    | [<Prev>] Previous of path: string
    | Exclude of excludeDir: string list

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "操作目錄"
            | Destination _ -> "輸出文件"
            | Previous _ -> "以前"
            | Exclude _ -> "排除目錄。"

type XTestArgs =
    | [<Path>] Path of path: string
    //| [<Dest>] Destination of destination: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "操作目錄"
            //| Destination _ -> ArgText.dest

type CompareArgs =
    | Left of left: string
    | Right of right: string
    | [<Name>] Name of name: string
    | [<Dest>] Destination of destination: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Left _ -> "左"
            | Right _ -> "右"
            | Destination _ -> ArgText.dest
            | Name _ -> ArgText.name

type ExportArgs =
    | [<Path>] Path of path: string
    | [<AltCommandLine("--diff")>] Difference of difference: string
    | [<Dest>] Destination of destination: string
    | [<Name>] Name of name: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> ArgText.path
            | Difference _ -> "差異文件。"
            | Destination _ -> ArgText.dest
            | Name _ -> ArgText.name

type MergeArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> ArgText.path
            | Destination _ -> ArgText.dest

type TestArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> ArgText.path
            | Destination _ -> ArgText.dest

type TestPrepareArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> ArgText.path
            | Destination _ -> ArgText.dest

type TestUpdateArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> ArgText.path
            | Destination _ -> ArgText.dest

type EqualArgs =
    | Left of left: string
    | Right of right: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Left _ -> "左。"
            | Right _ -> "右。"

// 主命令类型
type MainArgs =
    | [<NoPrefix>] ZAdd of ParseResults<AddArgs>
    | [<NoPrefix>] XAdd of ParseResults<XAddArgs>
    | [<NoPrefix>] Compare of ParseResults<CompareArgs>
    | [<NoPrefix>] XCompare of ParseResults<XCompareArgs>
    | [<NoPrefix>] Export of ParseResults<ExportArgs>
    | [<NoPrefix>] ZMerge of ParseResults<MergeArgs>
    | [<NoPrefix>] XMerge of ParseResults<XMergeArgs>
    | [<NoPrefix>] Test of ParseResults<TestArgs>
    | [<NoPrefix>] XTest of ParseResults<XTestArgs>
    | [<NoPrefix>] TestPrepare of ParseResults<TestPrepareArgs>
    | [<NoPrefix>] TestUpdate of ParseResults<TestUpdateArgs>
    | [<NoPrefix>] ZEqual of ParseResults<EqualArgs>
    | [<NoPrefix>] ZInit of ParseResults<InitArgs>
    | [<NoPrefix>] Init of ParseResults<Args.InitArgs>
    | [<NoPrefix>] Add of ParseResults<Args.AddArgs>
    | [<NoPrefix>] Diff of ParseResults<Args.DiffArgs>
    | [<NoPrefix>] Merge of ParseResults<Args.MergeArgs>
    | [<NoPrefix>] Equal of ParseResults<Args.EqualArgs>
    | [<NoPrefix>] Update of ParseResults<Args.UpdateArgs>
    | Version

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | ZAdd _ -> "添加狀態。"
            | XAdd _ -> "添加狀態。"
            | Compare _ -> "對比兩個狀態。"
            | XCompare _ -> "對比兩個狀態。"
            | Export _ -> "導出變更包。"
            | ZMerge _ -> "合併變更包。"
            | XMerge _ -> "合併變更包。"
            | Version -> "顯示版本信息。"
            | Test _ -> $"測試{ArgText.test}"
            | XTest _ -> $"測試{ArgText.test}"
            | TestPrepare _ -> $"生成測試數據{ArgText.test}"
            | TestUpdate _ -> $"更新測試數據{ArgText.test}"
            | ZEqual _ -> $"比較測試結果{ArgText.test}"
            | ZInit _ -> "初始化配置。"
            | Init _ -> Args.CmdText.init
            | Add _ -> Args.CmdText.add
            | Diff _ -> Args.CmdText.diff
            | Merge _ -> Args.CmdText.merge
            | Equal _ -> Args.CmdText.equal
            | Update _ -> Args.CmdText.update

module Parse =
    let add (results: ParseResults<AddArgs>) =
        let name = results.TryGetResult(AddArgs.Name)
        let path = results.TryGetResult(AddArgs.Path)
        let dest = results.TryGetResult(AddArgs.Destination)
        let includeDir =
            results.TryGetResult(AddArgs.Include)
            |> Option.defaultValue []
        let exclude =
            results.TryGetResult(AddArgs.Exclude)
            |> Option.defaultValue []
        match name, path, dest with
        | Some n, _, Some d ->
            let config = Config.tryGet n
            match config with
            | Some c ->
                let dest = c.Destination +/ "status" +/ d
                DirStatus.add c.Path dest c.Include c.Exclude
            | None ->
                failwith $"未能找到{n}配置"
        | _, Some p, Some d ->
            DirStatus.add p d includeDir exclude
        | _ -> failwith "參數錯誤"

    let xadd (results: ParseResults<XAddArgs>) =
        let path = results.TryGetResult(XAddArgs.Path)
        let dest = results.TryGetResult(XAddArgs.Destination)
        let prev = results.TryGetResult(XAddArgs.Previous)
        // GetResults 是「同一旗標的每次出現都收回來」：--exclude a b 和 --exclude a --exclude b 都吃。
        // 用 TryGetResult 的話是「最後一次蓋掉前面」，--exclude a --exclude b 只會拿到 b，a 被靜默丟掉
        // （而且值根本沒進到這裡，連未命中警告都看不到）。
        // 沒共用成 helper 是因為參數型別是 Expr<'U -> 'T>，靠 F# 自動引號化，只有語法上的建構子吃得到。
        let exc = results.GetResults(XAddArgs.Exclude) |> List.concat
        logger.I $"excludeDir: {exc}"
        // 早點驗證 --exclude 的形狀：寫錯就立刻報錯，不要等掃完才發現
        let keepDir, report = Record.excludeToKeepDir exc
        let prevMap =
            match prev with
            | Some p -> Record.jsonToMap p
            | None -> Map.empty
        match path with
        | Some p ->
            let path = p
            logger.I $"path: {path}"
            // 輸出文件
            match dest with
            | Some d ->
                let sw = Stopwatch.StartNew()
                let m = Record.add path prevMap keepDir
                report ()   // Map.ofSeq 已完整枚舉，這裡報未命中才準
                let text = JsonSerializer.Serialize<Map<string, Record.Entry>>(m, Common.options)
                File.WriteAllText(d, text, Encoding.UTF8)
                sw.Stop()
                logger.I $"use time: %A{sw.Elapsed.TotalSeconds}"
            | None -> ()
        | None -> ()

    /// <summary>d</summary>
    /// <param name="result">差異</param>
    /// p 源
    ///.d 目標
    let xexport p d ri result =
        for i in result do
            logger.I $"file: {i}"
            // 複製文件
            let destFile = d +/ "data" +/ i
            Directory.createDirectoryFor destFile
            File.Copy(p +/ i, destFile, true)
        // status.json 也要走 createDirFor：沒有任何變更時上面的迴圈不跑，
        // d 根本不存在，直接 File.Copy 會 DirectoryNotFoundException。
        // overwrite 要 true，否則第二次跑就 IOException（上面那份是 true）。
        let statusFile = d +/ "status.json"
        Directory.createDirectoryFor statusFile
        File.Copy(ri, statusFile, true)

    let xcompare (results: ParseResults<XCompareArgs>) =
        let left = results.TryGetResult(XCompareArgs.Left)
        let right = results.TryGetResult(XCompareArgs.Right)
        let path = results.TryGetResult(XCompareArgs.Path)
        let dest = results.TryGetResult(XCompareArgs.Destination)

        let toSet = Map.keys >> Set.ofSeq
        let createDirFor (dest: string) =
            let targetDir = Path.GetDirectoryName(dest)
            Directory.CreateDirectory(targetDir) |> ignore

        match left, right with
        | Some le, Some ri ->
            let mLe = Record.jsonToMap le
            let mRi = Record.jsonToMap ri

            let compareReslut = Map.compare mLe mRi

            let result =
                compareReslut
                // 這裡是 filter 不是 map：Map.map 只是把值換成 bool，key 一個都不會少，
                // 接 Map.keys 等於沒過濾，連根目錄那筆 "." 都會被拿去 File.Copy 而炸掉。
                |> Map.filter Record.predAddFile
                |> Map.keys

            //let kLe = mLe |> toSet
            //let kRi = mRi |> toSet
            //let kSet = Set.union kLe kRi
            //let compareResult =
            //    kSet
            //    |> Set.toSeq
            //    |> Seq.map (fun k ->
            //        let tryFind = Map.tryFind k
            //        k, (tryFind mLe, tryFind mRi))
            //    |> Map.ofSeq
            //let result =
            //    Map.fold (fun f k (l, r) ->
            //        match l, r with
            //        | Some (Record.File li), Some (Record.File ri) when li <> ri ->
            //            // 都是文件且已修改
            //            ri.Path :: f
            //        | Some (Record.Dir _), Some (Record.File ri) ->
            //            // 從目錄變成文件
            //            ri.Path :: f
            //        | None, Some (Record.File ri) ->
            //            // 新增文件
            //            ri.Path :: f
            //        | _, _ -> f
            //    ) [] compareResult



            match path, dest with
            | Some p, Some d ->
                for i in result do
                    logger.I $"file: {i}"
                    // 複製文件
                    let destFile = d +/ "data" +/ i
                    createDirFor destFile
                    File.Copy(p +/ i, destFile, true)
                // status.json 也要走 createDirFor：沒有任何變更時上面的迴圈不跑，
                // d 根本不存在，直接 File.Copy 會 DirectoryNotFoundException。
                // overwrite 要 true，否則第二次跑就 IOException（上面那份是 true）。
                let statusFile = d +/ "status.json"
                createDirFor statusFile
                File.Copy(ri, statusFile, true)
            | _ -> ()
        | _ -> ()
        ()

    let depth (n: string) =
        //String.split ["\\"] n
        n.Split("\\", StringSplitOptions.RemoveEmptyEntries)
        |> Array.length

    module Sort =
        let addItem x = depth x, x

        let removeItem x = - depth x, x

    /// path 合併到 dest
    let xmerge (results: ParseResults<XMergeArgs>) =
        /// 補丁包目錄
        let path = results.TryGetResult(XMergeArgs.Path)
        /// 應用補丁包的目錄
        let dest = results.TryGetResult(XMergeArgs.Destination)
        /// 緩存
        let prev = results.TryGetResult(XMergeArgs.Previous)
        let excludeDir =
            results.TryGetResult(XMergeArgs.Exclude)
            |> Option.defaultValue []
        let keepDir, report = Record.excludeToKeepDir excludeDir
        let prevMap =
            match prev with
            | Some p -> Record.jsonToMap p
            | None -> Map.empty
        match path, dest with
        | Some p, Some d ->
            let destStatus = Record.add d prevMap keepDir // 目標狀態
            report ()
            let pathStatus = Record.jsonToMap (p +/ "status.json")
            let compareResult =
                Map.compare destStatus pathStatus
            //let depth (n: string) =
            //    //String.split ["\\"] n
            //    n.Split("\\", StringSplitOptions.RemoveEmptyEntries)
            //    |> Array.length
            let fromTop a b =
                let da = depth a
                let db = depth b
                let cmp = compare da db
                if cmp <> 0 then
                    cmp
                else
                    compare a b
                //compare (getDepth a) (getDepth b)
            //let topSort
            let fromButtom a b =
                //compare (getDepth b) (getDepth a)
                let da = depth a
                let db = depth b
                let cmp = compare db da
                if cmp <> 0 then
                    cmp
                else
                    compare a b
            let addDir =
                compareResult
                |> Map.filter Record.predAddDir
                |> Map.keys
                //|> Seq.sortWith fromTop
                |> Seq.sortBy (fun x -> depth x, x) // 由淺到深
            let addFile =
                compareResult
                |> Map.filter Record.predAddFile
                |> Map.keys
                //|> Seq.sortWith fromTop
                |> Seq.sortBy (fun x -> depth x, x) // 由淺到深
            let removeDir =
                compareResult
                |> Map.filter Record.predRemoveDir
                |> Map.keys
                //|> Seq.sortWith fromButtom
                |> Seq.sortBy (fun x -> - (depth x), x)
            let removeFile =
                compareResult
                |> Map.filter Record.predRemoveFile
                |> Map.keys
                //|> Seq.sortWith fromButtom
                |> Seq.sortBy (fun x -> - (depth x), x)
            // 開始操作。順序：建目錄（淺→深）→ 放檔案 → 刪檔案 → 刪目錄（深→淺），
            // 四個清單上面都排好了：建目錄要父先於子，刪目錄要子先於父（不然父目錄非空刪不掉）。
            for i in addDir do
                let target = d +/ i
                // (Some File, Some Dir)：dest 這格躺著一個檔案，CreateDirectory 會拋，先清掉
                File.deleteIfExists target
                Directory.CreateDirectory(target) |> ignore
            for i in addFile do
                let target = d +/ i
                // (Some Dir, Some File)：dest 這格是一個目錄，File.Copy 會拋，先清掉
                Directory.deleteIfExists target true
                Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
                // 檔案本體在變更包的 data/ 底下（xcompare 寫出來的樣子）
                File.Copy(p +/ "data" +/ i, target, true)
                // File.Copy 不保留修改時間，不補回來的話之後 xadd 會把整棵樹看成「全改過」
                match Map.tryFind i pathStatus with
                | Some (Record.File f) -> File.SetLastWriteTimeUtc(target, f.MTimeUtc)
                | _ -> ()
            for i in removeFile do
                File.deleteIfExists <| d +/ i
            for i in removeDir do
                Directory.deleteIfExists (d +/ i) true
            ()
        | _ -> ()

    module Test =
        let addFile file =
            Directory.createDirectoryFor file
            File.writeAllTextEncoding file "" Encoding.UTF8

        let deleteFile file =
            File.deleteIfExists file

        let updateFile file content =
            Directory.createDirectoryFor file
            File.writeAllTextEncoding file content Encoding.UTF8


        // 差異計算：純函數，返回 (新增, 修改, 刪除) 三個列表
        let diffCompare (s1: Map<string, Record.Entry>) (s2: Map<string, Record.Entry>) =
            let addItem, updateItem, deleteItem =
                Map.compareGroup s1 s2

            let addResult =
                addItem
                |> Map.toSeq
                |> Seq.sortBy (fst >> Sort.addItem)
                |> List.ofSeq

            let deleteResult =
                deleteItem
                |> Map.toSeq
                |> Seq.sortBy (fst >> Sort.removeItem)
                |> List.ofSeq

            let updateResult =
                updateItem
                // compareGroup 給的是「兩邊都有」的全集，改沒改要自己篩
                |> Map.filter Record.predUpdateFile
                |> Map.toSeq
                |> Seq.sortBy (fst >> Sort.addItem)
                |> List.ofSeq

            addResult, updateResult, deleteResult

        let updateDir dir_1 =
            let addFile_ item result =
                addFile <| dir_1 +/ item
                item :: result

            let addFileIgnore item result =
                addFile <| dir_1 +/ item
                result

            let addDir item result =
                Directory.createDir (dir_1 +/ item)
                item :: result

            let addItem =
                []
                |> addFile_ @"1.txt" // 增加文件
                |> addDir @"Step13"
                |> addDir @"Step14"
                |> addDir @"Step10\2"
                |> addFile_ @"Step14\2.txt"
                |> addFileIgnore @"Step15\3.txt"
                |> addFile_ @"Step9\1.txt"
                |> List.rev

            // 增加文件
            // Test.addFile <| dir_1 +/ "1.txt"

            // 增加深層文件
            // Test.addFile <| dir_1 +/ "Step9" +/ "1.txt"

            // 憑空增加文件
            // Test.addFile <| dir_1 +/ "Step14" +/ "2.txt"

            // 增加目錄（名字必須是 base 沒有的：Step0..Step12 都在 base 裡，
            // 那些名字 copyTree 已經兩邊都複製過了，再 createDir 是空操作）
            // Directory.createDir (dir_1 +/ "Step13")

            // 增加深層目录
            // Directory.createDir <| dir_1 +/ @"Step10\2"

            // 過濾目錄
            // Test.addFile <| dir_1 +/ @"Step15/3.txt"

            let updateFile_ item content result =
                updateFile <| dir_1 +/ item <| content
                item :: result

            // 修改文件
            // Test.updateFile <| dir_1 +/ "Installation Commands and Concepts.rst" <| ""

            // 修改深層文件
            // Test.updateFile <| dir_1 +/ @"Step7\CMakeLists.txt" <| ""

            let updateItem =
                []
                |> updateFile_ @"Installation Commands and Concepts.rst" ""
                |> updateFile_ @"Step7\CMakeLists.txt" ""
                |> List.rev

            let deleteFile item result =
                File.deleteIfExists <| dir_1 +/ item
                item :: result

            let deleteDir item result =
                Directory.deleteIfExists <| dir_1 +/ item <| true
                item :: result

            // 刪除文件
            // File.deleteIfExists <| dir_1 +/ "Before You Begin.rst"

            // 刪除深層文件
            // File.deleteIfExists <| dir_1 +/ @"Step8\CMakePresets.json"

            // 刪除目錄
            // Directory.deleteIfExists <| dir_1 +/ @"Step10\MathFunctions" <| true

            // 刪除多層目錄
            // Directory.deleteIfExists <| dir_1 +/ @"Step12" <| true

            let deleteItem =
                []
                |> deleteDir @"Step10\MathFunctions"
                |> deleteDir @"Step12\MathFunctions"
                |> deleteFile @"Step8\CMakePresets.json"
                |> deleteFile "Before You Begin.rst"
                |> deleteDir @"Step12"
                |> List.rev

            addItem, updateItem, deleteItem

        // 兩個狀態文件生成補丁包，返回差異結果供測試斷言

        module Status =
            /// 目錄至狀態
            let create path (cache: Map<string, Record.Entry>) (ignoreList: string list) =
                Record.add path cache (fun x ->
                    not <| List.contains x ignoreList)

            let write (status: Map<string, Record.Entry>) path =
                let contents = JsonSerializer.Serialize(status, Common.options)
                File.writeAllTextEncoding path contents Encoding.UTF8
                ()

            let read path =
                let contents = File.readAllTextEncoding path Encoding.UTF8
                JsonSerializer.Deserialize<Map<string, Record.Entry>>(contents, Common.options)

        let diff s1Path s2Path dir_1 diffBase =
            // 空差異時 data\ 不會被建立，先建好 diffBase 才能寫 deletion.json
            Directory.createDir diffBase
            let s1 = Status.read s1Path
            let s2 = Status.read s2Path
            let addResult, updateResult, deleteResult = diffCompare s1 s2

            let diffPath = diffBase +/ "data"

            // AI生成：複製走節流進度（小總量逐項，大總量每秒一行）
            let copyProgress =
                Progress.over (addResult.Length + updateResult.Length) "diff"

            // 添加
            for (i, e) in addResult do
                copyProgress i
                let source = dir_1 +/ i
                let target = diffPath +/ i
                Directory.createDirectoryFor target
                match e with
                | Record.File { Path = _ } ->
                    File.copy source target true
                | Record.Dir { PathEntry = _ } ->
                    Directory.createDirectory target

            // 修改，需要根據原來是甚麼類型來操作：
            // 從文件變成目錄，先刪掉原來的文件；
            // 從目錄變成文件，先刪掉原來的目錄。
            // 前新後舊
            for (i, e) in updateResult do
                copyProgress i
                let source = dir_1 +/ i
                let target = diffPath +/ i
                Directory.createDirectoryFor target
                match e with
                | Record.File { Path = _; MTimeUtc = lwt }, Record.File { Path = _ } ->
                    // 文件就直接替換
                    File.copy source target true
                    File.setLastWriteTimeUtc target lwt
                | Record.File { Path = _; MTimeUtc = lwt }, Record.Dir { PathEntry = _ } ->
                    // 從目錄變成文件，先刪掉原來的目錄
                    Directory.deleteIfExists target true
                    File.copy source target true
                    File.setLastWriteTimeUtc target lwt
                | Record.Dir { PathEntry = _ }, Record.File { Path = _ } ->
                    // 從文件變成目錄，先刪掉原來的文件
                    File.deleteIfExists target
                    Directory.createDirectory target
                    // 目錄的修改時間不用管
                | Record.Dir { PathEntry = _ }, Record.Dir { PathEntry = _ } ->
                    // 無意義，不處理
                    ()

            // 刪除可以生成列表
            // AI生成：刪除列表走節流進度
            let delProgress = Progress.over deleteResult.Length "diff-delete"
            for i in deleteResult do
                delProgress (fst i)

            // 寫入到本地
            let deleteFile =
                JsonSerializer.Serialize(deleteResult |> List.map fst, Common.options)
            File.writeAllTextEncoding <| diffBase +/ "deletion.json"
            <| deleteFile <| Encoding.UTF8

            addResult, updateResult, deleteResult

        // 執行合併
        let merge path dest =
            // 讀變更部分
            let data = path +/ "data"
            // AI生成：排序後落成 list 取總數，複製走節流進度
            let status =
                Record.add data Map.empty (fun _ -> true)
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.sortBy (fun x ->
                    let p = Record.Entry.path x
                    depth p, p
                    )
                |> List.ofSeq
            let copyProgress = Progress.over status.Length "merge"
            for i in status do
                let p = Record.Entry.path i
                copyProgress p
                let target = dest +/ p
                match i with
                | Record.File { Path = p; MTimeUtc = lwt } ->
                    Directory.createDirectoryFor target
                    Directory.deleteIfExists target true
                    File.copy (data +/ p) target true
                    File.setLastWriteTimeUtc target lwt
                | Record.Dir { PathEntry = p; } ->
                    File.deleteIfExists target
                    Directory.createDirectory target
            // 讀刪除部分
            let deleteFile = path +/ "deletion.json"
            let content = File.readAllTextEncoding deleteFile Encoding.UTF8
            let deleteList = JsonSerializer.Deserialize<string list>(content, Common.options)
            // AI生成：刪除走節流進度
            let delProgress = Progress.over deleteList.Length "merge-delete"
            for i in deleteList do
                delProgress i
                let target = dest +/ i
                Directory.deleteIfExists target true
                File.deleteIfExists target

        type Config = {
            //Name: string
            Path: string
            //Destination: string
            Include: string list
            Exclude: string list
        }

        let equal dir_1 dir_2 inList exList =
            let s1 = Status.create dir_1 Map.empty exList
            let s2a = Record.add dir_2 Map.empty (fun x -> true)
            let a, u, d = Map.compareGroup s1 s2a
            a, u, d

        module X =
            let configPath = 
                let userPath = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
                let dirStatusPath = userPath +/ "DirStatus"
                dirStatusPath +/ "xconfig.json"

            let writeConfig (configMap: Map<string, Config>) =
                let json = JsonSerializer.Serialize(
                        configMap, Common.options)
                File.writeAllTextEncoding configPath json Encoding.UTF8

            let readConfig () =
                let json = File.readAllTextEncoding configPath Encoding.UTF8
                let configDataMap = JsonSerializer.Deserialize<Map<string, Config>>(json, Common.options)
                //logger.I $"config: %A{configDataMap}"
                configDataMap

            /// 配置不存在時報乾淨的錯誤，而不是 KeyNotFoundException
            let private tryGetConfig (m: Map<string, Config>) name =
                match Map.tryFind name m with
                | Some c -> c
                | None -> failwith $"未能找到{name}配置"

            let init name path includeList excludeList =
                let userPath = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
                let dirStatusPath = userPath +/ "DirStatus"
                Directory.createDir dirStatusPath

                // 創建配置文件
                //let config = {
                //    Name = name
                //    Path = path
                //    Destination = path
                //    Include = includeList
                //    Exclude = excludeList
                //}
                //config

                Directory.createDir <| dirStatusPath +/ name

                let configPath = dirStatusPath +/ "xconfig.json"

                logger.I $"configPath: {configPath}"

                let config : Config = {
                    Path = path
                    Include = includeList
                    Exclude = excludeList
                }

                if File.exists configPath |> not then
                    writeConfig <| Map [(name, config);]
                else // 已存在就讀回來
                    let currentConfig = readConfig ()
                    // 添加到全局配置
                    writeConfig <| Map.add name config currentConfig

            /// 獲取配置和目錄
            let getConfigPath name =
                let m = readConfig ()
                let config = tryGetConfig m name
                logger.I $"name: %A{config}"

                let userPath = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
                let dirStatusPath = userPath +/ "DirStatus" +/ name
                config, dirStatusPath

            let add name version =
                let config, dirStatusPath = getConfigPath name

                // 緩存
                let cachePath = dirStatusPath +/ "cache.json"
                let cache =
                    match File.exists <| cachePath with
                    | true ->
                        Status.read <| cachePath
                    | false -> Map.empty

                let s =
                    Status.create <| config.Path <| cache <| config.Exclude

                let versionPath = dirStatusPath +/ $"{version}.json"

                // 寫入本地
                Status.write s versionPath
                Status.write s  cachePath

                versionPath

            let private xDiff = diff
            let private xMerge = merge
            let private xEqual = equal

            let diff name (left: string) (right: string) dest =
                // let m = readConfig ()
                // let config = tryGetConfig m name

                // let userPath = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
                // let dirStatusPath = userPath +/ "DirStatus" +/ name

                let config, dirStatusPath = getConfigPath name

                // CLI：左=舊、右=新（與 compare 等命令一致）；內部 Test.diff 參數順序是 (新, 舊)
                // AI生成：--left 支持版本名或全路徑（跨機場景：外來快照是拷來的任意路徑檔案），
                //         --right 只支持版本名（新狀態固定是本機配置目錄裡的版本）
                let oldJson =
                    match Path.IsPathFullyQualified left with
                    | true -> left
                    | false -> dirStatusPath +/ $"{left}.json"
                if Path.IsPathFullyQualified right then
                    failwith "--right 只支持版本名：新狀態必須是本機配置目錄裡的版本"
                let newJson = dirStatusPath +/ $"{right}.json"

                xDiff newJson oldJson config.Path dest |> ignore

            let merge diffPath (destPath: string) =
                let destPath =
                    match Path.isPathFullyQualified destPath with
                        | true -> destPath
                        | false ->
                            let config, _ = getConfigPath destPath
                            config.Path
                // failwith $"destPath: %s{destPath}"
                xMerge diffPath destPath

            let equal left right includeList excludeList =
                let a, u, d =
                    xEqual left right includeList excludeList
                let uCheck =
                    u
                    |> Map.toSeq
                    |> Seq.map snd
                    |> Seq.exists (fun (a, b) ->
                        not <| Record.Entry.equal a b)
                a |> Map.isEmpty && d |> Map.isEmpty && not uCheck

    let xtest (results: ParseResults<XTestArgs>) =
        let path = results.TryGetResult(XTestArgs.Path)
        logger.I $"path: {path}"
        let mapTests =
            test "map test" {
                let userPath = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

                let dirStatusPath = userPath +/ "DirStatus"
                if Directory.exists dirStatusPath |> not then
                    Directory.createDir dirStatusPath

                //Expect.equal (Seq.isEmpty diff) true ""
                // 基礎目錄
                let createTestDir baseDir =
                    // let baseDir = @"d:\test\tutorial"
                    let dir_1 = @"d:\test\tutorial-1"
                    let dir_2 = @"d:\test\tutorial-2"
                    Directory.deleteIfExists dir_1 true
                    Directory.createDir dir_1
                    Record.copyTree baseDir dir_1 (fun _ -> true)
                    Directory.deleteIfExists dir_2 true
                    Directory.createDir dir_2
                    dir_1, dir_2

                let dir_1, dir_2 = createTestDir @"d:\test\tutorial"

                let dir_1Config = userPath +/ "DirStatus/xtest"
                Directory.deleteIfExists dir_1Config true
                Directory.createDir dir_1Config

                Record.copyTree dir_1 dir_2 (fun _ -> true)

                let addItem, updateItem, deleteItem =
                    Test.updateDir dir_1

                let s1 = Test.Status.create dir_1 Map.empty ["Step15"]
                let s2 = Test.Status.create dir_2 Map.empty []

                // s1與s2應該轉成json
                let s1Path = @"d:\test\status\s1.json"
                let s2Path = @"d:\test\status\s2.json"
                Test.Status.write s1 s1Path 
                Test.Status.write s2 s2Path

                let diffBase = @"d:\test\tutorial_diff"
                Directory.deleteIfExists diffBase true

                let addResult, updateResult, deleteResult =
                    Test.diff s1Path s2Path dir_1 diffBase

                Expect.equal (addResult |> List.map fst) addItem "add"

                Expect.equal (deleteResult |> List.map fst) deleteItem "delete"

                Expect.equal (updateResult |> List.map fst) updateItem "update"

                // 應用
                Test.merge diffBase dir_2

                let a, u, d = Test.equal dir_1 dir_2 [] ["Step15"]

                Expect.equal a Map.empty "add"
                Expect.equal d Map.empty "delete"

                // logger.I $"update: %A{u}"

                let uCheck =
                    u
                    |> Map.toSeq
                    |> Seq.map snd
                    |> Seq.exists (fun (a, b) ->
                        not <| Record.Entry.equal a b)

                Expect.equal uCheck false "update"

                // 測試外部接口
                let testApi () =
                    let dir_1, dir_2 = createTestDir @"d:\test\tutorial"

                    Test.X.init "xtest-1" dir_1 [] ["Step15"]
                    Test.X.init "xtest-2" dir_2 [] []

                    // 修改dir_1
                    Test.updateDir dir_1 |> ignore

                    // 添加版本
                    let a1 = Test.X.add "xtest-1" "a1"
                    let _ = Test.X.add "xtest-2" "b1"

                    // 更新 xtest-1
                    let _ = Test.updateDir dir_1

                    let a2 = Test.X.add "xtest-1" "a2"

                    let diffPath = @"D:\test\d"
                    Directory.deleteIfExists diffPath true

                    Test.X.diff "xtest-1" "a1" "a2" diffPath

                    // 合併
                    Test.X.merge diffPath dir_2

                    // 測試
                    let b2 = Test.X.add "xtest-2" "b2"

                    let result = Test.X.equal a2 b2 [] ["Step15"]

                    Expect.equal result true "result"

                    let testCache () =
                        Test.X.init "CMake" "D:/repos/CMake/Help" []
                            [".git"; "build"; "build-clang-mingw"; ".claude"; ".kilo"]

                        let sw = Stopwatch.StartNew()
                        let _ = Test.X.add "CMake" "1"
                        sw.Stop()
                        logger.I $"第一次：{sw.ElapsedMilliseconds} ms"

                        let sw = Stopwatch.StartNew()
                        let _ = Test.X.add "CMake" "2"
                        sw.Stop()
                        logger.I $"第二次：{sw.ElapsedMilliseconds} ms"

                    // testCache ()

                    ()
                testApi ()
            }
        runTestsWithCLIArgs [] Array.empty mapTests
        |> ignore

    let compare (results: ParseResults<CompareArgs>) =
        let name = results.TryGetResult(CompareArgs.Name)
        let left = results.TryGetResult(CompareArgs.Left)
        let right = results.TryGetResult(CompareArgs.Right)
        let dest = results.TryGetResult(CompareArgs.Destination)
        match name, left, right, dest with
        | Some n, Some le, Some ri, Some d ->
            let config = Config.tryGet n
            match config with
            | Some c ->
                let left = c.Destination +/ "status" +/ le
                let right = c.Destination +/ "status" +/ ri
                let d = c.Destination +/ "diff" +/ d
                DirStatus.compare left right d
            | None ->
                failwith $"未能找到{n}配置"
        | None, Some le, Some ri, Some d ->
            DirStatus.compare le ri d
        | _ -> ()

    let export (results: ParseResults<ExportArgs>) =
        let name = results.TryGetResult(ExportArgs.Name)
        let path = results.TryGetResult(ExportArgs.Path)
        let dest = results.TryGetResult(ExportArgs.Destination)
        let diff = results.GetResult(ExportArgs.Difference)
        match name, path, dest with
        | Some n, _, Some d ->
            let config = Config.tryGet n
            match config with
            | Some c ->
                let diff = c.Destination +/ "diff" +/ diff
                let dest = c.Destination +/ "export" +/ d
                DirStatus.export c.Path diff dest
            | None ->
                failwith $"未能找到{n}配置"
        | Some n, _, None -> // 不指定-d就和--diff一樣
            let config = Config.tryGet n
            match config with
            | Some c ->
                let dest = c.Destination +/ "export" +/ diff
                let diff = c.Destination +/ "diff" +/ diff
                DirStatus.export c.Path diff dest
            | None ->
                failwith $"未能找到{n}配置"
        | None, Some p, Some d ->
            DirStatus.export p diff d
        | _ -> ()

module Cmd =
    let init (results: ParseResults<Args.InitArgs>) =
        let name =
            results.GetResult(Args.InitArgs.Name, "")
        let path =
            results.GetResult(Args.InitArgs.Path, "")
        let includePath =
            results.GetResult(Args.InitArgs.IncludePath, [])
        let excludePath =
            results.GetResult(Args.InitArgs.ExcludePath, [])
        Parse.Test.X.init name path includePath excludePath

    let add (results: ParseResults<Args.AddArgs>) =
        let name =
            results.GetResult(Args.AddArgs.Name, "")
        let version =
            results.GetResult(Args.AddArgs.Version, "")
        Parse.Test.X.add name version

    let diff (results: ParseResults<Args.DiffArgs>) =
        let name =
            results.GetResult(Args.DiffArgs.Name, "")
        let left =
            results.GetResult(Args.DiffArgs.Left, "")
        let right =
            results.GetResult(Args.DiffArgs.Right, "")
        let dest =
            results.GetResult(Args.DiffArgs.Destination, "")
        Parse.Test.X.diff name left right dest

    let merge (results: ParseResults<Args.MergeArgs>) =
        let diff =
            results.GetResult(Args.MergeArgs.Diff, "")
        let dest =
            results.GetResult(Args.MergeArgs.Destination, "")
        let name =
            results.GetResult(Args.MergeArgs.Name, "")
        // 優先用name
        let dest =
            if name <> "" then
                name
            else
                dest
        Parse.Test.X.merge diff dest

    let equal (results: ParseResults<Args.EqualArgs>) =
        let left =
            results.GetResult(Args.EqualArgs.Left, "")
        let right =
            results.GetResult(Args.EqualArgs.Right, "")
        let includePath =
            results.GetResult(Args.EqualArgs.IncludePath, [])
        let excludePath =
            results.GetResult(Args.EqualArgs.ExcludePath, [])
        let r = Parse.Test.X.equal left right includePath excludePath
        logger.I $"result: {r}"

    let update (results: ParseResults<Args.UpdateArgs>) =
        let path =
            results.GetResult(Args.UpdateArgs.Path, "")
        Parse.Test.updateDir path |> ignore

[<EntryPoint>]
let main argv =
    logger.I "begin"

    try
        let parser = ArgumentParser.Create<MainArgs>(programName = "myapp")
        let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        match results.GetAllResults() with
        | [ZInit results] ->
            let path =
                results.TryGetResult(InitArgs.Path)
                |> Option.defaultValue ""
            let dest =
                results.TryGetResult(InitArgs.Destination)
                |> Option.defaultValue ""
            let name = results.TryGetResult(InitArgs.Name)
            DirStatus.init path dest name
        | [Init results] ->
            Cmd.init results
        | [Add results] ->
            Cmd.add results |> ignore
        | [Diff results] ->
            Cmd.diff results
        | [Merge results] ->
            Cmd.merge results
        | [Equal results] ->
            Cmd.equal results
        | [Update results] ->
            Cmd.update results
        | [ZAdd results] ->
            Parse.add results
        | [XAdd results] ->
            Parse.xadd results
        | [Compare results] ->
            Parse.compare results
        | [XCompare results] ->
            Parse.xcompare results
        | [XMerge results] ->
            Parse.xmerge results
        | [Export results] ->
            Parse.export results
        | [ZMerge results] ->
            let path = results.GetResult(MergeArgs.Path)
            let dest = results.GetResult(MergeArgs.Destination)
            DirStatus.merge path dest
        | [Test results] ->
            let path = results.GetResult(TestArgs.Path)
            let dest = results.GetResult(TestArgs.Destination)
            DirStatus.test path dest
        | [XTest results] ->
            //let path = results.GetResult(XTestArgs.Path)
            Parse.xtest results
            ()
        | [ZEqual results] ->
            let path = results.GetResult(EqualArgs.Left)
            let dest = results.GetResult(EqualArgs.Right)
            DirStatus.equal path dest
        | [TestPrepare results] ->
            let path = results.GetResult(TestPrepareArgs.Path)
            let dest = results.GetResult(TestPrepareArgs.Destination)
            DirStatus.testPrepare path dest
        | [TestUpdate results] ->
            let path = results.GetResult(TestUpdateArgs.Path)
            let dest = results.GetResult(TestUpdateArgs.Destination)
            DirStatus.testUpdate path dest
        | [Version] ->
            printfn "版本 1.0.0"
        | _ ->
            printfn "请指定有效的命令。"
        logger.I "end"
        0
    with :? ArguParseException as e ->
        eprintfn "%s" e.Message
        1
