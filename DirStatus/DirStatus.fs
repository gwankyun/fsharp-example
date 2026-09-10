module DirStatus
open System.IO
open Status
open Serilog
//open FSLogger
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open Expecto
open Common
open FSharpPlus

//let options =
//    JsonFSharpOptions.Default()
//        .ToJsonSerializerOptions()

//// 启用格式化（带缩进）
//options.WriteIndented <- true

//let logger = Logger.ColorConsole

let fromPath path =
    let content = File.ReadAllText(path, Encoding.UTF8)
    JsonSerializer.Deserialize<Status.RelativePath seq>(content, Common.options)

module Status =
    let add path (includeDir: string list) (exclude: string list)
            (previous: Map<string, Status.RelativePath>) =
        //if exclude.IsSome then
        //    logger.I $"排除：{exclude.Value}"
        // stat 快路径：相对路径、大小、修改时间都没变时，直接复用缓存里的哈希，
        // 不必重新读一遍文件内容。
        let resolveHash relPath (fileInfo: System.IO.FileInfo) =
            match Map.tryFind relPath previous with
            | Some prev when not prev.IsContainer
                          && prev.Length = fileInfo.Length
                          && prev.LastWriteTime = fileInfo.LastWriteTime ->
                prev.Hash
            | _ ->
                // 读盘算哈希是扫描里最慢的一步，先把文件名报出来，
                // 卡住的时候才看得出卡在哪个文件上。
                logger.I $"读取 {relPath}"
                Status.hashFile fileInfo.FullName
        let fileList =
            Status.getChildItemIf path true (fun x ->
                let rela = Path.relativePath path x
                //logger.D $"rela: %s{rela}"
                //true
                //let inc = List.forall (fun i -> String.startsWith i rela) includeDir
                //let inc = List.contains rela includeDir
                let inc =
                    match includeDir |> List.isEmpty with
                    | true -> true
                    | false -> List.contains rela includeDir
                //let exc = List.forall (fun i -> not <| String.startsWith i rela) exclude
                //let exc = not <| List.contains rela exclude
                let exc =
                    match exclude |> List.isEmpty with
                    | true -> true
                    | false -> not <| List.contains rela exclude
                inc && exc
                )
            //true
            //|> Seq.map Status.toFileSystemInfoDetail
            //|> Seq.map (fun x ->
            //    { x with FullName = Path.relativePath path x.FullName})
            |> Seq.map (Status.toRelativePathWith resolveHash path)
        //let fileList =
        //    match includeDir |> List.isEmpty with
        //    | true -> fileList
        //    | false ->
        //        fileList
        //        |> Seq.filter (fun x ->
        //            let startsWith i = x.Path |> String.startsWith i
        //            List.forall startsWith includeDir)
        //let fileList =
        //    match exclude |> List.isEmpty with
        //    | true -> fileList
        //    | false ->
        //        fileList
        //        |> Seq.filter (fun x ->
        //            let startsWith i = x.Path |> String.startsWith i |> not
        //            List.forall startsWith exclude)
        fileList

    type ItemType = Status.RelativePath

    let compare (leftJson: Status.RelativePath seq)
        (rightJson: ItemType seq) =

        let diff : Map<string, Status.Difference> =
            Status.compareObject leftJson rightJson (fun x -> x.Path)
            |> Map.filter (fun _ v ->
                match v with
                | Some le, Some ri ->
                    match le.IsContainer, ri.IsContainer with
                    | true, true -> false           // 目录之間不比內容
                    | _ -> le.Hash <> ri.Hash       // 以內容哈希判定是否變化
                | _ -> true
            )
            |> Map.map (fun _ v ->
                match v with
                | Some _, Some ri ->
                    { Status.Difference.Path = ri.Path;
                      Status.Difference.IsContainer = ri.IsContainer;
                      Status.Difference.Operation = Status.Modification;
                      Status.Difference.Hash = ri.Hash }
                | Some le, None ->
                    { Path = le.Path;
                      IsContainer = le.IsContainer;
                      Operation = Status.Deletion;
                      Hash = "" }
                | None, Some ri ->
                    { Path = ri.Path;
                      IsContainer = ri.IsContainer;
                      Operation = Status.Creation;
                      Hash = ri.Hash }
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

let appendJson p =
    match p |> String.endsWith ".json" with
    | true -> p
    | false -> p ++ ".json"

/// 讀取上一份清單用於 stat 快路徑。讀不了就當作沒有快取，全量重算哈希。
let loadManifest p =
    try
        fromPath p |> Seq.map (fun x -> x.Path, x) |> Map.ofSeq
    with e ->
        logger.I $"無法讀取上一份清單 {p}，將全量重算哈希：{e.Message}"
        Map.empty

/// 寫入前確保父目錄存在（File.WriteAllText 不會自動建立上層目錄）。
let private ensureParent (p: string) =
    let dir = System.IO.Path.GetDirectoryName p
    if not <| System.String.IsNullOrEmpty dir then
        Directory.createDirectory dir

let add path (destination: string) (includeDir: string list) (exclude: string list)
        (previousPath: string option) =
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    logger.I $"path: {path} destination: {destination}"
    let destination = appendJson destination
    let previous =
        match previousPath with
        | Some p when File.exists p -> loadManifest p
        | _ -> Map.empty
    let fileList = Status.add path includeDir exclude previous
    let text =
        JsonSerializer.Serialize<Status.RelativePath seq>(fileList, Common.options)
    ensureParent destination
    File.WriteAllText(destination, text, Encoding.UTF8)
    // 讓快取檔自閉合成最新清單，下次產生快照時可直接複用哈希。
    match previousPath with
    | Some p when p <> destination ->
        ensureParent p
        File.WriteAllText(p, text, Encoding.UTF8)
    | _ -> ()

let compare left right destination =
    printfn $"left: {left} right: {right} destination: {destination}"
    let left = appendJson left
    let right = appendJson right
    if File.exists left |> not then
        failwith $"{left} not exists"
    if File.exists right |> not then
        failwith $"{right} not exists"
    let leftJson = fromPath left
    let rightJson = fromPath right

    let destination = appendJson destination

    let diff = Status.compare leftJson rightJson
    logger.D $"diff: %A{diff}"
    // 寫入文件
    let text =
        JsonSerializer.Serialize<Status.Difference seq>(diff, Common.options)
    logger.D $"destination: {destination}"
    ensureParent destination
    File.WriteAllText(destination, text, Encoding.UTF8)

let diffFrom path =
    let content = File.ReadAllText(path, Encoding.UTF8)
    JsonSerializer.Deserialize<Status.Difference seq>(content, Common.options)

/// 導出變更包。回傳校驗失敗的項（空表示全部通過）。
let export path difference destination (snapshot: string option) =
    let difference = appendJson difference
    logger.D $"path: {path}"
    logger.D $"difference: {difference}"
    logger.D $"destination: {destination}"
    if Directory.exists path |> not then
        failwith $"{path} not exists"
    if File.exists difference |> not then
        failwith $"{difference} not exists"
    //printfn $"path: {path} difference: {difference} destination: {destination}"
    Directory.deleteIfExists destination true
    Directory.createDirectory destination
    // 即使這次沒有任何新增/修改（例如空 diff 或純刪除），也保證 data/ 存在，
    // 否則 merge 端會因為缺少 data/ 而報錯。
    Directory.createDirectory (destination +/ "data")
    let diff = diffFrom difference

    /// 把單項寫進 data/，並就地校驗「即將出貨的那一份」與清單相符。
    /// 要驗的是 bundle 裡的副本，不是來源目錄——來源在 add 與 export 之間
    /// 可能已被改動，那樣 diff.json 就在說謊，必須當場擋下而不是靜默出貨。
    let createItem (i: Status.Difference) =
        let destFile = destination +/ "data" +/ i.Path
        if i.IsContainer then
            Directory.createDirectory destFile
        else
            let dir = Path.GetDirectoryName destFile
            Directory.createDirectory dir
            //File.Copy(path +/ i.Path, destFile)
            File.copyTo destFile true <| path +/ i.Path
            if i.Hash = "" then
                logger.W $"差異項 {i.Path} 沒有目標哈希，跳過校驗。"
            else
                let actual = Status.hashFile destFile
                if actual <> i.Hash then
                    failwith $"校驗失敗：{i.Path}（清單 {i.Hash}，實際 {actual}）"

    // 收集失敗項而不是靜默吞掉——調用方據此返回非零退出碼。
    let failures = ResizeArray<Status.Difference>()
    for i in diff do
        logger.I $"{i}"
        match i.Operation with
        | Status.Operation.Creation
        | Status.Operation.Modification ->
            try
                createItem i
            with e ->
                eprintfn "導出失敗：%s（%s）" i.Path e.Message
                failures.Add i
        | Status.Operation.Deletion -> ()
    //File.Copy(difference, destination +/ "diff.json")
    File.copyTo <| destination +/ "diff.json" <| true <| difference
    // 把來源（外網）當前清單一併放進 bundle，合併端 merge 完可直接拿它收口驗證，
    // 不必另外傳一份檔案過去。
    match snapshot with
    | Some s when File.exists s ->
        File.copyTo <| destination +/ "source.json" <| true <| s
    | Some s ->
        logger.W $"找不到快照 {s}，bundle 內將不含 source.json。"
    | None -> ()
    List.ofSeq failures

let merge path destination =
    Check.dir path
    Check.dir destination
    Check.dir <| path +/ "data"
    Check.file <| path +/ "diff.json"
    let diff = diffFrom <| path +/ "diff.json"

    /// 先把源文件拷成目標同目錄下的臨時文件。此時尚未觸碰目標，
    /// 拷貝失敗不會破壞已經存在的資料。
    let stageFile (srcPath: string) (destPath: string) =
        let dir = System.IO.Path.GetDirectoryName destPath
        if not <| System.String.IsNullOrEmpty dir then
            Directory.createDirectory dir
        let tmp = destPath + ".dstmp"
        File.copyTo tmp true srcPath
        tmp

    /// 移除可能存在的舊目標（文件或目錄），再把臨時文件原子替換到位。
    let commitFile (tmp: string) (destPath: string) =
        if Directory.exists destPath then
            Directory.delete destPath true
        System.IO.File.Move(tmp, destPath, true)

    let applyItem (i: Status.Difference) =
        let destPath = destination +/ i.Path
        let srcPath = path +/ "data" +/ i.Path
        match i.Operation with
        | Status.Operation.Deletion ->
            if Directory.exists destPath then
                Directory.delete destPath true
            else
                File.deleteIfExists destPath
            // 刪完之後路徑不該還在，否則重複執行會踩到殘留。
            if Directory.exists destPath || File.exists destPath then
                failwith $"刪除後路徑仍然存在：{i.Path}"
        | Status.Operation.Creation
        | Status.Operation.Modification ->
            if i.IsContainer then
                // 目標本應是目錄：清掉同名文件（若有）即可，重複執行也安全。
                if File.exists destPath then File.delete destPath
                Directory.createDirectory destPath
            else
                let tmp = stageFile srcPath destPath
                try
                    // 提交前先校驗暫存檔。File.Move 是原子且不轉換內容，
                    // 所以驗暫存檔與驗落地檔等價，但校驗失敗時目標目錄
                    // 完全沒被碰過，不需要回滾。
                    if i.Hash = "" then
                        logger.W $"差異項 {i.Path} 沒有目標哈希，跳過校驗。"
                    else
                        let actual = Status.hashFile tmp
                        if actual <> i.Hash then
                            failwith $"校驗失敗：{i.Path}（清單 {i.Hash}，實際 {actual}）"
                    commitFile tmp destPath
                with _ ->
                    File.deleteIfExists tmp
                    reraise ()

    // 收集失敗項而不是靜默吞掉——調用方據此返回非零退出碼。
    let failures = ResizeArray<Status.Difference>()
    for i in diff do
        try
            applyItem i
        with e ->
            eprintfn "合併失敗：%s（%s）" i.Path e.Message
            failures.Add i
    List.ofSeq failures

/// 對整個目錄重算快照，與期望清單比對，回傳可讀的不符項（空表示一致）。
/// 這是「同步完成」的收口：不靠信任，靠重新讀一遍每個檔案的內容。
let verify path (snapshot: string) (includeDir: string list) (exclude: string list) =
    Check.dir path
    Check.file snapshot
    let expected = fromPath snapshot
    let actual = Status.add path includeDir exclude Map.empty
    logger.I $"核對 {path} 與 {snapshot}"
    Status.compareObject actual expected (fun (x: Status.RelativePath) -> x.Path)
    |> Map.toList
    |> List.choose (fun (_, pair) ->
        match pair with
        // 兩邊都是目錄 → 不比內容（與 compare 的判據一致）。
        | Some a, Some e when a.IsContainer && e.IsContainer -> None
        | Some a, Some e when a.IsContainer <> e.IsContainer -> Some $"型別不符: {a.Path}"
        | Some a, Some e when a.Hash <> e.Hash -> Some $"內容不符: {a.Path}"
        | Some _, Some _ -> None
        | Some a, None -> Some $"多餘:     {a.Path}"
        | None, Some e -> Some $"缺失:     {e.Path}"
        | None, None -> None)

let writeAllText (path: string) (contents: string) =
    File.WriteAllText(path, contents)

let testPrepare path destination =
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

let equal src dest =
    let srcStatus = Status.add src [] [] Map.empty
    let destStatus = Status.add dest [] [] Map.empty
    let diff = Status.compare srcStatus destStatus
    logger.D $"diff: %A{diff}"
    let mapTests =
        Expecto.Tests.test "map test" {
            Expect.equal (Seq.isEmpty diff) true ""
        }
    runTestsWithCLIArgs [] Array.empty mapTests
    |> ignore

let testUpdate path destination =
    let src = destination +/ "src"
    //let dest = destination +/ "dest"

    let deleteFile = src +/ "delete.txt"
    let deleteFile2 = src +/ "Tutorial" +/ "delete.txt"

    let deleteDir = src +/ "delete"
    let deleteDir2 = src +/ "Tutorial" +/ "delete"

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

let test path destination =
    if Directory.exists path |> not then
        failwith $"{path} must exists"

    let src = destination +/ "src"
    let dest = destination +/ "dest"

    let statusBase = destination +/ "DirStatus" +/ "Step1" +/ "status"
    let status1 = statusBase +/ "1.json"
    let status2 = statusBase +/ "2.json"

    // 準備測試數據
    testPrepare path destination

    add src status1 [] [] None

    // 更新測試數據
    testUpdate path destination

    add src status2 [] [] None

    let diff = destination +/ "DirStatus" +/ "Step1" +/ "diff" +/ "diff.json"
    File.deleteIfExists diff

    compare status1 status2 diff

    //let exportPath = destination +/ "export" "diff"
    let exportPath =  destination +/ "DirStatus" +/ "Step1" +/ "export"

    // 導出變更包
    let exportFailed = export src diff exportPath None
    if not <| List.isEmpty exportFailed then
        failwith $"導出有 {exportFailed.Length} 項校驗失敗"

    // 合併
    let failed = merge exportPath dest
    if not <| List.isEmpty failed then
        failwith $"合併有 {failed.Length} 項失敗"

    // 測試結果
    equal src dest
