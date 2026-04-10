open Argu
open Common

type PathAttribute() =
    inherit AltCommandLineAttribute([|"-p"|])

type DestAttribute() =
    inherit AltCommandLineAttribute([|"-d"; "--dest"|])

type NameAttribute() =
    inherit AltCommandLineAttribute([|"-n"|])

type NoPrefixAttribute() =
    inherit CliPrefixAttribute(CliPrefix.None)

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
    | [<NoPrefix>] Add of ParseResults<AddArgs>
    | [<NoPrefix>] Compare of ParseResults<CompareArgs>
    | [<NoPrefix>] Export of ParseResults<ExportArgs>
    | [<NoPrefix>] Merge of ParseResults<MergeArgs>
    | [<NoPrefix>] Test of ParseResults<TestArgs>
    | [<NoPrefix>] TestPrepare of ParseResults<TestPrepareArgs>
    | [<NoPrefix>] TestUpdate of ParseResults<TestUpdateArgs>
    | [<NoPrefix>] Equal of ParseResults<EqualArgs>
    | [<NoPrefix>] Init of ParseResults<InitArgs>
    | Version

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Add _ -> "添加狀態。"
            | Compare _ -> "對比兩個狀態。"
            | Export _ -> "導出變更包。"
            | Merge _ -> "合併變更包。"
            | Version -> "顯示版本信息。"
            | Test _ -> $"測試{ArgText.test}"
            | TestPrepare _ -> $"生成測試數據{ArgText.test}"
            | TestUpdate _ -> $"更新測試數據{ArgText.test}"
            | Equal _ -> $"比較測試結果{ArgText.test}"
            | Init _ -> "初始化配置。"

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
        | None, Some p, Some d ->
            DirStatus.export p diff d
        | _ -> ()

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<MainArgs>(programName = "myapp")
        let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        match results.GetAllResults() with
        | [Init results] ->
            let path =
                results.TryGetResult(InitArgs.Path)
                |> Option.defaultValue ""
            let dest =
                results.TryGetResult(InitArgs.Destination)
                |> Option.defaultValue ""
            let name = results.TryGetResult(InitArgs.Name)
            DirStatus.init path dest name
        | [Add results] ->
            Parse.add results
        | [Compare results] ->
            Parse.compare results
        | [Export results] ->
            Parse.export results
        | [Merge results] ->
            let path = results.GetResult(MergeArgs.Path)
            let dest = results.GetResult(MergeArgs.Destination)
            DirStatus.merge path dest
        | [Test results] ->
            let path = results.GetResult(TestArgs.Path)
            let dest = results.GetResult(TestArgs.Destination)
            DirStatus.test path dest
        | [Equal results] ->
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
        0
    with :? ArguParseException as e ->
        eprintfn "%s" e.Message
        1
