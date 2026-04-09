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

type InitArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string
    | [<Name>] Name of name: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "指定源路径。"
            | Destination _ -> "指定目标路径。"
            | Name _ -> "指名名稱。"

type AddArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string
    | [<Name>] Name of name: string
    | Include of includeDir: string list
    | Exclude of exclude: string list

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "指定源路径。"
            | Destination _ -> "指定目标路径。"
            | Include _ -> "包含目錄。"
            | Exclude _ -> "排除目錄。"
            | Name _ -> "排除目錄。"

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
            | Destination _ -> "輸出路徑"
            | Name _ -> "排除目錄。"

type ExportArgs =
    | [<Path>] Path of path: string
    | [<AltCommandLine("--diff")>] Difference of difference: string
    | [<Dest>] Destination of destination: string
    | [<Name>] Name of name: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "指定源路径。"
            | Difference _ -> "差異文件。"
            | Destination _ -> "指定目标路径。"
            | Name _ -> "排除目錄。"

type MergeArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "指定源路径。"
            | Destination _ -> "指定目标路径。"

type TestArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "指定源路径。"
            | Destination _ -> "指定目标路径。"

// 主命令类型
type MainArgs =
    | [<NoPrefix>] Add of ParseResults<AddArgs>
    | [<NoPrefix>] Compare of ParseResults<CompareArgs>
    | [<NoPrefix>] Export of ParseResults<ExportArgs>
    | [<NoPrefix>] Merge of ParseResults<MergeArgs>
    | [<NoPrefix>] Test of ParseResults<TestArgs>
    | [<NoPrefix>] Init of ParseResults<InitArgs>
    | Version

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Add _ -> "添加文件或目录。"
            | Compare _ -> "對比。"
            | Export _ -> "導出。"
            | Merge _ -> "合併。"
            | Version -> "显示版本信息。"
            | Test _ -> "測試。"
            | Init _ -> "初始化。"

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
                let dest = c.Destination +/ d
                DirStatus.add c.Path dest c.Include c.Exclude
            | None ->
                failwith $"未能找到{n}配置"
        | _, Some p, Some d ->
            DirStatus.add p d includeDir exclude
        | _ -> failwith "參數錯誤"

   let compare (results: ParseResults<CompareArgs>) = 
        let name = results.TryGetResult(CompareArgs.Name)
        let left = results.TryGetResult(Left)
        let right = results.TryGetResult(Right)
        let dest = results.TryGetResult(CompareArgs.Destination)
        match name, left, right, dest with
        | Some n, Some le, Some ri, _ ->
            let config = Config.tryGet n
            match config with
            | Some c ->
                let left = c.Destination +/ le
                let right = c.Destination +/ ri
                DirStatus.compare left right c.Destination
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
        | Some n, _, _ ->
            let config = Config.tryGet n
            match config with
            | Some c ->
                let diff = c.Destination +/ diff
                DirStatus.export c.Path diff c.Destination
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
        | [Version] ->
            printfn "版本 1.0.0"
        | _ ->
            printfn "请指定有效的命令。"
        0
    with :? ArguParseException as e ->
        eprintfn "%s" e.Message
        1
