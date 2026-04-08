open Argu

type PathAttribute() =
    inherit AltCommandLineAttribute([|"-p"|])

type DestAttribute() =
    inherit AltCommandLineAttribute([|"-d"; "--dest"|])

type NoPrefixAttribute() =
    inherit CliPrefixAttribute(CliPrefix.None)

type AddArgs =
    | [<Path>] Path of path: string
    | [<Dest>] Destination of destination: string
    | Exclude of exclude: string list

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "指定源路径。"
            | Destination _ -> "指定目标路径。"
            | Exclude _ -> "排除目錄。"

type CompareArgs =
    | Left of left: string
    | Right of right: string
    | [<Dest>] Destination of destination: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Left _ -> "左"
            | Right _ -> "右"
            | Destination _ -> "輸出路徑"

type ExportArgs =
    | [<Path>] Path of path: string
    | [<AltCommandLine("--diff")>] Difference of difference: string
    | [<Dest>] Destination of destination: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "指定源路径。"
            | Difference _ -> "差異文件。"
            | Destination _ -> "指定目标路径。"

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

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<MainArgs>(programName = "myapp")
        let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        match results.GetAllResults() with
        | [Add addResults] ->
            let path = addResults.GetResult(AddArgs.Path)
            let dest = addResults.GetResult(AddArgs.Destination)
            let exclude = addResults.TryGetResult(AddArgs.Exclude)
            DirStatus.add path dest exclude
        | [Compare compareResults] ->
            let left = compareResults.GetResult(Left)
            let right = compareResults.GetResult(Right)
            let dest = compareResults.GetResult(CompareArgs.Destination)
            DirStatus.compare left right dest
        | [Export exportResults] ->
            let path = exportResults.GetResult(ExportArgs.Path)
            let dest = exportResults.GetResult(ExportArgs.Destination)
            let diff = exportResults.GetResult(ExportArgs.Difference)
            DirStatus.export path diff dest
        | [Merge mergeResults] ->
            let path = mergeResults.GetResult(MergeArgs.Path)
            let dest = mergeResults.GetResult(MergeArgs.Destination)
            DirStatus.merge path dest
        | [Test testResults] ->
            let path = testResults.GetResult(TestArgs.Path)
            let dest = testResults.GetResult(TestArgs.Destination)
            DirStatus.test path dest
        | [Version] ->
            printfn "版本 1.0.0"
        | _ ->
            printfn "请指定有效的命令。"
        0
    with :? ArguParseException as e ->
        eprintfn "%s" e.Message
        1
