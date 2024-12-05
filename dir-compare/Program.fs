// For more information see https://aka.ms/fsharp-console-apps
open FSLogger
open Argu

let logger = Logger.ColorConsole

type CliArguments =
    | Version
    // | Walk of dir: string
    | Init of path: string
    | Compare of dir1: string * dir2: string
    | Add of path: string * saveFile: string
    | Test of dir: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version -> "版本號"
            // | Walk dir -> "遍歷"
            | Init path -> "初始化"
            | Compare (dir1, dir2) -> "對比"
            | Add (path, saveFile) -> "添加"
            | Test dir -> "測試"

[<EntryPoint>]
let main args =
    logger.I $"main"

    logger.I $"args: %A{args}"

    let parser =
        ArgumentParser.Create<CliArguments>(programName = "dir-compare.exe")

    let result = parser.Parse args

    // let all = result.GetAllResults()

    logger.I $"%A{parser.PrintUsage()}"

    let version = result.TryGetResult Version
    if version.IsSome then
        logger.I $""

    let init = result.TryGetResult Init
    if init.IsSome then
        logger.I $"{init.Value}"

    exit 0
