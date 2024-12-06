// For more information see https://aka.ms/fsharp-console-apps
open FSLogger
open Argu
open Common
open State

let logger = Logger.ColorConsole

type CliArguments =
    | Version
    // | Walk of dir: string
    | Init of path: string
    | Compare of path:string * dir1: string * dir2: string
    | Add of path: string * saveFile: string
    | Test of dir: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version -> "版本號"
            // | Walk dir -> "遍歷"
            | Init path -> "初始化"
            | Compare (path, dir1, dir2) -> "對比"
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

    let createDirIfNotExists path =
        if path |> Directory.exists |> not then
            Directory.createDir path

    let init = result.TryGetResult Init
    if init.IsSome then
        logger.I $"{init.Value}"
        let path = init.Value
        if Directory.exists path then
            let state = Path.join path ".state"
            createDirIfNotExists state

    let add = result.TryGetResult Add
    if add.IsSome then
        let path, file = add.Value
        let target = Path.joinList [path; ".state"; $"%s{file}.txt"]
        let statePath =
            Path.join path ".state"
            |> FileInfo.ofFullName
            |> FileInfo.fullName
        logger.I $"statePath: %s{statePath}"
        if path |> Directory.exists then
            let st =
                State.createFilter path 
                    (fun x ->
                    x.FullName |> String.startsWith statePath |> not)
            State.write target st

    // let compare = result.TryGetResult Compare
    // if compare.IsSome then
    //     let path, st1, st2 = compare.Value
    //     let s1 = State.read <| Path.joinList [path; ".state"; $"%s{st1}.txt"]
    //     let s2 = State.read <| Path.joinList [path; ".state"; $"%s{st2}.txt"]
    //     let diff = State.diff s1 s2
    //     let diffPath = 
    //     Difference.write path 

    exit 0
