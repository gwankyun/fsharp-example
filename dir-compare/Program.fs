// For more information see https://aka.ms/fsharp-console-apps
open FSLogger
open Argu
open Common
open State
open Main

let logger = Logger.ColorConsole

type CliArguments =
    | Version
    // | Walk of dir: string
    | Init of path: string
    | Compare of path: string * dir1: string * dir2: string
    | Add of path: string * saveFile: string
    | Test of dir: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version -> "版本號"
            // | Walk dir -> "遍歷"
            | Init path -> "初始化"
            | Compare(path, dir1, dir2) -> "對比"
            | Add(path, saveFile) -> "添加"
            | Test dir -> "測試"

let createDirIfNotExists path =
    if path |> Directory.exists |> not then
        Directory.createDir path

let statePost= ".state"

module DirCompare =
    let init path =
        let path = path

        if Directory.exists path then
            let state = Path.join path statePost
            createDirIfNotExists state

    let pred (x: System.IO.FileInfo) =
        x.FullName
        |> String.startsWith statePost
        |> not

    let add path file =
        let target = Path.joinList [ path; statePost; $"%s{file}.txt" ]
        let statePath =
            Path.join path statePost
            |> FileInfo.ofFullName
            |> FileInfo.fullName
        logger.I $"statePath: %s{statePath}"

        if path |> Directory.exists then
            let st =
                State.createFilter path pred
                // (fun x ->
                //     x.FullName
                //     |> String.startsWith statePath
                //     |> not)

            State.write target st
    
    let compare path st1 st2 =
        let s1 = State.read <| Path.joinList [path; ".state"; $"%s{st1}.txt"]
        let s2 = State.read <| Path.joinList [path; ".state"; $"%s{st2}.txt"]
        let diff = State.diff s1 s2
        // let diffPath =
        Difference.write path (Path.join path statePost) diff
    
    let test path =
        let path = Path.join Directory.current path
        logger.I $"path: %s{path}"

        let deleteIf path =
            if Directory.exists path then
                Directory.delete path true

        deleteIf path
        Directory.createDir path

        Main.initSrc path

        let copyPath = Path.join Directory.current "copy"
        // if Directory.exists copyPath then
        //     Directory.delete copyPath true
        deleteIf copyPath

        Main.copyDirectory path copyPath true

        // 初始
        init path

        add path "1"

        Main.changeFile path |> ignore

        add path "2"

        let readState f = State.read (Path.joinList [ path; ".state"; f ])
        let diff = State.diff (readState "2.txt") (readState "1.txt")
        let diffPath = Path.join Directory.current "diff"
        deleteIf diffPath
        Difference.write path diffPath diff

        Difference.merge copyPath diffPath diff

        // let result = State.equal (State.createFilter path pred) (State.createFilter copyPath pred)
        // logger.I "result: %A{result}"

[<EntryPoint>]
let main args =
    logger.I $"main"

    logger.I $"args: %A{args}"

    let parser = ArgumentParser.Create<CliArguments>(programName = "dir-compare.exe")

    let result = parser.Parse args

    // let all = result.GetAllResults()

    logger.I $"%A{parser.PrintUsage()}"

    let version = result.TryGetResult Version

    if version.IsSome then
        logger.I $""


    let init = result.TryGetResult Init

    if init.IsSome then
        // 創建.state文件夾
        logger.I $"{init.Value}"
        DirCompare.init init.Value

    let add = result.TryGetResult Add

    if add.IsSome then
        let path, file = add.Value
        DirCompare.add path file

    let compare = result.TryGetResult Compare
    if compare.IsSome then
        let path, st1, st2 = compare.Value
        DirCompare.compare path st1 st2

    let test = result.TryGetResult Test
    if test.IsSome then
        DirCompare.test test.Value

    exit 0
