// For more information see https://aka.ms/fsharp-console-apps
open FSLogger
open Argu
open Common
open State
open Main
open System
open System.IO
open System.Threading
open FSharpPlus

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

        let deleteIfExists path =
            if Directory.exists path then
                Directory.delete path true

        deleteIfExists path
        Directory.createDir path

        Main.initSrc path

        let copyPath = Path.join Directory.current "copy"
        deleteIfExists copyPath

        Main.copyDirectory path copyPath true

        // 初始
        init path

        add path "1"

        Main.changeFile path |> ignore

        add path "2"

        let readState f = State.read (Path.joinList [ path; ".state"; f ])
        let diff = State.diff (readState "2.txt") (readState "1.txt")
        let diffPath = Path.join Directory.current "diff"
        deleteIfExists diffPath
        Difference.write path diffPath diff

        Difference.merge copyPath diffPath diff

        // let result = State.equal (State.createFilter path pred) (State.createFilter copyPath pred)
        // logger.I "result: %A{result}"

module Diff =
    let sort (k1: string) (k2: string) =
        let level f =
            f
            |> String.split [@"\"]
            |> Seq.length
        compareWith level k1 k2

    let sortWith f =
        let s k1 k2 =
            sort (f k1) (f k2)
        s

    let fileInfoToStr (f: FileInfo) =
        let t =
            if f |> FileInfo.isDir then "d" else "f"
        let lwt = f.LastWriteTime.ToFileTime()
        // let p = Path.getRelativePath path f.FullName
        $"%s{t}|%d{lwt}"

    let timeFormat = @"yyyy-MM-dd HH:mm:ss:fffK"

    let addState path =
        let fileCont =
            Directory.getAllFileSystemEntries path
            |> Array.sortWith sort
            |> Array.map (fun i ->
                let info = i |> FileInfo.ofFullName
                let rela = Path.getRelativePath path info.FullName
                let t = 
                    match info.FullName |> Directory.exists with
                    | true -> "d"
                    | false -> "f"
                let lwt = info.LastWriteTime.ToString(timeFormat)
                $"%s{t}|%s{lwt}|%s{rela}"
                )
        // for i in fileCont do
        //     logger.I $"%s{i}"
        fileCont

    let test path =
        let path = Path.join Directory.current path
        logger.I $"path: %s{path}"

        let deleteIfExists path =
            if Directory.exists path then
                Directory.delete path true

        deleteIfExists path
        Directory.createDir path
        Main.initSrc path

        let now = DateTime.Now
        logger.I $"now: %A{now.ToString(timeFormat)}"
        Thread.Sleep(1 * 1000);


        let copyPath = Path.join Directory.current "copy"
        deleteIfExists copyPath

        Main.copyDirectory path copyPath true

        let later = DateTime.Now

        logger.I $"%A{now < later}"

        Thread.Sleep(1 * 1000);

        Main.changeFile path |> ignore

        let state = addState path

        let newFile =
            state
            |> Array.filter (fun x ->
                let s = String.split [@"|"] x |> Array.ofSeq
                let t = s[0]
                // let info = s[2] |> FileInfo.ofFullName
                let dt = DateTime.ParseExact(s[1], timeFormat, null)
                dt > now && t = "f"
                )
        logger.I $"newFile: %A{newFile}"

        // 複製出來
        deleteIfExists <| Path.join Directory.current "newFile"
        for i in newFile do
            let s = String.split [@"|"] i |> Array.ofSeq
            let info =
                FileInfo.ofFullName <| Path.joinList [
                    Directory.current; "newFile"; s[2]
                ]
            if info.Directory.Exists |> not then
                info.Directory.Create()
            let src = FileInfo.ofFullName <| Path.join path s[2]
            logger.I $"src: %A{src}"
            src.CopyTo(info.FullName) |> ignore

        let stateToMap state =
            state
            |> Array.map (fun x ->
                let a = String.split [@"|"] x |> Seq.toArray
                a[2], (a[0], a[1]))
            |> Map.ofArray

        let copyState = addState copyPath
        let pathMap = stateToMap state
        let copyMap = stateToMap copyState
        let compare = Map.compare pathMap copyMap
        logger.I $"compare: %A{compare}"

        let mapToArray pred =
            Map.toArray
            >> Array.filter pred
            >> Array.sortWith (sortWith fst)
            >> Array.map (fun (k, (t, d)) -> $"{t}|{d}|{k}")

        let deleteItem =
            Map.difference copyMap pathMap
            |> mapToArray (fun _ -> true)
            |> Array.rev
        logger.I $"deleteItem: \n%A{deleteItem}"

        let addItem =
            Map.difference pathMap copyMap
            |> mapToArray (fun (_, (t, _)) -> t = "d")
        logger.I $"addItem: \n%A{addItem}"

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
        // Diff.add st2

    let test = result.TryGetResult Test
    if test.IsSome then
        // DirCompare.test test.Value
        // Diff.addState test.Value
        Diff.test test.Value

    exit 0
