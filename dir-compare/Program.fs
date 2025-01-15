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
open Expecto

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

let timeFormat = @"yyyy-MM-dd HH:mm:ss:fffK"

type Item =
    | DirItem of path: string * lastWrite: DateTime
    | FileItem of path: string * lastWrite: DateTime

module Item =
    let ofString text =
        let a =
            String.split [@"|"] text
            |> Seq.toArray
        let t = a[0]
        let w = a[1]
        let p = a[2]
        let lw = DateTime.ParseExact(w, timeFormat, null)
        if t = "d" then
            DirItem(path = p, lastWrite = lw)
        else
            FileItem(path = p, lastWrite = lw)

    let path (item: Item) =
        match item with
        | DirItem(p, _) -> p
        | FileItem(p, _) -> p

    let toString (item: Item) =
        match item with
        | DirItem(path, lastWrite) ->
            let lwt = lastWrite.ToString(timeFormat)
            $"d|%s{lwt}|%s{path}"
        | FileItem(path, lastWrite) ->
            let lwt = lastWrite.ToString(timeFormat)
            $"f|%s{lwt}|%s{path}"

    let ofFileSystemInfo relateTo (info: FileSystemInfo) =
        let fullName = info.FullName
        let rela = Path.getRelativePath relateTo fullName
        let lwt = info.LastWriteTime
        match info with
        | :? DirectoryInfo -> DirItem(rela, lwt)
        | :? FileInfo -> FileItem(rela, lwt)
        | _ -> failwith "other FileSystemInfo"

    let toFileInfo relateTo (item: Item) =
        match item with
        | DirItem(_) -> failwith "not File"
        | FileItem(p, _) ->
            FileInfo.ofFullName <| Path.join relateTo p

type RelativePath =
    | RelativeDir of path: string
    | RelativeFile of path: string

type RelaPath =
    | RelaDir of path: string * relaTo: Type.Path
    | RelaFile of path: string * relaTo: Type.Path

// type RelaInfo =
//     | Rela

module RelaPath =
    let copy (source: RelaPath) dest =
        let joinDest = Path.join dest
        match source with
        | RelaDir(rela, _) ->
            let destPath = joinDest rela
            Directory.createDir destPath
        | RelaFile(rela, path) ->
            let destPath = joinDest rela
            Directory.createDirectoryFor destPath
            let sourcePath = Path.join path rela
            File.copy sourcePath destPath true

    let path (p: RelaPath) =
        match p with
        | RelaDir(rela, _) -> rela
        | RelaFile(rela, _) -> rela

    let ofPath relateTo path =
        let rela = Path.getRelativePath relateTo path
        match path |> FileInfo.ofFullName |> FileInfo.isDir with
        | true -> RelaDir(rela, relateTo)
        | false -> RelaFile(rela, relateTo)

    let ofDir relateTo path =
        let rela = Path.getRelativePath relateTo path
        RelaDir(rela, relateTo)

    let ofFile relateTo path =
        let rela = Path.getRelativePath relateTo path
        RelaFile(rela, relateTo)

    let ofFileSystemInfo relateTo (info: FileSystemInfo) =
        let fullName = info.FullName
        let rela = Path.getRelativePath relateTo fullName
        match info with
        | :? DirectoryInfo -> RelaDir(rela, relateTo)
        | :? FileInfo -> RelaFile(rela, relateTo)
        | _ -> failwith "other FileSystemInfo"

    let enumerate path =
        Directory.enumerateFileSystemInfos path
        |> Seq.map (ofFileSystemInfo path)

// type Info =
//     { Type: string
//       LastWrite: DateTime
//       Path: string }

module DirCompare =
    let init path =
        let path = path

        if Directory.exists path then
            let state = Path.join path statePost
            createDirIfNotExists state

    let pred (x: FileInfo) =
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

    let addState path pred =
        let fileCont path =
            Directory.enumerateFileSystemInfos path
            |> Seq.map (Item.ofFileSystemInfo path)
            |> Seq.sortWith (sortWith Item.path)
            |> Array.ofSeq

        // for i in fileCont do
        //     logger.I $"%s{i}"
        // path |> Array.collect fileCont
        path |> fileCont

    let stateToMap state =
        state
        |> Array.map (fun x ->
            // let a = String.split [@"|"] x |> Seq.toArray
            // a[2], (a[0], a[1])
            match x with
            | DirItem(p, w) as d -> p, d
            | FileItem(p, w) as f -> p, f
            )
        |> Map.ofArray

    let check state copyState =
        let pathMap = stateToMap state
        let copyMap = stateToMap copyState

        let tupleToItem k (t, d) =
            let dt = DateTime.ParseExact(d, timeFormat, null)
            if t = "d" then
                DirItem(k, dt)
            else
                FileItem(k, dt)

        let compare =
            Map.compare pathMap copyMap
            |> Map.filter (fun _ (a, b) ->
                match a, b with
                | Some(DirItem(_)), Some(DirItem(_)) -> false
                | Some(FileItem(_, ad)), Some(FileItem(_, bd)) -> ad <> bd
                | None, None -> failwith "???"
                | _ -> true
                )
        
        logger.I $"compare: %A{compare}"
        compare |> Map.isEmpty

    let text = "======================================================"
    // 開始

    let deleteIfExists path =
        if Directory.exists path then
            Directory.delete path true

    let testAdd path copyPath now ignoreFile =

        let state = addState path ignoreFile

        let newFile =
            state
            |> Array.filter (fun x ->
                match x with
                | DirItem(_) -> false
                | FileItem(p, w) -> w > now
                )
            // |> Array.sortWith sort
        logger.I $"newFile: %A{newFile}"

        // 複製出來
        let newFilePath = Path.join Directory.current "newFile"
        deleteIfExists newFilePath
        for i in newFile do
            let p = Item.path i

            let info =
                Item.toFileInfo newFilePath i

            if info.Directory.Exists |> not then
                info.Directory.Create()
            let src = FileInfo.ofFullName <| Path.join path p
            logger.I $"src: %A{src}"
            src.CopyTo(info.FullName) |> ignore

        let copyState = addState copyPath ignoreFile
        let pathMap = stateToMap state
        let copyMap = stateToMap copyState
        let compare = Map.compare pathMap copyMap
        logger.I $"compare: %A{compare}"

        let mapToArray pred =
            Map.toArray
            >> Array.filter pred
            >> Array.sortWith (sortWith fst)
            >> Array.map (fun (k, item) ->
                item |> Item.toString
                // $"{t}|{d}|{k}"
                )

        logger.I text

        logger.I text

        let deleteItem =
            Map.difference copyMap pathMap
            |> mapToArray (fun _ -> true)
            |> Array.rev
        logger.I $"deleteItem: \n%A{deleteItem}"
        logger.I text

        let addItem =
            Map.difference pathMap copyMap
            |> mapToArray (fun (_, item) ->
                match item with
                | FileItem(_) -> true
                | _ -> false
            )
        logger.I $"addItem: \n%A{addItem}"
        logger.I text

        // let updateItem =
        //     Map.compare pathMap copyMap
        //     |> 

        Path.join Directory.current "newFile", deleteItem, addItem

    let merge path copyPath newFilePath deleteItem addItem =
        for i in deleteItem do
            logger.I $"d: %A{i}"
            let item = Item.ofString i
            let join = Path.join copyPath
            match item with
            | DirItem(p, d) -> Directory.delete <| join p <| true |> ignore
            | FileItem(p, d) -> File.delete <| join p |> ignore

        for i in addItem do
            logger.I $"a: %A{i}"
            let item = Item.ofString i
            let join = Path.join copyPath
            match item with
            | DirItem(p, _) -> Directory.createDir <| join p
            | FileItem(p, _) ->
                let src = Path.join path p
                let dest = join p
                if File.exists src |> not then
                    failwith $"%s{src} not exist"
                Directory.createDirectoryFor dest
                File.copy src dest true

        logger.I text

        // let newFilePath = Path.join Directory.current "newFile"
        let newFileList = Directory.getAllFileSystemEntries newFilePath
        for i in newFileList do
            logger.I $"ni: %A{i}"
            let r = Path.getRelativePath newFilePath i
            logger.I $"r: %A{r}"

        Main.copyDirectory newFilePath copyPath true

        logger.I text

    // let tests =
    //     test "A simple test" {
    //         let subject = "Hello World"
    //         Expect.equal subject "Hello World" "The strings should equal"
    //     }

    let runTest path =
        let path = Path.join Directory.current path
        logger.I $"path: %s{path}"

        test "" {

            deleteIfExists path
            Directory.createDir path
            Main.initSrc path

            let now = DateTime.Now
            logger.I $"now: %A{now.ToString(timeFormat)}"
            Thread.Sleep(1 * 1000);

            let ignoreFile (_, _, path) =
                String.startsWith @"u\git" path |> not

            let copyPath = Path.join Directory.current "copy"
            deleteIfExists copyPath

            Main.copyDirectory path copyPath true

            let later = DateTime.Now

            logger.I $"%A{now < later}"

            Thread.Sleep(1 * 1000);

            Main.changeFile path |> ignore

            // 將修改部分複製出來
            let newFilePath, deleteItem, addItem = testAdd path copyPath now ignoreFile

            // 合併修改到copy目錄
            logger.I $"dest: %A{copyPath}"
            merge path copyPath newFilePath deleteItem addItem

            // 對比
            let copyState = addState copyPath ignoreFile

            let state = addState path ignoreFile
            let checkEq = check state copyState
            logger.I $"checkEq: %A{checkEq}"

            Expect.equal true checkEq "checkEq"

            logger.I $"path: %A{path}"
            logger.I $"copyPath: %A{copyPath}"

            // let xyz = Path.join path @"x\y\z.txt"
            // Directory.createDirectoryFor xyz
            // File.writeAllText xyz "x"

            // let rela = RelaPath.ofFile path xyz
            // logger.I $"rela: %A{rela}"

            // deleteIfExists path
            // Directory.createDir path
            // Main.initSrc path

            // deleteIfExists copyPath
            // Directory.createDir copyPath

            // RelaPath.enumerate path
            // |> Seq.iter (fun x -> RelaPath.copy x copyPath)

            // let ri x =
            //     x
            //     |> Seq.map RelaPath.path
            //     |> Seq.sort
            //     |> Seq.toList

            // let rp = RelaPath.enumerate path |> ri
            // let rc = RelaPath.enumerate copyPath |> ri
            // Expect.equal rp rc "xxxx"
        }

    let test path =
        runTestsWithCLIArgs [] Array.empty (runTest path) |> ignore

module DiffLocal =
    type State = Item []

    type Difference =
        { Modification: State
          Creation: State
          Deletion: State
        }

    module State =
        let empty: State = Array.empty

        let add path =
            Directory.enumerateFileSystemInfos path
            |> Seq.map (Item.ofFileSystemInfo path)
            |> Seq.sortWith (Diff.sortWith Item.path)
            |> Array.ofSeq

        let write dest (state: State) =
            state
            |> Array.map Item.toString
            |> File.writeAllLines dest

        let read path : State=
            File.readAlllines path
            |> Array.map Item.ofString

        let diff (newState: State) (oldState: State) =
            let stateToMap (state: State) =
                state
                |> Array.map (fun x ->
                    match x with
                    | DirItem(p, _) as d -> p, d
                    | FileItem(p, _) as f -> p, f)
                |> Map.ofArray
            let newMap = stateToMap newState
            let oldMap = stateToMap oldState
            let mapToArray m =
                m
                |> Map.toArray
                |> Array.sortWith (Diff.sortWith fst)
                |> Array.map snd
            let modifyItem =
                Map.compare newMap oldMap
                |> Map.filter (fun _ v ->
                    match v with
                    | Some(DirItem(_)), Some(FileItem(_)) -> true
                    | Some(FileItem(_)), Some(DirItem(_)) -> true
                    | Some(FileItem(_, nw)), Some(FileItem(_, ow)) ->
                        nw <> ow
                    | _ -> false)
                |> Map.map (fun _ v ->
                    match v with
                    | Some(item), Some(_) -> item
                    | _ -> failwith "not modify")
                |> mapToArray
                |> Array.rev
            let deleteItem =
                Map.difference oldMap newMap
                |> mapToArray
            let addItem =
                Map.difference newMap oldMap
                |> mapToArray
            // addItem, modifyItem, deleteItem
            {
                Difference.Modification = modifyItem
                Creation = addItem
                Deletion = deleteItem
            }

    module Difference =
        let write path (difference: Difference) =
            let modification = difference.Modification
            let creation = difference.Creation
            let deletion = difference.Deletion
            if Directory.exists path then
                Directory.delete path true
            

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
