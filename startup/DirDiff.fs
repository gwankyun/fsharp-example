module DirDiff
open Common
open Common.extra
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open FSLogger

// 1. Either create the serializer options from the F# options...
let options =
    JsonFSharpOptions.Default()
        // Add any .WithXXX() calls here to customize the format
        .ToJsonSerializerOptions()

let logger = Logger.ColorConsole

type Item = {
    Path: string
    LastWrite: int64
}

let inline (</>) path1 path2 = Path.join path1 path2

let inline (|?) (option: 'T option) defaultValue = 
    Option.defaultValue defaultValue option

let relativePath parent path =
    Path.getRelativePath parent path

module DateTime =
    type T = System.DateTime
    let toFileTime (dt: T) =
        dt.ToFileTime()

    let toFileTimeUtc (dt: T) =
        dt.ToFileTimeUtc()

let writeFile name text =
    let path = Path.join Directory.baseDir name
    File.writeAllTextEncoding path text Encoding.UTF8

// 讀入

let readFile name =
    let path = Path.join Directory.baseDir name
    File.readAllTextEncoding path Encoding.UTF8

type Difference = {
    Creation: string list
    Modification: string list
    Deletion: string list
}

module DateType =
    type T = Item list

    let toStringList (dt: T) =
        dt
        |> List.map (fun x -> x.Path)

    let compare (oldData: T) (newData: T) : Difference =
        let index (data: T) =
            data
            |> List.map (fun x -> x.Path)
            |> List.indexed
            |> List.map Tuple2.swap
            |> Map.ofList
        let indexOld = index oldData
        let indexNew = index newData
        let toMap (data: T) =
            data
            |> List.map (fun d -> d.Path, d.LastWrite)
            |> Map.ofList
        let mapOld, mapNew = Tuple2.map toMap (oldData, newData)
        let c = Map.compare mapOld mapNew
        let toList x =
            x
            |> Map.toList
            |> List.map fst
        let creation =
            c
            |> Map.filter (fun _ (a, b) ->
                a.IsNone && b.IsSome
                )
            |> toList
            |> List.sortBy (fun x -> indexNew[x])
        let deletion =
            c
            |> Map.filter (fun _ (a, b) ->
                a.IsSome && b.IsNone
                )
            |> toList
            |> List.sortBy (fun x -> indexOld[x])
        let modification =
            c
            |> Map.filter (fun _ (a, b) ->
                Option.map2 (<>) a b |? false
                )
            |> toList
            |> List.sortBy (fun x -> indexNew[x])
        { Creation = creation
          Modification = modification
          Deletion = deletion }

type EntryDiff = {
    DirDiff: Difference
    FileDiff: Difference
}

type Operation =
    | DirCreation
    | FileCreation
    | FileModification
    | DirDeletion
    | FileDeletion

module Difference =
    type T = Difference
    let toList (dir: T) (file: T) =
        let trans op = List.map (fun x -> x, op)
        [ dir.Creation |> trans DirCreation;
          file.Creation |> trans FileCreation;
          file.Modification |> trans FileModification;
          file.Deletion |> trans FileDeletion |> List.rev;
          dir.Deletion |> trans DirDeletion |> List.rev; ]
        |> List.concat

module EntryDiff =
    type T = EntryDiff

    let dataDir = "data"
    let backupDir = "backup"

    let save src dest (e: T) =
        if Directory.exists dest |> not then
            Directory.createDir dest
        let ls = Difference.toList e.DirDiff e.FileDiff
        Directory.createDir (dest </> dataDir)
        Directory.createDir (dest </> backupDir)
        for i in ls do
            match i with
            | path, FileCreation ->
                let srcPath = src </> path
                let destPath = dest </> dataDir </> path
                Directory.createDirectoryFor destPath
                File.copy srcPath destPath true
            | path, FileModification ->
                let srcPath = src </> path
                let destPath = dest </> dataDir </> path
                Directory.createDirectoryFor destPath
                File.copy srcPath destPath true
            | _ -> ()
        let text = JsonSerializer.Serialize<(string * Operation) list>(ls, options)
        File.writeAllTextEncoding (dest </> "info.json") text Encoding.UTF8

    let load dest =
        let text = File.readAllTextEncoding (dest </> "info.json") Encoding.UTF8
        let info = JsonSerializer.Deserialize<(string * Operation) list>(text, options)
        info

let progress path info : DateType.T =
    info
    |> List.map (fun x ->
        let rela = relativePath path
        { Path = x |> Entry.path |> rela
          LastWrite =
            x |> Entry.lastWriteTime |> DateTime.toFileTime }
        )

type Status = {
        Dir: DateType.T
        File: DateType.T
    }

module Status =
    type T = Status

    let copyNeed (d: Difference) (f: Difference) src dest =
        // 目錄創建，淺到深
        let createDir = d.Creation
        for i in createDir do
            let path = dest </> i
            Directory.createDir path

        // 文件修改
        let modifyFile = f.Modification |> List.rev

        let copy fileList =
            for i in fileList do
                let srcPath = src </> i
                let destPath = dest </> i
                if Directory.exists srcPath then
                    //assert(false)
                    failwith $"%A{srcPath} is dir"
                File.copy srcPath destPath true

        copy modifyFile

        // 文件新增
        let createFile = f.Creation |> List.rev
        //printfn $"%A{createFile}"
        copy createFile
        createDir, (List.append modifyFile createFile)

    let fromPath path : Status =
        let d, f =
            Directory.traverse path
        let d, f = Tuple2.map (progress path) (d, f)
        { Dir = d; File = f }

    let write path info =
        let text = JsonSerializer.Serialize<DateType.T>(info)
        File.writeAllTextEncoding path text Encoding.UTF8

    let read path =
        let text = File.readAllTextEncoding path Encoding.UTF8
        JsonSerializer.Deserialize<DateType.T>(text)

    //let merge (d: Difference) (f: Difference) src dest =
    let merge (entry: EntryDiff) src dest =
        let { EntryDiff.DirDiff = d; FileDiff = f } = entry
        logger.D $"d: %A{d}"
        logger.D $"f: %A{f}"
        //logger.D $"ls: %A{ls}"

        let ls = Difference.toList d f

        for i in ls do
            match i with
            | path, DirCreation ->
                let destPath = Path.join dest path
                logger.D $"DirCreation: %A{destPath}"
                Directory.createDir destPath
            | path, DirDeletion ->
                let destPath = Path.join dest path
                logger.D $"DirDeletion: %A{destPath}"
                Directory.delete destPath true
            | path, FileDeletion ->
                let destPath = Path.join dest path
                File.delete destPath
            | path, FileCreation ->
                let srcPath = Path.join src path
                let destPath = Path.join dest path
                File.copy srcPath destPath true
            | path, FileModification ->
                let srcPath = Path.join src path
                let destPath = Path.join dest path
                File.copy srcPath destPath true

    let merge2 src dest backup =
        let ls = EntryDiff.load src
        let backupDir = src </> EntryDiff.backupDir
        let src = src </> EntryDiff.dataDir
        let backupFile path =
            if backup then
                let backupPath = backupDir </> path
                let destPath = dest </> path
                if File.exists destPath then
                    Directory.createDirectoryFor backupPath
                    File.copy destPath backupPath true
        for i in ls do
            match i with
            | path, DirCreation ->
                let destPath = dest </> path
                logger.D $"DirCreation: %A{destPath}"
                Directory.createDir destPath
            | path, DirDeletion ->
                let destPath = dest </> path
                logger.D $"DirDeletion: %A{destPath}"
                Directory.delete destPath true
            | path, FileDeletion ->
                let destPath = dest </> path
                logger.D $"FileDeletion: %A{destPath}"
                backupFile path
                File.delete destPath
            | path, FileCreation ->
                let srcPath = src </> path
                let destPath = dest </> path
                logger.D $"FileCopying: %A{destPath}"
                File.copy srcPath destPath true
            | path, FileModification ->
                let srcPath = src </> path
                let destPath = dest </> path
                backupFile path
                logger.D $"FileCopying: %A{destPath}"
                File.copy srcPath destPath true

    let equal src dest =
        let  { Dir = sd; File = sf }, { Dir = dd; File = df } =
            Tuple2.map fromPath (src, dest)
        let toStr x = List.map (fun y -> y.Path) x
        // 目錄衹用存在就好
        let sd, dd = Tuple2.map toStr (sd, dd)
        if (sd <> dd) then
            printfn $"sd: %A{sd}"
            printfn $"sd: %A{dd}"
            failwith $"%A{sd} %A{dd}"
        if (sf <> df) then
            failwith $"%A{sf} %A{df}"
        (sd = dd) && (sf = df)

    let compare (oldData: T) (newData: T) : EntryDiff.T =
        let { Dir = oldDir; File = oldFile } = oldData
        let { Dir = newDir; File = newFile } = newData
        { DirDiff = DateType.compare oldDir newDir;
          FileDiff = DateType.compare oldFile newFile }

let createDir path =
    Directory.deleteIfExists path true
    Directory.createDir path
