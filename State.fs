namespace State
open Common
open System.IO
open System.Text
open FSharpPlus

type VirtualFileInfo =
    { RelativePath: string
      Type: string
      LastWriteTime: string }
    static member Default =
        { RelativePath = ""
          Type = ""
          LastWriteTime = "" }

module VirtualFileInfo =
    let ofFileInfo path (info: FileInfo) =
        let rela = Path.getRelativePath path info.FullName
        let t = if FileInfo.isDir info then "d" else "f"
        let lwt = info.LastWriteTime
        { RelativePath = rela
          Type = t
          LastWriteTime = lwt.ToString(@"yyyy-MM-dd HH:mm:ss:fffK") }

    let toFileInfo path (info: VirtualFileInfo) =
        FileInfo.ofFullName
        <| Path.join path info.RelativePath

    let sort k1 k2 =
        let level f =
            f
            // |> FileInfo.fullName
            |> String.split [@"\"]
            |> Seq.length
        compareWith level k1 k2

type State = State of VirtualFileInfo array

type Difference =
    { Creation: (string * string) list
      Modification: (string * string) list
      Deletion: (string * string) list }

module Difference =
    let read path =
        let lines =
            File.readAlllinesEncoding
                (Path.join path "difference.txt")
                Encoding.UTF8
        let r =
            Array.fold (fun s t ->
                let a = String.split ["|"] t |> Seq.toArray
                let c = a[0]
                let path = a[2]
                let t = a[1]
                let h = path, t
                match c with
                | "c" -> { s with Creation = h::s.Creation }
                | "m" -> { s with Modification = h::s.Modification }
                | "d" -> { s with Deletion = h::s.Deletion }
                | _ -> failwith "error type") 
                { Creation = []
                  Modification = []
                  Deletion = [] } (lines |> Array.rev)
        r

    let write path diffPath diff =
        let addStateM = diff.Creation
        let updateStateM = diff.Modification
        let deleteStateM = diff.Deletion

        // 將修改複製出來
        Directory.createDir diffPath

        let diffDate = Path.join diffPath "data"

        let copyAction (i, _) =
            let destFi = FileInfo.ofFullName (Path.join path i)
            if destFi |> FileInfo.isDir then
                Directory.createDir (Path.join diffDate i)
            else
                let diffDateFi = Path.join diffDate i |> FileInfo.ofFullName
                let diffDateDir = diffDateFi |> FileInfo.directoryName
                if Directory.exists diffDateDir |> not then
                    Directory.createDir diffDateDir
                destFi |> FileInfo.copyTo (diffDateFi |> FileInfo.fullName) false |> ignore

        for i in updateStateM do
            copyAction i

        for i in addStateM do
            // logger.I $"[{__LINE__}] i: %A{i}"
            copyAction i

        let diffFile =
            let mt t = List.map (fun (x, ty) -> $"%s{t}|%s{ty}|%s{x}")
            (addStateM |> mt "c") @
            (updateStateM |> mt "m") @
            (deleteStateM |> mt "d")

        // 生成刪除表
        File.writeAllLinesEncoding
            (Path.join diffPath "difference.txt")
            diffFile
            Encoding.UTF8
    let merge path diffPath diff =
        let diffDate = Path.join diffPath "data"

        // let r = read diff
        // printfn $"[{__LINE__}] %A{r}"

        let copy i t ow =
            let fp = Path.join path i
            if t = "d" then
                Directory.createDir fp
            else
                File.copy 
                    (Path.join diffDate i)
                    fp
                    ow

        let delete i t =
            let path = Path.join path i
            if t = "d" then
                Directory.delete path false
            else
                File.delete path

        diff.Creation
        |> List.iter (fun (p, t) -> copy p t false)

        diff.Modification
        |> List.iter (fun (p, t) -> copy p t true)

        diff.Deletion
        |> List.iter (fun (p, t) -> delete p t)

module State =
    let toArray t=
        match t with
        | State value -> value

    /// <summary>將狀態寫入本地</summary>
    /// <param name="state"></param>
    /// <param name="path"></param>
    /// <returns></returns>
    let write path (state: State) =
        File.writeAllLinesEncoding
            path
            (state
            |> toArray
            |> Array.map (fun x ->
                $"%s{x.Type}|%s{x.LastWriteTime}|%s{x.RelativePath}"))
            Encoding.UTF8

    let ofString str =
        str
        |> String.split ["|"]
        |> Array.ofSeq
        |> (fun x ->
            { VirtualFileInfo.RelativePath = x[2]
              VirtualFileInfo.Type = x[0]
              VirtualFileInfo.LastWriteTime = x[1] })

    /// <summary>讀取保存的狀態</summary>
    /// <param name="path"></param>
    /// <returns></returns>
    let read path =
        // File.SetAttributes((path: string), FileAttributes.Normal)
        File.readAlllinesEncoding path Encoding.UTF8
        |> Array.map ofString
        |> State

    /// <summary>讀取目錄狀態</summary>
    /// <param name="src"></param>
    /// <returns></returns>
    let create src =
        // let fileList = Directory.getAllFileSystemEntries src
        let fileList =
            Directory.enumerateFileSystemEntriesAll src
            |> Seq.toArray
        fileList
        |> Array.sortWith VirtualFileInfo.sort
        |> Array.map FileInfo.ofFullName
        |> Array.map (VirtualFileInfo.ofFileInfo src)
        |> State

    /// <summary>對比兩個目錄狀態</summary>
    /// <param name="destState"></param>
    /// <param name="srcState"></param>
    /// <returns></returns>
    let diff destState srcState =

        let stateToMap (state: State) =
            state
            |> toArray
            // |> Array.map (fun x -> State.ofString x)
            |> Array.map (fun x -> x.RelativePath, x)
            |> Map.ofArray

        let srcStateM = srcState |> stateToMap
        let destStateM = destState |> stateToMap

        let toList m =
            m
            |> Map.toList
            |> List.map (fun (k, v) -> k, v.Type)
            |> List.sortWith (fun (k1, _) (k2, _) -> VirtualFileInfo.sort k1 k2)
            // |> Seq.toList

        let stateCompare =
            Map.compare srcStateM destStateM

        let addStateM =
            Map.difference destStateM srcStateM

        let deleteStateM =
            Map.difference srcStateM destStateM

        let updateStateM =
            stateCompare
            |> Map.chooseValues (fun v ->
                match v with
                | Some v1, Some v2 ->
                    let isDir (x: VirtualFileInfo) = x.Type = "d"
                    let v1d = isDir v1
                    let v2d = isDir v2
                    match v1d, v2d with
                    | false, false ->
                        Option.ofPair (v1.LastWriteTime <> v2.LastWriteTime, v1)
                    | true, true -> None
                    | _, _ -> Some v1
                | _ -> None)
        {
            Difference.Creation = addStateM |> toList
            Difference.Modification = updateStateM |> toList
            Difference.Deletion = (deleteStateM |> toList |> List.rev)
        }

