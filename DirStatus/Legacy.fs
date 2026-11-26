module Legacy
open Argu
open AttributeCommon

module Legacy =

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

    type XAddArgs =
        | [<Path>] Path of path: string
        | [<Dest>] Destination of path: string
        | [<Prev>] Previous of path: string
        | Exclude of excludeDir: string list

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Path _ -> "操作目錄"
                | Destination _ -> "輸出文件"
                | Previous _ -> "以前"
                | Exclude _ -> "排除目錄。"

    type XCompareArgs =
        | [<Path>] Path of path: string
        | [<Dest>] Destination of path: string
        | Left of left: string
        | Right of right: string

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Path _ -> "操作目錄"
                | Destination _ -> "輸出文件"
                | Left _ -> "左"
                | Right _ -> "右"

    type XMergeArgs =
        | [<Path>] Path of path: string
        | [<Dest>] Destination of path: string
        | [<Prev>] Previous of path: string
        | Exclude of excludeDir: string list

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Path _ -> "操作目錄"
                | Destination _ -> "輸出文件"
                | Previous _ -> "以前"
                | Exclude _ -> "排除目錄。"

    type XTestArgs =
        | [<Path>] Path of path: string
        //| [<Dest>] Destination of destination: string

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Path _ -> "操作目錄"
                //| Destination _ -> ArgText.dest

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
