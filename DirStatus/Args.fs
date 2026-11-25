module Args
open Argu
open AttributeCommon

module Args =

    module Text =
        let name = "配置名"
        let path = "目錄路徑"
        let includePath = "包含目錄"
        let excludePath = "過濾目錄"
        let version = "版本號"
        let destination = "輸出路徑"
        let right = "右對比項，以此為準"
        let left = "左對比項"
        let diff = "變更包路徑"

    module CmdText =
        let init = "初始化目錄"
        let add = "添加版本"
        let diff = "比較版本"
        let merge = "合併版本"
        let update = "更新目錄"
        let equal = "對比目錄"

    type InitArgs =
        | [<Name>] Name of path: string
        | [<Path>] Path of path: string
        | IncludePath of IncludePath: string list
        | ExcludePath of ExcludePath: string list

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Name _ -> Text.name
                | Path _ -> Text.path
                | IncludePath _ -> Text.includePath
                | ExcludePath _ -> Text.excludePath

    type AddArgs =
        | [<Name>] Name of path: string
        | Version of path: string

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Name _ -> Text.name
                | Version _ -> Text.version

    type DiffArgs =
        | [<Name>] Name of path: string
        | [<Dest>] Destination of path: string
        | Left of Left: string
        | Right of Right: string

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Name _ -> Text.name
                | Destination _ -> Text.destination
                | Left _ -> Text.left
                | Right _ -> Text.right

    type EqualArgs =
        | Left of Left: string
        | Right of Right: string
        | IncludePath of IncludePath: string list
        | ExcludePath of ExcludePath: string list

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Left _ -> Text.left
                | Right _ -> Text.right
                | IncludePath _ -> Text.includePath
                | ExcludePath _ -> Text.excludePath

    type UpdateArgs =
        | Path of path: string

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Path _ -> Text.path

    type MergeArgs =
        | Diff of path: string
        | [<Dest>] Destination of path: string
        | [<Name>] Name of path: string

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Diff _ -> Text.diff
                | Destination _ -> Text.destination
                | Name _ -> Text.name
