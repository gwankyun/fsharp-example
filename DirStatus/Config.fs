module Config
open System.Text.Json
open System.Text.Json.Serialization
open System.IO
open System.Text
open Common

type Config = {
    //Name: string
    Path: string
    Destination: string
    Include: string list
    Exclude: string list
}

let writeConfig config =
    let userProfileDirectory =
        System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile)
    let configDir = userProfileDirectory +/ "DirStatus"
    let configFile = configDir +/ "config.json"
    // 寫入文件
    let text =
        JsonSerializer.Serialize<Map<string, Config>>(config, Common.options)
    File.WriteAllText(configFile, text, Encoding.UTF8)

let readConfig configFile =
    let config = 
        let content = File.ReadAllText(configFile, Encoding.UTF8)
        JsonSerializer.Deserialize<Map<string, Config>>(content, Common.options)
    config

let tryGet name =
    let userProfileDirectory =
        System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile)
    let configDir = userProfileDirectory +/ "DirStatus"
    let configFile = configDir +/ "config.json"
    let config = readConfig configFile
    config |> Map.tryFind name

let init path destination name =
    //Check.dir path

    // 获取用户主目录
    let userProfileDirectory =
        System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile)
    printfn "用户主目录: %s" userProfileDirectory

    let configDir = userProfileDirectory +/ "DirStatus"
    if not <| Directory.exists configDir then
        Directory.createDirectory configDir


    let configFile = configDir +/ "config.json"

    if not <| File.exists configFile then
        writeConfig Map.empty

    match name with
    | Some n ->
        let c: Config = {
            Path = path
            Destination = destination
            Include = []
            Exclude = []
        }
        let config = readConfig configFile
        match tryGet n with
        | Some _ ->
            let config =
                config
                |> Map.remove n
                |> Map.add n c
            writeConfig config
        | None ->
            let config =
                config |> Map.add n c
            writeConfig config
    | None -> ()
