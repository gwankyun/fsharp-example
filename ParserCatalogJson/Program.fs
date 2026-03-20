// For more information see https://aka.ms/fsharp-console-apps
open System.IO
open System.Text.Json
open System.Text
open System

type ComponentReference = {
    Id: string
    Origin: string
    Version: string option
    Chip: string option
    Language: string option
    // 其他属性...
}

// 解析组件引用字符串
let parseComponentReference (input: string) =
    let parts = input.Split ','
    let id_ = parts[0]
    let origin = input
    
    // 解析属性
    let properties = 
        parts[1..]
        |> Array.map (fun part ->
            let keyValue = part.Split '='
            if keyValue.Length = 2 then
                Some (keyValue[0].Trim(), keyValue[1].Trim())
            else
                None
        )
        |> Array.choose id
    
    // 构建结果
    let getProperty name =
        properties
        |> Array.tryFind (fun (key, _) -> key = name)
        |> Option.map snd
    
    {
        Id = id_
        Origin = origin
        Version = getProperty "version"
        Chip = getProperty "chip"
        Language = getProperty "language"
    }


module JsonElement =
    // 通用的属性获取函数
    let private tryGetProperty
        (predicate: JsonValueKind -> bool)
        (transform: JsonElement -> 'a)
        (element: JsonElement)
        (key: string) =
        let mutable e = Unchecked.defaultof<JsonElement>
        match element.TryGetProperty(key, &e) with
        | true when predicate e.ValueKind -> Some (transform e)
        | _ -> None

    // 简化的字符串获取
    let tryGetString =
        tryGetProperty ((=) JsonValueKind.String) (fun e -> e.GetString())
    
    // 简化的对象获取
    let tryGetObject = tryGetProperty ((=) JsonValueKind.Object) id
    
    // 简化的数组获取
    let tryGetArray = tryGetProperty ((=) JsonValueKind.Array) id

let extractFileNameFromUrl (url: string) =
    let uri = Uri(url)
    let path = uri.AbsolutePath
    // 分割路径并获取最后一个非空部分
    path.Split('/')
    |> Array.filter (fun part -> not (String.IsNullOrWhiteSpace part))
    |> function
        | [||] -> ""
        | parts -> parts[parts.Length - 1]

let checkField (c: string option) (version: string option) =
    match c, version with
    | Some va, Some vb -> va = vb
    | None, None -> true
    | _ -> false

type Vsix = {
    Origin: string
    Url: string
}

type Config = {
    Catalog: string
}

let tryGetConfig configFile : Config option =
    match File.Exists(configFile) with
    | true ->
        let configJson = File.ReadAllText(configFile, Encoding.UTF8)
        let config = JsonSerializer.Deserialize<Config>(configJson)
        printfn "vs: %s" config.Catalog
        Some config
    | false -> None

let createConfig configFile =
    let config : Config = {
        Catalog = @"D:/26/Catalog.json";
    }
    let configJson = JsonSerializer.Serialize(config)
    File.WriteAllText(configFile, configJson, Encoding.UTF8)

[<EntryPoint>]
let main argv =
    // 跨平台获取用户主目录
    let userHome = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    printfn "User home directory: %s" userHome
    let configFile = Path.Combine(userHome, "PaserCatalog/config.json")

    let config = tryGetConfig configFile
    match config with
    | Some config ->
        let vsixPath = Path.Combine(userHome, "PaserCatalog", "miss.txt")
        let catalog = Path.Combine(userHome, "PaserCatalog", config.Catalog)
        let outFile = Path.Combine(userHome, "PaserCatalog", "out.json")

        // 讀配置
        let vsixConfig =
            File.ReadAllLines(vsixPath, Encoding.UTF8)
            |> Array.map parseComponentReference
            |> Array.map (fun x -> x.Id, x)
            |> Map.ofArray

        for i in vsixConfig do
            //printfn "%A" i
            ()

        // 读取 JSON 文件
        let jsonString = File.ReadAllText(catalog, Encoding.UTF8)

        // 解析 JSON
        let doc = JsonDocument.Parse(jsonString)

        // 根節點
        let root = doc.RootElement

        let array = root.GetProperty("packages")

        using (new StreamWriter(outFile, false)) (fun writer ->
            let result =
                array.EnumerateArray()
                |> Seq.map (fun i ->
                    let id = i.GetProperty("id").GetString()
                    let config = vsixConfig |> Map.tryFind id

                    config
                    |> Option.bind (fun c -> // 參數要一致
                        let version = JsonElement.tryGetString i "version"
                        let chip = JsonElement.tryGetString i "chip"
                        let language = JsonElement.tryGetString i "language"

                        let checkVersion = checkField c.Version version
                        let checkChip = checkField c.Chip chip
                        let checkLanguage = checkField c.Language language

                        if checkVersion && checkChip && checkLanguage then
                            Some c
                        else
                            None
                    )
                    |> Option.bind (fun c -> // payloads數組檢測
                        JsonElement.tryGetArray i "payloads"
                        |> Option.filter (fun p -> p.GetArrayLength() = 1)
                        |> Option.map (fun p -> c, p[0])
                    )
                    |> Option.bind (fun (c, payload) -> // 獲取URL
                        JsonElement.tryGetString payload "url"
                        |> Option.map (fun url -> c, url)
                    )
                    |> Option.map (fun (c, u) -> // 轉換
                        let fileName = JsonElement.tryGetString i "fileName"
                        match fileName with
                        | Some fname -> 
                            let ufile = extractFileNameFromUrl u
                            if fname <> ufile then
                                failwith "fileName <> url"
                        | None -> ()
                        { Origin = c.Origin; Url = u }
                    )
                )
                |> Seq.filter Option.isSome
                |> Seq.map Option.get
            for i in result do
                printfn "origin: %s" i.Origin
                printfn "url: %s\n" i.Url
            // 配置格式化选项
            let options = JsonSerializerOptions()
            options.WriteIndented <- true  // 启用缩进格式化
            let json = JsonSerializer.Serialize(result, options)
            writer.Write(json)
        )
    | None ->
        // 創建配置文件
        createConfig configFile
        exit 1
    0
