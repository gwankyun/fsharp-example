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

// 讀配置
let vsixConfig =
    File.ReadAllLines(@"d:\vs\vsix.txt", Encoding.UTF8)
    |> Array.map parseComponentReference
    |> Array.map (fun x -> x.Id, x)
    |> Map.ofArray

for i in vsixConfig do
    //printfn "%A" i
    ()

// 读取 JSON 文件
let jsonString = File.ReadAllText(@"d:\26\Catalog.json", Encoding.UTF8)

// 解析 JSON
let doc = JsonDocument.Parse(jsonString)

// 根節點
let root = doc.RootElement

let array = root.GetProperty("packages")

module JsonElement =
    let tryGetString (element: JsonElement) (key: string) =
        let mutable e = Unchecked.defaultof<JsonElement>
        match element.TryGetProperty(key, &e) with
        | true ->
            match e.ValueKind with
            | JsonValueKind.String -> Some <| e.GetString()
            | _ -> None
        | false -> None

    let tryGetObject (element: JsonElement) (key: string) =
        let mutable e = Unchecked.defaultof<JsonElement>
        match element.TryGetProperty(key, &e) with
        | true ->
            match e.ValueKind with
            | JsonValueKind.Object -> Some <| e
            | _ -> None
        | false -> None

    let tryGetArray (element: JsonElement) (key: string) =
        let mutable e = Unchecked.defaultof<JsonElement>
        match element.TryGetProperty(key, &e) with
        | true ->
            match e.ValueKind with
            | JsonValueKind.Array -> Some <| e
            | _ -> None
        | false -> None

let extractFileNameFromUrl (url: string) =
    let uri = Uri(url)
    let path = uri.AbsolutePath
    // 分割路径并获取最后一个非空部分
    path.Split('/')
    |> Array.filter (fun part -> not (String.IsNullOrWhiteSpace part))
    |> function
        | [||] -> ""
        | parts -> parts[parts.Length - 1]

using (new StreamWriter("D:/vs_out.txt", false)) (fun writer ->
    for i in array.EnumerateArray() do
        let id = i.GetProperty("id").GetString()
        let config = vsixConfig |> Map.tryFind id

        config |> Option.iter (fun c ->
            let version = JsonElement.tryGetString i "version"
            let chip = JsonElement.tryGetString i "chip"
            let language = JsonElement.tryGetString i "language"

            let checkField c version =
                match c, version with
                | Some va, Some vb -> va = vb
                | None, None -> true
                | _ -> false

            let checkVersion = checkField c.Version version
            let checkChip = checkField c.Chip chip
            let checkLanguage = checkField c.Language language

            if checkVersion && checkChip && checkLanguage then
                let payloads = JsonElement.tryGetArray i "payloads"
                payloads |> Option.iter (fun p ->
                    if p.GetArrayLength() <> 1 then
                        failwith "p len <> 1"
                    let url = JsonElement.tryGetString p[0] "url"
                    url |> Option.iter (fun u ->
                        let fileName = JsonElement.tryGetString i "fileName"
                        if fileName.IsSome then
                            let ufile = extractFileNameFromUrl u
                            if fileName.Value <> ufile then
                                failwith "fileName <> url"
                        printfn "origin: %s" c.Origin
                        writer.WriteLine($"origin: {c.Origin}")
                        printfn "url: %s\n" u
                        writer.WriteLine($"url: {u}")
                        writer.WriteLine($"")
                    )
                )
            )
)
