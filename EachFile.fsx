#load "Common.fsx"
open Common
open System.Text
open System.IO
open System

let flip (f: 'a -> 'b -> 'c) =
    fun (a: 'b) (b: 'a) -> f b a

let file =
    let path = @"e:\local\coroutine_example"
    Directory.getAllFileSystemEntries path
    |> Seq.ofArray
    |> Seq.map FileInfo.ofFileName
    |> Seq.filter (FileInfo.fullName >> String.startsWith (Path.join path "build")
        >> not)
    |> Seq.filter (fun f ->
        let isSrc = [".cpp"; ".h"; ".hpp"; ".cmake"] |> List.contains f.Extension
        let isCMake = f.Name = "CMakeLists.txt"
        isSrc || isCMake)

for f in file do
    printfn "%A" f

    let result =
        let fullName = f |> FileInfo.fullName
        let vcpkgRoot = Environment.getVariable "VCPKG_ROOT"
        let jsonPath = Path.join vcpkgRoot @"installed\x64-windows\share\opencc\t2s.json"
        Diagnostics.startProcess
            "opencc"
            [ "-i"
              fullName
              "-c"
              jsonPath
              "-o"
              fullName ]

    printfn "%A" result
