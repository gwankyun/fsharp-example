module Main
open System.Text
open State
open Common

module Main = 
    let rec copyDirectory (sourceDir: string) (destinationDir: string) (recursive: bool) =
        let dir = DirectoryInfo.ofFullName sourceDir

        let dirs = dir |> DirectoryInfo.getDirectories

        Directory.createDir destinationDir

        for file in dir |> DirectoryInfo.getFiles do
            let targetFilePath = Path.combine destinationDir file.Name
            file |> FileInfo.copyTo targetFilePath true |> ignore

        if recursive then
            for subDir in dirs do
                let newDestDir = Path.combine destinationDir subDir.Name
                copyDirectory subDir.FullName newDestDir true

    let createDirectoryFor (path: string) =
        let dir =
            FileInfo.ofFullName path
            |> FileInfo.directoryName
        if Directory.exists dir |> not then
            Directory.createDir dir

    let writeAllText path contents =
        createDirectoryFor path
        File.writeAllTextEncoding path contents Encoding.UTF8

    type Entry =
        | DirEntry of path: string * content: Entry list
        | TextFileEntry of path: string * encoding: Encoding * content: string

    module Entry =
        let rec write (path: string) (entry: Entry) =
            match entry with
            | DirEntry(p, content) ->
                let current = Path.join path p
                Directory.createDir current
                content |> List.iter (write current)
            | TextFileEntry(p, encoding, content) ->
                let file = Path.join path p
                File.writeAllTextEncoding file content encoding

    let initSrc src =
        // writeAllText (Path.join src @"fst\snd\delete.txt") ""
        // writeAllText (Path.join src @"fst\snd\update.txt") ""
        // writeAllText (Path.join src @"fst\snd\nochange.txt") ""
        // writeAllText (Path.join src @"u\update.txt") ""

        // writeAllText (Path.join src @"d\delete.txt") ""

        // writeAllText (Path.join src @"u\git\1.txt") ""
        // writeAllText (Path.join src @"u\git\2.txt") ""

        let d = new System.IO.DirectoryInfo(src)
        let s = d.Name
        let p = d.Parent.FullName

        let entry =
            DirEntry(s, [
                DirEntry("fst", [
                    DirEntry("snd", [
                        TextFileEntry("delete.txt", Encoding.UTF8, "");
                        TextFileEntry("update.txt", Encoding.UTF8, "");
                        TextFileEntry("nochange.txt", Encoding.UTF8, "");
                    ]);
                ]);
                DirEntry("u", [
                    TextFileEntry("update.txt", Encoding.UTF8, "");
                    DirEntry("git", [
                        TextFileEntry("1.txt", Encoding.UTF8, "");
                        TextFileEntry("2.txt", Encoding.UTF8, "");
                    ]);
                ]);
                DirEntry("d", [
                    TextFileEntry("delete.txt", Encoding.UTF8, "");
                ]);
            ])
        Entry.write p entry

    let changeFile dest =
        // 新增文件
        let addFile =
            let file = [ ("add.txt", "f"); (@"a\add2.txt", "f") ]
            for (i, _) in file do
                let addFile = Path.joinList [ dest; i ]
                writeAllText addFile ""
            ("a", "d")::file

        // 刪除文件
        let deleteFile =
            let file = [ ("delete.txt", "f"); (@"d\delete.txt", "f") ]
            for (i, _) in file do
                let deleteFile = Path.joinList [ dest; i ]
                File.delete deleteFile
            Directory.delete (Path.join dest "d") true
            ("d", "d")::file |> List.rev

        // 修改文件
        let updateFile =
            let file = [ ("update.txt", "f"); (@"u\update.txt", "f") ]
            for i, _ in file do
                let updateFile = Path.joinList [ dest; i ]
                writeAllText updateFile "1"
            file
        // addFile, updateFile, deleteFile
        {
            State.Difference.Creation = addFile
            State.Difference.Modification = updateFile
            State.Difference.Deletion = deleteFile
        }

    // let testSort =