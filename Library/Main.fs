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
        | FileEntry of path: string

    module Entry =
        let rec write (path: string) (entry: Entry) =
            match entry with
            | DirEntry(p, content) ->
                let current = Path.join path p
                Directory.createDir current
                content |> List.iter (write current)
            | FileEntry(p) ->
                let file = Path.join path p
                File.writeAllTextEncoding file "" Encoding.UTF8

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
                        FileEntry("delete.txt");
                        FileEntry("update.txt");
                        FileEntry("nochange.txt");
                    ]);
                ]);
                DirEntry("u", [
                    FileEntry("update.txt");
                    DirEntry("git", [
                        FileEntry("1.txt");
                        FileEntry("2.txt");
                    ]);
                ]);
                DirEntry("d", [
                    FileEntry("delete.txt");
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