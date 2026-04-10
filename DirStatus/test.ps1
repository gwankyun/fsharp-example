$path = "C:\local\repos\CMake\Help\guide\tutorial\Step1"

$dest = "D:\temp"

$name = "Step1"

# 創建測試數據
./bin/DirStatus/DirStatus.exe testprepare -p $path -d $dest

# 添加一個工程
./bin/DirStatus/DirStatus.exe init -p "D:\temp\src\" -d "D:\temp\DirStatus\Step1\" -n $name

# 添加狀態
./bin/DirStatus/DirStatus.exe add -n $name -d 1

# 更新測試數據
./bin/DirStatus/DirStatus.exe testupdate -p $path -d $dest

# 添加狀態
./bin/DirStatus/DirStatus.exe add -n $name -d 2

# 對比狀態
./bin/DirStatus/DirStatus.exe compare -n $name --left 1 --right 2 -d "diff"

# 導出差異文件
./bin/DirStatus/DirStatus.exe export -n $name --diff "diff" -d "diff"

# 合併差異文件
$export = "D:\temp\DirStatus\Step1\export\diff\"
./bin/DirStatus/DirStatus.exe merge -p $export -d "D:\temp\dest\"

# 對比結果
./bin/DirStatus/DirStatus.exe equal --left "D:\temp\src\" --right "D:\temp\dest\"
