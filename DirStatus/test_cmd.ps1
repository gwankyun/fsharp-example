param(
    [string]$Source = "D:\test\tutorial",
    [string]$WorkDir = "D:\test\test_cmd",
    [string]$Name = "test-cmd",
    [string]$Exclude = "Step15"
)

$ErrorActionPreference = 'Stop'

# publish.ps1 另行执行，这里只调用发布产物
$Exe = Join-Path (Split-Path $PSScriptRoot -Parent) "bin\DirStatus\DirStatus.exe"
if (-not (Test-Path $Exe)) { throw "找不到 $Exe，请先运行 publish.ps1" }

$dir1 = Join-Path $WorkDir "dir1"
$dir2 = Join-Path $WorkDir "dir2"
$bundle = Join-Path $WorkDir "bundle"
$cfgDir = Join-Path $env:USERPROFILE "DirStatus\$Name"

function Run([string[]]$ArgList) {
    & $Exe @ArgList
    if ($LASTEXITCODE -ne 0) { throw "DirStatus $($ArgList -join ' ') 失败 (exit $LASTEXITCODE)" }
}

# 清空相關目錄
Remove-Item -Recurse -Force -ErrorAction SilentlyContinue $dir1, $dir2, $bundle, $cfgDir
New-Item -ItemType Directory -Force -Path $dir1, $dir2, $bundle | Out-Null

# 複製dir1
Copy-Item -Recurse -Force "$Source\*" "$dir1\"

# 複製dir2
Copy-Item -Recurse -Force "$dir1\*" "$dir2\"

# 建配置 + 更新dir1前的快照（同 xtest，排除 Step15）
Run @('yinit', '-n', $Name, '-p', $dir1, '--excludepath', $Exclude)
Run @('yadd', '-n', $Name, '--version', '1')

# 更新dir1
Run @('yupdate', '--path', $dir1)
Run @('yadd', '-n', $Name, '--version', '2')

# 對比dir1和dir2，生成變更包（左=舊、右=新）
Run @('ydiff', '-n', $Name, '--left', '1', '--right', '2', '-d', $bundle)

# dir2合併變更包
Run @('ymerge', '--diff', $bundle, '-d', $dir2)

# 過濾驗證：$Exclude 不在快照裡，dir2 不應有它
if (Test-Path (Join-Path $dir2 $Exclude)) { throw "$Exclude 未被过滤，出现在 dir2 中" }

# 對比dir1和dir2（$Exclude 不參與比對）
$out = & $Exe yequal --left $dir1 --right $dir2 --excludepath $Exclude
$out
if ("$out" -match 'result:\s*True') {
    Write-Host "PASS"
} else {
    throw "yequal 结果不为 True"
}
