param(
    [Parameter(Mandatory=$true, HelpMessage="Path to the vs_out.txt file")]
    [string]$Path,
    
    [Parameter(Mandatory=$true, HelpMessage="Target base directory for downloads")]
    [string]$Target
)

# 读取vs_out.txt文件的JSON内容
$jsonContent = Get-Content -Path $Path -Raw

# 解析JSON
$vsixItems = $jsonContent | ConvertFrom-Json

# 遍历每个项目
foreach ($item in $vsixItems) {
    # 提取Origin和Url
    $origin = $item.Origin
    $url = $item.Url
    
    # 构建完整的Origin目录路径
    $fullOriginPath = Join-Path -Path $Target -ChildPath $origin
    
    # 确保Origin目录存在
    if (-not (Test-Path -Path $fullOriginPath -PathType Container)) {
        Write-Host "Creating directory: $fullOriginPath"
        New-Item -Path $fullOriginPath -ItemType Directory -Force | Out-Null
    }
    
    # 提取文件名
    $fileName = [System.IO.Path]::GetFileName($url)
    $destinationPath = Join-Path -Path $fullOriginPath -ChildPath $fileName
    
    # 下载文件
    Write-Host "Downloading $fileName to $destinationPath"
    try {
        Invoke-WebRequest -Uri $url -OutFile $destinationPath -ErrorAction Stop
        Write-Host "Download completed successfully"
    } catch {
        Write-Host "Error downloading file: $($_.Exception.Message)" -ForegroundColor Red
    }
    
    # 添加分隔线
    Write-Host "------------------------"
}

Write-Host "All downloads completed!"
