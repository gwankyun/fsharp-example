Write-Output $PWD

$base = Join-Path $PWD "compare"

if (Test-Path $base -PathType Container) {
    Remove-Item $base -Recurse
}

New-Item $base -ItemType Directory

$source = Join-Path $base "source"
$copy = Join-Path $base "copy"

New-Item $source -ItemType Directory
New-Item $copy -ItemType Directory

dotnet run --project ./dir-compare --no-restore --inittest $source

Copy-Item -Path $source -Destination $copy -Recurse

dotnet run --project ./dir-compare --no-restore --init $source

dotnet run --project ./dir-compare --no-restore --add $source "1"
dotnet run --project ./dir-compare --no-restore --add $source "2"
dotnet run --project ./dir-compare --no-restore --diff $source "2" "1"
# dotnet run --project ./dir-compare --no-restore --merge $copy (Join-Path $)
