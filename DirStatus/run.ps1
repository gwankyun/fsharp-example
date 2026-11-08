param(
    [switch]$NoBuild,
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$Rest
)

$ErrorActionPreference = 'Stop'
$commonArgs = @('--no-restore', '-v', 'q', '--project', "$PSScriptRoot\DirStatus.fsproj")
if ($NoBuild) {
    dotnet run --no-build @commonArgs -- @Rest
} else {
    dotnet run @commonArgs -- @Rest
}
exit $LASTEXITCODE
