[CmdletBinding(PositionalBinding = $false)]
param(
    [Alias('c')]
    [ValidateSet('Debug', 'Release')]
    [string]$Configuration = 'Release',

    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$Rest
)

dotnet publish .\DirStatus\ -o .\bin\DirStatus\ -c $Configuration --no-restore @Rest
