<#
.SYNOPSIS
  Runs the paper workflow on a user-supplied CSV and compares the aggregate results with a folder of frozen reference outputs.
  Every discrepancy is reported; nothing is changed to fit the reference.
.EXAMPLE
  .\run_validate.ps1 -DataCsv "C:\path\to\study_data.csv" -ReferenceDir "C:\path\to\reference_outputs"
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$ReferenceDir,
    [string]$OutRoot = ''             # optional parent folder for the run folder (default: <repository>\runs)
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_common.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv (convert SPSS files privately first).' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $ReferenceDir -PathType Container)) { Write-Host 'FAIL: the reference directory was not found.' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$ref = (Resolve-Path -LiteralPath $ReferenceDir).Path
$code = Invoke-SiewRun -RepoDir $repo -Mode 'validate' -ModeArgs @($csv, $ref, '--no-resume') -OutRoot $OutRoot
exit $code
