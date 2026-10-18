<#
.SYNOPSIS
  Runs the paper workflow (cumulative grid + final L01-L56 distributed-lag models) on a user-supplied canonical study CSV.
  The confidential data location is never stored in this repository. SPSS (.sav) files are not accepted.
.EXAMPLE
  .\run_paper.ps1 -DataCsv "C:\path\to\study_data.csv"
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [string]$OutRoot = '',            # optional parent folder for the run folder (default: <repository>\runs)
    [switch]$Archived,                # heavy archived L01-L59 branch (legacy comparison only)
    [switch]$RainfallSensitivity      # optional named rainfall zero-reconstruction sensitivity
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_common.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv (convert SPSS files privately first).' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$extra = @($csv, '--no-resume')
if ($Archived) { $extra += '--archived' }
if ($RainfallSensitivity) { $extra += '--rainfall-sensitivity' }
$code = Invoke-SiewRun -RepoDir $repo -Mode 'paper' -ModeArgs $extra -OutRoot $OutRoot
exit $code
