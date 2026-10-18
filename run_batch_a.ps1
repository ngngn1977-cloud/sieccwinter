<#
.SYNOPSIS
  Runs the core analyses and Batch A (assessment-day validation, influence diagnostics, 240 bivariate tests, descriptive analyses) on the accepted
  canonical CSV, then compares the core and the Batch A results with their reference bundles using the corrected strict validator.
  Creates a new run folder; reports EXECUTION, INPUT CHECKS, CORE comparison and BATCH A comparison separately.
.EXAMPLE
  .\run_batch_a.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -CoreReferenceDir "C:\path\to\reference_bundle_01" -BatchAReferenceDir "C:\path\to\reference_bundle_batchA_01"
.NOTES
  Exit codes: 0 executed and all comparisons passed; 1 execution failure; 2 invalid arguments or R missing; 4 executed but an input check or a comparison FAILED.
  -ExpectedAssessmentDays defaults to 33 (the study data). Use -ExpectedAssessmentDays '' only for non-study (e.g. synthetic) data.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$CoreReferenceDir,
    [Parameter(Mandatory = $true)][string]$BatchAReferenceDir,
    [string]$ExpectedAssessmentDays = '33',
    [string]$OutRoot = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_batch_a.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $CoreReferenceDir -PathType Container)) { Write-Host 'FAIL: the core reference directory was not found.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $BatchAReferenceDir -PathType Container)) { Write-Host 'FAIL: the Batch A reference directory was not found.' -ForegroundColor Red; exit 2 }
if ($ExpectedAssessmentDays -ne '' -and $ExpectedAssessmentDays -notmatch '^[0-9]+$') { Write-Host 'FAIL: -ExpectedAssessmentDays must be a whole number or empty.' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$core = (Resolve-Path -LiteralPath $CoreReferenceDir).Path
$batch = (Resolve-Path -LiteralPath $BatchAReferenceDir).Path
$code = Invoke-SiewBatchARun -RepoDir $repo -DataCsv $csv -CoreReferenceDir $core -BatchAReferenceDir $batch -ExpectedAssessmentDays $ExpectedAssessmentDays -OutRoot $OutRoot
exit $code
