<#
.SYNOPSIS
  Runs the core analyses, Batch A and Batch B (Firth logistic sensitivity, ordinal integration, exposure transformations, nonlinear exposure forms, with their
  independent mathematical verification) on the accepted canonical CSV, then compares the core, Batch A and Batch B results with their reference bundles using the
  corrected strict validator. Creates a new run folder; reports EXECUTION, INPUT CHECKS, the three numerical comparisons and the mathematical verification separately.
.EXAMPLE
  .\run_batch_b.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -CoreReferenceDir "C:\path\to\reference_bundle_01" -BatchAReferenceDir "C:\path\to\reference_bundle_batchA_01" -BatchBReferenceDir "C:\path\to\reference_bundle_batchB_01"
.NOTES
  Exit codes: 0 executed and everything passed; 1 execution failure; 2 invalid arguments or R missing; 3 executed, every comparison passed, but at least one documented legacy
  limitation was needed (Firth frozen convergence bound and/or MASS::polr stopping short of the maximum; see RUN_SUMMARY.md); 4 executed but an input check, a comparison or a mathematical verification FAILED.
  -ExpectedAssessmentDays defaults to 33 (the study data). Use -ExpectedAssessmentDays '' only for non-study (e.g. synthetic) data.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$CoreReferenceDir,
    [Parameter(Mandatory = $true)][string]$BatchAReferenceDir,
    [Parameter(Mandatory = $true)][string]$BatchBReferenceDir,
    [string]$ExpectedAssessmentDays = '33',
    [string]$OutRoot = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_batch_b.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $CoreReferenceDir -PathType Container)) { Write-Host 'FAIL: the core reference directory was not found.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $BatchAReferenceDir -PathType Container)) { Write-Host 'FAIL: the Batch A reference directory was not found.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $BatchBReferenceDir -PathType Container)) { Write-Host 'FAIL: the Batch B reference directory was not found.' -ForegroundColor Red; exit 2 }
if ($ExpectedAssessmentDays -ne '' -and $ExpectedAssessmentDays -notmatch '^[0-9]+$') { Write-Host 'FAIL: -ExpectedAssessmentDays must be a whole number or empty.' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$core = (Resolve-Path -LiteralPath $CoreReferenceDir).Path
$batchA = (Resolve-Path -LiteralPath $BatchAReferenceDir).Path
$batchB = (Resolve-Path -LiteralPath $BatchBReferenceDir).Path
$code = Invoke-SiewBatchBRun -RepoDir $repo -DataCsv $csv -CoreReferenceDir $core -BatchAReferenceDir $batchA -BatchBReferenceDir $batchB -ExpectedAssessmentDays $ExpectedAssessmentDays -OutRoot $OutRoot
exit $code
