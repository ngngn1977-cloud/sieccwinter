<#
.SYNOPSIS
  Batch C: primary cumulative bootstrap reproduction and verification. 180 target-scheme combinations (90 primary M2-L cumulative models x participant and assessment-date-cluster
  bootstrap), 2,000 resamples each, with independent verification, a separate descriptive scientific-validity assessment, a strict comparison with the Batch C reference bundle and a
  package-source fingerprint. Creates a NEW private run folder. Core, Batch A, Batch B and the ordinal step are not rerun; accepted runs and references are only read.
.EXAMPLE
  .\run_batch_c.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -BatchCReferenceDir "C:\path\to\reference_bundle_batchC_01" -AcceptedRunDir "C:\path\to\runs\batchb_..."
  Resume after an interruption (same run folder; completed targets are reused only if input, configuration, protocol and source are unchanged):
  .\run_batch_c.ps1 -DataCsv ... -BatchCReferenceDir ... -ResumeRunDir "C:\path\to\runs\batchc_..."
.NOTES
  Statuses (separate): EXECUTION, INPUT CHECKS, NUMERICAL REPRODUCTION (vs the frozen references), MATHEMATICAL VERIFICATION, SCIENTIFIC VALIDITY (descriptive), SOURCE INTEGRITY.
  Exit codes: 0 executed and verified, numerical reproduction passed; 1 execution failure; 2 invalid arguments or R missing; 4 executed, but numerical reproduction, mathematical verification, an input check or source integrity FAILED.
  Scientific-validity REVIEW flags never change the exit code or any result; they are for the review.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$BatchCReferenceDir,
    [string]$AcceptedRunDir = '',
    [string]$ResumeRunDir = '',
    [string]$OutRoot = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_batch_c.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $BatchCReferenceDir -PathType Container)) { Write-Host 'FAIL: the Batch C reference directory was not found.' -ForegroundColor Red; exit 2 }
if ($AcceptedRunDir -ne '' -and -not (Test-Path -LiteralPath (Join-Path $AcceptedRunDir 'cumulative_results.csv') -PathType Leaf)) { Write-Host 'FAIL: -AcceptedRunDir must contain cumulative_results.csv.' -ForegroundColor Red; exit 2 }
if ($ResumeRunDir -ne '' -and -not (Test-Path -LiteralPath (Join-Path $ResumeRunDir 'checkpoints') -PathType Container)) { Write-Host 'FAIL: -ResumeRunDir must be an earlier Batch C run folder containing a checkpoints folder.' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$ref = (Resolve-Path -LiteralPath $BatchCReferenceDir).Path
$acc = ''; if ($AcceptedRunDir -ne '') { $acc = (Resolve-Path -LiteralPath $AcceptedRunDir).Path }
$res = ''; if ($ResumeRunDir -ne '') { $res = (Resolve-Path -LiteralPath $ResumeRunDir).Path }
$code = Invoke-SiewBatchCRun -RepoDir $repo -DataCsv $csv -BatchCReferenceDir $ref -AcceptedRunDir $acc -ResumeRunDir $res -OutRoot $OutRoot
exit $code
