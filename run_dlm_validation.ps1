<#
.SYNOPSIS
  Phase 3D: validation of the nine final L01-L56 M2-L distributed-lag outputs (lag-specific estimates and SEs, cumulative curves, pointwise and simultaneous bands, simultaneous critical values, descriptive peak lags).
  Reproduces the accepted fits, compares them with the audited L56 reconstruction (strict validator), verifies them with an independent route, separates exact seeded reproduction from Monte Carlo stability and reports NOT_TESTED
  for what has no reference (Figure 6 provenance). Creates a NEW private run folder. Accepted runs and reference bundles are only read; the archived L59 analyses are not touched; no figure is redrawn.
.EXAMPLE
  .\run_dlm_validation.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -DlmReferenceDir "C:\path\to\reference_bundle_dlm_01" -AcceptedRunDir "C:\path\to\runs\paper_..." -OutRoot "C:\path\to\private_validation\runs"
.NOTES
  Statuses (separate): EXECUTION, INPUT CHECKS, ACCEPTED FIT REPRODUCTION, REFERENCE COMPARISON, LAG BASIS, CURVES, CUMULATIVE, PEAK LAG, SIMULTANEOUS EXACT SEEDED, SIMULTANEOUS MC STABILITY, CONVERGENCE SENSITIVITY,
  MANUSCRIPT CONCORDANCE, FIGURE PROVENANCE (always NOT_TESTED), DLNM EQUALITY (NOT_TESTED if dlnm is not installed), SOURCE INTEGRITY. NOT_TESTED is never PASS.
  Exit codes: 0 all PASS; 3 all tested statuses PASS, some NOT_TESTED; 1 execution failure; 2 invalid arguments or R missing; 4 a status FAILED or needs REVIEW.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$DlmReferenceDir,
    [string]$AcceptedRunDir = '',
    [string]$OutRoot = '',
    [switch]$NoMonteCarlo,
    [switch]$SyntheticNoManuscriptConcordance   # synthetic dry runs only: skips the manuscript comparison (status NOT_TESTED, never PASS); never use for the study run
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_dlm_validation.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $DlmReferenceDir -PathType Container)) { Write-Host 'FAIL: the DLM reference directory was not found.' -ForegroundColor Red; exit 2 }
foreach ($f in @('dlmL56_audited_estimates.csv', 'dlmL56_audited_discrete.csv', 'dlmL56_audited_critical_10000.csv')) { if (-not (Test-Path -LiteralPath (Join-Path $DlmReferenceDir $f) -PathType Leaf)) { Write-Host ("FAIL: {0} not found in the DLM reference directory." -f $f) -ForegroundColor Red; exit 2 } }
if ($AcceptedRunDir -ne '') { foreach ($f in @('dlm_final_summary.csv', 'dlm_final_lag_curves.csv', 'dlm_final_cumulative_curves.csv')) { if (-not (Test-Path -LiteralPath (Join-Path $AcceptedRunDir $f) -PathType Leaf)) { Write-Host ("FAIL: -AcceptedRunDir must contain {0}." -f $f) -ForegroundColor Red; exit 2 } } }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$ref = (Resolve-Path -LiteralPath $DlmReferenceDir).Path
$acc = ''; if ($AcceptedRunDir -ne '') { $acc = (Resolve-Path -LiteralPath $AcceptedRunDir).Path }
$code = Invoke-SiewDlmValidation -RepoDir $repo -DataCsv $csv -DlmReferenceDir $ref -AcceptedRunDir $acc -OutRoot $OutRoot -NoMonteCarlo:$NoMonteCarlo -SyntheticNoManuscriptConcordance:$SyntheticNoManuscriptConcordance
exit $code
