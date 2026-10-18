<#
.SYNOPSIS
  Phase 3D validation protocol V2 (candidate; a revision introduced AFTER the orientation investigation). Runs the existing V1 validation and the existing simulation diagnostic unchanged inside one new private run folder and re-expresses their evidence as
  separate checks: production reproduction, deterministic agreement, orientation-aligned simulation agreement, native-orientation Monte Carlo agreement, and the reconstructed reference split into deterministic and simulated parts. Saves aggregate
  covariance matrices, eigen orientation and RNG metadata; keeps exact differences visible; computes the V1 criteria for the record. V1 protocol, reports and recorded FAIL statuses are preserved. No tolerance or Monte Carlo criterion is changed.
.EXAMPLE
  .\run_dlm_validation_v2.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -DlmReferenceDir "C:\path\to\reference_bundle_dlm_01" -AcceptedRunDir "C:\path\to\runs\paper_..." -OutRoot "C:\path\to\private_validation\runs"
.NOTES
  Exit codes: 0 every status PASS; 3 every required status PASS while MANUSCRIPT_CONCORDANCE is REVIEW or figure/dlnm statuses are NOT_TESTED (documented); 1 execution failure; 2 invalid arguments or R missing; 4 a required status FAILED.
  -SyntheticNoManuscriptConcordance is for synthetic dry runs only (manuscript status NOT_TESTED, never PASS); never use it for the study run.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$DlmReferenceDir,
    [string]$AcceptedRunDir = '',
    [string]$OutRoot = '',
    [switch]$NoMonteCarlo,
    [switch]$SyntheticNoManuscriptConcordance
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_dlm_validation_v2.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $DlmReferenceDir -PathType Container)) { Write-Host 'FAIL: the DLM reference directory was not found.' -ForegroundColor Red; exit 2 }
foreach ($f in @('dlmL56_audited_estimates.csv', 'dlmL56_audited_discrete.csv', 'dlmL56_audited_critical_10000.csv')) { if (-not (Test-Path -LiteralPath (Join-Path $DlmReferenceDir $f) -PathType Leaf)) { Write-Host ("FAIL: {0} not found in the DLM reference directory." -f $f) -ForegroundColor Red; exit 2 } }
if ($AcceptedRunDir -ne '') { foreach ($f in @('dlm_final_summary.csv', 'dlm_final_lag_curves.csv', 'dlm_final_cumulative_curves.csv')) { if (-not (Test-Path -LiteralPath (Join-Path $AcceptedRunDir $f) -PathType Leaf)) { Write-Host ("FAIL: -AcceptedRunDir must contain {0}." -f $f) -ForegroundColor Red; exit 2 } } }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$ref = (Resolve-Path -LiteralPath $DlmReferenceDir).Path
$acc = ''; if ($AcceptedRunDir -ne '') { $acc = (Resolve-Path -LiteralPath $AcceptedRunDir).Path }
$code = Invoke-SiewDlmValidationV2 -RepoDir $repo -DataCsv $csv -DlmReferenceDir $ref -AcceptedRunDir $acc -OutRoot $OutRoot -NoMonteCarlo:$NoMonteCarlo -SyntheticNoManuscriptConcordance:$SyntheticNoManuscriptConcordance
exit $code
