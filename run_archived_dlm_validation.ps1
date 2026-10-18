<#
.SYNOPSIS
  Phase 3E: validation of the archived L01-L59 DLM grid (168 models, seven tiers; q24, q9) and the archived lag-form sensitivity (216 models; q216) against the frozen V1.4 outputs.
  Deterministic quantities only (sample sizes, coefficients, SEs, intervals, global tests, multiplicity adjustments, lag-specific and cumulative curves); the lag bases of the nine (L, df) combinations are verified.
  Simultaneous critical values and bands are NOT compared (Monte Carlo quantities; separate validation required). Separate from the nine final L01-L56 models; the V2.1 acceptance is not extended.
  Creates a NEW private run folder. The data file and the reference folder are only read; no result is altered to fit a reference.
.EXAMPLE
  .\run_archived_dlm_validation.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -ReferenceDir "C:\path\to\WINTER_SOURCE\outputs\SIECC_FULL_20260801T125700\models" -OutRoot "C:\path\to\private_validation\runs"
.NOTES
  The reference folder must contain the five frozen files named in config/archived_dlm_validation_protocol.json (their SHA-256 are declared there): fixed_DLM_summary.csv, DLM_lag_length_df_sensitivity_summary.csv,
  fixed_DLM_lag_curves.csv, fixed_DLM_cumulative_curves.csv, DLM_lag_length_df_sensitivity_curves.csv. A missing or non-matching file fails the input-provenance status and nothing is compared.
  Exit codes: 0 all PASS; 3 all compared statuses PASS with the separate-validation items open (expected); 1 execution failure; 2 invalid arguments or R missing; 4 a status FAILED or evidence is missing.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$ReferenceDir,
    [string]$OutRoot = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_archived_dlm.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $ReferenceDir -PathType Container)) { Write-Host 'FAIL: the reference directory was not found.' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$ref = (Resolve-Path -LiteralPath $ReferenceDir).Path
if ($OutRoot -ne '') { $outFull = [System.IO.Path]::GetFullPath($OutRoot); if ($outFull.TrimEnd('\').StartsWith($ref.TrimEnd('\') + '\', [System.StringComparison]::OrdinalIgnoreCase)) { Write-Host 'FAIL: -OutRoot must not be inside the reference directory.' -ForegroundColor Red; exit 2 } }
$code = Invoke-SiewArchivedDlm -RepoDir $repo -DataCsv $csv -ReferenceDir $ref -OutRoot $OutRoot
exit $code
