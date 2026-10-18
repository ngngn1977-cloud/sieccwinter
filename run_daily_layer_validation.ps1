<#
.SYNOPSIS
  Phase 3H / Batch D: the documented archived daily-lag layer (horizon L01-L59): crude daily-lag associations (XYD, 1,416), adjusted single-day regressions (DAY, 1,416) and the outcome-wide max-|r| permutation test (MAXT, 240).
  Numerical reproduction against the frozen V1.4 output, independent verification (including the optimised MAXT engine against the original route on all 3 x 10,000 replicates) and a SEPARATE assessment of the permutation assumptions (REVIEW).
  Creates a NEW private run folder; the data file and the reference folder are only read; no result is altered to fit a reference. Separate from the nine final L01-L56 models.
.EXAMPLE
  .\run_daily_layer_validation.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -ReferenceDir "C:\path\to\reference_bundle_dailylag_01" -OutRoot "C:\path\to\private_validation\runs"
.NOTES
  The reference folder must contain XY_daily_all_1416.csv, single_day_adjusted_regression_grid.csv and selected_xy_maxT.csv (SHA-256 declared in config/daily_layer_validation_protocol.json); the bundle reference_bundle_dailylag_01 holds hash-verified byte copies.
  A missing or non-matching file fails the input-provenance status and nothing is compared.
  Exit codes: 0 all PASS; 3 all reproduction/verification statuses PASS with the MAXT assumptions under REVIEW (expected); 1 execution failure; 2 invalid arguments or R missing; 4 a status FAILED or evidence is missing.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$ReferenceDir,
    [string]$OutRoot = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_daily_layer.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $ReferenceDir -PathType Container)) { Write-Host 'FAIL: the reference directory was not found.' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$ref = (Resolve-Path -LiteralPath $ReferenceDir).Path
if ($OutRoot -ne '') { $outFull = [System.IO.Path]::GetFullPath($OutRoot); if ($outFull.TrimEnd('\').StartsWith($ref.TrimEnd('\') + '\', [System.StringComparison]::OrdinalIgnoreCase)) { Write-Host 'FAIL: -OutRoot must not be inside the reference directory.' -ForegroundColor Red; exit 2 } }
$code = Invoke-SiewDailyLayer -RepoDir $repo -DataCsv $csv -ReferenceDir $ref -OutRoot $OutRoot
exit $code
