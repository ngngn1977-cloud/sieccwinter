<#
.SYNOPSIS
  Phase 3F: exploratory cold-burden scan (22,302 models): numerical reproduction against the frozen V1.4 output and mathematical verification (burden features, thresholds, coefficient scales, inference, multiplicity, independent refit sample).
  The scientific interpretation of the scan is NOT assessed and no model is promoted. Creates a NEW private run folder; the data file and the reference folder are only read; no result is altered to fit a reference.
.EXAMPLE
  .\run_cold_burden_validation.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -ReferenceDir "C:\path\to\reference_bundle_coldburden_01" -OutRoot "C:\path\to\private_validation\runs"
.NOTES
  The reference folder must contain cold_burden_results.csv and cold_thresholds.csv (SHA-256 declared in config/cold_burden_validation_protocol.json); the bundle reference_bundle_coldburden_01 holds hash-verified byte copies of the frozen V1.4 files.
  A missing or non-matching file fails the input-provenance status and nothing is compared.
  Exit codes: 0 all PASS; 3 all PASS with the scientific interpretation not assessed (expected); 1 execution failure; 2 invalid arguments or R missing; 4 a status FAILED or evidence is missing.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$ReferenceDir,
    [string]$OutRoot = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_cold_burden.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $ReferenceDir -PathType Container)) { Write-Host 'FAIL: the reference directory was not found.' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$ref = (Resolve-Path -LiteralPath $ReferenceDir).Path
if ($OutRoot -ne '') { $outFull = [System.IO.Path]::GetFullPath($OutRoot); if ($outFull.TrimEnd('\').StartsWith($ref.TrimEnd('\') + '\', [System.StringComparison]::OrdinalIgnoreCase)) { Write-Host 'FAIL: -OutRoot must not be inside the reference directory.' -ForegroundColor Red; exit 2 } }
$code = Invoke-SiewColdBurden -RepoDir $repo -DataCsv $csv -ReferenceDir $ref -OutRoot $OutRoot
exit $code
