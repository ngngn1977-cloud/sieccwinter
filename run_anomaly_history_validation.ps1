<#
.SYNOPSIS
  Phase 3I: the two exploratory families of the archived V1.4 programme listed in the deposited record as ANOM (7,080 daily-reference anomaly models, windows 1-59 days) and ALT (312 alternative exposure-history tests, histories L01-L56).
  Numerical reproduction against the frozen V1.4 output and independent mathematical verification. Creates a NEW private run folder; the data file and the reference folder are only read; no result is altered to fit a reference.
  Exploratory and separate from primary inference, the nine final L01-L56 models and every earlier family. No scientific interpretation is made.
.EXAMPLE
  .\run_anomaly_history_validation.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -ReferenceDir "C:\path\to\reference_bundle_anomhist_01" -OutRoot "C:\path\to\private_validation\runs"
.NOTES
  The reference folder must contain daily_anomaly_results.csv, alternative_method_results.csv, daily_anomaly_diagnostics.csv and alternative_PCA_variance.csv (SHA-256 declared in config/anomaly_history_validation_protocol.json); the bundle reference_bundle_anomhist_01 holds hash-verified byte copies.
  A missing or non-matching file fails the input-provenance status and nothing is compared.
  Exit codes: 0 all statuses PASS; 4 a status FAILED or evidence is missing; 1 execution failure or inconsistent outcome; 2 invalid arguments or R missing.
  The R and wrapper outcomes are saved in HANDOFF_OUTCOME.txt in the run folder (written after RUN_MANIFEST.csv and deliberately not listed in it).
  -ProtocolFile is for the synthetic tests only.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$ReferenceDir,
    [string]$OutRoot = '',
    [string]$ProtocolFile = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_anomaly_history.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $ReferenceDir -PathType Container)) { Write-Host 'FAIL: the reference directory was not found.' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$ref = (Resolve-Path -LiteralPath $ReferenceDir).Path
if ($OutRoot -ne '') { $outFull = [System.IO.Path]::GetFullPath($OutRoot); if ($outFull.TrimEnd('\').StartsWith($ref.TrimEnd('\') + '\', [System.StringComparison]::OrdinalIgnoreCase)) { Write-Host 'FAIL: -OutRoot must not be inside the reference directory.' -ForegroundColor Red; exit 2 } }
$pf = ''
if ($ProtocolFile -ne '') { if (-not (Test-Path -LiteralPath $ProtocolFile -PathType Leaf)) { Write-Host 'FAIL: the protocol file was not found.' -ForegroundColor Red; exit 2 }; $pf = (Resolve-Path -LiteralPath $ProtocolFile).Path }
$code = Invoke-SiewAnomalyHistory -RepoDir $repo -DataCsv $csv -ReferenceDir $ref -OutRoot $OutRoot -ProtocolFile $pf
exit $code
