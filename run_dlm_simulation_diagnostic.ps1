<#
.SYNOPSIS
  Phase 3D.3: DIAGNOSTIC of the simultaneous critical-value discrepancies of the nine final L01-L56 DLMs. With identical standard-normal draws it compares the production covariance factor (eigen decomposition as MASS::mvrnorm),
  an eigen factor with explicitly aligned signs and the symmetric square root (diagnostic only), for the production route and the independent route; traces covariances, seeds and decompositions; checks that every factor reconstructs its
  covariance; examines eigenvalue ordering, near-repeated eigenvalues and eigenvector signs; tests whether orientation explains the discrepancies; clarifies the percentage metrics and the "5_OF_9 exact" status line.
  Creates a NEW private run folder. Production fits, epsilon, simulation methods, seeds, draw counts, references, criteria and earlier runs are not changed; the study CSV, the validation run and the reference bundle are only read.
.EXAMPLE
  .\run_dlm_simulation_diagnostic.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -ValidationRunDir "C:\path\to\runs\dlmvalidate_..." -DlmReferenceDir "C:\path\to\reference_bundle_dlm_01" -OutRoot "C:\path\to\runs"
.NOTES
  Exit codes: 0 all self-checks PASS and the mechanism explained; 3 self-checks PASS, some status PARTIAL or NOT_TESTED; 1 execution failure; 2 invalid arguments or R missing; 4 a self-check FAILED.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$ValidationRunDir,
    [string]$DlmReferenceDir = '',
    [string]$OutRoot = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_dlm_simdiag.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
foreach ($f in @('dlm_critical_values.csv', 'dlm_verification_checks.csv')) { if (-not (Test-Path -LiteralPath (Join-Path $ValidationRunDir $f) -PathType Leaf)) { Write-Host ("FAIL: -ValidationRunDir must be a dlmvalidate_* folder containing {0}." -f $f) -ForegroundColor Red; exit 2 } }
if ($DlmReferenceDir -ne '' -and -not (Test-Path -LiteralPath (Join-Path $DlmReferenceDir 'dlmL56_audited_critical_10000.csv') -PathType Leaf)) { Write-Host 'FAIL: -DlmReferenceDir must contain dlmL56_audited_critical_10000.csv.' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$val = (Resolve-Path -LiteralPath $ValidationRunDir).Path
$ref = ''; if ($DlmReferenceDir -ne '') { $ref = (Resolve-Path -LiteralPath $DlmReferenceDir).Path }
$code = Invoke-SiewDlmSimDiag -RepoDir $repo -DataCsv $csv -ValidationRunDir $val -DlmReferenceDir $ref -OutRoot $OutRoot
exit $code
