<#
.SYNOPSIS
  Ordinal refinement step (Phase 3B.2): refits the 80 ordinal M2-L specifications with one documented, scaled and tightly controlled MASS::polr protocol, checks every model
  against pre-declared numerical acceptance criteria (independent maximum-likelihood fit), and compares the refined results with the preserved legacy polr table.
  Creates a NEW private run folder; never writes to an accepted run folder or reference bundle. Core, Batch A and Batch B are not rerun.
.EXAMPLE
  .\run_ordinal_refinement.ps1 -DataCsv "C:\path\to\siecc_winter_canonical.csv" -BatchBReferenceDir "C:\path\to\reference_bundle_batchB_01" -LegacyRunDir "C:\path\to\runs\batchb_..."
.NOTES
  Statuses (separate): EXECUTION, INPUT CHECKS, LEGACY REPRODUCTION, LEGACY NUMERICAL VERIFICATION (informational), REFINED NUMERICAL VERIFICATION.
  Exit codes: 0 executed, legacy reproduction and refined verification passed; 1 execution failure; 2 invalid arguments or R missing; 4 executed, but legacy reproduction or refined verification FAILED.
  -LegacyRunDir is optional and only read (the accepted Batch B run whose ordinal_integration.csv must be reproduced exactly).
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [Parameter(Mandatory = $true)][string]$BatchBReferenceDir,
    [string]$LegacyRunDir = '',
    [string]$OutRoot = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_ordinal_refinement.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $BatchBReferenceDir -PathType Container)) { Write-Host 'FAIL: the Batch B reference directory was not found.' -ForegroundColor Red; exit 2 }
if ($LegacyRunDir -ne '' -and -not (Test-Path -LiteralPath (Join-Path $LegacyRunDir 'ordinal_integration.csv') -PathType Leaf)) { Write-Host 'FAIL: -LegacyRunDir must contain ordinal_integration.csv.' -ForegroundColor Red; exit 2 }
$csv = (Resolve-Path -LiteralPath $DataCsv).Path
$ref = (Resolve-Path -LiteralPath $BatchBReferenceDir).Path
$legacy = ''
if ($LegacyRunDir -ne '') { $legacy = (Resolve-Path -LiteralPath $LegacyRunDir).Path }
$code = Invoke-SiewOrdinalRefinement -RepoDir $repo -DataCsv $csv -BatchBReferenceDir $ref -LegacyRunDir $legacy -OutRoot $OutRoot
exit $code
