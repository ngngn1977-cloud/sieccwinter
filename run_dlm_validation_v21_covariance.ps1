<#
.SYNOPSIS
  Phase 3D V2.1 candidate: covariance validation rule corrected after reviewing V2 (post hoc; not prespecified; adoption pending review). Reads the aggregate 3 x 3 cross-basis covariance matrices saved in a V2 run folder (no participant data, no refitting),
  verifies their provenance and computes delta_SE = max |sqrt(lambda) - 1| over the generalized eigenvalues of V_candidate v = lambda V_reference v (the exact worst-case relative standard-error difference over all linear contrasts) for the nine models,
  compared with the existing relative SE tolerances of V1. Earlier statuses are carried forward only as labelled previously reviewed evidence. Writes to a NEW run folder; the V2 run folder is only read.
.EXAMPLE
  .\run_dlm_validation_v21_covariance.ps1 -V2RunDir "C:\path\to\runs\dlmvalidatev2_..." -OutRoot "C:\path\to\private_validation\runs"
.NOTES
  If required aggregate files are missing the run reports exactly which and fails the input-provenance status; it never refits.
  Exit codes: 0 every new status PASS and nothing carried forward open; 3 new statuses PASS, carried-forward REVIEW/NOT_TESTED items remain (expected); 4 a new status FAILED; 1 execution failure; 2 invalid arguments or R missing.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$V2RunDir,
    [string]$OutRoot = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_dlm_v21.ps1')
if (-not (Test-Path -LiteralPath $V2RunDir -PathType Container)) { Write-Host 'FAIL: -V2RunDir was not found.' -ForegroundColor Red; exit 2 }
$v2 = (Resolve-Path -LiteralPath $V2RunDir).Path
if ($OutRoot -ne '') {
    New-Item -ItemType Directory -Path $OutRoot -Force | Out-Null
    $rootFull = (Resolve-Path -LiteralPath $OutRoot).Path
    if ($rootFull.TrimEnd('\') -eq $v2.TrimEnd('\') -or $rootFull.StartsWith($v2.TrimEnd('\') + '\')) { Write-Host 'FAIL: -OutRoot must not be inside the V2 run folder.' -ForegroundColor Red; exit 2 }
    $OutRoot = $rootFull
}
$code = Invoke-SiewDlmV21 -RepoDir $repo -V2RunDir $v2 -OutRoot $OutRoot
exit $code
