<#
.SYNOPSIS
  Diagnostic-only audit of the saved Batch C bootstrap estimates. Reads only the target checkpoints and aggregate tables of a finished Batch C run folder (no participant data, no CSV, no refitting)
  and writes aggregate outputs to a NEW private run folder. The audited run folder is never modified.
.EXAMPLE
  .\run_bootstrap_diagnostics.ps1 -BatchCRunDir "C:\path\to\private_validation\runs\batchc_..." -OutRoot "C:\path\to\private_validation\runs"
.NOTES
  Statuses (separate): EXECUTION, CHECKPOINT INTEGRITY, CHECKPOINTS UNCHANGED, ACCOUNTING, QUANTILE RECONSTRUCTION, ORIGINAL RUN PROVENANCE, SOURCE INTEGRITY.
  Extremeness diagnostics are descriptive (not separation, not pass/fail) and never change a result, flag, interval or criterion.
  Exit codes: 0 executed, all checks passed; 1 execution failure; 2 invalid arguments or R missing; 4 executed, but a check failed.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$BatchCRunDir,
    [string]$OutRoot = ''
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_bootstrap_diagnostics.ps1')
if (-not (Test-Path -LiteralPath (Join-Path $BatchCRunDir 'checkpoints') -PathType Container)) { Write-Host 'FAIL: -BatchCRunDir must be a Batch C run folder containing a checkpoints folder.' -ForegroundColor Red; exit 2 }
foreach ($f in @('bootstrap_summary.csv', 'bootstrap_validity.csv', 'batch_c_metadata.json')) { if (-not (Test-Path -LiteralPath (Join-Path $BatchCRunDir $f) -PathType Leaf)) { Write-Host ("FAIL: {0} not found in the Batch C run folder." -f $f) -ForegroundColor Red; exit 2 } }
$runDir = (Resolve-Path -LiteralPath $BatchCRunDir).Path
if ($OutRoot -ne '') {
    New-Item -ItemType Directory -Path $OutRoot -Force | Out-Null
    $rootFull = (Resolve-Path -LiteralPath $OutRoot).Path
    if ($rootFull.TrimEnd('\') -eq $runDir.TrimEnd('\') -or $rootFull.StartsWith($runDir.TrimEnd('\') + '\')) { Write-Host 'FAIL: -OutRoot must not be inside the audited run folder.' -ForegroundColor Red; exit 2 }
    $OutRoot = $rootFull
}
$code = Invoke-SiewBootstrapAudit -RepoDir $repo -BatchCRunDir $runDir -OutRoot $OutRoot
exit $code
