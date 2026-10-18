<#
.SYNOPSIS
  One entry point for the controlled manual run. Runs, each in a fresh PowerShell/R process and in this order: environment and dependency check, input SHA-256 check,
  the paper analysis, every strict validation against its frozen reference bundle (core, Batch A, Batch B, refined ordinal, Batch C, DLM V2 and V2.1, archived DLM,
  cold burden, daily layer, ANOM/ALT), the ANOM sample-SD diagnostic, the coded model record v2 (a new file; the legacy record is never touched),
  then a summary that lists every status verbatim. It reuses the existing run_*.ps1 wrappers
  unchanged and alters no estimator, tolerance, protocol or result.
.EXAMPLE
  Study run (single line; the paths file lists the private folders, see manual_run_paths.template.json):
  powershell -NoProfile -ExecutionPolicy Bypass -File .\run_all.ps1 -PathsFile "..\private_validation\manual_run_paths.json"
.EXAMPLE
  Synthetic demo (shipped synthetic CSV; no private data):
  powershell -NoProfile -ExecutionPolicy Bypass -File .\run_all.ps1 -Demo
.EXAMPLE
  Resume an interrupted run (completed steps are reused after their manifests are checked; failed executions are retried):
  powershell -NoProfile -ExecutionPolicy Bypass -File .\run_all.ps1 -PathsFile "..\private_validation\manual_run_paths.json" -ResumeRunDir "..\private_validation\runs\fullrun_..."
.NOTES
  Exit codes: 0 every step PASS (demo: every executed step PASS; the study-reference steps are NOT_TESTED by design and listed);
  3 no FAIL, but REVIEW, NOT_TESTED or DIAGNOSTIC present; 4 at least one FAIL (ANOM_ALT is governed by ANOM_VERIFICATION_STATUS_V2 since protocol v2, 12 Oct 2026; the v1 status is shown for the record);
  1 a step did not execute or the run stopped (no FULL_RUN_MANIFEST.csv; resume possible); 2 refused before any analysis (arguments, R, environment, input hash, disk).
  Only PASS counts as PASS. FULL_RUN_MANIFEST.csv is written last.
#>
[CmdletBinding()]
param(
    [string]$PathsFile = '',
    [switch]$Demo,
    [string]$OutRoot = '',
    [string]$ResumeRunDir = '',
    [switch]$RainfallSensitivity,
    [double]$MinFreeGB = 2
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_full.ps1')
if ($Demo -and $PathsFile -ne '') { Write-Host 'FAIL: use either -Demo or -PathsFile, not both.' -ForegroundColor Red; exit 2 }
if (-not $Demo -and $PathsFile -eq '') { Write-Host 'FAIL: -PathsFile is required for the study run (or use -Demo for the synthetic demo).' -ForegroundColor Red; exit 2 }
if ($ResumeRunDir -ne '' -and -not (Test-Path -LiteralPath $ResumeRunDir -PathType Container)) { Write-Host 'FAIL: -ResumeRunDir was not found.' -ForegroundColor Red; exit 2 }
$code = Invoke-SiewFullRun -RepoDir $repo -PathsFile $PathsFile -Demo:$Demo -OutRoot $OutRoot -ResumeRunDir $ResumeRunDir -RainfallSensitivity:$RainfallSensitivity -MinFreeGB $MinFreeGB
exit $code
