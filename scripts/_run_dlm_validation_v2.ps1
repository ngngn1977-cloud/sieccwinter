# Helper for run_dlm_validation_v2.ps1 (dot-sourced). Windows PowerShell 5.1 compatible. Prints no participant-level values.
# Exit codes: 0 = every status PASS; 3 = every required status PASS while a separately reported status is REVIEW or NOT_TESTED (documented); 1 = execution failure; 2 = invalid arguments or R missing;
#             4 = a required status FAILED (genuine fitting, covariance, sampling, unresolved-orientation or stability failure).
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

function Invoke-SiewDlmValidationV2 {
    param(
        [Parameter(Mandatory = $true)][string]$RepoDir,
        [Parameter(Mandatory = $true)][string]$DataCsv,
        [Parameter(Mandatory = $true)][string]$DlmReferenceDir,
        [string]$AcceptedRunDir = '',
        [string]$OutRoot = '',
        [switch]$NoMonteCarlo,
        [switch]$SyntheticNoManuscriptConcordance
    )
    $ErrorActionPreference = 'Stop'
    if (-not (Test-Path -LiteralPath $script:RscriptExe -PathType Leaf)) { Write-Host ("FAIL: R was not found at {0}" -f $script:RscriptExe) -ForegroundColor Red; return 2 }
    if (-not (Test-Path -LiteralPath (Join-Path $RepoDir 'run.R') -PathType Leaf)) { Write-Host ("FAIL: run.R not found in {0}" -f $RepoDir) -ForegroundColor Red; return 2 }
    if ([string]::IsNullOrWhiteSpace($OutRoot)) { $OutRoot = Join-Path $RepoDir 'runs' }
    $stamp = Get-Date -Format 'yyyyMMdd_HHmmss'
    $runDir = $null
    for ($i = 0; $i -lt 50; $i++) {
        $suffix = ('{0:x4}' -f (Get-Random -Maximum 65536))
        $cand = Join-Path $OutRoot ("dlmvalidatev2_{0}_{1}" -f $stamp, $suffix)
        if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break }
    }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null
    $runDir = (Resolve-Path -LiteralPath $runDir).Path
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host 'Phase 3D validation V2 candidate (reuses the V1 validation and the simulation diagnostic unchanged; V1 results and records are preserved). Progress is written to run_console.log ...'

    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), 'dlmvalidatev2', (ConvertTo-QuotedArg $DataCsv), '--dlm-reference', (ConvertTo-QuotedArg $DlmReferenceDir))
    if ($AcceptedRunDir -ne '') { $argList += @('--accepted-run', (ConvertTo-QuotedArg $AcceptedRunDir)) }
    if ($NoMonteCarlo) { $argList += @('--no-mc') }
    if ($SyntheticNoManuscriptConcordance) { $argList += @('--synthetic-no-manuscript-concordance') }
    $argList += @('--out', (ConvertTo-QuotedArg $runDir))
    $outLog = Join-Path $RepoDir ("dlmvalidatev2_console_{0}.tmp" -f $stamp)
    $errLog = Join-Path $RepoDir ("dlmvalidatev2_err_{0}.tmp" -f $stamp)
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = $p.ExitCode
    $consoleLog = Join-Path $runDir 'run_console.log'
    Move-Item -LiteralPath $outLog -Destination $consoleLog -Force
    if (Test-Path -LiteralPath $errLog) { Add-Content -LiteralPath $consoleLog -Value '--- stderr ---'; Add-Content -LiteralPath $consoleLog -Value (Get-Content -LiteralPath $errLog); Remove-Item -LiteralPath $errLog -Force }
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)

    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_dlm_validation.R')), (ConvertTo-QuotedArg $runDir), [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'
    $finErr = Join-Path $RepoDir ("dlmvalidatev2_fin_err_{0}.tmp" -f $stamp)
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) { Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr); Remove-Item -LiteralPath $finErr -Force }

    $st = @{}
    $stFile = Join-Path $runDir 'DLM_V2_STATUS.txt'
    if (Test-Path -LiteralPath $stFile) { foreach ($line in (Get-Content -LiteralPath $stFile)) { if ($line -match '^([A-Z_0-9]+)=(.*)$') { $st[$matches[1]] = $matches[2] } } }
    Write-Host ''
    Write-Host 'REQUIRED'
    $required = @('EXECUTION_STATUS', 'INPUT_CHECK_STATUS', 'PRODUCTION_REPRODUCTION_STATUS', 'DETERMINISTIC_AGREEMENT_STATUS', 'ORIENTATION_ALIGNED_SIMULATION_STATUS', 'NATIVE_ORIENTATION_MC_STATUS', 'REFERENCE_DETERMINISTIC_STATUS', 'REFERENCE_SIMULATION_MC_STATUS', 'SOURCE_INTEGRITY_STATUS')
    $reported = @('MANUSCRIPT_CONCORDANCE_STATUS', 'FIGURE_PROVENANCE_STATUS', 'DLNM_EQUALITY_STATUS')
    $reqBad = $false; $reqNt = $false; $repOpen = $false
    foreach ($k in $required) {
        $v = if ($st.ContainsKey($k)) { $st[$k] } else { 'FAIL' }
        $colour = if ($v -eq 'PASS') { 'Green' } elseif ($v -eq 'NOT_TESTED') { 'Yellow' } else { 'Red' }
        Write-Host ("  {0,-42} {1}" -f $k, $v) -ForegroundColor $colour
        if ($v -eq 'NOT_TESTED') { $reqNt = $true } elseif ($v -ne 'PASS') { $reqBad = $true }
    }
    Write-Host 'REPORTED SEPARATELY (not acceptance requirements of the five checks)'
    foreach ($k in $reported) {
        $v = if ($st.ContainsKey($k)) { $st[$k] } else { 'FAIL' }
        $colour = if ($v -eq 'PASS') { 'Green' } else { 'Yellow' }
        Write-Host ("  {0,-42} {1}" -f $k, $v) -ForegroundColor $colour
        if ($v -ne 'PASS') { $repOpen = $true }
    }
    Write-Host 'V1 CRITERIA, COMPUTED FOR THE RECORD (do not decide V2 acceptance)'
    foreach ($k in @('V1_REFERENCE_COMPARISON_STATUS', 'V1_SIMULTANEOUS_EXACT_SEEDED_STATUS')) { if ($st.ContainsKey($k)) { Write-Host ("  {0,-42} {1}" -f $k, $st[$k]) } }
    foreach ($k in @('DISCREPANCIES_BEYOND_ALLOWANCE', 'EXPLAINED_BY_ORIENTATION')) { if ($st.ContainsKey($k)) { Write-Host ("  {0,-42} {1}" -f $k, $st[$k]) } }
    Write-Host ''
    $execOk = $st.ContainsKey('EXECUTION_STATUS') -and $st['EXECUTION_STATUS'] -eq 'PASS'
    if (-not $execOk) {
        Write-Host ("FAIL - EXECUTION FAILED (R exit code {0}, finalise exit code {1})" -f $rc, $f.ExitCode) -ForegroundColor Red
        $errs = Select-String -LiteralPath $consoleLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3
        foreach ($e in $errs) { Write-Host ('  ' + $e.Line) }
        $code = 1
    } elseif ($reqBad) {
        Write-Host 'FAIL - a required status FAILED (genuine fitting, covariance, sampling, unresolved-orientation or stability failure; see dlm_v2_checks.csv and RUN_SUMMARY.md).' -ForegroundColor Red
        $code = 4
    } elseif ($reqNt -or $repOpen) {
        Write-Host ("OK WITH REVIEW OR NOT_TESTED - every tested required status passed in {0} s; REVIEW and NOT_TESTED are not PASS (see RUN_SUMMARY.md, dlm_v2_exact_differences_visible.csv)." -f $wall) -ForegroundColor Yellow
        $code = 3
    } else {
        Write-Host ("PASS - every status passed in {0} s" -f $wall) -ForegroundColor Green
        $code = 0
    }
    Write-Host ("Output folder: {0}" -f $runDir)
    return $code
}
