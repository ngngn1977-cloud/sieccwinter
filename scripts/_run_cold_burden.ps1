# Helper for run_cold_burden_validation.ps1 (dot-sourced). Windows PowerShell 5.1 compatible. Prints aggregate statuses only; no participant-level values.
# Exit codes: 0 = every status PASS and nothing open; 3 = every status PASS while the scientific interpretation stays NOT ASSESSED (expected); 1 = execution failure; 2 = invalid arguments or R missing;
#             4 = a status FAILED (including missing or non-matching reference evidence).
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

function Invoke-SiewColdBurden {
    param(
        [Parameter(Mandatory = $true)][string]$RepoDir,
        [Parameter(Mandatory = $true)][string]$DataCsv,
        [Parameter(Mandatory = $true)][string]$ReferenceDir,
        [string]$OutRoot = ''
    )
    $ErrorActionPreference = 'Stop'
    if (-not (Test-Path -LiteralPath $script:RscriptExe -PathType Leaf)) { Write-Host ("FAIL: R was not found at {0}" -f $script:RscriptExe) -ForegroundColor Red; return 2 }
    if (-not (Test-Path -LiteralPath (Join-Path $RepoDir 'run.R') -PathType Leaf)) { Write-Host ("FAIL: run.R not found in {0}" -f $RepoDir) -ForegroundColor Red; return 2 }
    if ([string]::IsNullOrWhiteSpace($OutRoot)) { $OutRoot = Join-Path $RepoDir 'runs' }
    $stamp = Get-Date -Format 'yyyyMMdd_HHmmss'
    $runDir = $null
    for ($i = 0; $i -lt 50; $i++) {
        $suffix = ('{0:x4}' -f (Get-Random -Maximum 65536))
        $cand = Join-Path $OutRoot ("coldburden_{0}_{1}" -f $stamp, $suffix)
        if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break }
    }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null
    $runDir = (Resolve-Path -LiteralPath $runDir).Path
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host 'Cold-burden scan: fitting the 22,302 models with the validated engine, comparing with the frozen V1.4 output and verifying definitions, scales, inference and multiplicity (a few minutes) ...'
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), 'coldburden', (ConvertTo-QuotedArg $DataCsv), '--reference-dir', (ConvertTo-QuotedArg $ReferenceDir), '--out', (ConvertTo-QuotedArg $runDir))
    $outLog = Join-Path $RepoDir ("coldburden_console_{0}.tmp" -f $stamp)
    $errLog = Join-Path $RepoDir ("coldburden_err_{0}.tmp" -f $stamp)
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = $p.ExitCode
    $consoleLog = Join-Path $runDir 'run_console.log'
    Move-Item -LiteralPath $outLog -Destination $consoleLog -Force
    if (Test-Path -LiteralPath $errLog) { Add-Content -LiteralPath $consoleLog -Value '--- stderr ---'; Add-Content -LiteralPath $consoleLog -Value (Get-Content -LiteralPath $errLog); Remove-Item -LiteralPath $errLog -Force }
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)
    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_dlm_validation.R')), (ConvertTo-QuotedArg $runDir), [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'
    $finErr = Join-Path $RepoDir ("coldburden_fin_err_{0}.tmp" -f $stamp)
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) { Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr); Remove-Item -LiteralPath $finErr -Force }

    $st = @{}
    $stFile = Join-Path $runDir 'COLDBURDEN_STATUS.txt'
    if (Test-Path -LiteralPath $stFile) { foreach ($line in (Get-Content -LiteralPath $stFile)) { if ($line -match '^([A-Z_0-9]+)=(.*)$') { $st[$matches[1]] = $matches[2] } } }
    Write-Host ''
    Write-Host 'STATUSES'
    $bad = $false
    foreach ($k in @('EXECUTION_STATUS', 'INPUT_PROVENANCE_STATUS', 'THRESHOLDS_STATUS', 'NUMERICAL_REPRODUCTION_STATUS', 'FEATURE_VERIFICATION_STATUS', 'SCALE_AND_INFERENCE_STATUS', 'MULTIPLICITY_STATUS', 'INDEPENDENT_REFIT_STATUS', 'SI_STATEMENT_STATUS', 'COVERAGE_STATUS', 'SOURCE_INTEGRITY_STATUS')) {
        $v = if ($st.ContainsKey($k)) { $st[$k] } else { 'FAIL' }
        $colour = if ($v -eq 'PASS') { 'Green' } elseif ($v -eq 'NOT_TESTED') { 'Yellow' } else { 'Red' }
        Write-Host ("  {0,-32} {1}" -f $k, $v) -ForegroundColor $colour
        if ($v -ne 'PASS') { $bad = $true }
    }
    foreach ($k in @('MODELS_FITTED', 'COMPARED_CELLS_RESULTS', 'INFINITE_EQUAL_PAIRS', 'COMPARED_CELLS_THRESHOLDS', 'RUNTIME_SECONDS_SCAN', 'RUNTIME_SECONDS_TOTAL', 'PROVENANCE_PROBLEMS')) { if ($st.ContainsKey($k)) { Write-Host ("  {0,-32} {1}" -f $k, $st[$k]) } }
    Write-Host 'CARRIED UNCHANGED / NOT ASSESSED'
    foreach ($k in @('FINAL_NINE_L56_NUMERICAL_VALIDATION', 'ARCHIVED_DLM_DETERMINISTIC', 'MANUSCRIPT_CONCORDANCE_STATUS', 'FIGURE_PROVENANCE_STATUS', 'DLNM_EQUALITY_STATUS', 'SCIENTIFIC_INTERPRETATION')) { if ($st.ContainsKey($k)) { Write-Host ("  {0,-38} {1}" -f $k, (($st[$k]) -replace ' \(.*$', '')) } }
    Write-Host ''
    $execOk = $st.ContainsKey('EXECUTION_STATUS') -and $st['EXECUTION_STATUS'] -eq 'PASS'
    if (-not $execOk) {
        Write-Host ("FAIL - EXECUTION FAILED (R exit code {0}, finalise exit code {1})" -f $rc, $f.ExitCode) -ForegroundColor Red
        $errs = Select-String -LiteralPath $consoleLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3
        foreach ($e in $errs) { Write-Host ('  ' + $e.Line) }
        $code = 1
    } elseif ($bad) {
        Write-Host 'FAIL - a status FAILED or evidence is missing (see RUN_SUMMARY.md, coldburden_comparison_coverage.csv, coldburden_comparison_detail.csv, coldburden_reference_provenance.csv). Nothing was altered to fit a reference.' -ForegroundColor Red
        $code = 4
    } elseif ($rc -eq 0) {
        Write-Host ("PASS - all statuses passed in {0} s" -f $wall) -ForegroundColor Green
        $code = 0
    } else {
        Write-Host ("OK WITH INTERPRETATION NOT ASSESSED - all statuses passed in {0} s; the scientific interpretation of the scan is not assessed and no model is promoted" -f $wall) -ForegroundColor Yellow
        $code = 3
    }
    Write-Host ("Output folder: {0}" -f $runDir)
    return $code
}
