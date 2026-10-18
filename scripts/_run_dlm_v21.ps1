# Helper for run_dlm_validation_v21_covariance.ps1 (dot-sourced). Windows PowerShell 5.1 compatible. Reads aggregate files only; prints no participant-level values.
# Exit codes: 0 = every new status PASS and nothing carried forward is open; 3 = every new status PASS while carried-forward items are REVIEW or NOT_TESTED (expected); 1 = execution failure; 2 = invalid arguments or R missing;
#             4 = a new status FAILED (including missing or inconsistent input files: nothing is refitted).
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

function Invoke-SiewDlmV21 {
    param(
        [Parameter(Mandatory = $true)][string]$RepoDir,
        [Parameter(Mandatory = $true)][string]$V2RunDir,
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
        $cand = Join-Path $OutRoot ("dlmv21cov_{0}_{1}" -f $stamp, $suffix)
        if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break }
    }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null
    $runDir = (Resolve-Path -LiteralPath $runDir).Path
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host 'V2.1 candidate covariance check: reads the aggregate covariance matrices saved in the V2 run folder (no data, no refit) ...'
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), 'dlmv21cov', (ConvertTo-QuotedArg $V2RunDir), '--out', (ConvertTo-QuotedArg $runDir))
    $outLog = Join-Path $RepoDir ("dlmv21cov_console_{0}.tmp" -f $stamp)
    $errLog = Join-Path $RepoDir ("dlmv21cov_err_{0}.tmp" -f $stamp)
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = $p.ExitCode
    $consoleLog = Join-Path $runDir 'run_console.log'
    Move-Item -LiteralPath $outLog -Destination $consoleLog -Force
    if (Test-Path -LiteralPath $errLog) { Add-Content -LiteralPath $consoleLog -Value '--- stderr ---'; Add-Content -LiteralPath $consoleLog -Value (Get-Content -LiteralPath $errLog); Remove-Item -LiteralPath $errLog -Force }
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)
    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_dlm_validation.R')), (ConvertTo-QuotedArg $runDir), [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'
    $finErr = Join-Path $RepoDir ("dlmv21cov_fin_err_{0}.tmp" -f $stamp)
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) { Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr); Remove-Item -LiteralPath $finErr -Force }

    $st = @{}
    $stFile = Join-Path $runDir 'DLM_V21_STATUS.txt'
    if (Test-Path -LiteralPath $stFile) { foreach ($line in (Get-Content -LiteralPath $stFile)) { if ($line -match '^([A-Z_0-9]+)=(.*)$') { $st[$matches[1]] = $matches[2] } } }
    Write-Host ''
    Write-Host 'NEW STATUSES'
    $bad = $false
    foreach ($k in @('EXECUTION_STATUS', 'INPUT_PROVENANCE_STATUS', 'COVARIANCE_RULE_V21_STATUS', 'SOURCE_INTEGRITY_STATUS')) {
        $v = if ($st.ContainsKey($k)) { $st[$k] } else { 'FAIL' }
        $colour = if ($v -eq 'PASS') { 'Green' } elseif ($v -eq 'NOT_TESTED') { 'Yellow' } else { 'Red' }
        Write-Host ("  {0,-34} {1}" -f $k, $v) -ForegroundColor $colour
        if ($v -ne 'PASS') { $bad = $true }
    }
    foreach ($k in @('COVARIANCE_MODELS_PASS', 'LARGEST_DELTA_SE', 'MISSING_FILES')) { if ($st.ContainsKey($k)) { Write-Host ("  {0,-34} {1}" -f $k, $st[$k]) } }
    Write-Host 'CARRIED FORWARD (previously reviewed evidence, not recomputed)'
    foreach ($key in ($st.Keys | Where-Object { $_ -like 'CARRIED_FORWARD_*' } | Sort-Object)) { Write-Host ("  {0,-60} {1}" -f $key.Substring(16), ($st[$key] -replace ' \(previously.*$', '')) }
    Write-Host ''
    $execOk = $st.ContainsKey('EXECUTION_STATUS') -and $st['EXECUTION_STATUS'] -eq 'PASS'
    if (-not $execOk) {
        Write-Host ("FAIL - EXECUTION FAILED (R exit code {0}, finalise exit code {1})" -f $rc, $f.ExitCode) -ForegroundColor Red
        $errs = Select-String -LiteralPath $consoleLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3
        foreach ($e in $errs) { Write-Host ('  ' + $e.Line) }
        $code = 1
    } elseif ($bad) {
        Write-Host 'FAIL - a new status FAILED or could not be tested (see dlm_v21_provenance.csv, dlm_v21_covariance_checks.csv, RUN_SUMMARY.md). Nothing was refitted.' -ForegroundColor Red
        $code = 4
    } elseif ($rc -eq 0) {
        Write-Host ("PASS - covariance rule V2.1 passed in {0} s; adoption remains pending review" -f $wall) -ForegroundColor Green
        $code = 0
    } else {
        Write-Host ("OK WITH CARRIED-FORWARD OPEN ITEMS - new statuses passed in {0} s; carried-forward REVIEW / NOT_TESTED items are not PASS; adoption remains pending review" -f $wall) -ForegroundColor Yellow
        $code = 3
    }
    Write-Host ("Output folder: {0}" -f $runDir)
    return $code
}
