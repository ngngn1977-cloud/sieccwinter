# Helper for run_dlm_simulation_diagnostic.ps1 (dot-sourced). Windows PowerShell 5.1 compatible. Prints no participant-level values.
# Exit codes: 0 = diagnostic executed, replica/reconstruction checks PASS and the mechanism fully explained (or not applicable); 3 = executed and checks PASS, but a status is PARTIAL or NOT_TESTED (explicit in the output);
#             1 = execution failure; 2 = invalid arguments or R missing; 4 = a diagnostic self-check FAILED (production replica, factor reconstruction or source integrity).
# The diagnostic never changes a production result, criterion or earlier status.
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

function Invoke-SiewDlmSimDiag {
    param(
        [Parameter(Mandatory = $true)][string]$RepoDir,
        [Parameter(Mandatory = $true)][string]$DataCsv,
        [Parameter(Mandatory = $true)][string]$ValidationRunDir,
        [string]$DlmReferenceDir = '',
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
        $cand = Join-Path $OutRoot ("dlmsimdiag_{0}_{1}" -f $stamp, $suffix)
        if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break }
    }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null
    $runDir = (Resolve-Path -LiteralPath $runDir).Path
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host 'Simulation diagnostic for the DLM simultaneous critical values (diagnostic only; nothing in production or in earlier validation runs is changed) ...'

    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), 'dlmsimdiag', (ConvertTo-QuotedArg $DataCsv), '--validation-run', (ConvertTo-QuotedArg $ValidationRunDir))
    if ($DlmReferenceDir -ne '') { $argList += @('--dlm-reference', (ConvertTo-QuotedArg $DlmReferenceDir)) }
    $argList += @('--out', (ConvertTo-QuotedArg $runDir))
    $outLog = Join-Path $RepoDir ("dlmsimdiag_console_{0}.tmp" -f $stamp)
    $errLog = Join-Path $RepoDir ("dlmsimdiag_err_{0}.tmp" -f $stamp)
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = $p.ExitCode
    $consoleLog = Join-Path $runDir 'run_console.log'
    Move-Item -LiteralPath $outLog -Destination $consoleLog -Force
    if (Test-Path -LiteralPath $errLog) { Add-Content -LiteralPath $consoleLog -Value '--- stderr ---'; Add-Content -LiteralPath $consoleLog -Value (Get-Content -LiteralPath $errLog); Remove-Item -LiteralPath $errLog -Force }
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)

    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_dlm_validation.R')), (ConvertTo-QuotedArg $runDir), [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'
    $finErr = Join-Path $RepoDir ("dlmsimdiag_fin_err_{0}.tmp" -f $stamp)
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) { Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr); Remove-Item -LiteralPath $finErr -Force }

    $st = @{}
    $stFile = Join-Path $runDir 'DLM_SIMDIAG_STATUS.txt'
    if (Test-Path -LiteralPath $stFile) { foreach ($line in (Get-Content -LiteralPath $stFile)) { if ($line -match '^([A-Z_0-9]+)=(.*)$') { $st[$matches[1]] = $matches[2] } } }
    Write-Host ''
    $keys = @('EXECUTION_STATUS', 'PRODUCTION_REPLICA_STATUS', 'FACTOR_RECONSTRUCTION_STATUS', 'MECHANISM_EXPLANATION_STATUS', 'REFERENCE_PATTERN_STATUS', 'SOURCE_INTEGRITY_STATUS')
    $bad = $false; $partial = $false
    foreach ($k in $keys) {
        $v = if ($st.ContainsKey($k)) { $st[$k] } else { 'FAIL' }
        $colour = if ($v -eq 'PASS' -or $v -eq 'NOT_APPLICABLE') { 'Green' } elseif ($v -eq 'PARTIAL' -or $v -eq 'NOT_TESTED') { 'Yellow' } else { 'Red' }
        Write-Host ("{0,-36} {1}" -f $k, $v) -ForegroundColor $colour
        if ($v -eq 'PARTIAL' -or $v -eq 'NOT_TESTED') { $partial = $true } elseif ($v -ne 'PASS' -and $v -ne 'NOT_APPLICABLE') { $bad = $true }
    }
    foreach ($k in @('MODELS', 'DISCREPANCIES_PRODUCTION_ROUTES', 'EXPLAINED_BY_ORIENTATION', 'UNEXPLAINED', 'REFERENCE_MISMATCHES', 'REFERENCE_MISMATCHES_REPRODUCED_BY_A_SIGN_PATTERN')) { if ($st.ContainsKey($k)) { Write-Host ("{0,-36} {1}" -f $k, $st[$k]) } }
    Write-Host ''
    $execOk = $st.ContainsKey('EXECUTION_STATUS') -and $st['EXECUTION_STATUS'] -eq 'PASS'
    if (-not $execOk) {
        Write-Host ("FAIL - EXECUTION FAILED (R exit code {0}, finalise exit code {1})" -f $rc, $f.ExitCode) -ForegroundColor Red
        $errs = Select-String -LiteralPath $consoleLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3
        foreach ($e in $errs) { Write-Host ('  ' + $e.Line) }
        $code = 1
    } elseif ($bad) {
        Write-Host 'FAIL - a diagnostic self-check failed (see dlm_sd_production_replica.csv, dlm_sd_factor_reconstruction.csv, RUN_SUMMARY.md).' -ForegroundColor Red
        $code = 4
    } elseif ($partial) {
        Write-Host ("DONE WITH PARTIAL OR NOT_TESTED ITEMS - self-checks passed in {0} s; see dlm_sd_unexplained_differences.csv and RUN_SUMMARY.md. PARTIAL and NOT_TESTED are not PASS." -f $wall) -ForegroundColor Yellow
        $code = 3
    } else {
        Write-Host ("PASS - self-checks passed and the mechanism is explained in {0} s (diagnostic only)" -f $wall) -ForegroundColor Green
        $code = 0
    }
    Write-Host ("Output folder: {0}" -f $runDir)
    return $code
}
