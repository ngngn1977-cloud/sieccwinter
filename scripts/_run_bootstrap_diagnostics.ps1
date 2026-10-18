# Helper for run_bootstrap_diagnostics.ps1 (dot-sourced; not meant to be run directly). Windows PowerShell 5.1 compatible. Prints no participant-level values.
# Exit codes: 0 = audit executed and every check passed; 1 = execution failure; 2 = invalid arguments or R missing; 4 = executed, but a check (integrity, unchanged checkpoints, accounting, quantile reconstruction, provenance, source integrity) FAILED.
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

function Invoke-SiewBootstrapAudit {
    param(
        [Parameter(Mandatory = $true)][string]$RepoDir,
        [Parameter(Mandatory = $true)][string]$BatchCRunDir,
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
        $cand = Join-Path $OutRoot ("bootdiag_{0}_{1}" -f $stamp, $suffix)
        if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break }
    }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null
    $runDir = (Resolve-Path -LiteralPath $runDir).Path
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host 'Auditing the saved bootstrap checkpoints (read only; no participant data, no refitting) ...'
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), 'bootdiag', (ConvertTo-QuotedArg $BatchCRunDir), '--out', (ConvertTo-QuotedArg $runDir))
    $outLog = Join-Path $RepoDir ("bootdiag_console_{0}.tmp" -f $stamp)
    $errLog = Join-Path $RepoDir ("bootdiag_err_{0}.tmp" -f $stamp)
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = $p.ExitCode
    $consoleLog = Join-Path $runDir 'run_console.log'
    Move-Item -LiteralPath $outLog -Destination $consoleLog -Force
    if (Test-Path -LiteralPath $errLog) { Add-Content -LiteralPath $consoleLog -Value '--- stderr ---'; Add-Content -LiteralPath $consoleLog -Value (Get-Content -LiteralPath $errLog); Remove-Item -LiteralPath $errLog -Force }
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)

    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_bootstrap_diagnostics.R')), (ConvertTo-QuotedArg $runDir), [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'
    $finErr = Join-Path $RepoDir ("bootdiag_fin_err_{0}.tmp" -f $stamp)
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) { Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr); Remove-Item -LiteralPath $finErr -Force }

    $st = @{}
    $stFile = Join-Path $runDir 'BOOTSTRAP_DIAGNOSTICS_STATUS.txt'
    if (Test-Path -LiteralPath $stFile) { foreach ($line in (Get-Content -LiteralPath $stFile)) { if ($line -match '^([A-Z_0-9]+)=(.*)$') { $st[$matches[1]] = $matches[2] } } }
    Write-Host ''
    foreach ($k in @('EXECUTION_STATUS', 'CHECKPOINT_INTEGRITY_STATUS', 'CHECKPOINTS_UNCHANGED_STATUS', 'ACCOUNTING_STATUS', 'QUANTILE_RECONSTRUCTION_STATUS', 'ORIGINAL_RUN_PROVENANCE_STATUS', 'SOURCE_INTEGRITY_STATUS', 'OUTPUT_COMPLETENESS_STATUS')) {
        $v = if ($st.ContainsKey($k)) { $st[$k] } else { 'FAIL' }
        $colour = if ($v -eq 'PASS') { 'Green' } else { 'Red' }
        Write-Host ("{0,-36} {1}" -f $k, $v) -ForegroundColor $colour
    }
    foreach ($k in @('TARGETS', 'BINARY_TARGETS', 'PREVIOUSLY_FLAGGED_TARGETS', 'TARGETS_WITH_X2_EXTREME')) { if ($st.ContainsKey($k)) { Write-Host ("{0,-36} {1}" -f $k, $st[$k]) } }
    Write-Host ''
    $execOk = $st.ContainsKey('EXECUTION_STATUS') -and $st['EXECUTION_STATUS'] -eq 'PASS'
    $allPass = $execOk
    foreach ($k in @('CHECKPOINT_INTEGRITY_STATUS', 'CHECKPOINTS_UNCHANGED_STATUS', 'ACCOUNTING_STATUS', 'QUANTILE_RECONSTRUCTION_STATUS', 'ORIGINAL_RUN_PROVENANCE_STATUS', 'SOURCE_INTEGRITY_STATUS', 'OUTPUT_COMPLETENESS_STATUS')) { if (-not ($st.ContainsKey($k) -and $st[$k] -eq 'PASS')) { $allPass = $false } }
    if ($allPass -and $rc -eq 0) {
        Write-Host ("PASS - audit executed and every check passed in {0} s (extremeness diagnostics are descriptive; see RUN_SUMMARY.md)" -f $wall) -ForegroundColor Green
        $code = 0
    } elseif (-not $execOk) {
        Write-Host ("FAIL - EXECUTION FAILED (R exit code {0})" -f $rc) -ForegroundColor Red
        $errs = Select-String -LiteralPath $consoleLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3
        foreach ($e in $errs) { Write-Host ('  ' + $e.Line) }
        $code = 1
    } else {
        Write-Host 'FAIL - the audit executed, but at least one check failed (see audit_checkpoint_checks.csv, audit_quantile_reconstruction.csv, audit_provenance.json).' -ForegroundColor Red
        $code = 4
    }
    Write-Host ("Output folder: {0}" -f $runDir)
    return $code
}
