# Helper for run_anomaly_history_validation.ps1 (dot-sourced). Windows PowerShell 5.1 compatible. Prints aggregate statuses only; no participant-level values.
# Required statuses: ANOM_VERIFICATION_STATUS_V2 replaces ANOM_VERIFICATION_STATUS (protocol v2, post hoc, 12 Oct 2026); the v1 status is shown and recorded as superseded.
# Exit codes: 0 = every required status PASS and R exit code 0; 4 = a status FAILED, is missing, is NOT_TESTED or has an unexpected value (including missing or non-matching reference evidence), or R exit code 4;
#             1 = execution failure (R failed, no status file, EXECUTION_STATUS not PASS, an R exit code other than 0 or 4) or statuses and R exit code inconsistent; 2 = invalid arguments or R missing.
# There is NO permitted non-PASS value in this batch (no REVIEW exception). The R and the wrapper outcomes are saved in HANDOFF_OUTCOME.txt, written after RUN_MANIFEST.csv and deliberately NOT listed in it.
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'
# Protocol v2 (post hoc, 12 Oct 2026): ANOM_VERIFICATION_STATUS_V2 governs; ANOM_VERIFICATION_STATUS (v1 composition) is shown, superseded, kept for the record, and never required.
$script:AnomHistRequired = @('EXECUTION_STATUS', 'INPUT_PROVENANCE_STATUS', 'ANOM_REPRODUCTION_STATUS', 'ANOM_VERIFICATION_STATUS_V2', 'ALT_REPRODUCTION_STATUS', 'ALT_VERIFICATION_STATUS', 'COVERAGE_STATUS', 'SOURCE_INTEGRITY_STATUS')

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

# Pure aggregation: $Status is a hashtable of the saved status file, $RExitCode the actual exit code of the R process. No I/O.
function Resolve-SiewAnomHistOutcome {
    param([Parameter(Mandatory = $true)][hashtable]$Status, [Parameter(Mandatory = $true)][int]$RExitCode, [double]$Seconds = 0)
    $rows = @(); $bad = $false
    foreach ($k in $script:AnomHistRequired) {
        $v = if ($Status.ContainsKey($k)) { $Status[$k] } else { 'MISSING' }
        $ok = ($v -eq 'PASS'); if (-not $ok) { $bad = $true }
        $colour = if ($ok) { 'Green' } elseif ($v -eq 'NOT_TESTED') { 'Yellow' } else { 'Red' }
        $rows += [pscustomobject]@{ Name = $k; Value = $v; Colour = $colour; Ok = $ok }
    }
    $execOk = $Status.ContainsKey('EXECUTION_STATUS') -and $Status['EXECUTION_STATUS'] -eq 'PASS'
    if (-not $execOk -or ($RExitCode -notin @(0, 4))) {
        return [pscustomobject]@{ Code = 1; Kind = 'EXECUTION_FAILURE'; Rows = $rows; Message = ("FAIL - EXECUTION FAILED (R exit code {0}; EXECUTION_STATUS {1})" -f $RExitCode, $(if ($Status.ContainsKey('EXECUTION_STATUS')) { $Status['EXECUTION_STATUS'] } else { 'MISSING' })); Colour = 'Red' }
    }
    if ($bad -or $RExitCode -eq 4) {
        return [pscustomobject]@{ Code = 4; Kind = 'STATUS_FAILURE'; Rows = $rows; Message = 'FAIL - a status FAILED, is missing, is NOT_TESTED or has an unexpected value, or R reported failure (see RUN_SUMMARY.md, anomhist_comparison_coverage.csv, anom_comparison_detail.csv, alt_comparison_detail.csv, alt_pca_sign_diagnostic.csv, anomhist_reference_provenance.csv). Nothing was altered to fit a reference.'; Colour = 'Red' }
    }
    if ($RExitCode -eq 0) {
        return [pscustomobject]@{ Code = 0; Kind = 'PASS'; Rows = $rows; Message = ("PASS - ANOM and ALT reproduced and verified (numerical reproduction and mathematical verification only; exploratory families) in {0} s" -f $Seconds); Colour = 'Green' }
    }
    return [pscustomobject]@{ Code = 1; Kind = 'INCONSISTENT'; Rows = $rows; Message = ("FAIL - the statuses and the R exit code ({0}) are inconsistent; nothing is accepted" -f $RExitCode); Colour = 'Red' }
}

function Invoke-SiewAnomalyHistory {
    param(
        [Parameter(Mandatory = $true)][string]$RepoDir,
        [Parameter(Mandatory = $true)][string]$DataCsv,
        [Parameter(Mandatory = $true)][string]$ReferenceDir,
        [string]$OutRoot = '',
        [string]$ProtocolFile = ''
    )
    $ErrorActionPreference = 'Stop'
    if (-not (Test-Path -LiteralPath $script:RscriptExe -PathType Leaf)) { Write-Host ("FAIL: R was not found at {0}" -f $script:RscriptExe) -ForegroundColor Red; return 2 }
    if (-not (Test-Path -LiteralPath (Join-Path $RepoDir 'run.R') -PathType Leaf)) { Write-Host ("FAIL: run.R not found in {0}" -f $RepoDir) -ForegroundColor Red; return 2 }
    if ([string]::IsNullOrWhiteSpace($OutRoot)) { $OutRoot = Join-Path $RepoDir 'runs' }
    $stamp = Get-Date -Format 'yyyyMMdd_HHmmss'
    $runDir = $null
    for ($i = 0; $i -lt 50; $i++) {
        $suffix = ('{0:x4}' -f (Get-Random -Maximum 65536))
        $cand = Join-Path $OutRoot ("anomhist_{0}_{1}" -f $stamp, $suffix)
        if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break }
    }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null
    $runDir = (Resolve-Path -LiteralPath $runDir).Path
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host 'Exploratory families: ANOM 7,080 (daily-reference anomaly models) and ALT 312 (alternative exposure-history tests); comparing with the frozen V1.4 output (a few minutes) ...'
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), 'anomhist', (ConvertTo-QuotedArg $DataCsv), '--reference-dir', (ConvertTo-QuotedArg $ReferenceDir), '--out', (ConvertTo-QuotedArg $runDir))
    if ($ProtocolFile -ne '') { $argList += @('--protocol-file', (ConvertTo-QuotedArg $ProtocolFile)) }       # synthetic tests only
    $outLog = Join-Path $RepoDir ("anomhist_console_{0}.tmp" -f $stamp)
    $errLog = Join-Path $RepoDir ("anomhist_err_{0}.tmp" -f $stamp)
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = [int]$p.ExitCode
    $consoleLog = Join-Path $runDir 'run_console.log'
    Move-Item -LiteralPath $outLog -Destination $consoleLog -Force
    if (Test-Path -LiteralPath $errLog) { Add-Content -LiteralPath $consoleLog -Value '--- stderr ---'; Add-Content -LiteralPath $consoleLog -Value (Get-Content -LiteralPath $errLog); Remove-Item -LiteralPath $errLog -Force }
    Add-Content -LiteralPath $consoleLog -Value ('R_EXIT_CODE={0}' -f $rc)
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)
    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_anomaly_history.R')), (ConvertTo-QuotedArg $runDir), [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'
    $finErr = Join-Path $RepoDir ("anomhist_fin_err_{0}.tmp" -f $stamp)
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) { Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr); Remove-Item -LiteralPath $finErr -Force }

    $st = @{}
    $stFile = Join-Path $runDir 'ANOMHIST_STATUS.txt'
    if (Test-Path -LiteralPath $stFile) { foreach ($line in (Get-Content -LiteralPath $stFile)) { if ($line -match '^([A-Z_0-9]+)=(.*)$') { $st[$matches[1]] = $matches[2] } } }
    $res = Resolve-SiewAnomHistOutcome -Status $st -RExitCode $rc -Seconds $wall
    Write-Host ''
    Write-Host 'STATUSES'
    foreach ($r in $res.Rows) { Write-Host ("  {0,-32} {1}" -f $r.Name, $r.Value) -ForegroundColor $r.Colour }
    $v1 = $(if ($st.ContainsKey('ANOM_VERIFICATION_STATUS')) { $st['ANOM_VERIFICATION_STATUS'] } else { 'MISSING' })
    Write-Host ("  {0,-32} {1}  (V1, superseded by protocol v2, kept for the record; not counted)" -f 'ANOM_VERIFICATION_STATUS', $v1) -ForegroundColor Gray
    foreach ($k in @('ANOM_VERIFICATION_GOVERNED_BY', 'ANOM_IDENTITY_V1_BY_N', 'ANOM_IDENTITY_V2_ROWS')) { if ($st.ContainsKey($k)) { Write-Host ('  {0,-32} {1}' -f $k, $st[$k]) -ForegroundColor Gray } }
    foreach ($k in @('ANOM_CELLS', 'ANOMDIAG_CELLS', 'ALT_CELLS', 'ALTPCA_CELLS', 'ANOM_REFIT_COVERAGE', 'ALT_PCA_SIGN_DIFFERENCES', 'RUNTIME_SECONDS', 'PROVENANCE_PROBLEMS')) { if ($st.ContainsKey($k)) { Write-Host ('  {0,-26} {1}' -f $k, $st[$k]) } }
    Write-Host 'CARRIED UNCHANGED / NOT ASSESSED'
    foreach ($k in @('FINAL_NINE_L56_NUMERICAL_VALIDATION', 'ARCHIVED_DLM_DETERMINISTIC', 'COLD_BURDEN_NUMERICAL', 'DAILY_LAYER_NUMERICAL', 'MANUSCRIPT_CONCORDANCE_STATUS', 'FIGURE_PROVENANCE_STATUS', 'DLNM_EQUALITY_STATUS', 'J02_GLM_EPSILON_ENTRY_POINTS')) { if ($st.ContainsKey($k)) { Write-Host ('  {0,-38} {1}' -f $k, (($st[$k]) -replace ' \(.*$', '')) } }
    Write-Host ''
    if ($res.Kind -eq 'EXECUTION_FAILURE') {
        $errs = Select-String -LiteralPath $consoleLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3
        Write-Host ("{0}; finalise exit code {1}" -f $res.Message, $f.ExitCode) -ForegroundColor $res.Colour
        foreach ($e in $errs) { Write-Host ('  ' + $e.Line) }
    } else {
        Write-Host $res.Message -ForegroundColor $res.Colour
    }
    Write-Host ("R exit code: {0}; wrapper exit code: {1}" -f $rc, $res.Code)
    # separate hand-off file: both outcomes, after the manifest; not listed in RUN_MANIFEST.csv by design
    $manHash = ''
    $manFile = Join-Path $runDir 'RUN_MANIFEST.csv'
    if (Test-Path -LiteralPath $manFile) { $manHash = (Get-FileHash -LiteralPath $manFile -Algorithm SHA256).Hash }
    $lines = @('HANDOFF_OUTCOME (written by run_anomaly_history_validation.ps1 after RUN_MANIFEST.csv; NOT listed in RUN_MANIFEST.csv by design, so the manifest stays valid)',
        ('WRITTEN_LOCAL=' + (Get-Date -Format 'yyyy-MM-dd HH:mm:ss')),
        ('R_EXIT_CODE=' + $rc), ('FINALISE_EXIT_CODE=' + $f.ExitCode), ('WRAPPER_EXIT_CODE=' + $res.Code), ('WRAPPER_KIND=' + $res.Kind), ('WRAPPER_MESSAGE=' + $res.Message), ('WALL_SECONDS=' + $wall),
        ('RUN_MANIFEST_SHA256_AT_WRITE=' + $manHash), ('RUN_FOLDER=' + $runDir))
    foreach ($r in $res.Rows) { $lines += ('{0}={1}' -f $r.Name, $r.Value) }
    $lines += ('ANOM_VERIFICATION_V1_SUPERSEDED={0} (v1 composition, kept for the record; not counted)' -f $(if ($st.ContainsKey('ANOM_VERIFICATION_STATUS')) { $st['ANOM_VERIFICATION_STATUS'] } else { 'MISSING' }))
    Set-Content -LiteralPath (Join-Path $runDir 'HANDOFF_OUTCOME.txt') -Value $lines -Encoding UTF8
    Write-Host ("Hand-off file: {0}" -f (Join-Path $runDir 'HANDOFF_OUTCOME.txt'))
    Write-Host ("Output folder: {0}" -f $runDir)
    return $res.Code
}
