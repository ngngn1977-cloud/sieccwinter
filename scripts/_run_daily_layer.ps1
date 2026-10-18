# Helper for run_daily_layer_validation.ps1 (dot-sourced). Windows PowerShell 5.1 compatible. Prints aggregate statuses only; no participant-level values.
# Exit codes: 0 = every status PASS and nothing open; 3 = every reproduction / verification status PASS while the MAXT assumptions stay REVIEW (expected; REVIEW is an assessment, never a pass, and it is the ONLY permitted non-PASS value, for MAXT_ASSUMPTIONS_STATUS only);
#             1 = execution failure (R failed, no status file, or EXECUTION_STATUS not PASS, or an R exit code other than 0, 3, 4); 2 = invalid arguments or R missing;
#             4 = a status FAILED, is missing, is NOT_TESTED or has an unexpected value (including missing or non-matching reference evidence).
# Phase 3H.2: the status aggregation is a separate pure function (Resolve-SiewDailyLayerOutcome) so that it can be tested without running R. The actual R exit code is recorded in run_console.log.
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'
$script:DailyLayerRequired = @('EXECUTION_STATUS', 'INPUT_PROVENANCE_STATUS', 'XYD_REPRODUCTION_STATUS', 'XYD_VERIFICATION_STATUS', 'DAY_REPRODUCTION_STATUS', 'DAY_VERIFICATION_STATUS', 'MAXT_REPRODUCTION_STATUS', 'MAXT_VERIFICATION_STATUS', 'COVERAGE_STATUS', 'SOURCE_INTEGRITY_STATUS')
$script:DailyLayerAssumptions = 'MAXT_ASSUMPTIONS_STATUS'

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

# Pure aggregation: $Status is a hashtable of the saved status file, $RExitCode the actual exit code of the R process.
# Returns Code, Kind, Message, Rows (name, value, colour, ok). No I/O.
function Resolve-SiewDailyLayerOutcome {
    param([Parameter(Mandatory = $true)][hashtable]$Status, [Parameter(Mandatory = $true)][int]$RExitCode, [double]$Seconds = 0)
    $rows = @(); $bad = $false
    foreach ($k in $script:DailyLayerRequired) {
        $present = $Status.ContainsKey($k); $v = if ($present) { $Status[$k] } else { 'MISSING' }
        $ok = ($v -eq 'PASS'); if (-not $ok) { $bad = $true }
        $colour = if ($ok) { 'Green' } elseif ($v -eq 'NOT_TESTED') { 'Yellow' } else { 'Red' }
        $rows += [pscustomobject]@{ Name = $k; Value = $v; Colour = $colour; Ok = $ok }
    }
    $ka = $script:DailyLayerAssumptions; $va = if ($Status.ContainsKey($ka)) { $Status[$ka] } else { 'MISSING' }
    $assumptionsPass = ($va -eq 'PASS'); $assumptionsReview = ($va -eq 'REVIEW')
    $okA = ($assumptionsPass -or $assumptionsReview); if (-not $okA) { $bad = $true }
    $colA = if ($assumptionsPass) { 'Green' } elseif ($assumptionsReview) { 'Yellow' } else { 'Red' }
    $rows = @($rows[0..7]) + @([pscustomobject]@{ Name = $ka; Value = $va; Colour = $colA; Ok = $okA }) + @($rows[8..9])         # display order: ... MAXT_VERIFICATION, MAXT_ASSUMPTIONS, COVERAGE, SOURCE_INTEGRITY
    $execOk = $Status.ContainsKey('EXECUTION_STATUS') -and $Status['EXECUTION_STATUS'] -eq 'PASS'
    if (-not $execOk -or ($RExitCode -notin @(0, 3, 4))) {
        return [pscustomobject]@{ Code = 1; Kind = 'EXECUTION_FAILURE'; Rows = $rows; Message = ("FAIL - EXECUTION FAILED (R exit code {0}; EXECUTION_STATUS {1})" -f $RExitCode, $(if ($Status.ContainsKey('EXECUTION_STATUS')) { $Status['EXECUTION_STATUS'] } else { 'MISSING' })); Colour = 'Red' }
    }
    if ($bad -or $RExitCode -eq 4) {
        return [pscustomobject]@{ Code = 4; Kind = 'STATUS_FAILURE'; Rows = $rows; Message = 'FAIL - a status FAILED, is missing, is NOT_TESTED or has an unexpected value, or R reported failure (see RUN_SUMMARY.md, dailylayer_comparison_coverage.csv, dailylayer_comparison_detail.csv, dailylayer_reference_provenance.csv). Nothing was altered to fit a reference.'; Colour = 'Red' }
    }
    if ($assumptionsPass -and $RExitCode -eq 0) {
        return [pscustomobject]@{ Code = 0; Kind = 'PASS'; Rows = $rows; Message = ("PASS - all statuses passed in {0} s" -f $Seconds); Colour = 'Green' }
    }
    if ($assumptionsReview -and $RExitCode -eq 3) {
        return [pscustomobject]@{ Code = 3; Kind = 'OK_WITH_REVIEW'; Rows = $rows; Message = ('OK WITH MAXT ASSUMPTIONS UNDER REVIEW - reproduction and verification statuses passed in {0} s; the MAXT permutation assumptions are an assessment (REVIEW), not a pass' -f $Seconds); Colour = 'Yellow' }
    }
    # all required statuses are acceptable but the R exit code does not match them (for example REVIEW with exit 0, or PASS with exit 3): never accepted silently
    return [pscustomobject]@{ Code = 1; Kind = 'INCONSISTENT'; Rows = $rows; Message = ("FAIL - the statuses and the R exit code ({0}) are inconsistent; nothing is accepted" -f $RExitCode); Colour = 'Red' }
}

function Invoke-SiewDailyLayer {
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
        $cand = Join-Path $OutRoot ("dailylayer_{0}_{1}" -f $stamp, $suffix)
        if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break }
    }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null
    $runDir = (Resolve-Path -LiteralPath $runDir).Path
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host 'Daily-lag layer: XYD 1,416, DAY 1,416 and MAXT 240 (10,000 permutations x 3 outcomes, run twice for verification); comparing with the frozen V1.4 output (several minutes) ...'
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), 'dailylayer', (ConvertTo-QuotedArg $DataCsv), '--reference-dir', (ConvertTo-QuotedArg $ReferenceDir), '--out', (ConvertTo-QuotedArg $runDir))
    $outLog = Join-Path $RepoDir ("dailylayer_console_{0}.tmp" -f $stamp)
    $errLog = Join-Path $RepoDir ("dailylayer_err_{0}.tmp" -f $stamp)
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = [int]$p.ExitCode
    $consoleLog = Join-Path $runDir 'run_console.log'
    Move-Item -LiteralPath $outLog -Destination $consoleLog -Force
    if (Test-Path -LiteralPath $errLog) { Add-Content -LiteralPath $consoleLog -Value '--- stderr ---'; Add-Content -LiteralPath $consoleLog -Value (Get-Content -LiteralPath $errLog); Remove-Item -LiteralPath $errLog -Force }
    Add-Content -LiteralPath $consoleLog -Value ('R_EXIT_CODE={0}' -f $rc)
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)
    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_dlm_validation.R')), (ConvertTo-QuotedArg $runDir), [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'
    $finErr = Join-Path $RepoDir ("dailylayer_fin_err_{0}.tmp" -f $stamp)
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) { Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr); Remove-Item -LiteralPath $finErr -Force }

    $st = @{}
    $stFile = Join-Path $runDir 'DAILYLAYER_STATUS.txt'
    if (Test-Path -LiteralPath $stFile) { foreach ($line in (Get-Content -LiteralPath $stFile)) { if ($line -match '^([A-Z_0-9]+)=(.*)$') { $st[$matches[1]] = $matches[2] } } }
    $res = Resolve-SiewDailyLayerOutcome -Status $st -RExitCode $rc -Seconds $wall
    Write-Host ''
    Write-Host 'STATUSES'
    foreach ($r in $res.Rows) { Write-Host ("  {0,-32} {1}" -f $r.Name, $r.Value) -ForegroundColor $r.Colour }
    foreach ($k in @('XYD_CELLS', 'DAY_CELLS', 'MAXT_CELLS', 'MAXT_REPLICATES_REQUESTED', 'RUNTIME_SECONDS', 'PROVENANCE_PROBLEMS')) { if ($st.ContainsKey($k)) { Write-Host ('  {0,-26} {1}' -f $k, $st[$k]) } }
    Write-Host 'CARRIED UNCHANGED / NOT ASSESSED'
    foreach ($k in @('FINAL_NINE_L56_NUMERICAL_VALIDATION', 'ARCHIVED_DLM_DETERMINISTIC', 'COLD_BURDEN_NUMERICAL', 'MANUSCRIPT_CONCORDANCE_STATUS', 'FIGURE_PROVENANCE_STATUS', 'DLNM_EQUALITY_STATUS', 'J02_GLM_EPSILON_ENTRY_POINTS')) { if ($st.ContainsKey($k)) { Write-Host ('  {0,-38} {1}' -f $k, (($st[$k]) -replace ' \(.*$', '')) } }
    Write-Host ''
    if ($res.Kind -eq 'EXECUTION_FAILURE') {
        $errs = Select-String -LiteralPath $consoleLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3
        Write-Host ("{0}; finalise exit code {1}" -f $res.Message, $f.ExitCode) -ForegroundColor $res.Colour
        foreach ($e in $errs) { Write-Host ('  ' + $e.Line) }
    } else {
        Write-Host $res.Message -ForegroundColor $res.Colour
    }
    Write-Host ("R exit code: {0}; wrapper exit code: {1}" -f $rc, $res.Code)
    Write-Host ("Output folder: {0}" -f $runDir)
    return $res.Code
}
