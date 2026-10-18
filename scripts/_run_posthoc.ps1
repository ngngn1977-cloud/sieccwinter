# Helper for run_posthoc.ps1 (dot-sourced). Windows PowerShell 5.1 compatible. Prints aggregate statuses only; no participant-level values.
# Post hoc steps of config/posthoc_sensitivity_protocol_v2.json (labelled SENSITIVITY / POST_HOC / RECONSTRUCTION; they cannot change q90, q9 or any accepted status).
# Exit codes: 0 = every status PASS (NOT_APPLICABLE_SYNTHETIC_INPUT is neutral and only allowed for the shipped synthetic CSV); 3 = REVIEW (no FAIL, but a REVIEW or NOT_TESTED status);
#             4 = a status FAILED or has an unexpected value; 1 = execution failure (R failed, no status file, EXECUTION_STATUS not PASS, or statuses inconsistent with the R exit code);
#             2 = invalid arguments or R missing.
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'
$script:PosthocItems = @('exposure_export', 'deletion', 'l59_bootstrap', 'no_april', 'source_agreement', 'exposure_residuals', 'profile_likelihood', 'early_late', 'h3_nested', 'nearest_station', 'exposure_qa', 'vif_summary', 'calendar_spline', 'age65')
$script:PosthocValues = @('PASS', 'REVIEW', 'NOT_TESTED', 'FAIL', 'NOT_APPLICABLE_SYNTHETIC_INPUT')

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

# Pure aggregation. $Status: hashtable of POSTHOC_STATUS.txt; $RExitCode: the actual exit code of R. Returns Code, Kind, Message, Rows. No I/O.
function Resolve-SiewPosthocOutcome {
    param([Parameter(Mandatory = $true)][hashtable]$Status, [Parameter(Mandatory = $true)][int]$RExitCode, [double]$Seconds = 0, [switch]$SyntheticDemo)
    $rows = @(); $fail = $false; $review = $false
    $keys = @($Status.Keys | Where-Object { $_ -match '_STATUS$' -and $_ -ne 'EXECUTION_STATUS' } | Sort-Object)
    foreach ($k in $keys) {
        $v = [string]$Status[$k]; $known = $script:PosthocValues -contains $v
        $ok = ($v -eq 'PASS') -or ($v -eq 'NOT_APPLICABLE_SYNTHETIC_INPUT' -and $SyntheticDemo)
        if (-not $known -or ($v -eq 'NOT_APPLICABLE_SYNTHETIC_INPUT' -and -not $SyntheticDemo) -or $v -eq 'FAIL') { $fail = $true }
        elseif ($v -in @('REVIEW', 'NOT_TESTED')) { $review = $true }
        $colour = $(if ($ok) { 'Green' } elseif ($v -in @('REVIEW', 'NOT_TESTED')) { 'Yellow' } else { 'Red' })
        $rows += [pscustomobject]@{ Name = $k; Value = $v; Colour = $colour; Ok = $ok }
    }
    $execOk = $Status.ContainsKey('EXECUTION_STATUS') -and $Status['EXECUTION_STATUS'] -eq 'PASS'
    if (-not $execOk -or $keys.Count -eq 0 -or ($RExitCode -notin @(0, 3, 4))) {
        return [pscustomobject]@{ Code = 1; Kind = 'EXECUTION_FAILURE'; Rows = $rows; Message = ('FAIL - EXECUTION FAILED (R exit code {0}; EXECUTION_STATUS {1})' -f $RExitCode, $(if ($Status.ContainsKey('EXECUTION_STATUS')) { $Status['EXECUTION_STATUS'] } else { 'MISSING' })); Colour = 'Red' }
    }
    $overall = [string]$Status['POSTHOC_OVERALL']; $expect = $(if ($fail) { 'FAIL' } elseif ($review) { 'REVIEW' } else { 'PASS' })
    $codeFor = @{ PASS = 0; REVIEW = 3; FAIL = 4 }
    if ($overall -ne $expect -or $RExitCode -ne $codeFor[$expect]) {
        if ($fail -or $RExitCode -eq 4) { return [pscustomobject]@{ Code = 4; Kind = 'STATUS_FAILURE'; Rows = $rows; Message = ('FAIL - a status FAILED or has an unexpected value (POSTHOC_OVERALL {0}; R exit code {1}). Nothing was altered to fit a reference.' -f $overall, $RExitCode); Colour = 'Red' } }
        return [pscustomobject]@{ Code = 1; Kind = 'INCONSISTENT'; Rows = $rows; Message = ('FAIL - the statuses ({0}), POSTHOC_OVERALL ({1}) and the R exit code ({2}) are inconsistent; nothing is accepted' -f $expect, $overall, $RExitCode); Colour = 'Red' }
    }
    if ($expect -eq 'FAIL') { return [pscustomobject]@{ Code = 4; Kind = 'STATUS_FAILURE'; Rows = $rows; Message = 'FAIL - a status FAILED (see RUN_SUMMARY.md and the *_comparison_*.csv / *_si_concordance.csv files). Nothing was altered to fit a reference.'; Colour = 'Red' } }
    if ($expect -eq 'REVIEW') { return [pscustomobject]@{ Code = 3; Kind = 'REVIEW'; Rows = $rows; Message = ('REVIEW - no status failed, but at least one is REVIEW or NOT_TESTED ({0} s); not counted as PASS' -f $Seconds); Colour = 'Yellow' } }
    return [pscustomobject]@{ Code = 0; Kind = 'PASS'; Rows = $rows; Message = ('PASS - all statuses passed in {0} s{1}' -f $Seconds, $(if ($SyntheticDemo) { ' (synthetic demo: comparisons with study values not applicable)' } else { '' })); Colour = 'Green' }
}

function Invoke-SiewPosthoc {
    param(
        [Parameter(Mandatory = $true)][string]$RepoDir,
        [Parameter(Mandatory = $true)][string]$Item,
        [Parameter(Mandatory = $true)][string]$DataCsv,
        [string]$OutRoot = '', [string]$V14ReferenceDir = '', [string]$ArchiveReferenceDir = '', [string]$CoreReferenceDir = '', [string]$NearestHistoriesCsv = '', [string]$GisReference = '', [switch]$SyntheticDemo
    )
    $ErrorActionPreference = 'Stop'
    if ($script:PosthocItems -notcontains $Item) { Write-Host ('FAIL: unknown post hoc item {0} (allowed: {1})' -f $Item, ($script:PosthocItems -join ', ')) -ForegroundColor Red; return 2 }
    if (-not (Test-Path -LiteralPath $script:RscriptExe -PathType Leaf)) { Write-Host ("FAIL: R was not found at {0}" -f $script:RscriptExe) -ForegroundColor Red; return 2 }
    if (-not (Test-Path -LiteralPath (Join-Path $RepoDir 'run.R') -PathType Leaf)) { Write-Host ("FAIL: run.R not found in {0}" -f $RepoDir) -ForegroundColor Red; return 2 }
    if ([string]::IsNullOrWhiteSpace($OutRoot)) { $OutRoot = Join-Path $RepoDir 'runs' }
    $stamp = Get-Date -Format 'yyyyMMdd_HHmmss'; $runDir = $null
    for ($i = 0; $i -lt 50; $i++) { $cand = Join-Path $OutRoot ('ph_{0}_{1}_{2}' -f $Item, $stamp, ('{0:x4}' -f (Get-Random -Maximum 65536))); if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break } }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null; $runDir = (Resolve-Path -LiteralPath $runDir).Path
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host ('Post hoc step {0} (labelled SENSITIVITY / POST_HOC / RECONSTRUCTION; cannot change q90, q9 or any accepted status) ...' -f $Item)
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), 'posthoc', $Item, (ConvertTo-QuotedArg $DataCsv), '--out', (ConvertTo-QuotedArg $runDir))
    foreach ($p in @(@('--v14-reference', $V14ReferenceDir), @('--archive-reference', $ArchiveReferenceDir), @('--core-reference', $CoreReferenceDir), @('--nearest-histories', $NearestHistoriesCsv), @('--gis-reference', $GisReference))) { if ($p[1] -ne '') { $argList += @($p[0], (ConvertTo-QuotedArg $p[1])) } }
    if ($SyntheticDemo) { $argList += '--synthetic-demo' }
    $outLog = Join-Path $RepoDir ("posthoc_console_{0}.tmp" -f $stamp); $errLog = Join-Path $RepoDir ("posthoc_err_{0}.tmp" -f $stamp)
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = [int]$p.ExitCode; $consoleLog = Join-Path $runDir 'run_console.log'
    Move-Item -LiteralPath $outLog -Destination $consoleLog -Force
    if (Test-Path -LiteralPath $errLog) { Add-Content -LiteralPath $consoleLog -Value '--- stderr ---'; Add-Content -LiteralPath $consoleLog -Value (Get-Content -LiteralPath $errLog); Remove-Item -LiteralPath $errLog -Force }
    Add-Content -LiteralPath $consoleLog -Value ('R_EXIT_CODE={0}' -f $rc)
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)
    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_dlm_validation.R')), (ConvertTo-QuotedArg $runDir), [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'; $finErr = Join-Path $RepoDir ("posthoc_fin_err_{0}.tmp" -f $stamp)
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) { Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr); Remove-Item -LiteralPath $finErr -Force }
    $st = @{}; $stFile = Join-Path $runDir 'POSTHOC_STATUS.txt'
    if (Test-Path -LiteralPath $stFile) { foreach ($line in (Get-Content -LiteralPath $stFile)) { if ($line -match '^([A-Z_0-9]+)=(.*)$') { $st[$matches[1]] = $matches[2] } } }
    $res = Resolve-SiewPosthocOutcome -Status $st -RExitCode $rc -Seconds $wall -SyntheticDemo:$SyntheticDemo
    Write-Host ''; Write-Host ('STATUSES ({0}, {1})' -f $(if ($st.ContainsKey('POSTHOC_STEP')) { $st['POSTHOC_STEP'] } else { $Item }), $(if ($st.ContainsKey('POSTHOC_LABEL')) { $st['POSTHOC_LABEL'] } else { '?' }))
    foreach ($r in $res.Rows) { Write-Host ("  {0,-46} {1}" -f $r.Name, $r.Value) -ForegroundColor $r.Colour }
    Write-Host 'PRIMARY INFERENCE: unchanged (q90, q9 and every accepted status are read-only for post hoc steps)'
    Write-Host ''
    if ($res.Kind -eq 'EXECUTION_FAILURE') { $errs = Select-String -LiteralPath $consoleLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3; Write-Host ("{0}; finalise exit code {1}" -f $res.Message, $f.ExitCode) -ForegroundColor $res.Colour; foreach ($e in $errs) { Write-Host ('  ' + $e.Line) } }
    else { Write-Host $res.Message -ForegroundColor $res.Colour }
    Write-Host ("R exit code: {0}; wrapper exit code: {1}" -f $rc, $res.Code)
    Write-Host ("Output folder: {0}" -f $runDir)
    return $res.Code
}
