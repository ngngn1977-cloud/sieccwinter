# Helper for run_batch_b.ps1 (dot-sourced; not meant to be run directly). Windows PowerShell 5.1 compatible. Prints no participant-level values.
# Exit codes: 0 = executed and everything passed; 1 = execution failure; 2 = invalid arguments or R missing;
#             3 = executed, every comparison and verification passed, with at least one documented legacy limitation; 4 = executed, but an input check, a numerical comparison or a mathematical verification FAILED.
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

function Invoke-SiewBatchBRun {
    param(
        [Parameter(Mandatory = $true)][string]$RepoDir,
        [Parameter(Mandatory = $true)][string]$DataCsv,
        [Parameter(Mandatory = $true)][string]$CoreReferenceDir,
        [Parameter(Mandatory = $true)][string]$BatchAReferenceDir,
        [Parameter(Mandatory = $true)][string]$BatchBReferenceDir,
        [string]$ExpectedAssessmentDays = '',
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
        $cand = Join-Path $OutRoot ("batchb_{0}_{1}" -f $stamp, $suffix)
        if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break }
    }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null
    $runDir = (Resolve-Path -LiteralPath $runDir).Path
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host 'Running core analyses + Batch A + Batch B + the three comparisons (progress is written to run_console.log in the run folder) ...'

    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), 'batchb', (ConvertTo-QuotedArg $DataCsv),
        '--core-reference', (ConvertTo-QuotedArg $CoreReferenceDir), '--batch-a-reference', (ConvertTo-QuotedArg $BatchAReferenceDir), '--batch-b-reference', (ConvertTo-QuotedArg $BatchBReferenceDir))
    if ($ExpectedAssessmentDays -ne '') { $argList += @('--expected-assessment-days', $ExpectedAssessmentDays) }
    $argList += @('--out', (ConvertTo-QuotedArg $runDir))
    $outLog = Join-Path $runDir 'run_console.log'
    $errLog = Join-Path $runDir 'run_console_err.tmp'
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = $p.ExitCode
    if (Test-Path -LiteralPath $errLog) {
        Add-Content -LiteralPath $outLog -Value '--- stderr ---'
        Add-Content -LiteralPath $outLog -Value (Get-Content -LiteralPath $errLog)
        Remove-Item -LiteralPath $errLog -Force
    }
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)

    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_batch_b.R')), (ConvertTo-QuotedArg $runDir), [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'
    $finErr = Join-Path $runDir 'finalise_err.tmp'
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) {
        Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr)
        Remove-Item -LiteralPath $finErr -Force
    }

    # statuses (aggregate lines only)
    $st = @{}
    $stFile = Join-Path $runDir 'BATCH_B_STATUS.txt'
    if (Test-Path -LiteralPath $stFile) {
        foreach ($line in (Get-Content -LiteralPath $stFile)) { if ($line -match '^([A-Z_0-9]+)=(.*)$') { $st[$matches[1]] = $matches[2] } }
    }
    function Show-Status($label, $key) {
        $v = if ($st.ContainsKey($key)) { $st[$key] } else { 'FAIL' }
        $colour = if ($v -eq 'PASS') { 'Green' } elseif ($v -like 'PASS_WITH*') { 'Yellow' } else { 'Red' }
        Write-Host ("{0,-38} {1}" -f $label, $v) -ForegroundColor $colour
    }
    Write-Host ''
    Show-Status 'EXECUTION' 'EXECUTION_STATUS'
    Show-Status 'INPUT CHECKS' 'INPUT_CHECK_STATUS'
    Show-Status 'CORE NUMERICAL COMPARISON' 'CORE_COMPARISON_STATUS'
    Show-Status 'BATCH A NUMERICAL COMPARISON' 'BATCH_A_COMPARISON_STATUS'
    Show-Status 'BATCH B NUMERICAL COMPARISON' 'BATCH_B_COMPARISON_STATUS'
    Show-Status 'BATCH B MATHEMATICAL VERIFICATION' 'BATCH_B_MATH_VERIFICATION_STATUS'
    Show-Status '  Firth verification' 'MATH_FIRTH_STATUS'
    Show-Status '  ordinal verification' 'MATH_ORDINAL_STATUS'
    Show-Status '  transformation verification' 'MATH_TRANSFORMATIONS_STATUS'
    Show-Status '  nonlinear verification' 'MATH_NONLINEAR_STATUS'
    Write-Host ''
    $execOk = $st.ContainsKey('EXECUTION_STATUS') -and $st['EXECUTION_STATUS'] -eq 'PASS'
    if ($execOk -and ($f.ExitCode -eq 0) -and ($rc -eq 0)) {
        Write-Host ("PASS - executed and everything passed in {0} s" -f $wall) -ForegroundColor Green
        $code = 0
    } elseif ($execOk -and ($f.ExitCode -eq 3) -and ($rc -eq 3)) {
        Write-Host ("PASS WITH DOCUMENTED LEGACY LIMITATION - executed and every comparison and verification passed in {0} s; see RUN_SUMMARY.md (batch_b_validation_summary.csv, firth_attribution.csv, ordinal_verification.csv)" -f $wall) -ForegroundColor Yellow
        $code = 3
    } elseif (-not $execOk) {
        Write-Host ("FAIL - EXECUTION FAILED (R exit code {0}, finalise exit code {1})" -f $rc, $f.ExitCode) -ForegroundColor Red
        $errs = Select-String -LiteralPath $outLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3
        foreach ($e in $errs) { Write-Host ('  ' + $e.Line) }
        $code = 1
    } else {
        Write-Host 'FAIL - the run executed, but an input check, a numerical comparison or a mathematical verification FAILED (see RUN_SUMMARY.md, VALIDATION_STATUS.csv, validation_detail.csv, batch_a_validation_detail.csv, batch_b_validation_detail.csv, batch_b_math_verification.csv).' -ForegroundColor Red
        $code = 4
    }
    Write-Host ("Output folder: {0}" -f $runDir)
    return $code
}
