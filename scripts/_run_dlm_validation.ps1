# Helper for run_dlm_validation.ps1 (dot-sourced; not meant to be run directly). Windows PowerShell 5.1 compatible. Prints no participant-level values.
# Exit codes: 0 = executed and every status PASS; 3 = executed, every tested status PASS, but some components are NOT_TESTED (documented: figure provenance, dlnm if not installed);
#             1 = execution failure; 2 = invalid arguments or R missing; 4 = executed, but a verification, reference comparison, concordance (REVIEW) or source-integrity status FAILED.
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

function Invoke-SiewDlmValidation {
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
        $cand = Join-Path $OutRoot ("dlmvalidate_{0}_{1}" -f $stamp, $suffix)
        if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break }
    }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null
    $runDir = (Resolve-Path -LiteralPath $runDir).Path
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host 'Validating the nine final L01-L56 DLM outputs (accepted fits, audited reconstruction, independent route, Monte Carlo stability). Progress is written to run_console.log in the run folder ...'

    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), 'dlmvalidate', (ConvertTo-QuotedArg $DataCsv), '--dlm-reference', (ConvertTo-QuotedArg $DlmReferenceDir))
    if ($AcceptedRunDir -ne '') { $argList += @('--accepted-run', (ConvertTo-QuotedArg $AcceptedRunDir)) }
    if ($NoMonteCarlo) { $argList += @('--no-mc') }
    if ($SyntheticNoManuscriptConcordance) { $argList += @('--synthetic-no-manuscript-concordance') }
    $argList += @('--out', (ConvertTo-QuotedArg $runDir))
    $outLog = Join-Path $RepoDir ("dlmvalidate_console_{0}.tmp" -f $stamp)
    $errLog = Join-Path $RepoDir ("dlmvalidate_err_{0}.tmp" -f $stamp)
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = $p.ExitCode
    $consoleLog = Join-Path $runDir 'run_console.log'
    Move-Item -LiteralPath $outLog -Destination $consoleLog -Force
    if (Test-Path -LiteralPath $errLog) { Add-Content -LiteralPath $consoleLog -Value '--- stderr ---'; Add-Content -LiteralPath $consoleLog -Value (Get-Content -LiteralPath $errLog); Remove-Item -LiteralPath $errLog -Force }
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)

    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_dlm_validation.R')), (ConvertTo-QuotedArg $runDir), [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'
    $finErr = Join-Path $RepoDir ("dlmvalidate_fin_err_{0}.tmp" -f $stamp)
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) { Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr); Remove-Item -LiteralPath $finErr -Force }

    $st = @{}
    $stFile = Join-Path $runDir 'DLM_VALIDATION_STATUS.txt'
    if (Test-Path -LiteralPath $stFile) { foreach ($line in (Get-Content -LiteralPath $stFile)) { if ($line -match '^([A-Z_0-9]+)=(.*)$') { $st[$matches[1]] = $matches[2] } } }
    Write-Host ''
    $keys = @('EXECUTION_STATUS', 'INPUT_CHECK_STATUS', 'ACCEPTED_FIT_REPRODUCTION_STATUS', 'REFERENCE_COMPARISON_STATUS', 'LAG_BASIS_VERIFICATION_STATUS', 'CURVE_VERIFICATION_STATUS', 'CUMULATIVE_VERIFICATION_STATUS', 'PEAK_LAG_VERIFICATION_STATUS',
              'SIMULTANEOUS_EXACT_SEEDED_STATUS', 'SIMULTANEOUS_MC_STABILITY_STATUS', 'CONVERGENCE_SENSITIVITY_STATUS', 'MANUSCRIPT_CONCORDANCE_STATUS', 'FIGURE_PROVENANCE_STATUS', 'DLNM_EQUALITY_STATUS', 'SOURCE_INTEGRITY_STATUS')
    $bad = $false; $notTested = $false
    foreach ($k in $keys) {
        $v = if ($st.ContainsKey($k)) { $st[$k] } else { 'FAIL' }
        $colour = if ($v -eq 'PASS') { 'Green' } elseif ($v -eq 'NOT_TESTED' -or $v -like 'PASS_PARTIAL*') { 'Yellow' } else { 'Red' }
        Write-Host ("{0,-40} {1}" -f $k, $v) -ForegroundColor $colour
        if ($v -eq 'NOT_TESTED' -or $v -like 'PASS_PARTIAL*') { $notTested = $true } elseif ($v -ne 'PASS') { $bad = $true }
    }
    foreach ($k in @('MODELS', 'CRITICAL_VALUES_EXACT_AGAINST_INDEPENDENT_ROUTE_AT_ACCEPTED_EPSILON')) { if ($st.ContainsKey($k)) { Write-Host ("{0,-40} {1}" -f $k, $st[$k]) } }
    Write-Host ''
    $execOk = $st.ContainsKey('EXECUTION_STATUS') -and $st['EXECUTION_STATUS'] -eq 'PASS'
    if (-not $execOk) {
        Write-Host ("FAIL - EXECUTION FAILED (R exit code {0}, finalise exit code {1})" -f $rc, $f.ExitCode) -ForegroundColor Red
        $errs = Select-String -LiteralPath $consoleLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3
        foreach ($e in $errs) { Write-Host ('  ' + $e.Line) }
        $code = 1
    } elseif ($bad) {
        Write-Host 'FAIL - the validation executed, but at least one verification, reference comparison, concordance or integrity status FAILED or needs REVIEW (see dlm_verification_checks.csv, dlm_reference_validation_detail.csv, dlm_mc_stability.csv, RUN_SUMMARY.md).' -ForegroundColor Red
        $code = 4
    } elseif ($notTested) {
        Write-Host ("OK WITH NOT_TESTED - every tested status passed in {0} s; the components shown as NOT_TESTED have no reference or comparator (see dlm_not_tested.csv). NOT_TESTED is not PASS." -f $wall) -ForegroundColor Yellow
        $code = 3
    } else {
        Write-Host ("PASS - every status passed in {0} s" -f $wall) -ForegroundColor Green
        $code = 0
    }
    Write-Host ("Output folder: {0}" -f $runDir)
    return $code
}
