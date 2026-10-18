# Shared helper for run_demo.ps1, run_paper.ps1, run_validate.ps1 (dot-sourced; not meant to be run directly).
# Windows PowerShell 5.1 compatible. Prints no participant-level values.
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'

function ConvertTo-QuotedArg {
    param([string]$Text)
    return '"' + ($Text -replace '"', '\"') + '"'
}

function Invoke-SiewRun {
    param(
        [Parameter(Mandatory = $true)][string]$RepoDir,
        [Parameter(Mandatory = $true)][ValidateSet('demo', 'paper', 'validate')][string]$Mode,
        [string[]]$ModeArgs = @(),
        [string]$OutRoot = ''
    )
    $ErrorActionPreference = 'Stop'
    if (-not (Test-Path -LiteralPath $script:RscriptExe -PathType Leaf)) {
        Write-Host ("FAIL: R was not found at {0}" -f $script:RscriptExe) -ForegroundColor Red
        return 2
    }
    if (-not (Test-Path -LiteralPath (Join-Path $RepoDir 'run.R') -PathType Leaf)) {
        Write-Host ("FAIL: run.R not found in {0}" -f $RepoDir) -ForegroundColor Red
        return 2
    }
    if ([string]::IsNullOrWhiteSpace($OutRoot)) { $OutRoot = Join-Path $RepoDir 'runs' }

    # unique run directory; an existing directory is never reused
    $stamp = Get-Date -Format 'yyyyMMdd_HHmmss'
    $runDir = $null
    for ($i = 0; $i -lt 50; $i++) {
        $suffix = ('{0:x4}' -f (Get-Random -Maximum 65536))
        $cand = Join-Path $OutRoot ("{0}_{1}_{2}" -f $Mode, $stamp, $suffix)
        if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break }
    }
    if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run directory' -ForegroundColor Red; return 2 }
    New-Item -ItemType Directory -Path $runDir -Force | Out-Null
    $runDir = (Resolve-Path -LiteralPath $runDir).Path

    Write-Host ("Mode: {0}" -f $Mode)
    Write-Host ("Run folder: {0}" -f $runDir)
    Write-Host 'Running (progress is written to run_console.log in the run folder) ...'

    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $argList = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'run.R')), $Mode)
    foreach ($a in $ModeArgs) { $argList += (ConvertTo-QuotedArg $a) }
    $argList += @('--out', (ConvertTo-QuotedArg $runDir))
    $outLog = Join-Path $runDir 'run_console.log'
    $errLog = Join-Path $runDir 'run_console_err.tmp'
    $p = Start-Process -FilePath $script:RscriptExe -ArgumentList $argList -WorkingDirectory $RepoDir `
        -NoNewWindow -Wait -PassThru -RedirectStandardOutput $outLog -RedirectStandardError $errLog
    $rc = $p.ExitCode
    if (Test-Path -LiteralPath $errLog) {
        Add-Content -LiteralPath $outLog -Value '--- stderr ---'
        Add-Content -LiteralPath $outLog -Value (Get-Content -LiteralPath $errLog)
        Remove-Item -LiteralPath $errLog -Force
    }
    $wall = [math]::Round($sw.Elapsed.TotalSeconds, 1)

    # standard hand-off files (also written when the run failed)
    $finArgs = @((ConvertTo-QuotedArg (Join-Path $RepoDir 'scripts\finalise_run.R')), (ConvertTo-QuotedArg $runDir), $Mode, [string]$rc, [string]$wall)
    $finOut = Join-Path $runDir 'finalise_console.log'
    $finErr = Join-Path $runDir 'finalise_err.tmp'
    $f = Start-Process -FilePath $script:RscriptExe -ArgumentList $finArgs -WorkingDirectory $RepoDir `
        -NoNewWindow -Wait -PassThru -RedirectStandardOutput $finOut -RedirectStandardError $finErr
    if (Test-Path -LiteralPath $finErr) {
        Add-Content -LiteralPath $finOut -Value (Get-Content -LiteralPath $finErr)
        Remove-Item -LiteralPath $finErr -Force
    }
    $finalOk = ($rc -eq 0) -and ($f.ExitCode -eq 0)

    Write-Host ''
    if ($finalOk) {
        Write-Host ("PASS - {0} run completed in {1} s" -f $Mode, $wall) -ForegroundColor Green
    } else {
        Write-Host ("FAIL - {0} run did not pass (R exit code {1}, finalise exit code {2})" -f $Mode, $rc, $f.ExitCode) -ForegroundColor Red
        $errs = Select-String -LiteralPath $outLog -Pattern '^ERROR' -ErrorAction SilentlyContinue | Select-Object -First 3
        foreach ($e in $errs) { Write-Host ('  ' + $e.Line) }
        Write-Host '  See RUN_SUMMARY.md, VALIDATION_STATUS.csv and run_console.log in the run folder.'
    }
    Write-Host ("Output folder: {0}" -f $runDir)
    if ($finalOk) { return 0 } else { return 1 }
}
