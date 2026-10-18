<#
.SYNOPSIS
  Tests the PowerShell wrapper of the daily layer (Phase 3H.2) on SYNTHETIC data and references only. No study data. Writes a results table and returns a non-zero exit code if any test fails.
  1. Pure status aggregation (Resolve-SiewDailyLayerOutcome): REVIEW only for MAXT_ASSUMPTIONS_STATUS; failures, missing, NOT_TESTED, unexpected values and R exit codes never accepted.
  2. The real wrapper function with a stub R process (status file and exit code under control).
  3. The real wrapper function with real R on synthetic data: exact reference (exit 3), genuine comparison failure (exit 4), missing evidence (exit 4), R failure (exit 1).
  4. The analytical outputs of the wrapper run equal those of a direct run.R call (hash comparison).
#>
param([string]$WorkDir = (Join-Path $env:TEMP 'siw_wrapper_test'), [string]$ResultCsv = '')
$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent $PSScriptRoot; $rscript = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'
. (Join-Path $repo 'scripts\_run_daily_layer.ps1'); $realR = $script:RscriptExe
if (Test-Path $WorkDir) { Remove-Item $WorkDir -Recurse -Force }; New-Item -ItemType Directory $WorkDir -Force | Out-Null
$results = New-Object System.Collections.ArrayList
function Check { param([string]$Name, $Expected, $Actual, [string]$Note = '') $ok = ($Expected -eq $Actual); [void]$script:results.Add([pscustomobject]@{ test = $Name; expected = [string]$Expected; actual = [string]$Actual; pass = $ok; note = $Note }); $c = if ($ok) { 'Green' } else { 'Red' }; Write-Host ("  [{0}] {1}: expected {2}, got {3}" -f $(if ($ok) { 'ok' } else { 'FAIL' }), $Name, $Expected, $Actual) -ForegroundColor $c }
$allPass = @{ EXECUTION_STATUS = 'PASS'; INPUT_PROVENANCE_STATUS = 'PASS'; XYD_REPRODUCTION_STATUS = 'PASS'; XYD_VERIFICATION_STATUS = 'PASS'; DAY_REPRODUCTION_STATUS = 'PASS'; DAY_VERIFICATION_STATUS = 'PASS'; MAXT_REPRODUCTION_STATUS = 'PASS'; MAXT_VERIFICATION_STATUS = 'PASS'; MAXT_ASSUMPTIONS_STATUS = 'REVIEW'; COVERAGE_STATUS = 'PASS'; SOURCE_INTEGRITY_STATUS = 'PASS' }
function Variant { param([hashtable]$Change = @{}, [string[]]$Remove = @()) $h = @{}; foreach ($k in $allPass.Keys) { $h[$k] = $allPass[$k] }; foreach ($k in $Change.Keys) { $h[$k] = $Change[$k] }; foreach ($k in $Remove) { $h.Remove($k) }; return $h }
Write-Host '1. pure aggregation'
Check 'REVIEW + all PASS + R exit 3' 3 (Resolve-SiewDailyLayerOutcome -Status (Variant) -RExitCode 3).Code
$m = Resolve-SiewDailyLayerOutcome -Status (Variant) -RExitCode 3 -Seconds 5; Check 'message for the REVIEW case' 'OK_WITH_REVIEW' $m.Kind; Check 'message text' $true ($m.Message -like 'OK WITH MAXT ASSUMPTIONS UNDER REVIEW*'); Check 'REVIEW displayed yellow' 'Yellow' (($m.Rows | Where-Object { $_.Name -eq 'MAXT_ASSUMPTIONS_STATUS' }).Colour); Check 'REVIEW value retained' 'REVIEW' (($m.Rows | Where-Object { $_.Name -eq 'MAXT_ASSUMPTIONS_STATUS' }).Value)
Check 'display order (ASSUMPTIONS after MAXT_VERIFICATION)' 'MAXT_ASSUMPTIONS_STATUS' $m.Rows[8].Name; Check 'eleven rows shown' 11 $m.Rows.Count
Check 'all PASS incl. assumptions PASS + R exit 0' 0 (Resolve-SiewDailyLayerOutcome -Status (Variant @{ MAXT_ASSUMPTIONS_STATUS = 'PASS' }) -RExitCode 0).Code
Check 'assumptions status missing' 4 (Resolve-SiewDailyLayerOutcome -Status (Variant -Remove @('MAXT_ASSUMPTIONS_STATUS')) -RExitCode 3).Code
Check 'assumptions FAIL' 4 (Resolve-SiewDailyLayerOutcome -Status (Variant @{ MAXT_ASSUMPTIONS_STATUS = 'FAIL' }) -RExitCode 3).Code
Check 'assumptions NOT_TESTED' 4 (Resolve-SiewDailyLayerOutcome -Status (Variant @{ MAXT_ASSUMPTIONS_STATUS = 'NOT_TESTED' }) -RExitCode 3).Code
Check 'assumptions unexpected value' 4 (Resolve-SiewDailyLayerOutcome -Status (Variant @{ MAXT_ASSUMPTIONS_STATUS = 'PASS_EXACT' }) -RExitCode 3).Code
foreach ($k in @('INPUT_PROVENANCE_STATUS', 'XYD_REPRODUCTION_STATUS', 'XYD_VERIFICATION_STATUS', 'DAY_REPRODUCTION_STATUS', 'DAY_VERIFICATION_STATUS', 'MAXT_REPRODUCTION_STATUS', 'MAXT_VERIFICATION_STATUS', 'COVERAGE_STATUS', 'SOURCE_INTEGRITY_STATUS')) {
    Check ("$k FAIL") 4 (Resolve-SiewDailyLayerOutcome -Status (Variant @{ $k = 'FAIL' }) -RExitCode 3).Code; Check ("$k REVIEW (the exception is only for MAXT_ASSUMPTIONS_STATUS)") 4 (Resolve-SiewDailyLayerOutcome -Status (Variant @{ $k = 'REVIEW' }) -RExitCode 3).Code
    Check ("$k missing") 4 (Resolve-SiewDailyLayerOutcome -Status (Variant -Remove @($k)) -RExitCode 3).Code }
Check 'NOT_TESTED in a required status' 4 (Resolve-SiewDailyLayerOutcome -Status (Variant @{ MAXT_VERIFICATION_STATUS = 'NOT_TESTED' }) -RExitCode 3).Code
Check 'EXECUTION_STATUS FAIL' 1 (Resolve-SiewDailyLayerOutcome -Status (Variant @{ EXECUTION_STATUS = 'FAIL' }) -RExitCode 3).Code
Check 'EXECUTION_STATUS missing' 1 (Resolve-SiewDailyLayerOutcome -Status (Variant -Remove @('EXECUTION_STATUS')) -RExitCode 3).Code
Check 'no status at all (R crashed before writing)' 1 (Resolve-SiewDailyLayerOutcome -Status @{} -RExitCode 1).Code
Check 'good statuses but R exit 1: never accepted' 1 (Resolve-SiewDailyLayerOutcome -Status (Variant) -RExitCode 1).Code
Check 'good statuses but R exit 255: never accepted' 1 (Resolve-SiewDailyLayerOutcome -Status (Variant) -RExitCode 255).Code
Check 'good statuses but R exit 2' 1 (Resolve-SiewDailyLayerOutcome -Status (Variant) -RExitCode 2).Code
Check 'good statuses but R exit 4' 4 (Resolve-SiewDailyLayerOutcome -Status (Variant) -RExitCode 4).Code
Check 'REVIEW but R exit 0 (inconsistent)' 1 (Resolve-SiewDailyLayerOutcome -Status (Variant) -RExitCode 0).Code
Check 'assumptions PASS but R exit 3 (inconsistent)' 1 (Resolve-SiewDailyLayerOutcome -Status (Variant @{ MAXT_ASSUMPTIONS_STATUS = 'PASS' }) -RExitCode 3).Code
Check 'all required PASS, assumptions FAIL and R exit 4' 4 (Resolve-SiewDailyLayerOutcome -Status (Variant @{ MAXT_ASSUMPTIONS_STATUS = 'FAIL' }) -RExitCode 4).Code

# stub R process: copies a prepared status file into --out and exits with a chosen code (run.R call only)
$stub = Join-Path $WorkDir 'stubR.cmd'
@'
@echo off
setlocal
set "OUT="
:l
if "%~1"=="" goto d
if "%~1"=="--out" set "OUT=%~2"
shift
goto l
:d
if defined OUT if defined STUB_STATUS copy /Y "%STUB_STATUS%" "%OUT%\DAILYLAYER_STATUS.txt" >nul
if defined STUB_RC exit /b %STUB_RC%
exit /b 0
'@ | Set-Content -Encoding ASCII $stub
function Write-Status { param([hashtable]$H, [string]$Path) $lines = foreach ($k in ($H.Keys | Sort-Object)) { "{0}={1}" -f $k, $H[$k] }; $lines | Set-Content -Encoding ASCII $Path }
function Run-Wrapper { param([string]$Exe, [string]$Out) $script:RscriptExe = $Exe; New-Item -ItemType Directory $Out -Force | Out-Null; $o = Invoke-SiewDailyLayer -RepoDir $repo -DataCsv (Join-Path $WorkDir 'dummy.csv') -ReferenceDir $WorkDir -OutRoot $Out; return ($o | Select-Object -Last 1) }
'x' | Set-Content (Join-Path $WorkDir 'dummy.csv')
Write-Host '2. wrapper with a stub R process'
$stubCases = @(
    @{ n = 'stub: all PASS + REVIEW, R exit 3'; h = (Variant); rc = 3; e = 3 }, @{ n = 'stub: all PASS incl. assumptions, R exit 0'; h = (Variant @{ MAXT_ASSUMPTIONS_STATUS = 'PASS' }); rc = 0; e = 0 },
    @{ n = 'stub: assumptions status missing'; h = (Variant -Remove @('MAXT_ASSUMPTIONS_STATUS')); rc = 3; e = 4 }, @{ n = 'stub: a required status missing'; h = (Variant -Remove @('XYD_VERIFICATION_STATUS')); rc = 3; e = 4 },
    @{ n = 'stub: genuine failure reported by R (exit 4)'; h = (Variant @{ XYD_REPRODUCTION_STATUS = 'FAIL' }); rc = 4; e = 4 }, @{ n = 'stub: good statuses, R exit 1'; h = (Variant); rc = 1; e = 1 })
$i = 0
foreach ($c in $stubCases) { $i++; $sf = Join-Path $WorkDir ("stub_status_{0}.txt" -f $i); Write-Status $c.h $sf; $env:STUB_STATUS = $sf; $env:STUB_RC = [string]$c.rc; $code = Run-Wrapper -Exe $stub -Out (Join-Path $WorkDir ("stub_out_{0}" -f $i)); Check $c.n $c.e $code
    $log = Get-ChildItem (Join-Path $WorkDir ("stub_out_{0}" -f $i)) -Recurse -Filter run_console.log | Select-Object -First 1; Check ("  run_console.log records R_EXIT_CODE={0}" -f $c.rc) $true ((Get-Content $log.FullName) -contains ('R_EXIT_CODE={0}' -f $c.rc)) }
$i++; $env:STUB_STATUS = ''; $env:STUB_RC = '1'; $code = Run-Wrapper -Exe $stub -Out (Join-Path $WorkDir 'stub_out_nostatus'); Check 'stub: R failure, no status file' 1 $code
Remove-Item Env:\STUB_STATUS -ErrorAction SilentlyContinue; Remove-Item Env:\STUB_RC -ErrorAction SilentlyContinue

Write-Host '3. wrapper with real R on synthetic data'
$shim = Join-Path $WorkDir 'shimR.cmd'
function Make-Shim { param([string]$ProtocolFile) "@echo off`r`n`"$rscript`" %* --protocol-file `"$ProtocolFile`"" | Set-Content -Encoding ASCII $shim }
$hashes = @{}
foreach ($v in @('exact', 'xyd_diff', 'missing_ref', 'broken_data')) {
    $d = Join-Path $WorkDir ("syn_" + $v); & $rscript (Join-Path $repo 'validation\make_daily_layer_synthetic.R') $d $v 2>$null | Out-Null; Make-Shim (Join-Path $d 'protocol_syn.json')
    $script:RscriptExe = $shim; $out = Join-Path $WorkDir ("out_" + $v); New-Item -ItemType Directory $out -Force | Out-Null
    $o = Invoke-SiewDailyLayer -RepoDir $repo -DataCsv (Join-Path $d 'syn.csv') -ReferenceDir (Join-Path $d 'ref') -OutRoot $out; $code = ($o | Select-Object -Last 1)
    $exp = @{ exact = 3; xyd_diff = 4; missing_ref = 4; broken_data = 1 }[$v]; Check ("real R, synthetic variant '{0}'" -f $v) $exp $code
    $rd = Get-ChildItem $out -Directory | Select-Object -First 1; $rl = Get-Content (Join-Path $rd.FullName 'run_console.log'); $rcl = ($rl | Where-Object { $_ -like 'R_EXIT_CODE=*' }); Check ("  run_console.log records the actual R exit code ({0})" -f $v) $true ($rcl -ne $null); Write-Host ("     {0}" -f ($rcl -join ' '))
    if ($v -eq 'exact') { $hashes['wrapper'] = $rd.FullName } }
# R execution failure through the shim: R replaced by a failing executable
$fail = Join-Path $WorkDir 'failR.cmd'; "@echo off`r`nexit /b 1" | Set-Content -Encoding ASCII $fail; $script:RscriptExe = $fail; $out = Join-Path $WorkDir 'out_rfail'; New-Item -ItemType Directory $out -Force | Out-Null
$o = Invoke-SiewDailyLayer -RepoDir $repo -DataCsv (Join-Path $WorkDir 'syn_exact\syn.csv') -ReferenceDir (Join-Path $WorkDir 'syn_exact\ref') -OutRoot $out; Check 'R process fails (exit 1, no outputs)' 1 ($o | Select-Object -Last 1)

Write-Host '4. analytical outputs unchanged by the wrapper (wrapper run vs direct run.R)'
$ErrorActionPreference = 'Continue'; $d = Join-Path $WorkDir 'syn_exact'; $direct =Join-Path $WorkDir 'direct_exact'; & $rscript (Join-Path $repo 'run.R') dailylayer (Join-Path $d 'syn.csv') --reference-dir (Join-Path $d 'ref') --out $direct --protocol-file (Join-Path $d 'protocol_syn.json') 2>$null | Out-Null; Check 'direct run.R exit code' 3 $LASTEXITCODE
$files = @('xyd_package_results.csv', 'day_package_results.csv', 'maxt_package_results.csv', 'maxt_replicate_accounting.csv', 'xyd_comparison_summary.csv', 'day_comparison_summary.csv', 'maxt_comparison_summary.csv', 'dailylayer_comparison_coverage.csv', 'xyd_categorical_comparison.csv', 'day_categorical_comparison.csv', 'xyd_independent_verification.csv', 'day_independent_refit.csv', 'maxt_engine_verification.csv', 'maxt_assumption_assessment.csv', 'day_multiplicity_verification.csv', 'xyd_multiplicity_verification.csv')
foreach ($f in $files) { $a = (Get-FileHash (Join-Path $hashes['wrapper'] $f)).Hash; $b = (Get-FileHash (Join-Path $direct $f)).Hash; Check ("identical: {0}" -f $f) $b $a }
$sa = (Get-Content (Join-Path $hashes['wrapper'] 'DAILYLAYER_STATUS.txt') | Where-Object { $_ -notlike 'RUNTIME_SECONDS=*' }) -join "`n"; $sb = (Get-Content (Join-Path $direct 'DAILYLAYER_STATUS.txt') | Where-Object { $_ -notlike 'RUNTIME_SECONDS=*' }) -join "`n"; Check 'identical: DAILYLAYER_STATUS.txt (without the runtime line)' $sb $sa
$script:RscriptExe = $realR
$n = $results.Count; $np = ($results | Where-Object { $_.pass }).Count; Write-Host ("`nTESTS: {0}; PASSED: {1}; FAILED: {2}" -f $n, $np, ($n - $np))
if ($ResultCsv -ne '') { $results | Export-Csv -NoTypeInformation -Encoding UTF8 -Path $ResultCsv }
exit $(if ($n -eq $np) { 0 } else { 1 })
