<#
.SYNOPSIS
  Tests the PowerShell wrapper of the anomaly / alternative-history run (Phase 3I) on SYNTHETIC data and references only. No study data. Writes a results table and returns a non-zero exit code if any test fails.
  1. Pure status aggregation (Resolve-SiewAnomHistOutcome): only PASS is accepted (no REVIEW exception); failures, missing, NOT_TESTED, unexpected values, REVIEW and R exit codes are never accepted.
  2. The real wrapper function with a stub R process (status file and exit code under control).
  3. The real wrapper function with real R on synthetic data: success (exit 0), genuine comparison failures (exit 4), a principal-component sign difference (exit 4), missing evidence (exit 4), a duplicated reference key (exit 4), unusable data (exit 1), R failure (exit 1).
  4. The separate hand-off file HANDOFF_OUTCOME.txt records both outcomes and does not invalidate RUN_MANIFEST.csv.
  5. The analytical outputs of the wrapper run equal those of a direct run.R call (hash comparison).
#>
param([string]$WorkDir = (Join-Path $env:TEMP 'siw_ah_wrapper_test'), [string]$ResultCsv = '')
$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent $PSScriptRoot; $rscript = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'
. (Join-Path $repo 'scripts\_run_anomaly_history.ps1'); $realR = $script:RscriptExe
if (Test-Path $WorkDir) { Remove-Item $WorkDir -Recurse -Force }; New-Item -ItemType Directory $WorkDir -Force | Out-Null
$results = New-Object System.Collections.ArrayList
function Check { param([string]$Name, $Expected, $Actual, [string]$Note = '') $ok = ($Expected -eq $Actual); [void]$script:results.Add([pscustomobject]@{ test = $Name; expected = [string]$Expected; actual = [string]$Actual; pass = $ok; note = $Note }); $c = if ($ok) { 'Green' } else { 'Red' }; Write-Host ("  [{0}] {1}: expected {2}, got {3}" -f $(if ($ok) { 'ok' } else { 'FAIL' }), $Name, $Expected, $Actual) -ForegroundColor $c }
$allPass = @{}; foreach ($k in $script:AnomHistRequired) { $allPass[$k] = 'PASS' }
function Variant { param([hashtable]$Change = @{}, [string[]]$Remove = @()) $h = @{}; foreach ($k in $allPass.Keys) { $h[$k] = $allPass[$k] }; foreach ($k in $Change.Keys) { $h[$k] = $Change[$k] }; foreach ($k in $Remove) { $h.Remove($k) }; return $h }
Write-Host '1. pure aggregation'
$m = Resolve-SiewAnomHistOutcome -Status (Variant) -RExitCode 0 -Seconds 5; Check 'all PASS + R exit 0' 0 $m.Code; Check 'kind PASS' 'PASS' $m.Kind; Check 'message text' $true ($m.Message -like 'PASS - ANOM and ALT reproduced and verified*'); Check 'eight rows shown' 8 $m.Rows.Count; Check 'rows green' $true (@($m.Rows | Where-Object { $_.Colour -ne 'Green' }).Count -eq 0)
foreach ($k in $script:AnomHistRequired) { if ($k -eq 'EXECUTION_STATUS') { continue }
    Check ("$k FAIL") 4 (Resolve-SiewAnomHistOutcome -Status (Variant @{ $k = 'FAIL' }) -RExitCode 4).Code; Check ("$k FAIL with a wrong R exit 0") 4 (Resolve-SiewAnomHistOutcome -Status (Variant @{ $k = 'FAIL' }) -RExitCode 0).Code
    Check ("$k REVIEW (no REVIEW exception exists in this batch)") 4 (Resolve-SiewAnomHistOutcome -Status (Variant @{ $k = 'REVIEW' }) -RExitCode 4).Code; Check ("$k NOT_TESTED") 4 (Resolve-SiewAnomHistOutcome -Status (Variant @{ $k = 'NOT_TESTED' }) -RExitCode 4).Code
    Check ("$k unexpected value") 4 (Resolve-SiewAnomHistOutcome -Status (Variant @{ $k = 'PASS_EXACT' }) -RExitCode 4).Code; Check ("$k missing") 4 (Resolve-SiewAnomHistOutcome -Status (Variant -Remove @($k)) -RExitCode 4).Code }
Check 'EXECUTION_STATUS FAIL' 1 (Resolve-SiewAnomHistOutcome -Status (Variant @{ EXECUTION_STATUS = 'FAIL' }) -RExitCode 4).Code
Check 'EXECUTION_STATUS missing' 1 (Resolve-SiewAnomHistOutcome -Status (Variant -Remove @('EXECUTION_STATUS')) -RExitCode 4).Code
Check 'no status at all (R crashed before writing)' 1 (Resolve-SiewAnomHistOutcome -Status @{} -RExitCode 1).Code
foreach ($rc in @(1, 2, 3, 255)) { Check ("good statuses but R exit ${rc}: never accepted") 1 (Resolve-SiewAnomHistOutcome -Status (Variant) -RExitCode $rc).Code }
Check 'good statuses but R exit 4 (inconsistent, still not accepted)' 4 (Resolve-SiewAnomHistOutcome -Status (Variant) -RExitCode 4).Code
Check 'a failed status is shown red' 'Red' (((Resolve-SiewAnomHistOutcome -Status (Variant @{ ALT_REPRODUCTION_STATUS = 'FAIL' }) -RExitCode 4).Rows | Where-Object { $_.Name -eq 'ALT_REPRODUCTION_STATUS' }).Colour)
Check 'NOT_TESTED is shown yellow' 'Yellow' (((Resolve-SiewAnomHistOutcome -Status (Variant @{ ALT_REPRODUCTION_STATUS = 'NOT_TESTED' }) -RExitCode 4).Rows | Where-Object { $_.Name -eq 'ALT_REPRODUCTION_STATUS' }).Colour)

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
if defined OUT if defined STUB_STATUS copy /Y "%STUB_STATUS%" "%OUT%\ANOMHIST_STATUS.txt" >nul
if defined STUB_RC exit /b %STUB_RC%
exit /b 0
'@ | Set-Content -Encoding ASCII $stub
function Write-Status { param([hashtable]$H, [string]$Path) $lines = foreach ($k in ($H.Keys | Sort-Object)) { "{0}={1}" -f $k, $H[$k] }; $lines | Set-Content -Encoding ASCII $Path }
function Run-Wrapper { param([string]$Exe, [string]$Out) $script:RscriptExe = $Exe; New-Item -ItemType Directory $Out -Force | Out-Null; $o = Invoke-SiewAnomalyHistory -RepoDir $repo -DataCsv (Join-Path $WorkDir 'dummy.csv') -ReferenceDir $WorkDir -OutRoot $Out; return ($o | Select-Object -Last 1) }
'x' | Set-Content (Join-Path $WorkDir 'dummy.csv')
Write-Host '2. wrapper with a stub R process'
$stubCases = @(
    @{ n = 'stub: all PASS, R exit 0'; h = (Variant); rc = 0; e = 0 }, @{ n = 'stub: all PASS, R exit 3 (inconsistent)'; h = (Variant); rc = 3; e = 1 }, @{ n = 'stub: a status missing'; h = (Variant -Remove @('ALT_VERIFICATION_STATUS')); rc = 4; e = 4 },
    @{ n = 'stub: genuine failure reported by R (exit 4)'; h = (Variant @{ ANOM_REPRODUCTION_STATUS = 'FAIL' }); rc = 4; e = 4 }, @{ n = 'stub: good statuses, R exit 1'; h = (Variant); rc = 1; e = 1 }, @{ n = 'stub: REVIEW is not accepted'; h = (Variant @{ COVERAGE_STATUS = 'REVIEW' }); rc = 0; e = 4 })
$i = 0
foreach ($c in $stubCases) { $i++; $sf = Join-Path $WorkDir ("stub_status_{0}.txt" -f $i); Write-Status $c.h $sf; $env:STUB_STATUS = $sf; $env:STUB_RC = [string]$c.rc; $outd = Join-Path $WorkDir ("stub_out_{0}" -f $i); $code = Run-Wrapper -Exe $stub -Out $outd; Check $c.n $c.e $code
    $rd = Get-ChildItem $outd -Directory | Select-Object -First 1; Check ("  run_console.log records R_EXIT_CODE={0}" -f $c.rc) $true ((Get-Content (Join-Path $rd.FullName 'run_console.log')) -contains ('R_EXIT_CODE={0}' -f $c.rc))
    $hf = Get-Content (Join-Path $rd.FullName 'HANDOFF_OUTCOME.txt'); Check '  hand-off file records the R exit code' $true ($hf -contains ('R_EXIT_CODE={0}' -f $c.rc)); Check '  hand-off file records the wrapper exit code' $true ($hf -contains ('WRAPPER_EXIT_CODE={0}' -f $c.e)) }
$env:STUB_STATUS = ''; $env:STUB_RC = '1'; $code = Run-Wrapper -Exe $stub -Out (Join-Path $WorkDir 'stub_out_nostatus'); Check 'stub: R failure, no status file' 1 $code
Remove-Item Env:\STUB_STATUS -ErrorAction SilentlyContinue; Remove-Item Env:\STUB_RC -ErrorAction SilentlyContinue

Write-Host '3. wrapper with real R on synthetic data (success, failures, missing evidence, duplicated key, unusable data)'
$script:RscriptExe = $realR; $ok = @{}
foreach ($v in @('exact', 'anom_diff', 'alt_diff', 'pca_sign', 'missing_ref', 'dup_key', 'broken_data')) {
    $d = Join-Path $WorkDir ("syn_" + $v); & $rscript (Join-Path $repo 'validation\make_anomaly_history_synthetic.R') $d $v 2>$null | Out-Null
    $out = Join-Path $WorkDir ("out_" + $v); New-Item -ItemType Directory $out -Force | Out-Null
    $o = Invoke-SiewAnomalyHistory -RepoDir $repo -DataCsv (Join-Path $d 'syn.csv') -ReferenceDir (Join-Path $d 'ref') -OutRoot $out -ProtocolFile (Join-Path $d 'protocol_syn.json'); $code = ($o | Select-Object -Last 1)
    $exp = @{ exact = 0; anom_diff = 4; alt_diff = 4; pca_sign = 4; missing_ref = 4; dup_key = 4; broken_data = 1 }[$v]; Check ("real R, synthetic variant '{0}'" -f $v) $exp $code
    $rd = Get-ChildItem $out -Directory | Select-Object -First 1; $rl = Get-Content (Join-Path $rd.FullName 'run_console.log'); $rcl = ($rl | Where-Object { $_ -like 'R_EXIT_CODE=*' }); $rexp = if ($v -eq 'exact') { 0 } elseif ($v -eq 'broken_data') { 1 } else { 4 }
    Check ("  run_console.log records the actual R exit code ({0})" -f $v) ('R_EXIT_CODE=' + $rexp) $rcl
    $hf = Get-Content (Join-Path $rd.FullName 'HANDOFF_OUTCOME.txt'); Check ("  hand-off file: R and wrapper outcomes ({0})" -f $v) $true (($hf -contains ('R_EXIT_CODE=' + $rexp)) -and ($hf -contains ('WRAPPER_EXIT_CODE=' + $exp)))
    if ($v -ne 'broken_data') { $ok[$v] = $rd.FullName }
    if ($v -eq 'pca_sign') { $sg = Import-Csv (Join-Path $rd.FullName 'alt_pca_sign_diagnostic.csv'); Check '  the sign flip is shown in the sign diagnostic' 1 @($sg | Where-Object { $_.sign_equal -eq 'FALSE' }).Count } }
# R execution failure through a failing executable
$fail = Join-Path $WorkDir 'failR.cmd'; "@echo off`r`nexit /b 1" | Set-Content -Encoding ASCII $fail; $script:RscriptExe = $fail; $out = Join-Path $WorkDir 'out_rfail'; New-Item -ItemType Directory $out -Force | Out-Null
$o = Invoke-SiewAnomalyHistory -RepoDir $repo -DataCsv (Join-Path $WorkDir 'syn_exact\syn.csv') -ReferenceDir (Join-Path $WorkDir 'syn_exact\ref') -OutRoot $out; Check 'R process fails (exit 1, no outputs)' 1 ($o | Select-Object -Last 1)
$rd = Get-ChildItem $out -Directory | Select-Object -First 1; Check '  hand-off file exists although R failed' $true (Test-Path (Join-Path $rd.FullName 'HANDOFF_OUTCOME.txt')); $script:RscriptExe = $realR
$emptyRef = Join-Path $WorkDir 'emptyref'; New-Item -ItemType Directory $emptyRef -Force | Out-Null
$o = Invoke-SiewAnomalyHistory -RepoDir $repo -DataCsv (Join-Path $WorkDir 'dummy.csv') -ReferenceDir $WorkDir -OutRoot (Join-Path $WorkDir 'out_overlap') -ProtocolFile ''; Check 'output folder inside the reference folder is refused by R (exit 1)' 1 ($o | Select-Object -Last 1)
$o = Invoke-SiewAnomalyHistory -RepoDir $repo -DataCsv (Join-Path $WorkDir 'dummy.csv') -ReferenceDir $emptyRef -OutRoot (Join-Path $WorkDir 'out_arg') -ProtocolFile ''; Check 'production protocol with a reference folder lacking the four frozen files: nothing compared, exit 4' 4 ($o | Select-Object -Last 1)

Write-Host '4. hand-off file and manifest'
$rd = $ok['exact']; $man = Import-Csv (Join-Path $rd 'RUN_MANIFEST.csv'); $bad = 0; foreach ($r in $man) { $f = Join-Path $rd $r.file; if ((Get-FileHash $f -Algorithm SHA256).Hash -ne $r.hash -or (Get-Item $f).Length -ne [int64]$r.bytes) { $bad++ } }
Check 'RUN_MANIFEST.csv verifies after the hand-off file was written (mismatches)' 0 $bad; Check 'HANDOFF_OUTCOME.txt is not listed in the manifest' 0 @($man | Where-Object { $_.file -eq 'HANDOFF_OUTCOME.txt' }).Count; Check 'HANDOFF_OUTCOME.txt exists' $true (Test-Path (Join-Path $rd 'HANDOFF_OUTCOME.txt'))
$hf = Get-Content (Join-Path $rd 'HANDOFF_OUTCOME.txt'); Check 'hand-off file records the manifest hash at writing' $true ($hf -contains ('RUN_MANIFEST_SHA256_AT_WRITE=' + (Get-FileHash (Join-Path $rd 'RUN_MANIFEST.csv') -Algorithm SHA256).Hash)); Check 'hand-off file lists the eight statuses' 8 @($hf | Where-Object { $_ -match '_STATUS(_V2)?=PASS$' }).Count; Check 'hand-off file: ANOM_VERIFICATION_STATUS_V2 is required and PASS' 1 @($hf | Where-Object { $_ -eq 'ANOM_VERIFICATION_STATUS_V2=PASS' }).Count; Check 'hand-off file: the v1 status is recorded as superseded' 1 @($hf | Where-Object { $_ -like 'ANOM_VERIFICATION_V1_SUPERSEDED=PASS (v1 composition, kept for the record; not counted)' }).Count

Write-Host '5. analytical outputs unchanged by the wrapper (wrapper run vs direct run.R)'
$ErrorActionPreference = 'Continue'; $d = Join-Path $WorkDir 'syn_exact'; $direct = Join-Path $WorkDir 'direct_exact'; & $rscript (Join-Path $repo 'run.R') anomhist (Join-Path $d 'syn.csv') --reference-dir (Join-Path $d 'ref') --out $direct --protocol-file (Join-Path $d 'protocol_syn.json') 2>$null | Out-Null; Check 'direct run.R exit code' 0 $LASTEXITCODE
$files = @('anom_package_results.csv', 'alt_package_results.csv', 'anom_diagnostics_package.csv', 'alt_pca_variance_package.csv', 'anom_comparison_summary.csv', 'alt_comparison_summary.csv', 'anomdiag_comparison_summary.csv', 'altpca_comparison_summary.csv', 'anomhist_comparison_coverage.csv', 'anom_categorical_comparison.csv', 'anom_independent_anomaly_check.csv', 'anom_feature_verification.csv', 'anom_independent_refit_summary.csv', 'anom_multiplicity_verification.csv', 'alt_feature_verification.csv', 'alt_refit_verification.csv', 'alt_pca_verification.csv', 'alt_multiplicity_verification.csv', 'alt_pca_sign_diagnostic.csv', 'anom_scale_and_inference_identities.csv')
foreach ($f in $files) { $a = (Get-FileHash (Join-Path $ok['exact'] $f)).Hash; $b = (Get-FileHash (Join-Path $direct $f)).Hash; Check ("identical: {0}" -f $f) $b $a }
$sa = (Get-Content (Join-Path $ok['exact'] 'ANOMHIST_STATUS.txt') | Where-Object { $_ -notlike 'RUNTIME_SECONDS=*' -and $_ -notlike 'OUTPUT_DIR=*' }) -join "`n"; $sb = (Get-Content (Join-Path $direct 'ANOMHIST_STATUS.txt') | Where-Object { $_ -notlike 'RUNTIME_SECONDS=*' -and $_ -notlike 'OUTPUT_DIR=*' }) -join "`n"; Check 'identical: ANOMHIST_STATUS.txt (without the runtime line)' $sb $sa
$script:RscriptExe = $realR
$n = $results.Count; $np = ($results | Where-Object { $_.pass }).Count; Write-Host ("`nTESTS: {0}; PASSED: {1}; FAILED: {2}" -f $n, $np, ($n - $np))
if ($ResultCsv -ne '') { $results | Export-Csv -NoTypeInformation -Encoding UTF8 -Path $ResultCsv }
exit $(if ($n -eq $np) { 0 } else { 1 })
