<#
.SYNOPSIS
  Tests run_all.ps1 / scripts\_run_full.ps1 on SYNTHETIC data only (no study data, no private folder). Writes under -WorkDir (default %TEMP%\siw_fullrun_test).
  1. Pure functions: step class from each wrapper's documented exit codes; overall outcome; folder-overlap guard.
  2. Real PowerShell processes: demo success (exit 0, summary, manifest written last and verifying, statuses), argument refusals (exit 2),
     fail-closed input hash in study mode (exit 2, no run folder), disk guard (exit 2), completed run never resumed (exit 2).
  3. Resume: an incomplete run reuses completed steps and reruns only the missing one; a tampered restored step and changed parameters are refused.
  4. Step failures through stub child processes (real orchestration code): FAIL (exit 4), execution failure (exit 1, no manifest, then resumed to exit 0), undocumented exit code.
.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File .\validation\test_run_all_wrapper.ps1
#>
param([string]$WorkDir = (Join-Path $env:TEMP 'siw_fullrun_test'), [string]$ResultCsv = '', [switch]$Keep)
$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent $PSScriptRoot
. (Join-Path $repo 'scripts\_run_full.ps1'); $realR = $script:RscriptExe; $realPS = $script:PowerShellExe
function Remove-Tree { param([string]$D) if (Test-Path -LiteralPath $D) { & cmd.exe /d /c rd /s /q ('\\?\' + [System.IO.Path]::GetFullPath($D)) } }   # rd with \\?\ removes paths longer than MAX_PATH
Remove-Tree $WorkDir; New-Item -ItemType Directory $WorkDir -Force | Out-Null
$WorkDir = (Resolve-Path $WorkDir).Path
$results = New-Object System.Collections.ArrayList
function Check { param([string]$Name, $Expected, $Actual) $ok = ([string]$Expected -eq [string]$Actual); [void]$script:results.Add([pscustomobject]@{ test = $Name; expected = [string]$Expected; actual = [string]$Actual; pass = $ok }); Write-Host ("  [{0}] {1}: expected {2}, got {3}" -f $(if ($ok) { 'ok' } else { 'FAIL' }), $Name, $Expected, $Actual) -ForegroundColor $(if ($ok) { 'Green' } else { 'Red' }) }
function Run-All { param([string[]]$A) $o = & $realPS -NoProfile -ExecutionPolicy Bypass -File (Join-Path $repo 'run_all.ps1') @A 2>&1 | ForEach-Object { [string]$_ }; return [pscustomobject]@{ Code = $LASTEXITCODE; Out = $o } }
function Step { param([string]$Name) return ($script:StepTable | Where-Object { $_.Name -eq $Name }) }
function Verify-Manifest { param([string]$Dir) $m = Import-Csv (Join-Path $Dir 'FULL_RUN_MANIFEST.csv'); $bad = 0; foreach ($r in $m) { $f = Join-Path $Dir $r.file; if (-not (Test-Path -LiteralPath $f) -or (Get-SiewSha256 $f) -ne $r.sha256) { $bad++ } }; return $bad }

Write-Host '1. pure functions'
foreach ($n in @('BATCH_A', 'ORDINAL_REFINED', 'BATCH_C', 'ANOM_ALT')) {
    Check "$n exit 0" 'PASS' (Resolve-SiewStepClass (Step $n) 0).Class; Check "$n exit 4" 'FAIL' (Resolve-SiewStepClass (Step $n) 4).Class; Check "$n exit 1" 'EXECUTION_FAILURE' (Resolve-SiewStepClass (Step $n) 1).Class
    Check "$n exit 3 is not documented for this wrapper (never REVIEW, never PASS)" 'EXECUTION_FAILURE' (Resolve-SiewStepClass (Step $n) 3).Class
}
foreach ($n in @('BATCH_B', 'DLM_V2', 'DLM_V21', 'ARCHIVED_DLM', 'COLD_BURDEN', 'DAILY_LAYER')) { Check "$n exit 3" 'REVIEW' (Resolve-SiewStepClass (Step $n) 3).Class; Check "$n exit 4" 'FAIL' (Resolve-SiewStepClass (Step $n) 4).Class; Check "$n exit 2" 'ARGUMENT_ERROR' (Resolve-SiewStepClass (Step $n) 2).Class }
foreach ($n in @('PAPER', 'CORE_VALIDATION')) { Check "$n exit 1 (run or checks failed)" 'FAIL' (Resolve-SiewStepClass (Step $n) 1).Class; Check "$n exit 3 undocumented" 'EXECUTION_FAILURE' (Resolve-SiewStepClass (Step $n) 3).Class }
Check 'diagnostic exit 0 is DIAGNOSTIC, never PASS' 'DIAGNOSTIC' (Resolve-SiewStepClass (Step 'ANOM_SAMPLE_SD_DIAGNOSTIC') 0).Class
Check 'diagnostic exit 1' 'EXECUTION_FAILURE' (Resolve-SiewStepClass (Step 'ANOM_SAMPLE_SD_DIAGNOSTIC') 1).Class
Check 'demo diagnostic synthetic test exit 1' 'FAIL' (Resolve-SiewStepClass (Step 'ANOM_SAMPLE_SD_DIAGNOSTIC') 1 -Demo).Class
Check 'undocumented exit 255' 'EXECUTION_FAILURE' (Resolve-SiewStepClass (Step 'COLD_BURDEN') 255).Class
Check 'coded record exit 0' 'PASS' (Resolve-SiewStepClass (Step 'CODED_MODEL_RECORD') 0).Class; Check 'coded record exit 4' 'FAIL' (Resolve-SiewStepClass (Step 'CODED_MODEL_RECORD') 4).Class
foreach ($n in @('EXPOSURE_EXPORT', 'PH_DELETION', 'PH_L59_BOOTSTRAP', 'PH_NO_APRIL', 'PH_SOURCE_AGREEMENT', 'PH_EXPOSURE_RESIDUALS', 'PH_PROFILE_LIKELIHOOD', 'PH_EARLY_LATE', 'PH_H3_NESTED', 'PH_NEAREST_STATION', 'PH_EXPOSURE_QA', 'PH_VIF_SUMMARY', 'PH_CALENDAR_SPLINE', 'PH_AGE65')) { Check "$n exit 0" 'PASS' (Resolve-SiewStepClass (Step $n) 0).Class; Check "$n exit 3" 'REVIEW' (Resolve-SiewStepClass (Step $n) 3).Class; Check "$n exit 4" 'FAIL' (Resolve-SiewStepClass (Step $n) 4).Class; Check "$n uses run_posthoc.ps1" 'run_posthoc.ps1' (Step $n).Script }
Check 'post hoc steps: fourteen, ids 17-30' '17,18,19,20,21,22,23,24,25,26,27,28,29,30' (($script:StepTable | Where-Object { $_.Script -eq 'run_posthoc.ps1' } | ForEach-Object { $_.Id }) -join ',')
function MkR { param([string]$C, [bool]$D = $false) [pscustomobject]@{ Class = $C; DeclaredDemo = $D } }
Check 'overall: all PASS' 0 (Resolve-SiewOverall @((MkR 'PASS'), (MkR 'PASS'))).Code
Check 'overall: REVIEW present' 3 (Resolve-SiewOverall @((MkR 'PASS'), (MkR 'REVIEW'))).Code
Check 'overall: DIAGNOSTIC present' 3 (Resolve-SiewOverall @((MkR 'PASS'), (MkR 'DIAGNOSTIC'))).Code
Check 'overall: FAIL beats REVIEW' 4 (Resolve-SiewOverall @((MkR 'REVIEW'), (MkR 'FAIL'), (MkR 'DIAGNOSTIC'))).Code
Check 'overall: execution failure beats FAIL (incomplete)' 1 (Resolve-SiewOverall @((MkR 'FAIL'), (MkR 'EXECUTION_FAILURE'))).Code
Check 'overall: stopped run' 1 (Resolve-SiewOverall @((MkR 'PASS')) -Stopped).Code
Check 'overall study: NOT_TESTED is not PASS' 3 (Resolve-SiewOverall @((MkR 'PASS'), (MkR 'NOT_TESTED' $true))).Code
Check 'overall demo: declared demo NOT_TESTED keeps the demo green' 0 (Resolve-SiewOverall @((MkR 'PASS'), (MkR 'NOT_TESTED' $true)) -Demo).Code
Check 'overall demo: an undeclared NOT_TESTED (dependency) is not green' 3 (Resolve-SiewOverall @((MkR 'PASS'), (MkR 'NOT_TESTED' $false)) -Demo).Code
Check 'inside: same folder' $true (Test-SiewInside 'C:\a\b' 'C:\a\b\'); Check 'inside: child' $true (Test-SiewInside 'C:\A\b\c' 'c:\a\b'); Check 'inside: sibling prefix is not inside' $false (Test-SiewInside 'C:\a\bc' 'C:\a\b')

Write-Host '2. real PowerShell processes'
$demoOut = Join-Path $WorkDir 'demo'
$r = Run-All @('-Demo', '-OutRoot', $demoOut, '-MinFreeGB', '0.5'); Check 'demo: exit 3 (the post hoc profile-likelihood verification is REVIEW on the synthetic data)' 3 $r.Code
$rd = @(Get-ChildItem $demoOut -Directory | Where-Object { $_.Name -like 'fullrun_demo_*' }); Check 'demo: one run folder' 1 $rd.Count; $rd = $rd[0].FullName
Check 'demo: OUTCOME line' $true (@($r.Out | Where-Object { $_ -eq 'OUTCOME=COMPLETED_WITH_REVIEW_NOT_TESTED_OR_DIAGNOSTIC (exit code 3)' }).Count -eq 1)
foreach ($f in @('FULL_RUN_SUMMARY.md', 'FULL_RUN_STATUS.csv', 'FULL_RUN_STATUS_LINES.csv', 'FULL_RUN_MANIFEST.csv', 'RUN_PARAMETERS.txt')) { Check "demo: $f exists" $true (Test-Path (Join-Path $rd $f)) }
$st = Import-Csv (Join-Path $rd 'FULL_RUN_STATUS.csv'); Check 'demo: 30 steps listed' 30 $st.Count
Check 'demo: PASS count' 19 @($st | Where-Object { $_.class -eq 'PASS' }).Count; Check 'demo: REVIEW only for the profile likelihood' 'PH_PROFILE_LIKELIHOOD' ((@($st | Where-Object { $_.class -eq 'REVIEW' }) | ForEach-Object { $_.step }) -join ','); Check 'demo: NOT_TESTED count' 10 @($st | Where-Object { $_.class -eq 'NOT_TESTED' }).Count
Check 'demo: core validation compared and passed' 'PASS' ($st | Where-Object { $_.step -eq 'CORE_VALIDATION' }).class
Check 'demo: coded model record written and checked' 'PASS' ($st | Where-Object { $_.step -eq 'CODED_MODEL_RECORD' }).class
Check 'demo: coded record attached the paper statuses' $true (@(Import-Csv (Join-Path $rd 'FULL_RUN_STATUS_LINES.csv') | Where-Object { $_.step -eq 'CODED_MODEL_RECORD' -and $_.key -eq 'ROWS_FOUND_IN_RUN' -and $_.value -eq '1689' }).Count -eq 1)
Check 'demo: the legacy record file name is never written' 0 @(Get-ChildItem $rd -Recurse -File -Filter 'SIECC_All_Models_with_Codes.csv').Count
Check 'demo: manifest verifies (mismatches)' 0 (Verify-Manifest $rd)
$mt = (Get-Item (Join-Path $rd 'FULL_RUN_MANIFEST.csv')).LastWriteTimeUtc; Check 'demo: manifest written last' 0 @(Get-ChildItem $rd -Recurse -File | Where-Object { $_.LastWriteTimeUtc -gt $mt }).Count
Check 'demo: no .partial file left' 0 @(Get-ChildItem $rd -Recurse -File -Filter '*.partial').Count
Check 'demo: summary never calls a NOT_TESTED step PASS' 10 @(Get-Content (Join-Path $rd 'FULL_RUN_SUMMARY.md') | Where-Object { $_ -match '\*\*NOT_TESTED\*\*' }).Count
Check 'demo: verbatim status lines include the core validation statuses' $true (@(Import-Csv (Join-Path $rd 'FULL_RUN_STATUS_LINES.csv') | Where-Object { $_.step -eq 'CORE_VALIDATION' -and $_.key -eq 'VALIDATION_STATUS' }).Count -eq 1)
Check 'demo: no participant code in any orchestrator file' 0 @(Get-ChildItem $rd -File | Select-String -Pattern 'SYN0[0-9]' -List).Count
$pg = @(Get-Content (Join-Path $rd 'run_progress.txt')); Check 'progress: run_progress.txt written' $true ($pg.Count -gt 0)
Check 'progress: RUN START line with mode and step count' 1 @($pg | Where-Object { $_ -match '\| RUN START \| mode demo \| 30 steps \|' }).Count
Check 'progress: preflight END line' 1 @($pg | Where-Object { $_ -match '\| step 01-02/30 PREFLIGHT \| END \| PASS \(exit 0\) \|' }).Count
foreach ($s in $script:StepTable) { Check ("progress: one END or NOT_TESTED line for step {0} {1}" -f $s.Id, $s.Name) 1 @($pg | Where-Object { $_ -match ("\| step {0}/30 {1} \| (END|NOT_TESTED) \|" -f $s.Id, $s.Name) }).Count }
Check 'progress: START lines for the 18 executed demo steps' 18 @($pg | Where-Object { $_ -match '\| step \d\d/30 [A-Z_0-9]+ \| START \|' }).Count
Check 'progress: END lines carry class, exit code and seconds' 17 @($pg | Where-Object { $_ -match '\| END \| PASS \(exit 0\) \| [0-9.]+ s \|' -and $_ -notmatch 'PREFLIGHT' }).Count
Check 'progress: every line has a timestamp and free disk space' $pg.Count @($pg | Where-Object { $_ -match '^\d{4}-\d\d-\d\d \d\d:\d\d:\d\d \| .* \| free [0-9.,]+ GB$' }).Count
Check 'progress: RUN END line with outcome and counts' 1 @($pg | Where-Object { $_ -match '\| RUN END \| COMPLETED_WITH_REVIEW_NOT_TESTED_OR_DIAGNOSTIC \(exit 3\) \| PASS 19, REVIEW 1, FAIL 0, NOT_TESTED 10,' }).Count
Check 'progress: the same lines appear on the console' $true (@($r.Out | Where-Object { $_ -like 'PROGRESS *| step 03/30 PAPER | END | PASS (exit 0)*' }).Count -eq 1)
$sm = Get-Content (Join-Path $rd 'FULL_RUN_SUMMARY.md') -Raw
Check 'summary: J02 implemented note' $true ($sm -like '*J02 (archived glm epsilon) is implemented in the archived code since 2026-10-06*')
Check 'summary: older step texts still say OPEN, explained' $true ($sm -like '*J02_GLM_EPSILON_ENTRY_POINTS=OPEN (no change made)*traced by the ANOM sample-SD diagnostic*')
Check 'summary: demo has no accepted-run note' $true ($sm -like '*Demo mode: no historical accepted run applies.*')
Check 'demo: no NOTE line on the console' 0 @($r.Out | Where-Object { $_ -like 'NOTE: *' }).Count
$pn = [pscustomobject]@{ mode = 'study'; dlm_accepted_run = ''; batch_b_legacy_run = ''; batch_c_accepted_run = '' }
Check 'notes: three empty optional keys give three notes' 3 @(Get-SiewReferenceNotes $pn).Count
$pn.dlm_accepted_run = 'X'; $pn.batch_b_legacy_run = 'Y'; $pn.batch_c_accepted_run = 'Z'; Check 'notes: all given, no note' 0 @(Get-SiewReferenceNotes $pn).Count
$pn.mode = 'demo'; $pn.dlm_accepted_run = ''; Check 'notes: none in demo mode' 0 @(Get-SiewReferenceNotes $pn).Count

Check 'no arguments: exit 2' 2 (Run-All @()).Code
Check '-Demo with -PathsFile: exit 2' 2 (Run-All @('-Demo', '-PathsFile', 'x.json')).Code
Check 'missing paths file: exit 2' 2 (Run-All @('-PathsFile', (Join-Path $WorkDir 'nope.json'))).Code
Check 'disk guard (absurd minimum): exit 2' 2 (Run-All @('-Demo', '-OutRoot', (Join-Path $WorkDir 'disk'), '-MinFreeGB', '1000000')).Code
Check 'disk guard: no run folder created' 0 @(Get-ChildItem (Join-Path $WorkDir 'disk') -Directory -ErrorAction SilentlyContinue).Count
Check 'resume of a completed run is refused: exit 2' 2 (Run-All @('-Demo', '-ResumeRunDir', $rd)).Code

# study mode with synthetic stand-ins: every folder exists, so the run reaches the input-hash check, which must refuse the non-accepted CSV
$fake = Join-Path $WorkDir 'fake_private'; New-Item -ItemType Directory $fake -Force | Out-Null
$keys = @('core_reference', 'batch_a_reference', 'batch_b_reference', 'batch_c_reference', 'dlm_reference', 'archived_dlm_reference', 'cold_burden_reference', 'daily_layer_reference', 'anomhist_reference', 'anom_failed_run')
$pj = [ordered]@{ data_csv = (Join-Path $repo 'inst\extdata\synthetic_siecc_winter.csv'); out_root = (Join-Path $fake 'runs') }
foreach ($k in $keys) { $d = Join-Path $fake $k; New-Item -ItemType Directory $d -Force | Out-Null; $pj[$k] = $k }
'file,bytes,algorithm,hash' | Set-Content (Join-Path $fake 'anom_failed_run\RUN_MANIFEST.csv')
$pjf = Join-Path $fake 'paths.json'; ($pj | ConvertTo-Json) | Set-Content -Encoding UTF8 $pjf
$r = Run-All @('-PathsFile', $pjf, '-MinFreeGB', '0.5'); Check 'study: synthetic CSV is not the accepted CSV -> refused (exit 2)' 2 $r.Code
Check 'study: refusal names the input hash' $true (@($r.Out | Where-Object { $_ -like '*INPUT_SHA256_OK=FALSE*' }).Count -ge 1)
Check 'study: nothing analysed (no run folder)' 0 @(Get-ChildItem (Join-Path $fake 'runs') -Directory -ErrorAction SilentlyContinue).Count
Check 'study: empty dlm_accepted_run is flagged as self-reference (wiring only)' 1 @($r.Out | Where-Object { $_ -like 'NOTE: dlm_accepted_run is empty*self-reference; proves the wiring only*' }).Count
Check 'study: empty batch_b_legacy_run and batch_c_accepted_run are flagged' 2 @($r.Out | Where-Object { $_ -like 'NOTE: batch_b_legacy_run is empty*' -or $_ -like 'NOTE: batch_c_accepted_run is empty*' }).Count
$pj5 = [ordered]@{}; foreach ($k in $pj.Keys) { $pj5[$k] = $pj[$k] }; foreach ($k in @('dlm_accepted_run', 'batch_b_legacy_run', 'batch_c_accepted_run')) { New-Item -ItemType Directory (Join-Path $fake ('acc_' + $k)) -Force | Out-Null; $pj5[$k] = ('acc_' + $k) }
$pjf5 = Join-Path $fake 'paths_accepted.json'; ($pj5 | ConvertTo-Json) | Set-Content -Encoding UTF8 $pjf5
$r = Run-All @('-PathsFile', $pjf5, '-MinFreeGB', '0.5'); Check 'study: all accepted runs given -> still refused at the input hash (exit 2)' 2 $r.Code; Check 'study: all accepted runs given -> no NOTE' 0 @($r.Out | Where-Object { $_ -like 'NOTE: *' }).Count
$pj4 = [ordered]@{}; foreach ($k in $pj.Keys) { $pj4[$k] = $pj[$k] }; $pj4['dlm_accepted_run'] = '<historical accepted paper run folder>'; $pjf4 = Join-Path $fake 'paths_placeholder.json'; ($pj4 | ConvertTo-Json) | Set-Content -Encoding UTF8 $pjf4
$r = Run-All @('-PathsFile', $pjf4, '-MinFreeGB', '0.5'); Check 'study: a <...> placeholder left in the paths file -> exit 2' 2 $r.Code
Check 'study: the placeholder key is named' $true (@($r.Out | Where-Object { $_ -like '*placeholder not replaced*dlm_accepted_run*' }).Count -ge 1)
Check 'study: placeholder refusal creates nothing' 0 @(Get-ChildItem (Join-Path $fake 'runs') -Directory -ErrorAction SilentlyContinue).Count
$tpl = Get-Content (Join-Path $repo 'manual_run_paths.template.json') -Raw | ConvertFrom-Json
Check 'template: the shipped template itself is refused as placeholders (never runnable as is)' $true (@($tpl.PSObject.Properties | Where-Object { $_.Name -notlike '_*' -and [string]$_.Value -match '[<>]' }).Count -eq 19)
Check 'template: recommends the historical accepted runs' $true ([string]$tpl._recommended -like '*HISTORICAL ACCEPTED*self-reference*')
$pj2 = [ordered]@{}; foreach ($k in $pj.Keys) { $pj2[$k] = $pj[$k] }; $pj2.Remove('dlm_reference'); $pjf2 = Join-Path $fake 'paths_missing.json'; ($pj2 | ConvertTo-Json) | Set-Content -Encoding UTF8 $pjf2
$r = Run-All @('-PathsFile', $pjf2); Check 'study: a required reference key missing -> exit 2' 2 $r.Code; Check 'study: the missing key is named' $true (@($r.Out | Where-Object { $_ -like '*dlm_reference (required)*' }).Count -ge 1)
$pj3 = [ordered]@{}; foreach ($k in $pj.Keys) { $pj3[$k] = $pj[$k] }; $pj3.out_root = (Join-Path $repo 'siw_out_root_must_not_exist'); $pjf3 = Join-Path $fake 'paths_inside.json'; ($pj3 | ConvertTo-Json) | Set-Content -Encoding UTF8 $pjf3
$r = Run-All @('-PathsFile', $pjf3); Check 'study: out_root inside the package -> exit 2' 2 $r.Code; Check 'study: nothing created inside the package' $false (Test-Path (Join-Path $repo 'siw_out_root_must_not_exist'))

Write-Host '3. resume'
$copy = Join-Path $WorkDir 'resume'; New-Item -ItemType Directory $copy -Force | Out-Null; Copy-Item $rd -Destination $copy -Recurse; $rc = Join-Path $copy (Split-Path -Leaf $rd)
Remove-Item (Join-Path $rc 'FULL_RUN_MANIFEST.csv'); Remove-Item (Join-Path $rc 'steps\04_CORE_VALIDATION\STEP_RESULT.txt')
$r = Run-All @('-Demo', '-ResumeRunDir', $rc, '-MinFreeGB', '0.5'); Check 'resume: exit 3 (as the demo)' 3 $r.Code
Check 'resume: PAPER restored, not rerun' $true (@($r.Out | Where-Object { $_ -eq '[03] PAPER: PASS (restored)' }).Count -eq 1)
Check 'resume: diagnostic restored, not rerun' $true (@($r.Out | Where-Object { $_ -eq '[15] ANOM_SAMPLE_SD_DIAGNOSTIC: PASS (restored)' }).Count -eq 1)
Check 'resume: CORE_VALIDATION rerun' 'PASS' ((Read-SiewKeyValue (Join-Path $rc 'steps\04_CORE_VALIDATION\STEP_RESULT.txt'))['CLASS'])
Check 'resume: the earlier validation run folder is kept, not deleted' 2 @(Get-ChildItem $rc -Directory | Where-Object { $_.Name -like 'validate_*' }).Count; Check 'resume: the earlier console log is kept' 1 @(Get-ChildItem (Join-Path $rc 'steps\04_CORE_VALIDATION') -File | Where-Object { $_.Name -like 'step_console.log.attempt_*' }).Count
Check 'resume: manifest written and verifies' 0 (Verify-Manifest $rc)
$pr = @(Get-Content (Join-Path $rc 'run_progress.txt')); Check 'resume progress: RUN RESUME appended to the same file' 1 @($pr | Where-Object { $_ -match '\| RUN RESUME \| mode demo | 30 steps\|' }).Count
Check 'resume progress: earlier lines kept' 1 @($pr | Where-Object { $_ -match '\| RUN START \|' }).Count
Check 'resume progress: 27 RESTORED lines' 27 @($pr | Where-Object { $_ -match '\| RESTORED \|' }).Count
Check 'resume progress: the rerun step has START and END' 2 @($pr | Where-Object { $_ -match '\| step 04/30 CORE_VALIDATION \| (START|END) \|' } | Select-Object -Last 2).Count
$copy2 = Join-Path $WorkDir 'resume_tamper'; New-Item -ItemType Directory $copy2 -Force | Out-Null; Copy-Item $rd -Destination $copy2 -Recurse; $rt = Join-Path $copy2 (Split-Path -Leaf $rd)
Remove-Item (Join-Path $rt 'FULL_RUN_MANIFEST.csv'); $pd = (Get-ChildItem $rt -Directory | Where-Object { $_.Name -like 'paper_*' })[0].FullName; Add-Content (Join-Path $pd 'RUN_MANIFEST.csv') 'tampered'
$r = Run-All @('-Demo', '-ResumeRunDir', $rt, '-MinFreeGB', '0.5'); Check 'resume: a restored step whose output changed is refused (exit 1)' 1 $r.Code
$copy3 = Join-Path $WorkDir 'resume_params'; New-Item -ItemType Directory $copy3 -Force | Out-Null; Copy-Item $rd -Destination $copy3 -Recurse; $rp = Join-Path $copy3 (Split-Path -Leaf $rd)
Remove-Item (Join-Path $rp 'FULL_RUN_MANIFEST.csv'); (Get-Content (Join-Path $rp 'RUN_PARAMETERS.txt')) -replace '^rainfall_sensitivity=False$', 'rainfall_sensitivity=True' | Set-Content (Join-Path $rp 'RUN_PARAMETERS.txt')
$r = Run-All @('-Demo', '-ResumeRunDir', $rp, '-MinFreeGB', '0.5'); Check 'resume: different parameters are refused (exit 2)' 2 $r.Code; Check 'resume: the differing parameter is named' $true (@($r.Out | Where-Object { $_ -like '*rainfall_sensitivity*' }).Count -ge 1)

Write-Host '4. step failures through stub child processes (orchestration code unchanged)'
$stubPS = Join-Path $WorkDir 'stubPS.cmd'; "@echo off`r`necho %* | findstr /C:""run_paper.ps1"" >nul && (echo STUB wrapper called& exit /b %STUB_PS_RC%)`r`n""$realPS"" %*`r`nexit /b %ERRORLEVEL%" | Set-Content -Encoding ASCII $stubPS      # only run_paper.ps1 is stubbed; every other wrapper (the post hoc steps) runs for real
$stubR = Join-Path $WorkDir 'stubR.cmd'
"@echo off`r`necho %* | findstr /C:`"test_anom_sample_sd_diag_synthetic.R`" >nul && (echo SYNTHETIC TESTS: 1 checks; 1 passed; 0 failed& exit /b 0)`r`n`"$realR`" %*`r`nexit /b %ERRORLEVEL%" | Set-Content -Encoding ASCII $stubR
$script:RscriptExe = $stubR; $script:PowerShellExe = $stubPS
function InProc { param([string]$Out, [string]$Resume = '') $o = Invoke-SiewFullRun -RepoDir $repo -Demo -OutRoot $Out -ResumeRunDir $Resume -MinFreeGB 0.5 6>$null; return ($o | Select-Object -Last 1) }
$env:STUB_PS_RC = '1'; $o1 = Join-Path $WorkDir 'stub1'; $c = InProc $o1; Check 'paper wrapper exit 1 (documented: run or checks failed) -> PAPER FAIL; run exit 4' 4 $c
$d1 = (Get-ChildItem $o1 -Directory)[0].FullName; $s1 = Import-Csv (Join-Path $d1 'FULL_RUN_STATUS.csv')
Check '  PAPER class' 'FAIL' ($s1 | Where-Object { $_.step -eq 'PAPER' }).class; Check '  CORE_VALIDATION not tested (dependency), not PASS' 'NOT_TESTED' ($s1 | Where-Object { $_.step -eq 'CORE_VALIDATION' }).class
Check '  a FAIL run is complete: manifest written' $true (Test-Path (Join-Path $d1 'FULL_RUN_MANIFEST.csv'))
$env:STUB_PS_RC = '4'; $o4 = Join-Path $WorkDir 'stub4'; $c = InProc $o4; Check 'paper wrapper exit 4 is not documented for run_paper -> EXECUTION_FAILURE; run exit 1' 1 $c
$env:STUB_PS_RC = '7'; $o7 = Join-Path $WorkDir 'stub7'; $c = InProc $o7; Check 'undocumented wrapper exit 7 -> EXECUTION_FAILURE; run exit 1' 1 $c
$d7 = (Get-ChildItem $o7 -Directory)[0].FullName; Check '  incomplete run: no manifest' $false (Test-Path (Join-Path $d7 'FULL_RUN_MANIFEST.csv'))
Check '  incomplete run: summary still written' $true (Test-Path (Join-Path $d7 'FULL_RUN_SUMMARY.md'))
$script:PowerShellExe = $realPS; Remove-Item Env:\STUB_PS_RC
$c = InProc $o7 $d7; Check '  resumed with the real wrappers: exit 3 (as the demo)' 3 $c
Check '  resumed: PAPER retried and PASS' 'PASS' ((Read-SiewKeyValue (Join-Path $d7 'steps\03_PAPER\STEP_RESULT.txt'))['CLASS'])
Check '  resumed: the failed attempt''s record is kept' 1 @(Get-ChildItem (Join-Path $d7 'steps\03_PAPER') -File | Where-Object { $_.Name -like 'STEP_RESULT.txt.attempt_*' }).Count
Check '  resumed: manifest verifies' 0 (Verify-Manifest $d7)
$script:RscriptExe = $realR

$n = $results.Count; $np = @($results | Where-Object { $_.pass }).Count; Write-Host ("`nTESTS: {0}; PASSED: {1}; FAILED: {2}" -f $n, $np, ($n - $np))
if ($ResultCsv -ne '') { $results | Export-Csv -NoTypeInformation -Encoding UTF8 -Path $ResultCsv }
if (-not $Keep -and $n -eq $np) { Remove-Tree $WorkDir }
exit $(if ($n -eq $np) { 0 } else { 1 })
