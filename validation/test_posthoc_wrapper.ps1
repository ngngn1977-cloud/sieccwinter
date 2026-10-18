<#
.SYNOPSIS
  Tests run_posthoc.ps1 / scripts\_run_posthoc.ps1 on SYNTHETIC data only (no study data, no private folder). Writes under -WorkDir (default %TEMP%\siw_posthoc_test).
  1. Pure function Resolve-SiewPosthocOutcome: every status combination, the synthetic neutral value, inconsistencies with the R exit code.
  2. Real processes: a synthetic demo step (exit 0, status file, manifest, aggregate files only), argument refusals (exit 2), the synthetic flag refused for another CSV (exit 1),
     a step without its reference (REVIEW, exit 3; NOT_TESTED is never PASS).
.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File .\validation\test_posthoc_wrapper.ps1
#>
param([string]$WorkDir = (Join-Path $env:TEMP 'siw_posthoc_test'), [string]$ResultCsv = '', [switch]$Keep)
$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent $PSScriptRoot
. (Join-Path $repo 'scripts\_run_posthoc.ps1')
function Remove-Tree { param([string]$D) if (Test-Path -LiteralPath $D) { & cmd.exe /d /c rd /s /q ('\\?\' + [System.IO.Path]::GetFullPath($D)) } }
Remove-Tree $WorkDir; New-Item -ItemType Directory $WorkDir -Force | Out-Null; $WorkDir = (Resolve-Path $WorkDir).Path
$results = New-Object System.Collections.ArrayList
function Check { param([string]$Name, $Expected, $Actual) $ok = ([string]$Expected -eq [string]$Actual); [void]$script:results.Add([pscustomobject]@{ test = $Name; expected = [string]$Expected; actual = [string]$Actual; pass = $ok }); Write-Host ("  [{0}] {1}: expected {2}, got {3}" -f $(if ($ok) { 'ok' } else { 'FAIL' }), $Name, $Expected, $Actual) -ForegroundColor $(if ($ok) { 'Green' } else { 'Red' }) }
function St { param([hashtable]$In) $out = @{ EXECUTION_STATUS = 'PASS' }; foreach ($k in $In.Keys) { $out[$k] = $In[$k] }; return $out }
$psExe = Join-Path $env:SystemRoot 'System32\WindowsPowerShell\v1.0\powershell.exe'
function Run-PH { param([string[]]$A) $o = & $psExe -NoProfile -ExecutionPolicy Bypass -File (Join-Path $repo 'run_posthoc.ps1') @A 2>&1 | ForEach-Object { [string]$_ }; return [pscustomobject]@{ Code = $LASTEXITCODE; Out = $o } }

Write-Host '1. pure function'
Check 'all PASS, R 0 -> 0' 0 (Resolve-SiewPosthocOutcome -Status (St @{ A_STATUS = 'PASS'; B_STATUS = 'PASS'; POSTHOC_OVERALL = 'PASS' }) -RExitCode 0).Code
Check 'a REVIEW, R 3 -> 3' 3 (Resolve-SiewPosthocOutcome -Status (St @{ A_STATUS = 'PASS'; B_STATUS = 'REVIEW'; POSTHOC_OVERALL = 'REVIEW' }) -RExitCode 3).Code
Check 'a NOT_TESTED, R 3 -> 3 (never PASS)' 3 (Resolve-SiewPosthocOutcome -Status (St @{ A_STATUS = 'PASS'; B_STATUS = 'NOT_TESTED'; POSTHOC_OVERALL = 'REVIEW' }) -RExitCode 3).Code
Check 'a FAIL, R 4 -> 4' 4 (Resolve-SiewPosthocOutcome -Status (St @{ A_STATUS = 'FAIL'; B_STATUS = 'REVIEW'; POSTHOC_OVERALL = 'FAIL' }) -RExitCode 4).Code
Check 'synthetic neutral value with -SyntheticDemo -> 0' 0 (Resolve-SiewPosthocOutcome -Status (St @{ A_STATUS = 'NOT_APPLICABLE_SYNTHETIC_INPUT'; B_STATUS = 'PASS'; POSTHOC_OVERALL = 'PASS' }) -RExitCode 0 -SyntheticDemo).Code
Check 'synthetic neutral value WITHOUT -SyntheticDemo -> 4 (never accepted for a study input)' 4 (Resolve-SiewPosthocOutcome -Status (St @{ A_STATUS = 'NOT_APPLICABLE_SYNTHETIC_INPUT'; B_STATUS = 'PASS'; POSTHOC_OVERALL = 'PASS' }) -RExitCode 0).Code
Check 'unexpected status value -> 4' 4 (Resolve-SiewPosthocOutcome -Status (St @{ A_STATUS = 'MAYBE'; POSTHOC_OVERALL = 'PASS' }) -RExitCode 0).Code
Check 'statuses PASS but R exit 3 -> 1 (inconsistent)' 1 (Resolve-SiewPosthocOutcome -Status (St @{ A_STATUS = 'PASS'; POSTHOC_OVERALL = 'PASS' }) -RExitCode 3).Code
Check 'REVIEW statuses but POSTHOC_OVERALL PASS -> 1 (inconsistent)' 1 (Resolve-SiewPosthocOutcome -Status (St @{ A_STATUS = 'REVIEW'; POSTHOC_OVERALL = 'PASS' }) -RExitCode 0).Code
Check 'no EXECUTION_STATUS -> 1' 1 (Resolve-SiewPosthocOutcome -Status @{ A_STATUS = 'PASS'; POSTHOC_OVERALL = 'PASS' } -RExitCode 0).Code
Check 'no status line at all -> 1' 1 (Resolve-SiewPosthocOutcome -Status @{ EXECUTION_STATUS = 'PASS' } -RExitCode 0).Code
Check 'R exit 1 -> 1' 1 (Resolve-SiewPosthocOutcome -Status (St @{ A_STATUS = 'PASS'; POSTHOC_OVERALL = 'PASS' }) -RExitCode 1).Code
Check 'fourteen declared items' 14 $script:PosthocItems.Count

Write-Host '2. real processes'
$syn = Join-Path $repo 'inst\extdata\synthetic_siecc_winter.csv'; $out = Join-Path $WorkDir 'runs'
$r = Run-PH @('-Item', 'exposure_export', '-DataCsv', $syn, '-OutRoot', $out, '-SyntheticDemo'); Check 'demo exposure export: exit 0' 0 $r.Code
$rd = @(Get-ChildItem -LiteralPath $out -Directory | Where-Object { $_.Name -like 'ph_exposure_export_*' }); Check 'demo: one run folder ph_exposure_export_*' 1 $rd.Count; $rd = $rd[0].FullName
foreach ($f in 'POSTHOC_STATUS.txt', 'RUN_SUMMARY.md', 'RUN_MANIFEST.csv', 'SESSION_INFO.txt', 'exposure_aggregate_export.csv', 'posthoc_metadata.json', 'SOURCE_FINGERPRINT.txt') { Check "demo: $f" $true (Test-Path -LiteralPath (Join-Path $rd $f)) }
$s = @{}; foreach ($l in Get-Content -LiteralPath (Join-Path $rd 'POSTHOC_STATUS.txt')) { if ($l -match '^([A-Z_0-9]+)=(.*)$') { $s[$matches[1]] = $matches[2] } }
Check 'demo: label POST_HOC' 'POST_HOC' $s['POSTHOC_LABEL']; Check 'demo: acceptance not applicable on synthetic input' 'NOT_APPLICABLE_SYNTHETIC_INPUT' $s['EXPORT_ACCEPTANCE_STATUS']; Check 'demo: verification PASS' 'PASS' $s['EXPORT_VERIFICATION_STATUS']
Check 'demo: primary inference statement' $true ($s['PRIMARY_INFERENCE_EFFECT'] -like 'NONE*q90, q9*')
Check 'demo: no participant code in any file' 0 @(Get-ChildItem -LiteralPath $rd -Recurse -File | Select-String -Pattern 'SYN0[0-9][0-9]' -List).Count
$m = Import-Csv -LiteralPath (Join-Path $rd 'RUN_MANIFEST.csv'); Check 'demo: RUN_MANIFEST verifies' 0 @($m | Where-Object { (Get-FileHash -LiteralPath (Join-Path $rd $_.file) -Algorithm SHA256).Hash -ne $_.hash }).Count
Check 'demo: the package is not written to (no console temp file left)' 0 @(Get-ChildItem -LiteralPath $repo -File -Filter 'posthoc_*.tmp').Count

$r = Run-PH @('-Item', 'no_april', '-DataCsv', $syn, '-OutRoot', $out); Check 'no reference and no synthetic flag: exit 3 (REVIEW: NOT_TESTED comparison, SI values not met by synthetic data; never PASS)' 3 $r.Code
Check 'no reference: the console says REVIEW' $true (@($r.Out | Where-Object { $_ -like 'REVIEW - *' }).Count -eq 1)

Check 'unknown item: exit 2' 2 (Run-PH @('-Item', 'nothing', '-DataCsv', $syn, '-OutRoot', $out)).Code
Check 'missing data file: exit 2' 2 (Run-PH @('-Item', 'exposure_export', '-DataCsv', (Join-Path $WorkDir 'nope.csv'), '-OutRoot', $out)).Code
Check 'missing reference folder: exit 2' 2 (Run-PH @('-Item', 'deletion', '-DataCsv', $syn, '-OutRoot', $out, '-V14ReferenceDir', (Join-Path $WorkDir 'nope'))).Code
'x' | Set-Content -LiteralPath (Join-Path $WorkDir 'near.txt'); Check 'nearest input must be a .csv file: exit 2' 2 (Run-PH @('-Item', 'nearest_station', '-DataCsv', $syn, '-OutRoot', $out, '-NearestHistoriesCsv', (Join-Path $WorkDir 'near.txt'))).Code
Check 'GIS reference that does not exist: exit 2' 2 (Run-PH @('-Item', 'exposure_qa', '-DataCsv', $syn, '-OutRoot', $out, '-GisReference', (Join-Path $WorkDir 'nope'))).Code
$r = Run-PH @('-Item', 'exposure_qa', '-DataCsv', $syn, '-OutRoot', $out); Check 'exposure QA without the GIS role: exit 3 (NOT_TESTED, never PASS)' 3 $r.Code
$r = Run-PH @('-Item', 'vif_summary', '-DataCsv', $syn, '-OutRoot', $out, '-SyntheticDemo'); Check 'demo VIF summary: exit 0' 0 $r.Code
$rv = @(Get-ChildItem -LiteralPath $out -Directory | Where-Object { $_.Name -like 'ph_vif_summary_*' })[0].FullName; Check 'demo VIF summary: SI concordance written, no participant code' $true ((Test-Path (Join-Path $rv 'vif_si_concordance.csv')) -and @(Get-ChildItem -LiteralPath $rv -Recurse -File | Select-String -Pattern 'SYN0[0-9][0-9]' -List).Count -eq 0)
$r = Run-PH @('-Item', 'age65', '-DataCsv', $syn, '-OutRoot', $out, '-SyntheticDemo'); Check 'demo age65: exit 0' 0 $r.Code
$ra = @(Get-ChildItem -LiteralPath $out -Directory | Where-Object { $_.Name -like 'ph_age65_*' })[0].FullName; Check 'demo age65: SI concordance written, no participant code' $true ((Test-Path (Join-Path $ra 'age65_si_concordance.csv')) -and @(Get-ChildItem -LiteralPath $ra -Recurse -File | Select-String -Pattern 'SYN0[0-9][0-9]' -List).Count -eq 0)
Check 'nearest list with a missing file: exit 2' 2 (Run-PH @('-Item', 'nearest_station', '-DataCsv', $syn, '-OutRoot', $out, '-NearestHistoriesCsv', ($syn + '|' + (Join-Path $WorkDir 'nope.csv')))).Code
# two synthetic nearest inputs in the raw-export layout (FEATURE_ID; JERUSALEM_LOCAL_DATE / LOCAL_DATE), built from the shipped synthetic CSV; neither is a declared export variant
$nd = Join-Path $WorkDir 'near'; New-Item -ItemType Directory $nd -Force | Out-Null; $gen = Join-Path $WorkDir 'gen_near.R'
@('a <- commandArgs(TRUE); root <- a[1]; out <- a[2]; source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root); cfg <- siw_load_config()',
  'd <- siw_prepare_data(siw_read_study_csv(file.path(root, "inst", "extdata", "synthetic_siecc_winter.csv"), cfg), cfg); n <- .siw_nearest_synthetic(d, siw_posthoc_protocol())',
  'x <- data.frame(FEATURE_ID = n$participant_code, METHOD = "NEAREST", JERUSALEM_LOCAL_DATE = n$LOCAL_DATE, n[, c("TD_MEAN", "RH_MEAN", "RAIN_TOTAL", "RAIN_DAYS1")])',
  'y <- x; names(y)[3] <- "LOCAL_DATE"; y$RH_MEAN <- 0.9 * y$RH_MEAN; utils::write.csv(x, file.path(out, "a.csv"), row.names = FALSE); utils::write.csv(y, file.path(out, "b.csv"), row.names = FALSE)') | Set-Content -LiteralPath $gen -Encoding ASCII
& $script:RscriptExe $gen ($repo -replace '\\', '/') ($nd -replace '\\', '/') 2>&1 | Out-Null; Check 'two synthetic nearest inputs written' $true ((Test-Path (Join-Path $nd 'a.csv')) -and (Test-Path (Join-Path $nd 'b.csv')))
$r = Run-PH @('-Item', 'nearest_station', '-DataCsv', $syn, '-OutRoot', $out, '-NearestHistoriesCsv', ((Join-Path $nd 'a.csv') + '|' + (Join-Path $nd 'b.csv')))
Check 'two nearest inputs: exit 3 (REVIEW: undeclared inputs, SI values not met by synthetic data)' 3 $r.Code
$rn = @(Get-ChildItem -LiteralPath $out -Directory | Where-Object { $_.Name -like 'ph_nearest_station_*' } | Sort-Object LastWriteTime)[-1].FullName
$s = @{}; foreach ($l in Get-Content -LiteralPath (Join-Path $rn 'POSTHOC_STATUS.txt')) { if ($l -match '^([A-Z_0-9]+)=(.*)$') { $s[$matches[1]] = $matches[2] } }
Check 'two nearest inputs: both run' '2 (input1, input2)' $s['NEAREST_VARIANTS_RUN']; Check 'two nearest inputs: fits PASS' 'PASS' $s['NEAREST_CHECK_STATUS']; Check 'two nearest inputs: provenance REVIEW (not declared variants)' 'REVIEW' $s['INPUT_PROVENANCE_STATUS']
Check 'two nearest inputs: FEATURE_ID matched every participant in both' $true (($s['NEAREST_VARIANT_INPUT1'] -match 'participants matched (\d+)/\1;') -and ($s['NEAREST_VARIANT_INPUT2'] -match 'participants matched (\d+)/\1;'))
Check 'two nearest inputs: variant summary and per-variant files' $true ((Test-Path (Join-Path $rn 'nearest_station_variant_summary.csv')) -and (Test-Path (Join-Path $rn 'nearest_station_si_concordance_input2.csv')))
Check 'two nearest inputs: no participant code in any output file' 0 @(Get-ChildItem -LiteralPath $rn -Recurse -File | Select-String -Pattern 'SYN0[0-9][0-9]' -List).Count
$ref = Join-Path $WorkDir 'ref'; New-Item -ItemType Directory $ref -Force | Out-Null; Check 'out root inside a reference folder: exit 2' 2 (Run-PH @('-Item', 'deletion', '-DataCsv', $syn, '-OutRoot', (Join-Path $ref 'runs'), '-V14ReferenceDir', $ref)).Code
$other = Join-Path $WorkDir 'other.csv'; Copy-Item -LiteralPath $syn -Destination $other; Add-Content -LiteralPath $other -Value ''; Check 'the other CSV differs from the shipped synthetic CSV by its bytes' $true ((Get-FileHash $other).Hash -ne (Get-FileHash $syn).Hash)
Check 'synthetic flag with another CSV: refused by R (exit 1)' 1 (Run-PH @('-Item', 'exposure_export', '-DataCsv', $other, '-OutRoot', $out, '-SyntheticDemo')).Code

$n = $results.Count; $np = @($results | Where-Object { $_.pass }).Count; Write-Host ("`nTESTS: {0}; PASSED: {1}; FAILED: {2}" -f $n, $np, ($n - $np))
if ($ResultCsv -ne '') { $results | Export-Csv -NoTypeInformation -Encoding UTF8 -Path $ResultCsv }
if (-not $Keep -and $n -eq $np) { Remove-Tree $WorkDir }
exit $(if ($n -eq $np) { 0 } else { 1 })
