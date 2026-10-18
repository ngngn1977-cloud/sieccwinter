# Helper for run_all.ps1 (dot-sourced). Windows PowerShell 5.1 compatible. Aggregate output only: no participant-level value is read or printed.
# Orchestrates the existing per-batch wrappers (each in its own PowerShell and R processes); it reimplements none of their logic and changes no estimator,
# tolerance, protocol or status. Statuses are copied verbatim from each step's status files; the step class comes only from the wrapper's documented exit code.
$script:RscriptExe = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'
# Every child PowerShell is Windows PowerShell 5.1 (never pwsh / PowerShell 7, whatever shell the user started from) with the 5.1 default PSModulePath: a PSModulePath
# inherited from PowerShell 7 hides the 5.1 core modules (for example Get-FileHash).
$script:PowerShellExe = $(if ($env:SystemRoot -and (Test-Path -LiteralPath (Join-Path $env:SystemRoot 'System32\WindowsPowerShell\v1.0\powershell.exe'))) { Join-Path $env:SystemRoot 'System32\WindowsPowerShell\v1.0\powershell.exe' } else { Join-Path $PSHOME 'powershell.exe' })
function Get-SiewWinPSModulePath {
    $sr = $(if ($env:SystemRoot) { $env:SystemRoot } else { 'C:\Windows' }); $pf = $(if ($env:ProgramFiles) { $env:ProgramFiles } else { 'C:\Program Files' })
    $docs = ''; try { $docs = [Environment]::GetFolderPath('MyDocuments') } catch { }
    $parts = @(); if ($docs -ne '') { $parts += (Join-Path $docs 'WindowsPowerShell\Modules') }
    $parts += (Join-Path $pf 'WindowsPowerShell\Modules'); $parts += (Join-Path $sr 'System32\WindowsPowerShell\v1.0\Modules')
    return ($parts -join ';')
}
# Run a script block with PSModulePath set to the 5.1 defaults (restored afterwards); used around every child PowerShell start.
function Invoke-SiewWithWinPSEnv {
    param([Parameter(Mandatory = $true)][scriptblock]$Block)
    $old = $env:PSModulePath; $env:PSModulePath = Get-SiewWinPSModulePath
    try { return (& $Block) } finally { $env:PSModulePath = $old }
}
# When a script written for Windows PowerShell 5.1 is started from PowerShell 7 (or any other host), run it again in powershell.exe 5.1 with the same parameters and return its exit code.
function Invoke-SiewRelaunchInWinPS {
    param([Parameter(Mandatory = $true)][string]$ScriptPath, [Parameter(Mandatory = $true)]$Bound)
    $a = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-STA', '-File', $ScriptPath)
    foreach ($k in $Bound.Keys) {
        $v = $Bound[$k]
        if ($v -is [System.Management.Automation.SwitchParameter]) { if ($v.IsPresent) { $a += ('-' + $k) } }
        elseif ($v -is [System.Collections.IDictionary]) { throw ('SIW_RELAUNCH_UNSUPPORTED_PARAMETER: -' + $k + ' (a hashtable) cannot be passed to Windows PowerShell 5.1 from this host; start the script from powershell.exe') }
        elseif ($v -is [System.Array]) { $a += ('-' + $k); $a += (@($v | ForEach-Object { [string]$_ }) -join $(if ($k -eq 'AuthorAccepted') { [string][char]31 } else { ';' })) }
        else { $a += ('-' + $k); $a += [string]$v }
    }
    $exe = $script:PowerShellExe
    $old = $env:PSModulePath; $env:PSModulePath = Get-SiewWinPSModulePath
    try { & $exe @a | Out-Host; $rc = $LASTEXITCODE } finally { $env:PSModulePath = $old }   # Out-Host: the child's lines are shown, not returned with the exit code
    if ($null -eq $rc) { $rc = 1 }
    return [int]$rc
}
# Private values come from the git-ignored local configuration config/private_local.json (template: config/private_local.template.json; restored by the private kit).
# Without it, no input is ever recognised as the accepted study CSV (every other CSV is NOT_THE_ACCEPTED_INPUT; the synthetic demo is unaffected).
function Get-SiewPrivateLocal {
    param([Parameter(Mandatory = $true)][string]$Key)
    $f = Join-Path (Split-Path -Parent $PSScriptRoot) 'config\private_local.json'
    if (-not (Test-Path -LiteralPath $f -PathType Leaf)) { return '' }
    try { $j = Get-Content -LiteralPath $f -Raw -Encoding UTF8 | ConvertFrom-Json; $v = $j.PSObject.Properties[$Key]; if ($null -eq $v -or $null -eq $v.Value) { return '' }; return [string]$v.Value } catch { return '' }
}
$script:AcceptedInputSha = (Get-SiewPrivateLocal 'accepted_input_sha256').ToUpper()
if ($script:AcceptedInputSha -notmatch '^[0-9A-F]{64}$') { $script:AcceptedInputSha = 'NOT_CONFIGURED' }
$script:SyntheticInputSha = 'C2462ED6286C4CCCDBE21913F164AB086B1C0D2DC19CC6FC18337301CF99C4D8'
$script:AnomFailedRunId = 'anomhist_20261004_141846_28bd'
$script:ReusableClasses = @('PASS', 'REVIEW', 'FAIL', 'NOT_TESTED', 'DIAGNOSTIC')

# Documented exit codes of each wrapper (see the .NOTES of each run_*.ps1). Any other code is an EXECUTION_FAILURE.
$script:StepTable = @(
    [pscustomobject]@{ Id = '03'; Name = 'PAPER';               Script = 'run_paper.ps1';                       Prefix = 'paper_';         Codes = @{ 0 = 'PASS'; 1 = 'FAIL'; 2 = 'ARGUMENT_ERROR' };                                   Review = '' ; What = 'paper analysis: 1,680 cumulative specifications and the nine final L01-L56 DLMs' }
    [pscustomobject]@{ Id = '04'; Name = 'CORE_VALIDATION';     Script = 'run_validate.ps1';                    Prefix = 'validate_';      Codes = @{ 0 = 'PASS'; 1 = 'FAIL'; 2 = 'ARGUMENT_ERROR' };                                   Review = '' ; What = 'core strict validation (cumulative grid, final L01-L56 summary) against the core reference bundle' }
    [pscustomobject]@{ Id = '05'; Name = 'BATCH_A';             Script = 'run_batch_a.ps1';                     Prefix = 'batcha_';        Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 4 = 'FAIL' };         Review = '' ; What = 'Batch A (diagnostics, bivariate tests, descriptives) against core and Batch A bundles' }
    [pscustomobject]@{ Id = '06'; Name = 'BATCH_B';             Script = 'run_batch_b.ps1';                     Prefix = 'batchb_';        Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'every comparison passed but a documented legacy limitation was needed (Firth frozen bound and/or MASS::polr)'; What = 'Batch B (Firth, ordinal, transformations, nonlinear) against core, Batch A and Batch B bundles' }
    [pscustomobject]@{ Id = '07'; Name = 'ORDINAL_REFINED';     Script = 'run_ordinal_refinement.ps1';          Prefix = 'ordinalrefine_'; Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 4 = 'FAIL' };         Review = '' ; What = 'refined ordinal protocol (80 fits) with legacy reproduction' }
    [pscustomobject]@{ Id = '08'; Name = 'BATCH_C';             Script = 'run_batch_c.ps1';                     Prefix = 'batchc_';        Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 4 = 'FAIL' };         Review = '' ; What = 'Batch C cumulative bootstraps (180 targets x 2,000 resamples) against the Batch C bundle' }
    [pscustomobject]@{ Id = '09'; Name = 'DLM_V2';              Script = 'run_dlm_validation_v2.ps1';           Prefix = 'dlmvalidatev2_'; Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'required statuses PASS; manuscript concordance REVIEW and/or figure/dlnm NOT_TESTED (documented)'; What = 'DLM validation V2 run (input of V2.1) against the DLM bundle' }
    [pscustomobject]@{ Id = '10'; Name = 'DLM_V21';             Script = 'run_dlm_validation_v21_covariance.ps1'; Prefix = 'dlmv21cov_';   Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'new V2.1 statuses PASS; carried-forward REVIEW/NOT_TESTED items remain (expected)'; What = 'DLM V2.1 covariance rule on the V2 run (final L01-L56 acceptance basis)' }
    [pscustomobject]@{ Id = '11'; Name = 'ARCHIVED_DLM';        Script = 'run_archived_dlm_validation.ps1';     Prefix = 'dlmarchived_';   Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'compared statuses PASS; separate-validation items open (expected)'; What = 'archived L01-L59 grid (168) and lag-form sensitivity (216), deterministic quantities' }
    [pscustomobject]@{ Id = '12'; Name = 'COLD_BURDEN';         Script = 'run_cold_burden_validation.ps1';      Prefix = 'coldburden_';    Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'all statuses PASS; scientific interpretation not assessed (expected)'; What = 'exploratory cold-burden scan (22,302 models)' }
    [pscustomobject]@{ Id = '13'; Name = 'DAILY_LAYER';         Script = 'run_daily_layer_validation.ps1';      Prefix = 'dailylayer_';    Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'reproduction and verification PASS; MAXT permutation assumptions under REVIEW (expected)'; What = 'archived daily-lag layer (XYD, DAY, MAXT)' }
    [pscustomobject]@{ Id = '14'; Name = 'ANOM_ALT';            Script = 'run_anomaly_history_validation.ps1';  Prefix = 'anomhist_';      Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 4 = 'FAIL' };         Review = '' ; What = 'exploratory ANOM (7,080) and ALT (312) families' }
    [pscustomobject]@{ Id = '15'; Name = 'ANOM_SAMPLE_SD_DIAGNOSTIC'; Script = 'validation\anom_sample_sd_diag.R'; Prefix = 'anomdiag';     Codes = @{ 0 = 'DIAGNOSTIC'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR' };                Review = '' ; What = 'post hoc ANOM equal-n sample-SD diagnostic on the failed run (decision only; changes no status)' }
    [pscustomobject]@{ Id = '16'; Name = 'CODED_MODEL_RECORD';  Script = 'scripts\make_coded_model_record.R';   Prefix = 'coded_model_record_'; Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 4 = 'FAIL' };   Review = '' ; What = 'coded model record v2 (stable codes and families of the cumulative, final L01-L56 and archived models; written beside, never over, the legacy record)' }
    # post hoc steps of config/posthoc_sensitivity_protocol_v2.json (steps 17-26 of 13 Oct 2026, steps 27-30 of 14 Oct 2026): labelled SENSITIVITY / POST_HOC / RECONSTRUCTION; they cannot change q90, q9 or any accepted status
    [pscustomobject]@{ Id = '17'; Name = 'EXPOSURE_EXPORT';       Item = 'exposure_export';    Script = 'run_posthoc.ps1'; Prefix = 'ph_exposure_export_';    Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'POST_HOC: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'POST_HOC aggregate exposure export (participant-days, UTCI categories, rainfall days, date span, winter/spring UTCI, minimum UCLA count)' }
    [pscustomobject]@{ Id = '18'; Name = 'PH_DELETION';           Item = 'deletion';           Script = 'run_posthoc.ps1'; Prefix = 'ph_deletion_';           Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'SENSITIVITY: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'SENSITIVITY selected deletion refits (6 models x 88 = 528) against the frozen V1.4 output' }
    [pscustomobject]@{ Id = '19'; Name = 'PH_L59_BOOTSTRAP';      Item = 'l59_bootstrap';      Script = 'run_posthoc.ps1'; Prefix = 'ph_l59_bootstrap_';      Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'SENSITIVITY: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'SENSITIVITY archived L01-L59 DLM bootstraps (18 rows: participant 2,000 / date 1,000) against the frozen V1.4 output' }
    [pscustomobject]@{ Id = '20'; Name = 'PH_NO_APRIL';           Item = 'no_april';           Script = 'run_posthoc.ps1'; Prefix = 'ph_no_april_';           Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'SENSITIVITY: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'SENSITIVITY no-April restriction of the 90 primary cumulative models against the archive engine' }
    [pscustomobject]@{ Id = '21'; Name = 'PH_SOURCE_AGREEMENT';   Item = 'source_agreement';   Script = 'run_posthoc.ps1'; Prefix = 'ph_source_agreement_';   Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'SENSITIVITY: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'SENSITIVITY ERA5-IMS source agreement (80 rows) against the archive engine' }
    [pscustomobject]@{ Id = '22'; Name = 'PH_EXPOSURE_RESIDUALS'; Item = 'exposure_residuals'; Script = 'run_posthoc.ps1'; Prefix = 'ph_exposure_residuals_'; Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'SENSITIVITY: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'SENSITIVITY exposure residual diagnostics (27 regressions) against the archive engine' }
    [pscustomobject]@{ Id = '23'; Name = 'PH_PROFILE_LIKELIHOOD'; Item = 'profile_likelihood'; Script = 'run_posthoc.ps1'; Prefix = 'ph_profile_likelihood_'; Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'SENSITIVITY: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'SENSITIVITY penalized profile likelihood of the 30 representative integration models (base R) against the archive engine' }
    [pscustomobject]@{ Id = '24'; Name = 'PH_EARLY_LATE';         Item = 'early_late';         Script = 'run_posthoc.ps1'; Prefix = 'ph_early_late_';         Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'RECONSTRUCTION: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'RECONSTRUCTION early/late analyses (SI Table S11): interaction rows reproduced, subgroup fits and q values reconstructed' }
    [pscustomobject]@{ Id = '25'; Name = 'PH_H3_NESTED';          Item = 'h3_nested';          Script = 'run_posthoc.ps1'; Prefix = 'ph_h3_nested_';          Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'POST_HOC: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'POST_HOC H3 nested test (constant versus varying lag weights; nine pairs; q9-nested)' }
    [pscustomobject]@{ Id = '26'; Name = 'PH_NEAREST_STATION';    Item = 'nearest_station';    Script = 'run_posthoc.ps1'; Prefix = 'ph_nearest_station_';    Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'POST_HOC: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'POST_HOC nearest-station check (120 IMS cumulative fits, six L01-L56 DLMs; 6 Oct 2026 analysis reimplemented in R)' }
    [pscustomobject]@{ Id = '27'; Name = 'PH_EXPOSURE_QA';        Item = 'exposure_qa';        Script = 'run_posthoc.ps1'; Prefix = 'ph_exposure_qa_';        Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'RECONSTRUCTION: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'RECONSTRUCTION exposure QA of the analysed IMS run (source identity with the canonical CSV, station coverage with the DST and 1-minute-rain rules, distances, station pool)' }
    [pscustomobject]@{ Id = '28'; Name = 'PH_VIF_SUMMARY';        Item = 'vif_summary';        Script = 'run_posthoc.ps1'; Prefix = 'ph_vif_summary_';        Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'RECONSTRUCTION: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'RECONSTRUCTION VIF summary of the 240 M2-L cumulative models (maximum and per-column VIFs)' }
    [pscustomobject]@{ Id = '29'; Name = 'PH_CALENDAR_SPLINE';    Item = 'calendar_spline';    Script = 'run_posthoc.ps1'; Prefix = 'ph_calendar_spline_';    Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'SENSITIVITY: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'SENSITIVITY calendar spline: the 90 representative M2-L models with ns(assessment day, df 2 and 3) against the archive engine' }
    [pscustomobject]@{ Id = '30'; Name = 'PH_AGE65';              Item = 'age65';              Script = 'run_posthoc.ps1'; Prefix = 'ph_age65_';              Codes = @{ 0 = 'PASS'; 1 = 'EXECUTION_FAILURE'; 2 = 'ARGUMENT_ERROR'; 3 = 'REVIEW'; 4 = 'FAIL' }; Review = 'SENSITIVITY: no status failed, but a REVIEW or NOT_TESTED status remains'; What = 'SENSITIVITY age 65+ restriction: the 90 representative M2-L models without participants aged under 65 (same estimators; q90-age65)' }
)
# reference roles of the post hoc steps (optional keys of the paths file; a missing role makes the step NOT_TESTED)
$script:PosthocRoleKeys = @('posthoc_v14_reference', 'archive_engine_reference', 'nearest_station_histories', 'gis_ims_v8_1_reference')
$script:PosthocStepRoles = @{ EXPOSURE_EXPORT = @(); PH_DELETION = @('posthoc_v14_reference'); PH_L59_BOOTSTRAP = @('posthoc_v14_reference'); PH_NO_APRIL = @('archive_engine_reference'); PH_SOURCE_AGREEMENT = @('archive_engine_reference')
    PH_EXPOSURE_RESIDUALS = @('archive_engine_reference'); PH_PROFILE_LIKELIHOOD = @('archive_engine_reference'); PH_EARLY_LATE = @('archive_engine_reference'); PH_H3_NESTED = @('core_reference'); PH_NEAREST_STATION = @('nearest_station_histories')
    PH_EXPOSURE_QA = @('gis_ims_v8_1_reference'); PH_VIF_SUMMARY = @(); PH_CALENDAR_SPLINE = @('archive_engine_reference'); PH_AGE65 = @() }

# Inputs each step needs (project mode: a step whose input is absent is NOT_TESTED with the reason; classic mode requires all of them up front).
$script:StepInputs = @{
    CORE_VALIDATION = @('core_reference'); BATCH_A = @('core_reference', 'batch_a_reference'); BATCH_B = @('core_reference', 'batch_a_reference', 'batch_b_reference'); ORDINAL_REFINED = @('batch_b_reference')
    BATCH_C = @('batch_c_reference'); DLM_V2 = @('dlm_reference'); ARCHIVED_DLM = @('archived_dlm_reference'); COLD_BURDEN = @('cold_burden_reference'); DAILY_LAYER = @('daily_layer_reference')
    ANOM_ALT = @('anomhist_reference'); ANOM_SAMPLE_SD_DIAGNOSTIC = @('anom_failed_run', 'anomhist_reference')
}
foreach ($k in $script:PosthocStepRoles.Keys) { if (@($script:PosthocStepRoles[$k]).Count) { $script:StepInputs[$k] = @($script:PosthocStepRoles[$k]) } }

# Demo mode: which synthetic test file covers each study-reference step that cannot run without frozen study references.
$script:DemoCoverage = @{
    BATCH_A = 'tests/testthat/test-13-batch-a.R'; BATCH_B = 'tests/testthat/test-14-batch-b.R'; ORDINAL_REFINED = 'tests/testthat/test-15-ordinal-refinement.R'
    BATCH_C = 'tests/testthat/test-17-batch-c.R'; DLM_V2 = 'tests/testthat/test-21-dlm-validation-v2.R'; DLM_V21 = 'tests/testthat/test-22-dlm-v21-covariance.R'
    ARCHIVED_DLM = 'tests/testthat/test-23-archived-dlm-validation.R, tests/testthat/test-27-archived-glm-epsilon.R'; COLD_BURDEN = 'tests/testthat/test-24-cold-burden.R'
    DAILY_LAYER = 'tests/testthat/test-25-daily-layer.R, validation/test_daily_layer_wrapper.ps1'; ANOM_ALT = 'tests/testthat/test-26-anomaly-history.R, validation/test_anomaly_history_wrapper.ps1'
}

# ---------- pure functions (no I/O) ----------

# Class of one step from the wrapper's actual exit code and its documented exit-code table.
function Resolve-SiewStepClass {
    param([Parameter(Mandatory = $true)]$Step, [Parameter(Mandatory = $true)][int]$ExitCode, [switch]$Demo)
    if ($Demo -and $Step.Name -eq 'ANOM_SAMPLE_SD_DIAGNOSTIC') {
        if ($ExitCode -eq 0) { return [pscustomobject]@{ Class = 'PASS'; Meaning = 'synthetic test of the diagnostic: every check passed (exit 0)' } }
        return [pscustomobject]@{ Class = 'FAIL'; Meaning = ("synthetic test of the diagnostic: a check failed or the test did not run (exit {0})" -f $ExitCode) }
    }
    if ($Step.Codes.ContainsKey($ExitCode)) {
        $c = $Step.Codes[$ExitCode]
        $m = switch ($c) {
            'PASS' { 'exit 0: every status of this step PASS' }
            'REVIEW' { 'exit 3: ' + $Step.Review + '; not counted as PASS' }
            'FAIL' { ("exit {0}: a status of this step FAILED or evidence is missing" -f $ExitCode) }
            'EXECUTION_FAILURE' { ("exit {0}: execution failure" -f $ExitCode) }
            'ARGUMENT_ERROR' { ("exit {0}: invalid arguments or R missing" -f $ExitCode) }
            'DIAGNOSTIC' { 'exit 0: diagnostic completed; its decision is informational and changes no status' }
        }
        return [pscustomobject]@{ Class = $c; Meaning = $m }
    }
    return [pscustomobject]@{ Class = 'EXECUTION_FAILURE'; Meaning = ("exit {0}: not a documented exit code of {1}; nothing is accepted" -f $ExitCode, $Step.Script) }
}

# Overall outcome. 1 = a step did not execute (or the run stopped); 4 = a FAIL; 3 = REVIEW, NOT_TESTED or DIAGNOSTIC present; 0 = every step PASS.
# In demo mode the declared demo NOT_TESTED steps (no frozen study reference exists for synthetic data) do not prevent 0; they are still listed as NOT_TESTED.
function Resolve-SiewOverall {
    param([Parameter(Mandatory = $true)][object[]]$Results, [switch]$Demo, [switch]$Stopped)
    $cls = @($Results | ForEach-Object { $_.Class })
    $count = [ordered]@{}; foreach ($k in @('PASS', 'REVIEW', 'FAIL', 'NOT_TESTED', 'DIAGNOSTIC', 'EXECUTION_FAILURE', 'ARGUMENT_ERROR')) { $count[$k] = @($cls | Where-Object { $_ -eq $k }).Count }
    $openNT = @($Results | Where-Object { $_.Class -eq 'NOT_TESTED' -and -not ($Demo -and $_.DeclaredDemo) }).Count
    if ($Stopped -or $count['EXECUTION_FAILURE'] -gt 0 -or $count['ARGUMENT_ERROR'] -gt 0) { $code = 1; $kind = 'INCOMPLETE' }
    elseif ($count['FAIL'] -gt 0) { $code = 4; $kind = 'COMPLETED_WITH_FAIL' }
    elseif ($count['REVIEW'] -gt 0 -or $openNT -gt 0 -or $count['DIAGNOSTIC'] -gt 0) { $code = 3; $kind = 'COMPLETED_WITH_REVIEW_NOT_TESTED_OR_DIAGNOSTIC' }
    elseif ($Demo) { $code = 0; $kind = 'DEMO_GREEN' }
    else { $code = 0; $kind = 'ALL_PASS' }
    return [pscustomobject]@{ Code = $code; Kind = $kind; Counts = $count }
}

function Test-SiewInside {
    param([string]$Inner, [string]$Outer)
    if ([string]::IsNullOrWhiteSpace($Inner) -or [string]::IsNullOrWhiteSpace($Outer)) { return $false }
    $a = $Inner.TrimEnd('\').ToLowerInvariant(); $b = $Outer.TrimEnd('\').ToLowerInvariant()
    return (($a -eq $b) -or $a.StartsWith($b + '\'))
}

# ---------- small I/O helpers ----------

function ConvertTo-SiewFullPath {
    param([string]$Path, [string]$Base = '')
    if ([string]::IsNullOrWhiteSpace($Path)) { return '' }
    $p = $Path
    if (-not [System.IO.Path]::IsPathRooted($p) -and $Base -ne '') { $p = Join-Path $Base $p }
    return ([System.IO.Path]::GetFullPath($p)).TrimEnd('\')
}

# A list of files (nearest-station export variants): a JSON array, or one string with the paths separated by '|' (a character Windows forbids in paths). Returns the full paths joined by '|'.
function ConvertTo-SiewPathList {
    param($Property, [string]$Base = '')
    if ($null -eq $Property -or $null -eq $Property.Value) { return '' }
    $items = $(if ($Property.Value -is [System.Array]) { @($Property.Value | ForEach-Object { [string]$_ }) } else { @(([string]$Property.Value) -split '\|') })
    return ((@($items | Where-Object { -not [string]::IsNullOrWhiteSpace($_) } | ForEach-Object { ConvertTo-SiewFullPath $_ $Base })) -join '|')
}

# Write a whole text file atomically: a .partial file is written first and then moved into place.
function Write-SiewAtomic {
    param([Parameter(Mandatory = $true)][string]$Path, [Parameter(Mandatory = $true)][AllowEmptyCollection()][AllowEmptyString()][string[]]$Lines)
    $tmp = $Path + '.partial'
    [System.IO.File]::WriteAllLines($tmp, $Lines, (New-Object System.Text.UTF8Encoding($false)))
    if (Test-Path -LiteralPath $Path) { [System.IO.File]::Replace($tmp, $Path, [NullString]::Value) } else { [System.IO.File]::Move($tmp, $Path) }
}

function Read-SiewKeyValue {
    param([string]$Path)
    $h = [ordered]@{}
    if ($Path -ne '' -and (Test-Path -LiteralPath $Path -PathType Leaf)) { foreach ($l in [System.IO.File]::ReadAllLines($Path)) { if ($l -match '^([A-Za-z][A-Za-z0-9_]*)=(.*)$') { $h[$matches[1]] = $matches[2] } } }
    return $h
}

# Every KEY=VALUE line of the step's *STATUS*.txt files and every row of VALIDATION_STATUS.csv, verbatim.
function Get-SiewStatusLines {
    param([string]$RunDir)
    if ([string]::IsNullOrWhiteSpace($RunDir) -or -not (Test-Path -LiteralPath $RunDir -PathType Container)) { return }
    foreach ($f in (Get-ChildItem -LiteralPath $RunDir -File | Where-Object { $_.Name -like '*STATUS*.txt' } | Sort-Object Name)) {
        foreach ($l in [System.IO.File]::ReadAllLines($f.FullName)) { if ($l -match '^([A-Z][A-Z0-9_]*)=(.*)$') { [pscustomobject]@{ file = $f.Name; key = $matches[1]; value = $matches[2] } } }
    }
    $vs = Join-Path $RunDir 'VALIDATION_STATUS.csv'
    if (Test-Path -LiteralPath $vs -PathType Leaf) {
        $i = 0
        foreach ($r in (Import-Csv -LiteralPath $vs)) { $i++; [pscustomobject]@{ file = 'VALIDATION_STATUS.csv'; key = ('row_' + $i); value = (($r.PSObject.Properties | ForEach-Object { '{0}={1}' -f $_.Name, $_.Value }) -join '; ') } }
    }
}

function Get-SiewFreeGB {
    param([string]$Path)
    $root = [System.IO.Path]::GetPathRoot($Path)
    return [math]::Round(((New-Object System.IO.DriveInfo($root)).AvailableFreeSpace / 1GB), 2)
}

# Run a child process (PowerShell wrapper or Rscript); every console line is shown indented and appended to $LogPath. Returns the exit code only.
function Invoke-SiewChild {
    param([Parameter(Mandatory = $true)][string]$Exe, [Parameter(Mandatory = $true)][string[]]$Arguments, [Parameter(Mandatory = $true)][string]$LogPath, [string]$WorkDir = '')
    $old = $ErrorActionPreference; $ErrorActionPreference = 'Continue'
    $here = Get-Location
    if ($WorkDir -ne '') { Set-Location -LiteralPath $WorkDir }
    $oldMod = $env:PSModulePath; if ((Split-Path -Leaf $Exe) -ieq 'powershell.exe') { $env:PSModulePath = Get-SiewWinPSModulePath }
    try {
        & $Exe @Arguments 2>&1 | ForEach-Object { $l = [string]$_; [System.IO.File]::AppendAllText($LogPath, $l + [Environment]::NewLine, (New-Object System.Text.UTF8Encoding($false))); Write-Host ('    ' + $l) }
        $rc = $LASTEXITCODE
    } finally { Set-Location $here; $ErrorActionPreference = $old; $env:PSModulePath = $oldMod }
    if ($null -eq $rc) { $rc = 1 }
    return [int]$rc
}

function Invoke-SiewWrapper {
    param([string]$RepoDir, [string]$Script, [string[]]$Arguments, [string]$LogPath)
    $all = @('-NoProfile', '-NonInteractive', '-ExecutionPolicy', 'Bypass', '-File', (Join-Path $RepoDir $Script)) + $Arguments
    return (Invoke-SiewChild -Exe $script:PowerShellExe -Arguments $all -LogPath $LogPath -WorkDir $RepoDir)
}

# ---------- run plan ----------

# Resolve and check every path. Study mode reads the paths file (JSON; relative paths are relative to the file's folder). Returns an object or throws 'SIW_FULLRUN_ARGS: ...'.
function Get-SiewRunPaths {
    param([string]$RepoDir, [string]$PathsFile, [switch]$Demo, [string]$OutRoot)
    $repo = ConvertTo-SiewFullPath $RepoDir
    $p = [ordered]@{ mode = $(if ($Demo) { 'demo' } else { 'study' }); repo = $repo }
    if ($Demo) {
        $p.data_csv = ConvertTo-SiewFullPath (Join-Path $repo 'inst\extdata\synthetic_siecc_winter.csv'); $p.expected_sha = $script:SyntheticInputSha
        $p.out_root = $(if ($OutRoot -ne '') { ConvertTo-SiewFullPath $OutRoot } else { Join-Path $repo 'runs' })
        foreach ($k in @('core_reference', 'batch_a_reference', 'batch_b_reference', 'batch_c_reference', 'dlm_reference', 'archived_dlm_reference', 'cold_burden_reference', 'daily_layer_reference', 'anomhist_reference', 'anom_failed_run', 'batch_b_legacy_run', 'batch_c_accepted_run', 'dlm_accepted_run') + $script:PosthocRoleKeys) { $p[$k] = '' }
        if (-not (Test-Path -LiteralPath $p.data_csv -PathType Leaf)) { throw 'SIW_FULLRUN_ARGS: the shipped synthetic CSV inst\extdata\synthetic_siecc_winter.csv is missing' }
        return [pscustomobject]$p
    }
    if ([string]::IsNullOrWhiteSpace($PathsFile) -or -not (Test-Path -LiteralPath $PathsFile -PathType Leaf)) { throw 'SIW_FULLRUN_ARGS: -PathsFile is required in study mode and must exist (see manual_run_paths.template.json)' }
    $pf = ConvertTo-SiewFullPath $PathsFile; $base = Split-Path -Parent $pf
    $j = Get-Content -LiteralPath $pf -Raw -Encoding UTF8 | ConvertFrom-Json
    $get = { param($k) $v = $j.PSObject.Properties[$k]; if ($null -eq $v -or $null -eq $v.Value) { '' } else { [string]$v.Value } }
    $req = @('core_reference', 'batch_a_reference', 'batch_b_reference', 'batch_c_reference', 'dlm_reference', 'archived_dlm_reference', 'cold_burden_reference', 'daily_layer_reference', 'anomhist_reference', 'anom_failed_run')
    $ph = @(@('data_csv', 'out_root') + $req + @('batch_b_legacy_run', 'batch_c_accepted_run', 'dlm_accepted_run') + $script:PosthocRoleKeys | Where-Object { (& $get $_) -match '[<>]' })
    if ($ph.Count) { throw ('SIW_FULLRUN_ARGS: placeholder not replaced in the paths file (a value still contains <...>): ' + ($ph -join ', ')) }
    $p.paths_file = $pf
    # project mode (written by the no-code launcher): absent inputs are allowed and become NOT_TESTED with a reason; the run folder is pre-created by the launcher
    $pmv = $j.PSObject.Properties['project_mode']; $p.project_mode = ($null -ne $pmv -and [bool]$pmv.Value)
    $p.project_demo = $false; $p.input_status = 'ACCEPTED'; $p.mode_label = ''; $p.run_dir = ''; $p.not_tested_reasons = @{}
    if ($p.project_mode) {
        $dv = $j.PSObject.Properties['demo']; $p.project_demo = ($null -ne $dv -and [bool]$dv.Value)
        $p.input_status = $(if ((& $get 'input_status') -eq 'NOT_THE_ACCEPTED_INPUT') { 'NOT_THE_ACCEPTED_INPUT' } else { 'ACCEPTED' })
        $p.mode_label = (& $get 'mode_label'); $p.run_dir = ConvertTo-SiewFullPath (& $get 'run_dir') $base
        $nt = $j.PSObject.Properties['not_tested_reasons']; if ($null -ne $nt -and $null -ne $nt.Value) { foreach ($q in $nt.Value.PSObject.Properties) { $p.not_tested_reasons[$q.Name] = [string]$q.Value } }
        # identification statuses beyond acceptance/protocol (CONFIRMED_BY_FROZEN_SOURCE, AUTHOR_ACCEPTED_EVIDENCE) with their evidence; shown in the run summary, never changes a step status
        $p.input_identification = @(); $ii = $j.PSObject.Properties['input_identification']
        if ($null -ne $ii -and $null -ne $ii.Value) { foreach ($q in $ii.Value.PSObject.Properties) { $p.input_identification += ('{0}: {1} - {2}' -f $q.Name, [string]$q.Value.status, [string]$q.Value.evidence) } }
        $p.step_identification = @(); $si = $j.PSObject.Properties['step_identification']
        if ($null -ne $si -and $null -ne $si.Value) { foreach ($q in $si.Value.PSObject.Properties) { foreach ($e in @($q.Value)) { $p.step_identification += ('{0}: {1}' -f $q.Name, [string]$e) } } }
    }
    $p.data_csv = ConvertTo-SiewFullPath (& $get 'data_csv') $base; $p.expected_sha = $(if ($p.project_demo) { $script:SyntheticInputSha } else { $script:AcceptedInputSha })
    $p.out_root = $(if ($OutRoot -ne '') { ConvertTo-SiewFullPath $OutRoot } else { ConvertTo-SiewFullPath (& $get 'out_root') $base })
    foreach ($k in $req) { $p[$k] = ConvertTo-SiewFullPath (& $get $k) $base }
    foreach ($k in @('batch_b_legacy_run', 'batch_c_accepted_run', 'dlm_accepted_run') + $script:PosthocRoleKeys) { $p[$k] = $(if ($k -in @('nearest_station_histories', 'gis_ims_v8_1_reference')) { ConvertTo-SiewPathList $j.PSObject.Properties[$k] $base } else { ConvertTo-SiewFullPath (& $get $k) $base }) }
    if ($p.input_status -eq 'NOT_THE_ACCEPTED_INPUT') {          # validation against frozen references is meaningless for another input: every reference and run is dropped
        foreach ($k in $req + @('batch_b_legacy_run', 'batch_c_accepted_run', 'dlm_accepted_run') + $script:PosthocRoleKeys) { if ($p[$k] -ne '' -or -not $p.not_tested_reasons.ContainsKey($k)) { $p.not_tested_reasons[$k] = 'the data file is NOT_THE_ACCEPTED_INPUT: validation against the frozen references is not meaningful' }; $p[$k] = '' }
    }
    $bad = @()
    if ($p.data_csv -eq '' -or -not (Test-Path -LiteralPath $p.data_csv -PathType Leaf)) { $bad += 'data_csv (file not found)' } elseif ([System.IO.Path]::GetExtension($p.data_csv).ToLower() -ne '.csv') { $bad += 'data_csv (must be .csv)' }
    if ($p.out_root -eq '') { $bad += 'out_root (required)' }
    foreach ($k in $req + @('batch_b_legacy_run', 'batch_c_accepted_run', 'dlm_accepted_run')) {
        if ($p[$k] -eq '') { if (($req -contains $k) -and -not $p.project_mode) { $bad += ($k + ' (required)') }; continue }
        if (-not (Test-Path -LiteralPath $p[$k] -PathType Container)) { $bad += ($k + ' (folder not found)') }
    }
    if ($p.project_mode) {
        if ($p.run_dir -eq '' -or -not (Test-Path -LiteralPath $p.run_dir -PathType Container)) { $bad += 'run_dir (project mode: the launcher must pre-create it)' }
        elseif (-not (Test-SiewInside $p.run_dir $p.out_root) -or $p.run_dir.TrimEnd('\').ToLowerInvariant() -eq $p.out_root.TrimEnd('\').ToLowerInvariant()) { $bad += 'run_dir (must be a new folder inside out_root)' }
    }
    if ($p.anom_failed_run -ne '' -and -not (Test-Path -LiteralPath (Join-Path $p.anom_failed_run 'RUN_MANIFEST.csv') -PathType Leaf)) { $bad += 'anom_failed_run (RUN_MANIFEST.csv not found)' }
    # post hoc roles are optional in every mode: a missing role makes its steps NOT_TESTED (reference folders; the nearest-station histories are one CSV file)
    foreach ($k in @('posthoc_v14_reference', 'archive_engine_reference')) { if ($p[$k] -ne '' -and -not (Test-Path -LiteralPath $p[$k] -PathType Container)) { $bad += ($k + ' (folder not found)') } }
    foreach ($nf in @($p.nearest_station_histories -split '\|' | Where-Object { $_ -ne '' })) { if (-not (Test-Path -LiteralPath $nf -PathType Leaf)) { $bad += 'nearest_station_histories (file not found)' } }
    foreach ($gf in @($p.gis_ims_v8_1_reference -split '\|' | Where-Object { $_ -ne '' })) { if (-not (Test-Path -LiteralPath $gf)) { $bad += 'gis_ims_v8_1_reference (folder or file not found)' } }
    if ($bad.Count) { throw ('SIW_FULLRUN_ARGS: ' + ($bad -join '; ')) }
    $guard = @($repo, (Split-Path -Parent $p.data_csv)) + @($req | ForEach-Object { $p[$_] }) + @(@('batch_b_legacy_run', 'batch_c_accepted_run', 'dlm_accepted_run', 'posthoc_v14_reference', 'archive_engine_reference') | ForEach-Object { $p[$_] } | Where-Object { $_ -ne '' })
    foreach ($nf in @($p.nearest_station_histories -split '\|' | Where-Object { $_ -ne '' })) { $guard += (Split-Path -Parent $nf) }
    foreach ($gf in @($p.gis_ims_v8_1_reference -split '\|' | Where-Object { $_ -ne '' })) { $guard += $(if (Test-Path -LiteralPath $gf -PathType Container) { $gf } else { Split-Path -Parent $gf }) }
    foreach ($g in $guard) { if (Test-SiewInside $p.out_root $g) { throw ('SIW_FULLRUN_ARGS: out_root must be outside the package folder, the data folder and every reference or run folder (overlaps ' + $g + ')') } }
    return [pscustomobject]$p
}

# Notes for empty optional accepted-run keys (study mode). Pure function.
function Get-SiewReferenceNotes {
    param($P)
    if ($P.mode -ne 'study' -or $P.input_status -eq 'NOT_THE_ACCEPTED_INPUT') { return }
    if ($P.dlm_accepted_run -eq '' -and $P.dlm_reference -ne '') { 'dlm_accepted_run is empty: DLM V2 compares with the PAPER output of this same run (self-reference; proves the wiring only, not agreement with the historical accepted run).' }
    if ($P.batch_b_legacy_run -eq '' -and $P.batch_b_reference -ne '') { 'batch_b_legacy_run is empty: the refined-ordinal step makes no comparison with the historical accepted Batch B run (optional comparison not made).' }
    if ($P.batch_c_accepted_run -eq '' -and $P.batch_c_reference -ne '') { 'batch_c_accepted_run is empty: Batch C makes no point-estimate comparison with a historical accepted run (optional comparison not made).' }
}

# One aggregate progress line (time | event | free disk) on the console and, once the run folder exists, appended to run_progress.txt.
function Write-SiewProgress {
    param([string]$RunDir, [string]$Text, [string]$Colour = 'Gray')
    $probe = $(if ($RunDir -ne '') { $RunDir } else { $env:TEMP })
    $line = '{0} | {1} | free {2} GB' -f (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'), $Text, (Get-SiewFreeGB $probe)
    Write-Host ('PROGRESS ' + $line) -ForegroundColor $Colour
    if ($RunDir -ne '') { [System.IO.File]::AppendAllText((Join-Path $RunDir 'run_progress.txt'), $line + [Environment]::NewLine, (New-Object System.Text.UTF8Encoding($false))) }
}

# Arguments of one step, or a NOT_TESTED reason. $Done maps step name -> result object of this run.
# Every wrapper writes its own run folder directly inside the full-run folder ($RunRoot), which keeps paths about as short as a stand-alone wrapper run (Windows MAX_PATH).
function Get-SiewStepArgs {
    param($Step, $P, [string]$RunRoot, [hashtable]$Done, [switch]$Demo, [switch]$RainfallSensitivity, [string]$DemoReference = '', [string]$Stamp = '')
    $d = $P.data_csv; $a = @('-DataCsv', $d, '-OutRoot', $RunRoot)
    if ($P.project_mode -and $script:StepInputs.ContainsKey($Step.Name) -and -not ($P.project_demo -and $script:PosthocStepRoles.ContainsKey($Step.Name))) {
        foreach ($k in $script:StepInputs[$Step.Name]) {
            if ($P.$k -eq '') { $why = $(if ($P.not_tested_reasons.ContainsKey($k)) { $P.not_tested_reasons[$k] } else { 'no matching input was identified in the project folder' }); return [pscustomobject]@{ Args = $null; NotTested = ('input absent (' + $k + '): ' + $why); DeclaredDemo = $false } }
        }
    }
    if ($Demo -and $script:DemoCoverage.ContainsKey($Step.Name)) { return [pscustomobject]@{ Args = $null; NotTested = ('demo mode: no frozen study reference exists for synthetic data; covered by the synthetic tests ' + $script:DemoCoverage[$Step.Name]); DeclaredDemo = $true } }
    if ($script:PosthocStepRoles.ContainsKey($Step.Name)) {
        # post hoc steps: the shipped synthetic CSV (demo, or the launcher's demo project) runs every step with comparisons NOT_APPLICABLE_SYNTHETIC_INPUT; otherwise a missing role is NOT_TESTED
        $syn = ($Demo -or $P.project_demo)
        if ($P.input_status -eq 'NOT_THE_ACCEPTED_INPUT') { return [pscustomobject]@{ Args = $null; NotTested = 'the data file is NOT_THE_ACCEPTED_INPUT: post hoc comparisons with the study references and manuscript values are not meaningful'; DeclaredDemo = $false } }
        if (-not $syn) { foreach ($k in $script:PosthocStepRoles[$Step.Name]) { if ($P.$k -eq '') { $why = $(if ($P.not_tested_reasons.ContainsKey($k)) { $P.not_tested_reasons[$k] } else { 'not given in the paths file' }); return [pscustomobject]@{ Args = $null; NotTested = ('input absent (' + $k + '): ' + $why); DeclaredDemo = $false } } } }
        $a = @('-Item', $Step.Item, '-DataCsv', $d, '-OutRoot', $RunRoot)
        if (-not $syn) { foreach ($kv in @(@('-V14ReferenceDir', $P.posthoc_v14_reference), @('-ArchiveReferenceDir', $P.archive_engine_reference), @('-CoreReferenceDir', $P.core_reference), @('-NearestHistoriesCsv', $P.nearest_station_histories), @('-GisReference', $P.gis_ims_v8_1_reference))) { if ($kv[1] -ne '') { $a += @($kv[0], $kv[1]) } } }
        else { $a += '-SyntheticDemo' }
        return [pscustomobject]@{ Args = $a; NotTested = ''; DeclaredDemo = $false; R = $false }
    }
    switch ($Step.Name) {
        'PAPER' { if ($RainfallSensitivity) { $a += '-RainfallSensitivity' } }
        'CORE_VALIDATION' { $a += @('-ReferenceDir', $(if ($Demo) { $DemoReference } else { $P.core_reference })) }
        'BATCH_A' { $a += @('-CoreReferenceDir', $P.core_reference, '-BatchAReferenceDir', $P.batch_a_reference) }
        'BATCH_B' { $a += @('-CoreReferenceDir', $P.core_reference, '-BatchAReferenceDir', $P.batch_a_reference, '-BatchBReferenceDir', $P.batch_b_reference) }
        'ORDINAL_REFINED' { $a += @('-BatchBReferenceDir', $P.batch_b_reference); if ($P.batch_b_legacy_run -ne '') { $a += @('-LegacyRunDir', $P.batch_b_legacy_run) } }
        'BATCH_C' {
            $a += @('-BatchCReferenceDir', $P.batch_c_reference); if ($P.batch_c_accepted_run -ne '') { $a += @('-AcceptedRunDir', $P.batch_c_accepted_run) }
            $partial = @(Get-ChildItem -LiteralPath $RunRoot -Directory -ErrorAction SilentlyContinue | Where-Object { $_.Name -like 'batchc_*' -and (Test-Path -LiteralPath (Join-Path $_.FullName 'checkpoints') -PathType Container) } | Sort-Object LastWriteTime)
            if ($partial.Count -ge 1) { $a += @('-ResumeRunDir', $partial[-1].FullName) }
        }
        'DLM_V2' {
            $acc = $P.dlm_accepted_run
            if ($acc -eq '') { $pp = $Done['PAPER']; if ($null -eq $pp -or $pp.Class -ne 'PASS' -or $pp.RunDir -eq '') { return [pscustomobject]@{ Args = $null; NotTested = 'dependency: dlm_accepted_run is empty and the PAPER step of this run did not PASS'; DeclaredDemo = $false } }; $acc = $pp.RunDir }
            $a += @('-DlmReferenceDir', $P.dlm_reference, '-AcceptedRunDir', $acc)
        }
        'DLM_V21' {
            $v2 = $Done['DLM_V2']; if ($null -eq $v2 -or $v2.RunDir -eq '' -or $v2.Class -in @('NOT_TESTED', 'EXECUTION_FAILURE', 'ARGUMENT_ERROR')) { return [pscustomobject]@{ Args = $null; NotTested = 'dependency: the DLM_V2 step of this run produced no completed run folder'; DeclaredDemo = $false } }
            $a = @('-V2RunDir', $v2.RunDir, '-OutRoot', $RunRoot)
        }
        { $_ -in @('ARCHIVED_DLM', 'COLD_BURDEN', 'DAILY_LAYER', 'ANOM_ALT') } {
            $ref = @{ ARCHIVED_DLM = $P.archived_dlm_reference; COLD_BURDEN = $P.cold_burden_reference; DAILY_LAYER = $P.daily_layer_reference; ANOM_ALT = $P.anomhist_reference }[$Step.Name]
            $a += @('-ReferenceDir', $ref)
        }
        'ANOM_SAMPLE_SD_DIAGNOSTIC' {
            if ($Demo) { return [pscustomobject]@{ Args = @((Join-Path $P.repo 'validation\test_anom_sample_sd_diag_synthetic.R')); NotTested = ''; DeclaredDemo = $false; R = $true } }
            return [pscustomobject]@{ Args = @((Join-Path $P.repo $Step.Script), '--data', $d, '--run', $P.anom_failed_run, '--reference-dir', $P.anomhist_reference, '--out', (Join-Path $RunRoot ('anomdiag_' + $Stamp))); NotTested = ''; DeclaredDemo = $false; R = $true }
        }
        'CODED_MODEL_RECORD' {
            $ra = @((Join-Path $P.repo $Step.Script), '--out', (Join-Path $RunRoot ('coded_model_record_' + $Stamp)), '--label', (Split-Path -Leaf $RunRoot))
            foreach ($dep in @(@('PAPER', '--paper-dir'), @('ARCHIVED_DLM', '--archived-dir'))) { $x = $Done[$dep[0]]; if ($null -ne $x -and $x.RunDir -ne '' -and $x.Class -notin @('EXECUTION_FAILURE', 'ARGUMENT_ERROR')) { $ra += @($dep[1], $x.RunDir) } }
            return [pscustomobject]@{ Args = $ra; NotTested = ''; DeclaredDemo = $false; R = $true }
        }
    }
    return [pscustomobject]@{ Args = $a; NotTested = ''; DeclaredDemo = $false; R = $false }
}

# SHA-256 of a file; long paths (Windows MAX_PATH) are opened with the \\?\ prefix, which Get-FileHash in PowerShell 5.1 cannot do.
function Get-SiewSha256 {
    param([string]$Path)
    $p = $Path; if ($p.Length -ge 240 -and -not $p.StartsWith('\\?\')) { $p = '\\?\' + $p }
    $sha = [System.Security.Cryptography.SHA256]::Create(); $fs = [System.IO.File]::OpenRead($p)
    try { return ([System.BitConverter]::ToString($sha.ComputeHash($fs)) -replace '-', '') } finally { $fs.Dispose(); $sha.Dispose() }
}

function Get-SiewManifestSha {
    param([string]$RunDir)
    if ($RunDir -eq '') { return '' }
    foreach ($n in @('RUN_MANIFEST.csv', 'MANIFEST.csv')) { $f = Join-Path $RunDir $n; if (Test-Path -LiteralPath $f -PathType Leaf) { return (Get-SiewSha256 $f) } }
    return ''
}

# ---------- summary, manifest ----------

function Write-SiewSummary {
    param([string]$RunDir, $P, [object[]]$Results, $Overall, $Pre, [string]$Started)
    $rel = { param($x) if ($x -eq '') { '' } else { $x.Substring([Math]::Min($RunDir.Length + 1, $x.Length)) } }
    $csv = @('step_id,step,class,exit_code,run_folder,seconds,meaning')
    foreach ($r in $Results) { $csv += ('"{0}","{1}","{2}","{3}","{4}","{5}","{6}"' -f $r.Id, $r.Name, $r.Class, $r.ExitCode, (& $rel $r.RunDir), $r.Seconds, ($r.Meaning -replace '"', "'")) }
    Write-SiewAtomic (Join-Path $RunDir 'FULL_RUN_STATUS.csv') $csv
    $lines = @('step_id,step,source_file,key,value')
    foreach ($r in $Results) { foreach ($s in @(Get-SiewStatusLines $r.RunDir)) { $lines += ('"{0}","{1}","{2}","{3}","{4}"' -f $r.Id, $r.Name, $s.file, $s.key, ($s.value -replace '"', "'")) } }
    Write-SiewAtomic (Join-Path $RunDir 'FULL_RUN_STATUS_LINES.csv') $lines
    $c = $Overall.Counts
    $md = @("# Full run summary ($($P.mode) mode)", '', ('**Outcome: {0}** (exit code {1}). Counts: PASS {2}, REVIEW {3}, FAIL {4}, NOT_TESTED {5}, DIAGNOSTIC {6}, EXECUTION_FAILURE {7}, ARGUMENT_ERROR {8}.' -f $Overall.Kind, $Overall.Code, $c['PASS'], $c['REVIEW'], $c['FAIL'], $c['NOT_TESTED'], $c['DIAGNOSTIC'], $c['EXECUTION_FAILURE'], $c['ARGUMENT_ERROR']), '')
    $md += 'Only PASS means PASS. REVIEW (wrapper exit 3: required statuses passed, documented review or not-tested items remain), NOT_TESTED, DIAGNOSTIC (informational decision) and FAIL are never counted as PASS. Each step''s own statuses are listed verbatim below and in `FULL_RUN_STATUS_LINES.csv`.'
    if ($P.project_mode) {
        $md += ''; $md += ('Project mode: **{0}**. Input: **{1}**. Steps whose inputs were not found in the project folder are NOT_TESTED with the reason shown.' -f $P.mode_label, $(if ($P.project_demo) { 'DEMO_SYNTHETIC_INPUT (the shipped synthetic CSV; numbers have no scientific meaning)' } else { $P.input_status }))
        if ($P.input_status -eq 'NOT_THE_ACCEPTED_INPUT') { $md += ''; $md += '**NOT_THE_ACCEPTED_INPUT: the data file is not the accepted canonical CSV. Every output of this run is exploratory and labelled; no validation step was run.**' }
        if (@($P.input_identification).Count) {
            $md += ''; $md += '## Input identification (post hoc declaration)'; $md += ''
            $md += 'These reference inputs are identified by evidence other than an acceptance-time hash or a protocol hash. Their identification status does not change any step status.'
            foreach ($x in @($P.input_identification)) { $md += ('* ' + $x) }
            if (@($P.step_identification).Count) { $md += ''; $md += 'Steps that use them:'; foreach ($x in @($P.step_identification)) { $md += ('* ' + $x) } }
        }
    }
    if ($P.mode -eq 'demo') { $md += ''; $md += 'Demo mode: fully synthetic input. Steps that need frozen study references are NOT_TESTED by design (no such reference exists for synthetic data); the synthetic tests named in each row cover them. The core validation compares the demo run with a reference generated from the same demo run (workflow check only). Synthetic numbers have no scientific meaning.' }
    $md += ''; $md += '## Preflight'; $md += ''
    foreach ($k in @('R_VERSION', 'R_VERSION_OK', 'IMPORT_MASS', 'IMPORT_jsonlite', 'PACKAGE_VERSION', 'CONFIG_SHA256', 'CONFIG_OK', 'SOURCE_FINGERPRINT', 'SOURCE_FILES', 'FINAL_DLM_GLM_EPSILON', 'ARCHIVED_GLM_EPSILON', 'INPUT_SHA256', 'INPUT_SHA256_EXPECTED', 'INPUT_SHA256_OK', 'INPUT_SHA256_POWERSHELL', 'ENVIRONMENT_STATUS', 'FREE_GB_AT_START')) { if ($Pre.Contains($k)) { $md += ('* {0} = {1}' -f $k, $Pre[$k]) } }
    $md += ''; $md += '## Steps'; $md += ''; $md += '| Step | Class | Exit | Seconds | Run folder | Meaning |'; $md += '|---|---|---|---|---|---|'
    foreach ($r in $Results) { $md += ('| {0} {1} | **{2}** | {3} | {4} | `{5}` | {6} |' -f $r.Id, $r.Name, $r.Class, $r.ExitCode, $r.Seconds, (& $rel $r.RunDir), $r.Meaning) }
    $md += ''; $md += '## Carried statuses (not changed by this run)'; $md += ''
    $md += '* Final L01-L56 numerical validation: ACCEPTED under post hoc V2.1. The DLM_V2 / DLM_V21 rows above are new evidence; they do not change the acceptance. Historical V1 FAIL records are preserved.'
    $md += ('* ANOM (post hoc protocol v2, author decision 12 Oct 2026): step 14 ANOM_ALT is governed by ANOM_VERIFICATION_STATUS_V2, in which the gaussian implied SD of every row is compared with the directly measured SD of that row''s own complete-case sample (tolerance 1e-9). ANOM_VERIFICATION_STATUS (the v1 composition, with the by-n row) is still computed and shown, superseded, kept for the record. The earlier runs {0} and run_20261012_114559_4737 keep ANOM_VERIFICATION_STATUS=FAIL in their own records. The step-15 diagnostic decision is informational.' -f $script:AnomFailedRunId)
    $md += '* Archived L01-L59 results remain labelled ARCHIVED (validation only; never mixed with the final L01-L56 models).'
    $md += '* Post hoc steps 17-30 (config/posthoc_sensitivity_protocol_v2.json: steps 17-26 declared 13 Oct 2026, steps 27-30 on 14 Oct 2026, after the 30 July 2026 family lock) are labelled SENSITIVITY, POST_HOC or RECONSTRUCTION. They read the primary results and never write them: q90, q9, q9-nested and every accepted status are unchanged by them. Their statuses are shown verbatim; REVIEW and NOT_TESTED are never counted as PASS.'
    $md += '* J02 (archived glm epsilon) is implemented in the archived code since 2026-10-06: siw_archived_glm_control gives the archived binary fits glm epsilon 1e-10, maxit 200; the final L01-L56 models keep 1e-12. The DAILY_LAYER and ANOM_ALT status texts still say J02_GLM_EPSILON_ENTRY_POINTS=OPEN (no change made): those source files are traced by the ANOM sample-SD diagnostic and were deliberately left unchanged, so that label is historical.'
    $md += '* Rainfall: the archival NA of the one IMS_IDW_RAIN_TOTAL_L35 cell is the default; the zero-mm treatment runs only with -RainfallSensitivity and is reported separately. Ages are reported as recorded; no participant is filtered.'
    $md += ''; $md += '## Accepted-run references'; $md += ''
    $notes = @(Get-SiewReferenceNotes $P)
    if ($P.mode -ne 'study') { $md += '* Demo mode: no historical accepted run applies.' } elseif ($P.input_status -eq 'NOT_THE_ACCEPTED_INPUT') { $md += '* Not applicable: the data file is NOT_THE_ACCEPTED_INPUT, so no comparison with a historical accepted run was made.' } elseif ($notes.Count) { foreach ($n in $notes) { $md += ('* NOTE: ' + $n) } } else { $md += '* dlm_accepted_run, batch_b_legacy_run and batch_c_accepted_run were all given (historical accepted runs).' }
    $md += ''; $md += '## Verbatim statuses per step'
    foreach ($r in $Results) {
        $md += ''; $md += ('### {0} {1} ({2})' -f $r.Id, $r.Name, $r.Class); $md += ''
        $st = @(Get-SiewStatusLines $r.RunDir); if ($r.Extra) { $st += @($r.Extra) }
        if ($st.Count -eq 0) { $md += ('_No status file. {0}_' -f $r.Meaning) } else { $md += '```'; foreach ($s in $st) { $md += ('{0}={1}' -f $s.key, $s.value) }; $md += '```' }
    }
    $md += ''; $md += ('Started {0}; finished {1}. Mode {2}. Data file: {3} (SHA-256 checked; contents never printed).' -f $Started, (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'), $P.mode, (Split-Path -Leaf $P.data_csv))
    Write-SiewAtomic (Join-Path $RunDir 'FULL_RUN_SUMMARY.md') $md
}

# SHA-256 of every file under the run folder (written last; the manifest does not list itself).
function Write-SiewFullManifest {
    param([string]$RunDir)
    $rows = @('file,bytes,sha256')
    # the launcher's live console logs (project mode) are still open while this runs; LAUNCHER_MANIFEST.csv records them afterwards
    $live = @((Join-Path $RunDir 'launcher\run_all_console.log'), (Join-Path $RunDir 'launcher\run_all_console_err.log'))
    foreach ($f in (Get-ChildItem -LiteralPath $RunDir -File -Recurse | Where-Object { $_.Name -ne 'FULL_RUN_MANIFEST.csv' -and $_.Name -notlike '*.partial' -and $live -notcontains $_.FullName } | Sort-Object FullName)) {
        $rows += ('"{0}",{1},{2}' -f $f.FullName.Substring($RunDir.Length + 1), $f.Length, (Get-SiewSha256 $f.FullName))
    }
    Write-SiewAtomic (Join-Path $RunDir 'FULL_RUN_MANIFEST.csv') $rows
}

# ---------- main ----------

function Invoke-SiewFullRun {
    param([Parameter(Mandatory = $true)][string]$RepoDir, [string]$PathsFile = '', [switch]$Demo, [string]$OutRoot = '', [string]$ResumeRunDir = '', [switch]$RainfallSensitivity, [double]$MinFreeGB = 2)
    $ErrorActionPreference = 'Stop'
    $started = Get-Date -Format 'yyyy-MM-dd HH:mm:ss'
    if (-not (Test-Path -LiteralPath $script:RscriptExe -PathType Leaf)) { Write-Host ("FAIL: R was not found at {0}" -f $script:RscriptExe) -ForegroundColor Red; return 2 }
    try { $P = Get-SiewRunPaths -RepoDir $RepoDir -PathsFile $PathsFile -Demo:$Demo -OutRoot $OutRoot } catch { Write-Host ('FAIL: ' + $_.Exception.Message) -ForegroundColor Red; return 2 }
    if ($ResumeRunDir -ne '') { $P.out_root = Split-Path -Parent (ConvertTo-SiewFullPath $ResumeRunDir) }
    if ($P.input_status -eq 'NOT_THE_ACCEPTED_INPUT') {
        $actual = (Get-FileHash -LiteralPath $P.data_csv -Algorithm SHA256).Hash
        if ($actual -eq $P.expected_sha) { Write-Host 'FAIL: input_status NOT_THE_ACCEPTED_INPUT was given, but the data file IS the expected input. Regenerate the plan.' -ForegroundColor Red; return 2 }
        $P.expected_sha = $actual
        Write-Host 'NOTE: the data file is NOT_THE_ACCEPTED_INPUT. The analysis runs for exploration only; every output is labelled; every validation step is NOT_TESTED.' -ForegroundColor Yellow
    }
    $refNotes = @(Get-SiewReferenceNotes $P)
    foreach ($n in $refNotes) { Write-Host ('NOTE: ' + $n) -ForegroundColor Yellow }
    $free = Get-SiewFreeGB $P.out_root
    if ($free -lt $MinFreeGB) { Write-Host ("FAIL: {0} GB free on the output drive; at least {1} GB required. Nothing was run." -f $free, $MinFreeGB) -ForegroundColor Red; return 2 }

    # 01-02 preflight in a fresh R process (always re-run, also on resume)
    $nSteps = 2 + $script:StepTable.Count; $tPre = Get-Date
    Write-SiewProgress '' ("step 01-02/{0} PREFLIGHT | START" -f $nSteps)
    Write-Host ("[01-02] environment, dependencies, configuration and input SHA-256 ({0} mode) ..." -f $P.mode)
    $tmpLog = [System.IO.Path]::GetTempFileName()
    $rcEnv = Invoke-SiewChild -Exe $script:RscriptExe -Arguments @((Join-Path $P.repo 'scripts\fullrun_env_check.R'), $P.repo, $P.data_csv, $P.expected_sha) -LogPath $tmpLog -WorkDir $P.repo
    $envLines = [System.IO.File]::ReadAllLines($tmpLog); Remove-Item -LiteralPath $tmpLog -Force
    $Pre = [ordered]@{}; foreach ($l in $envLines) { if ($l -match '^([A-Za-z][A-Za-z0-9_]*)=(.*)$') { $Pre[$matches[1]] = $matches[2] } }
    $psSha = (Get-FileHash -LiteralPath $P.data_csv -Algorithm SHA256).Hash
    $Pre['INPUT_SHA256_POWERSHELL'] = $psSha; $Pre['FREE_GB_AT_START'] = $free
    $hashOk = ($psSha -eq $P.expected_sha) -and ($Pre['INPUT_SHA256_OK'] -eq 'TRUE')
    if ($rcEnv -ne 0 -or $Pre['ENVIRONMENT_STATUS'] -ne 'PASS' -or -not $hashOk) {
        Write-Host ("FAIL - preflight refused (environment status {0}; input SHA-256 matches the expected file: {1}). Nothing was analysed." -f $Pre['ENVIRONMENT_STATUS'], $hashOk) -ForegroundColor Red
        foreach ($k in @('R_VERSION_OK', 'IMPORT_MASS_OK', 'IMPORT_jsonlite_OK', 'PACKAGE_LOAD_OK', 'CONFIG_OK', 'INPUT_SHA256_OK')) { if ($Pre.Contains($k) -and $Pre[$k] -ne 'TRUE') { Write-Host ("  {0}={1}" -f $k, $Pre[$k]) -ForegroundColor Red } }
        return 2
    }

    # run folder: new, or an earlier incomplete run of the same parameters
    $params = [ordered]@{ fullrun_version = '1'; mode = $P.mode; data_csv = $P.data_csv; data_sha256 = $psSha; source_fingerprint = $Pre['SOURCE_FINGERPRINT']; config_sha256 = $Pre['CONFIG_SHA256']; r_version = $Pre['R_VERSION']; rainfall_sensitivity = [bool]$RainfallSensitivity }
    foreach ($k in @('core_reference', 'batch_a_reference', 'batch_b_reference', 'batch_c_reference', 'dlm_reference', 'archived_dlm_reference', 'cold_burden_reference', 'daily_layer_reference', 'anomhist_reference', 'anom_failed_run', 'batch_b_legacy_run', 'batch_c_accepted_run', 'dlm_accepted_run') + $script:PosthocRoleKeys) { $params[$k] = $P.$k }
    if ($P.project_mode) { $params['project_mode'] = 'True'; $params['project_mode_label'] = $P.mode_label; $params['project_input_status'] = $(if ($P.project_demo) { 'DEMO_SYNTHETIC_INPUT' } else { $P.input_status }) }
    $paramLines = @($params.Keys | ForEach-Object { '{0}={1}' -f $_, $params[$_] })
    if ($ResumeRunDir -ne '') {
        $runDir = ConvertTo-SiewFullPath $ResumeRunDir
        $pf = Join-Path $runDir 'RUN_PARAMETERS.txt'
        if (-not (Test-Path -LiteralPath $pf -PathType Leaf)) { Write-Host 'FAIL: -ResumeRunDir is not a run_all.ps1 run folder (RUN_PARAMETERS.txt missing).' -ForegroundColor Red; return 2 }
        if (Test-Path -LiteralPath (Join-Path $runDir 'FULL_RUN_MANIFEST.csv')) { Write-Host 'FAIL: that run is already complete (FULL_RUN_MANIFEST.csv exists); it is never modified. Start a new run instead.' -ForegroundColor Red; return 2 }
        $old = Read-SiewKeyValue $pf; $diff = @($params.Keys | Where-Object { [string]$old[$_] -ne [string]$params[$_] })
        if ($diff.Count) { Write-Host ('FAIL: resume refused; parameters differ from the original run: ' + ($diff -join ', ')) -ForegroundColor Red; return 2 }
        Write-Host ("Resuming run folder: {0}" -f $runDir)
    } elseif ($P.project_mode) {
        $runDir = $P.run_dir
        $extra = @(Get-ChildItem -LiteralPath $runDir -Force | Where-Object { $_.Name -notin @('launcher', 'staged_references') })
        if ((Test-Path -LiteralPath (Join-Path $runDir 'RUN_PARAMETERS.txt')) -or $extra.Count) { Write-Host 'FAIL: the project run folder is not new (it already holds a run). Start the launcher again.' -ForegroundColor Red; return 2 }
        Write-SiewAtomic (Join-Path $runDir 'RUN_PARAMETERS.txt') $paramLines
        if ($P.input_status -eq 'NOT_THE_ACCEPTED_INPUT') { Write-SiewAtomic (Join-Path $runDir 'NOT_THE_ACCEPTED_INPUT.txt') @('NOT_THE_ACCEPTED_INPUT', ('The data file SHA-256 ' + $P.expected_sha + ' is not the accepted canonical CSV (' + $script:AcceptedInputSha + ').'), 'Every output of this run is exploratory; no validation step was run.') }
        Write-Host ("Run folder: {0}" -f $runDir)
    } else {
        New-Item -ItemType Directory -Path $P.out_root -Force | Out-Null
        $stamp = Get-Date -Format 'yyyyMMdd_HHmmss'; $runDir = $null
        for ($i = 0; $i -lt 50; $i++) { $cand = Join-Path $P.out_root ("{0}_{1}_{2}" -f $(if ($Demo) { 'fullrun_demo' } else { 'fullrun' }), $stamp, ('{0:x4}' -f (Get-Random -Maximum 65536))); if (-not (Test-Path -LiteralPath $cand)) { $runDir = $cand; break } }
        if ($null -eq $runDir) { Write-Host 'FAIL: could not create a unique run folder.' -ForegroundColor Red; return 2 }
        New-Item -ItemType Directory -Path $runDir | Out-Null; $runDir = (Resolve-Path -LiteralPath $runDir).Path
        Write-SiewAtomic (Join-Path $runDir 'RUN_PARAMETERS.txt') $paramLines
        Write-Host ("Run folder: {0}" -f $runDir)
    }
    Write-SiewProgress $runDir ("RUN {0} | mode {1} | {2} steps{3}" -f $(if ($ResumeRunDir -ne '') { 'RESUME' } else { 'START' }), $P.mode, $nSteps, $(if ($P.project_mode) { (' | project ' + $P.mode_label + ' | input ' + $(if ($P.project_demo) { 'DEMO_SYNTHETIC_INPUT' } else { $P.input_status })) } else { '' }))
    foreach ($n in $refNotes) { Write-SiewProgress $runDir ('NOTE | ' + $n) 'Yellow' }
    Write-SiewProgress $runDir ("step 01-02/{0} PREFLIGHT | END | PASS (exit 0) | {1} s" -f $nSteps, [math]::Round(((Get-Date) - $tPre).TotalSeconds, 1)) 'Green'
    New-Item -ItemType Directory -Path (Join-Path $runDir 'preflight') -Force | Out-Null
    $pstamp = Get-Date -Format 'yyyyMMdd_HHmmss'
    Write-SiewAtomic (Join-Path $runDir ("preflight\ENVIRONMENT_{0}.txt" -f $pstamp)) (@($Pre.Keys | ForEach-Object { '{0}={1}' -f $_, $Pre[$_] }))

    $results = New-Object System.Collections.ArrayList
    $results.Add([pscustomobject]@{ Id = '01'; Name = 'ENVIRONMENT'; Class = 'PASS'; ExitCode = $rcEnv; RunDir = ''; Seconds = ''; Meaning = ('R {0}, required packages, configuration SHA-256 and source fingerprint checked in a fresh R process' -f $Pre['R_VERSION']); DeclaredDemo = $false; Extra = @(@('R_VERSION_OK', 'IMPORT_MASS_OK', 'IMPORT_jsonlite_OK', 'CONFIG_OK', 'ENVIRONMENT_STATUS') | ForEach-Object { [pscustomobject]@{ key = $_; value = $Pre[$_] } }) }) | Out-Null
    if ($P.input_status -eq 'NOT_THE_ACCEPTED_INPUT') {
        $results.Add([pscustomobject]@{ Id = '02'; Name = 'INPUT_HASH'; Class = 'FAIL'; ExitCode = 0; RunDir = ''; Seconds = ''; Meaning = 'NOT_THE_ACCEPTED_INPUT: the input SHA-256 is not the accepted canonical CSV; the analysis ran for exploration only, every output is labelled, every validation step is NOT_TESTED'; DeclaredDemo = $false; Extra = @([pscustomobject]@{ key = 'INPUT_SHA256'; value = $psSha }, [pscustomobject]@{ key = 'INPUT_SHA256_ACCEPTED'; value = $script:AcceptedInputSha }, [pscustomobject]@{ key = 'INPUT_STATUS'; value = 'NOT_THE_ACCEPTED_INPUT' }) }) | Out-Null
    } else {
        $results.Add([pscustomobject]@{ Id = '02'; Name = 'INPUT_HASH'; Class = 'PASS'; ExitCode = 0; RunDir = ''; Seconds = ''; Meaning = ('SHA-256 of the input equals the {0} (R and PowerShell agree)' -f $(if ($Demo -or $P.project_demo) { 'documented synthetic CSV hash' } else { 'accepted canonical CSV hash' })); DeclaredDemo = $false; Extra = @([pscustomobject]@{ key = 'INPUT_SHA256'; value = $psSha }, [pscustomobject]@{ key = 'INPUT_SHA256_EXPECTED'; value = $P.expected_sha }) }) | Out-Null
    }

    $demoRef = ''
    $done = @{}; $stopped = $false
    foreach ($step in $script:StepTable) {
        $stepDir = Join-Path $runDir ("steps\{0}_{1}" -f $step.Id, $step.Name)
        $resFile = Join-Path $stepDir 'STEP_RESULT.txt'
        $prev = Read-SiewKeyValue $resFile
        $reusable = $prev.Count -and ($script:ReusableClasses -contains $prev['CLASS']) -and -not ($prev['CLASS'] -eq 'NOT_TESTED' -and $prev['DECLARED_DEMO'] -ne 'True')   # a dependency NOT_TESTED is re-evaluated
        if ($reusable) {
            $rd = $(if ($prev['RUN_FOLDER'] -ne '') { Join-Path $runDir $prev['RUN_FOLDER'] } else { '' })
            if ($rd -ne '' -and $prev['RUN_MANIFEST_SHA256'] -ne '' -and (Get-SiewManifestSha $rd) -ne $prev['RUN_MANIFEST_SHA256']) { Write-Host ("FAIL: the recorded output of step {0} {1} changed since it was recorded; resume refused." -f $step.Id, $step.Name) -ForegroundColor Red; return 1 }
            $r = [pscustomobject]@{ Id = $step.Id; Name = $step.Name; Class = $prev['CLASS']; ExitCode = $prev['EXIT_CODE']; RunDir = $rd; Seconds = $prev['SECONDS']; Meaning = ($prev['MEANING'] + ' (restored from STEP_RESULT.txt)'); DeclaredDemo = ($prev['DECLARED_DEMO'] -eq 'True'); Extra = $null }
            [void]$results.Add($r); $done[$step.Name] = $r; Write-Host ("[{0}] {1}: {2} (restored)" -f $step.Id, $step.Name, $r.Class)
            Write-SiewProgress $runDir ("step {0}/{1} {2} | RESTORED | {3} (exit {4})" -f $step.Id, $nSteps, $step.Name, $r.Class, $r.ExitCode); continue
        }
        $astamp = Get-Date -Format 'yyyyMMdd_HHmmss'
        foreach ($old in @($resFile, (Join-Path $stepDir 'step_console.log'))) { if (Test-Path -LiteralPath $old) { Move-Item -LiteralPath $old -Destination ($old + '.attempt_' + $astamp) } }   # earlier attempts are kept, never deleted
        New-Item -ItemType Directory -Path $stepDir -Force | Out-Null
        $freeNow = Get-SiewFreeGB $runDir
        if ($freeNow -lt $MinFreeGB) { Write-Host ("STOP: {0} GB free (< {1} GB) before step {2} {3}. Free space and resume with -ResumeRunDir." -f $freeNow, $MinFreeGB, $step.Id, $step.Name) -ForegroundColor Red; Write-SiewProgress $runDir ("step {0}/{1} {2} | STOPPED | free space below {3} GB" -f $step.Id, $nSteps, $step.Name, $MinFreeGB) 'Red'; $stopped = $true; break }
        if ($Demo -and $step.Name -eq 'CORE_VALIDATION') {
            $pp = $done['PAPER']
            if ($null -ne $pp -and $pp.Class -eq 'PASS') {
                $demoRef = Join-Path $runDir 'demo_self_reference'; New-Item -ItemType Directory -Path $demoRef -Force | Out-Null
                Copy-Item -LiteralPath (Join-Path $pp.RunDir 'cumulative_results.csv') -Destination (Join-Path $demoRef 'cumulative_regression_grid.csv') -Force
                Copy-Item -LiteralPath (Join-Path $pp.RunDir 'dlm_final_summary.csv') -Destination (Join-Path $demoRef 'DLM_L01_L56_nine_representative_summary.csv') -Force
            }
        }
        $sa = Get-SiewStepArgs -Step $step -P $P -RunRoot $runDir -Done $done -Demo:$Demo -RainfallSensitivity:$RainfallSensitivity -DemoReference $demoRef -Stamp $astamp
        $resumeDir = ''; $i = [array]::IndexOf([string[]]@($sa.Args), '-ResumeRunDir'); if ($i -ge 0) { $resumeDir = @($sa.Args)[$i + 1] }
        $before = @(Get-ChildItem -LiteralPath $runDir -Directory | ForEach-Object { $_.Name })
        if ($Demo -and $step.Name -eq 'CORE_VALIDATION' -and $demoRef -eq '') { $sa = [pscustomobject]@{ Args = $null; NotTested = 'dependency: the PAPER step of this run did not PASS'; DeclaredDemo = $false } }
        $t0 = Get-Date
        if ($null -eq $sa.Args) {
            $r = [pscustomobject]@{ Id = $step.Id; Name = $step.Name; Class = 'NOT_TESTED'; ExitCode = ''; RunDir = ''; Seconds = 0; Meaning = $sa.NotTested; DeclaredDemo = [bool]$sa.DeclaredDemo; Extra = $null }
            Write-Host ("[{0}] {1}: NOT_TESTED - {2}" -f $step.Id, $step.Name, $sa.NotTested) -ForegroundColor Yellow
            Write-SiewProgress $runDir ("step {0}/{1} {2} | NOT_TESTED | {3}" -f $step.Id, $nSteps, $step.Name, $(if ($sa.DeclaredDemo) { 'demo: no study reference' } elseif ($sa.NotTested -match '^(input absent \([a-z_]+\))') { $matches[1] } else { 'dependency did not run or pass' })) 'Yellow'
        } else {
            Write-SiewProgress $runDir ("step {0}/{1} {2} | START" -f $step.Id, $nSteps, $step.Name)
            Write-Host ("[{0}] {1}: {2} ..." -f $step.Id, $step.Name, $(if ($Demo -and $step.Name -eq 'ANOM_SAMPLE_SD_DIAGNOSTIC') { 'synthetic test of the declared diagnostic' } else { $step.What }))
            $log = Join-Path $stepDir 'step_console.log'
            if ($sa.R) { $rc = Invoke-SiewChild -Exe $script:RscriptExe -Arguments $sa.Args -LogPath $log -WorkDir $P.repo } else { $rc = Invoke-SiewWrapper -RepoDir $P.repo -Script $step.Script -Arguments $sa.Args -LogPath $log }
            $cl = Resolve-SiewStepClass -Step $step -ExitCode $rc -Demo:$Demo
            $rd = @(Get-ChildItem -LiteralPath $runDir -Directory | Where-Object { $_.Name -like ($step.Prefix + '*') -and ($before -notcontains $_.Name) } | Sort-Object LastWriteTime)
            $rdir = $(if ($rd.Count) { $rd[-1].FullName } elseif ($resumeDir -ne '') { $resumeDir } else { '' })
            $extra = $null
            if ($Demo -and $step.Name -eq 'ANOM_SAMPLE_SD_DIAGNOSTIC') { $extra = @(Get-Content -LiteralPath $log | Where-Object { $_ -match '^(SYNTHETIC TESTS:|validation/.*SHA256=)' } | ForEach-Object { if ($_ -match '^(.*?)(=|: )(.*)$') { [pscustomobject]@{ key = ($matches[1] -replace '[^A-Za-z0-9_]', '_').ToUpper(); value = $matches[3] } } }) }
            $r = [pscustomobject]@{ Id = $step.Id; Name = $step.Name; Class = $cl.Class; ExitCode = $rc; RunDir = $rdir; Seconds = [math]::Round(((Get-Date) - $t0).TotalSeconds, 1); Meaning = $cl.Meaning; DeclaredDemo = $false; Extra = $extra }
            $colour = switch ($r.Class) { 'PASS' { 'Green' } 'REVIEW' { 'Yellow' } 'DIAGNOSTIC' { 'Cyan' } 'NOT_TESTED' { 'Yellow' } default { 'Red' } }
            Write-Host ("[{0}] {1}: {2} ({3})" -f $step.Id, $step.Name, $r.Class, $r.Meaning) -ForegroundColor $colour
            Write-SiewProgress $runDir ("step {0}/{1} {2} | END | {3} (exit {4}) | {5} s" -f $step.Id, $nSteps, $step.Name, $r.Class, $r.ExitCode, $r.Seconds) $colour
        }
        if ($P.input_status -eq 'NOT_THE_ACCEPTED_INPUT' -and $r.RunDir -ne '' -and (Test-Path -LiteralPath $r.RunDir -PathType Container)) { Write-SiewAtomic (Join-Path $r.RunDir 'NOT_THE_ACCEPTED_INPUT.txt') @('NOT_THE_ACCEPTED_INPUT', 'This folder was produced from a data file that is not the accepted canonical CSV. Exploratory only; see the run summary.') }
        $relDir = $(if ($r.RunDir -ne '') { $r.RunDir.Substring($runDir.Length + 1) } else { '' })
        Write-SiewAtomic $resFile @(('STEP_ID=' + $r.Id), ('STEP_NAME=' + $r.Name), ('CLASS=' + $r.Class), ('EXIT_CODE=' + $r.ExitCode), ('MEANING=' + $r.Meaning), ('RUN_FOLDER=' + $relDir),
            ('RUN_MANIFEST_SHA256=' + (Get-SiewManifestSha $r.RunDir)), ('SECONDS=' + $r.Seconds), ('DECLARED_DEMO=' + $r.DeclaredDemo), ('FINISHED=' + (Get-Date -Format 'yyyy-MM-dd HH:mm:ss')))
        [void]$results.Add($r); $done[$step.Name] = $r
    }
    $overall = Resolve-SiewOverall -Results $results.ToArray() -Demo:$Demo -Stopped:$stopped
    $cnt = $overall.Counts
    Write-SiewProgress $runDir ("RUN END | {0} (exit {1}) | PASS {2}, REVIEW {3}, FAIL {4}, NOT_TESTED {5}, DIAGNOSTIC {6}, EXECUTION_FAILURE {7}, ARGUMENT_ERROR {8} | {9}" -f $overall.Kind, $overall.Code, $cnt['PASS'], $cnt['REVIEW'], $cnt['FAIL'], $cnt['NOT_TESTED'], $cnt['DIAGNOSTIC'], $cnt['EXECUTION_FAILURE'], $cnt['ARGUMENT_ERROR'], $(if (-not $stopped -and $overall.Code -ne 1) { 'summary and manifest follow' } else { 'incomplete: no manifest; resume possible' }))
    Write-SiewSummary -RunDir $runDir -P $P -Results $results.ToArray() -Overall $overall -Pre $Pre -Started $started
    if (-not $stopped -and $overall.Code -ne 1) { Write-SiewFullManifest -RunDir $runDir }
    Write-Host ''
    Write-Host ('{0,-4} {1,-27} {2}' -f 'ID', 'STEP', 'CLASS')
    foreach ($r in $results) { $colour = switch ($r.Class) { 'PASS' { 'Green' } 'REVIEW' { 'Yellow' } 'DIAGNOSTIC' { 'Cyan' } 'NOT_TESTED' { 'Yellow' } default { 'Red' } }; Write-Host ('{0,-4} {1,-27} {2}' -f $r.Id, $r.Name, $r.Class) -ForegroundColor $colour }
    $oc = switch ($overall.Code) { 0 { 'Green' } 3 { 'Yellow' } default { 'Red' } }
    Write-Host ('OUTCOME={0} (exit code {1})' -f $overall.Kind, $overall.Code) -ForegroundColor $oc
    if ($overall.Code -eq 1) { Write-Host 'The run is incomplete. FULL_RUN_MANIFEST.csv is not written; fix the cause and resume with -ResumeRunDir (completed steps are reused, failed executions are retried).' -ForegroundColor Red }
    Write-Host ('Summary: {0}' -f (Join-Path $runDir 'FULL_RUN_SUMMARY.md'))
    Write-Host ('Run folder: {0}' -f $runDir)
    return $overall.Code
}
