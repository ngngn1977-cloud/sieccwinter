# Shared module of the no-code interface (dot-sourced by siecc_launcher.ps1 and declare_project_inputs.ps1). Windows PowerShell 5.1.
# Inputs are identified by CONTENT: SHA-256 against values declared in the protocols or in config/project_inputs_declaration_v1.json. Byte size is used only to skip
# files before hashing; a file name alone never identifies anything. Only names, sizes and hashes are printed or recorded. The analysis and validation logic is not
# reimplemented: the run itself is run_all.ps1 (Invoke-SiewFullRun) in project mode.
. (Join-Path $PSScriptRoot '_run_full.ps1')
$script:DeclarationFileName = 'project_inputs_declaration_v1.json'
$script:DeclarationMarking = 'post hoc input-identification record, 7 Oct 2026; does not alter any protocol, plan, tolerance or accepted result'
$script:ProjectPathBudget = 110        # characters: + "\outputs\run_<stamp>_<id>\" (37) + the longest nested output path (about 110) stays below Windows' 260 (long paths are not enabled by default)
$script:DataHeaderColumns =@('participant_code', 'UCLA_Lonliness_SCORE', 'Isolation_cont', 'int_dummy', 'assessment_day', 'UTCI_L01')
$script:StepMinutes = [ordered]@{ PAPER = 5; CORE_VALIDATION = 5; BATCH_A = 1; BATCH_B = 2; ORDINAL_REFINED = 1; BATCH_C = 4; DLM_V2 = 5; DLM_V21 = 1; ARCHIVED_DLM = 4; COLD_BURDEN = 4; DAILY_LAYER = 3; ANOM_ALT = 5; ANOM_SAMPLE_SD_DIAGNOSTIC = 2; CODED_MODEL_RECORD = 1
    EXPOSURE_EXPORT = 2; PH_DELETION = 3; PH_L59_BOOTSTRAP = 6; PH_NO_APRIL = 2; PH_SOURCE_AGREEMENT = 2; PH_EXPOSURE_RESIDUALS = 2; PH_PROFILE_LIKELIHOOD = 6; PH_EARLY_LATE = 3; PH_H3_NESTED = 2; PH_NEAREST_STATION = 2; PH_EXPOSURE_QA = 2; PH_VIF_SUMMARY = 1; PH_CALENDAR_SPLINE = 2; PH_AGE65 = 2 }
$script:BundleRoles = @('core_reference', 'batch_a_reference', 'batch_b_reference', 'batch_c_reference', 'dlm_reference', 'archived_dlm_reference', 'cold_burden_reference', 'daily_layer_reference', 'anomhist_reference', 'posthoc_v14_reference', 'archive_engine_reference')
# participant-level nearest-station histories (post hoc step 26): one CSV identified by its header line only (never a value); local only, never staged or copied
$script:NearestHeaderColumns = @('TD_MEAN', 'RH_MEAN', 'RAIN_TOTAL', 'RAIN_DAYS1')        # plus participant_code (alias FEATURE_ID) and a local date (LOCAL_DATE or JERUSALEM_LOCAL_DATE)
# bundle/file statuses of the declaration that let a declaration-sourced bundle run; anything else (UNCONFIRMED, unknown) means NOT_TESTED
$script:AcceptedBundleStatuses = @('CONFIRMED_AT_ACCEPTANCE', 'CONFIRMED_BY_FROZEN_SOURCE', 'AUTHOR_ACCEPTED_EVIDENCE')
$script:DeclaredRuns = [ordered]@{
    anom_failed_run     = @{ Roles = @('anom_failed_run'); Required = @('anom_package_results.csv', 'anom_scale_and_inference_identities.csv', 'anomhist_metadata.json', 'source_manifest_anomhist.csv', 'ANOMHIST_STATUS.txt'); Label = 'failed ANOM run (anomhist_20261004_141846_28bd)' }
    paper_accepted_run  = @{ Roles = @('dlm_accepted_run'); Required = @('dlm_final_summary.csv', 'dlm_final_lag_curves.csv', 'dlm_final_cumulative_curves.csv'); Label = 'historical accepted paper run (paper_20260930_195007_26ca)' }
    batchb_accepted_run = @{ Roles = @('batch_b_legacy_run', 'batch_c_accepted_run'); Required = @('ordinal_integration.csv', 'cumulative_results.csv'); Label = 'historical accepted Batch B run (batchb_20261001_134405_0f79)' }
}
$script:RoleLabels = [ordered]@{
    data_csv = 'study data CSV'; core_reference = 'core reference bundle'; batch_a_reference = 'Batch A reference bundle'; batch_b_reference = 'Batch B reference bundle'; batch_c_reference = 'Batch C reference bundle'
    dlm_reference = 'DLM reference bundle'; archived_dlm_reference = 'archived DLM reference files'; cold_burden_reference = 'cold-burden reference files'; daily_layer_reference = 'daily-layer reference files'
    anomhist_reference = 'ANOM/ALT reference files'; anom_failed_run = 'failed ANOM run'; dlm_accepted_run = 'historical accepted paper run'; batch_b_legacy_run = 'historical accepted Batch B run (ordinal legacy)'; batch_c_accepted_run = 'historical accepted Batch B run (Batch C point estimates)'
    posthoc_v14_reference = 'post hoc V1.4 reference files'; archive_engine_reference = 'post hoc archive-engine reference files'; nearest_station_histories = 'nearest-station histories (participant-level)'; gis_ims_v8_1_reference = 'GIS run analysed, IMS V8.1 (participant/station-level)'
}

function Get-SiewConfigDir { param([string]$Repo) $c = Join-Path $Repo 'config'; if (Test-Path -LiteralPath (Join-Path $c 'sieccwinter_config.json')) { return $c }; return (Join-Path $Repo 'inst\extdata') }

# Expected files of every reference role: names and required groups from the existing plans; hash and size from the protocols (four roles) or the declaration (five roles).
function Get-SiewRoleSpecs {
    param([string]$Repo)
    $cfg = Get-SiewConfigDir $Repo; $out = New-Object System.Collections.ArrayList
    $plans = [ordered]@{ core_reference = 'validation_plan.json'; batch_a_reference = 'validation_plan_batchA.json'; batch_b_reference = 'validation_plan_batchB.json'; batch_c_reference = 'validation_plan_batchC.json'; dlm_reference = 'validation_plan_dlm.json' }
    foreach ($role in $plans.Keys) {
        $j = Get-Content -LiteralPath (Join-Path $cfg $plans[$role]) -Raw -Encoding UTF8 | ConvertFrom-Json; $files = @()
        foreach ($r in $j.references) {
            if ($role -eq 'core_reference' -and -not [bool]$r.required) { continue }        # the archived reference of the core plan is declared out of scope there; it belongs to the archived role
            foreach ($n in @($r.expected_records_by_file.PSObject.Properties.Name)) { $files += [pscustomobject]@{ file = $n; group = [string]$r.name; group_required = [bool]$r.required; sha256 = ''; bytes = [int64]-1 } }
        }
        [void]$out.Add([pscustomobject]@{ Role = $role; Source = 'declaration'; SourceFile = $plans[$role]; Files = $files })
    }
    $prots = [ordered]@{ archived_dlm_reference = 'archived_dlm_validation_protocol.json'; cold_burden_reference = 'cold_burden_validation_protocol.json'; daily_layer_reference = 'daily_layer_validation_protocol.json'; anomhist_reference = 'anomaly_history_validation_protocol.json' }
    foreach ($role in $prots.Keys) {
        $j = Get-Content -LiteralPath (Join-Path $cfg $prots[$role]) -Raw -Encoding UTF8 | ConvertFrom-Json
        $files = @($j.reference_files | ForEach-Object { [pscustomobject]@{ file = [string]$_.file; group = [string]$_.file; group_required = $true; sha256 = ([string]$_.sha256).ToUpper(); bytes = [int64]$_.bytes } })
        [void]$out.Add([pscustomobject]@{ Role = $role; Source = 'protocol'; SourceFile = $prots[$role]; Files = $files })
    }
    # post hoc reference roles: names, bytes and SHA-256 declared in posthoc_sensitivity_protocol_v2.json (13-14 Oct 2026); gis_ims_v8_1_reference is identified but never staged (participant-level)
    $ph = Join-Path $cfg 'posthoc_sensitivity_protocol_v2.json'
    if (Test-Path -LiteralPath $ph -PathType Leaf) {
        $j = Get-Content -LiteralPath $ph -Raw -Encoding UTF8 | ConvertFrom-Json
        foreach ($role in @('posthoc_v14_reference', 'archive_engine_reference', 'gis_ims_v8_1_reference')) {
            $files = @($j.reference_files | Where-Object { $_.role -eq $role } | ForEach-Object { [pscustomobject]@{ file = [string]$_.file; group = [string]$_.file; group_required = $true; sha256 = ([string]$_.sha256).ToUpper(); bytes = [int64]$_.bytes } })
            [void]$out.Add([pscustomobject]@{ Role = $role; Source = 'protocol'; SourceFile = 'posthoc_sensitivity_protocol_v2.json'; Files = $files })
        }
    }
    return $out.ToArray()
}

# The installed declaration (config/, else inst/extdata/), or an explicit file (synthetic tests). Returns $null when absent; throws when present but not valid.
function Read-SiewDeclaration {
    param([string]$Repo, [string]$Path = '')
    if ($Path -eq '') { $Path = Join-Path (Get-SiewConfigDir $Repo) $script:DeclarationFileName; if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { return $null } }
    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { throw ('SIW_PROJECT_DECLARATION_NOT_FOUND: ' + $Path) }
    $raw = Get-Content -LiteralPath $Path -Raw -Encoding UTF8
    if ($raw -match '<[^>]*>') { throw 'SIW_PROJECT_DECLARATION_HAS_PLACEHOLDERS' }
    $d = $raw | ConvertFrom-Json
    if ([string]$d.declaration_version -ne '1' -or [string]$d.status_of_this_document -ne $script:DeclarationMarking) { throw 'SIW_PROJECT_DECLARATION_NOT_VERSION_1 (wrong version or marking)' }
    # the two evidence-based statuses carry their own evidence; a declaration without it is not usable
    foreach ($b in @($d.bundles)) {
        foreach ($f in @($b.files)) {
            $fs = [string]$f.status
            if ($fs -eq 'AUTHOR_ACCEPTED_EVIDENCE') {
                foreach ($k in @('evidence', 'author', 'author_date')) { if ([string]$f.$k -eq '' -or ([string]$f.$k).Trim() -eq '') { throw ('SIW_PROJECT_DECLARATION_AUTHOR_EVIDENCE_INCOMPLETE (' + $b.role + ' / ' + $f.file + ': ' + $k + ' missing)') } }
            }
            elseif ($fs -eq 'CONFIRMED_BY_FROZEN_SOURCE') {
                if ([string]$f.source_path -eq '' -or ([string]$f.source_sha256).ToUpper() -ne ([string]$f.sha256).ToUpper()) { throw ('SIW_PROJECT_DECLARATION_FROZEN_SOURCE_INCOMPLETE (' + $b.role + ' / ' + $f.file + ': source path missing or source SHA-256 differs from the file SHA-256)') }
            }
        }
    }
    $d | Add-Member -NotePropertyName _path -NotePropertyValue $Path -Force
    return $d
}

function Get-SiewFileHashBy {
    param([string]$Path, [string]$Algorithm)
    if ($Algorithm -match '^(?i)md5$') { $p = $Path; if ($p.Length -ge 240 -and -not $p.StartsWith('\\?\')) { $p = '\\?\' + $p }; $m = [System.Security.Cryptography.MD5]::Create(); $fs = [System.IO.File]::OpenRead($p); try { return ([System.BitConverter]::ToString($m.ComputeHash($fs)) -replace '-', '') } finally { $fs.Dispose(); $m.Dispose() } }
    return (Get-SiewSha256 $Path)
}

# Verify every file listed in a RUN_MANIFEST.csv (columns file, bytes, algorithm, hash). Returns @{ Ok; Files; Problems }.
function Test-SiewRunManifest {
    param([string]$Folder)
    $mf = Join-Path $Folder 'RUN_MANIFEST.csv'; $probs = New-Object System.Collections.ArrayList; $n = 0
    if (-not (Test-Path -LiteralPath $mf -PathType Leaf)) { return [pscustomobject]@{ Ok = $false; Files = 0; Problems = @('RUN_MANIFEST.csv missing') } }
    foreach ($r in (Import-Csv -LiteralPath $mf)) {
        $n++; $f = Join-Path $Folder ([string]$r.file)
        if (-not (Test-Path -LiteralPath $f -PathType Leaf)) { [void]$probs.Add(('missing: ' + $r.file)); continue }
        if ($r.PSObject.Properties['bytes'] -and [string]$r.bytes -ne '' -and (Get-Item -LiteralPath $f).Length -ne [int64]$r.bytes) { [void]$probs.Add(('size differs: ' + $r.file)); continue }
        $alg = $(if ($r.PSObject.Properties['algorithm']) { [string]$r.algorithm } else { 'sha256' })
        if ((Get-SiewFileHashBy $f $alg) -ne ([string]$r.hash).ToUpper()) { [void]$probs.Add(('hash differs: ' + $r.file)) }
    }
    return [pscustomobject]@{ Ok = ($probs.Count -eq 0 -and $n -gt 0); Files = $n; Problems = $probs.ToArray() }
}

# Header-only check (the first line; never a value) that a CSV has the canonical study columns.
function Test-SiewDataHeader {
    param([string]$Path)
    try { $sr = New-Object System.IO.StreamReader($Path, [System.Text.Encoding]::UTF8, $true); try { $h = $sr.ReadLine() } finally { $sr.Dispose() } } catch { return $false }
    if ($null -eq $h) { return $false }
    $cols = @($h -split ',' | ForEach-Object { $_.Trim().Trim('"') })
    foreach ($c in $script:DataHeaderColumns) { if ($cols -notcontains $c) { return $false } }
    return $true
}

# Header-only check (the first line; never a value) for the declared nearest-station history layout.
function Test-SiewNearestHeader {
    param([string]$Path)
    try { $sr = New-Object System.IO.StreamReader($Path, [System.Text.Encoding]::UTF8, $true); try { $h = $sr.ReadLine() } finally { $sr.Dispose() } } catch { return $false }
    if ($null -eq $h) { return $false }
    $cols = @($h -split ',' | ForEach-Object { $_.Trim().Trim('"') })
    foreach ($c in $script:NearestHeaderColumns) { if ($cols -notcontains $c) { return $false } }
    return ((($cols -contains 'participant_code') -or ($cols -contains 'FEATURE_ID')) -and (($cols -contains 'LOCAL_DATE') -or ($cols -contains 'JERUSALEM_LOCAL_DATE')))
}

# Export variants of the nearest-station histories declared in the post hoc protocol (item nearest_station): id, bytes, SHA-256. Names, sizes and hashes only.
function Get-SiewNearestVariants {
    param([string]$Repo)
    if ($null -ne $script:NearestVariantsOverride) { return @($script:NearestVariantsOverride) }        # synthetic launcher tests only
    $ph = Join-Path (Get-SiewConfigDir $Repo) 'posthoc_sensitivity_protocol_v2.json'; if (-not (Test-Path -LiteralPath $ph -PathType Leaf)) { return @() }
    $it = @((Get-Content -LiteralPath $ph -Raw -Encoding UTF8 | ConvertFrom-Json).items | Where-Object { $_.id -eq 'nearest_station' }) | Select-Object -First 1
    if ($null -eq $it -or $null -eq $it.PSObject.Properties['variants']) { return @() }
    return @($it.variants | Where-Object { ([string]$_.sha256) -match '^[0-9A-Fa-f]{64}$' } | ForEach-Object { [pscustomobject]@{ id = [string]$_.id; bytes = [int64]$_.bytes; sha256 = ([string]$_.sha256).ToUpper() } })
}

# Content-based identification of every role in a project folder (outputs\ excluded). Returns roles, data, ambiguities and unknown files.
function Find-SiewProjectInputs {
    param([Parameter(Mandatory = $true)][string]$ProjectDir, [Parameter(Mandatory = $true)][string]$Repo, $Declaration, [switch]$Demo)
    $proj = ConvertTo-SiewFullPath $ProjectDir; $outDir = Join-Path $proj 'outputs'
    $all = @(Get-ChildItem -LiteralPath $proj -Recurse -File -Force -ErrorAction SilentlyContinue | Where-Object { -not (Test-SiewInside $_.FullName $outDir) })
    $hash = @{}; $H = { param($f) if (-not $hash.ContainsKey($f.FullName)) { $hash[$f.FullName] = Get-SiewSha256 $f.FullName }; $hash[$f.FullName] }
    $bySize = @{}; foreach ($f in $all) { $k = [string]$f.Length; if (-not $bySize.ContainsKey($k)) { $bySize[$k] = New-Object System.Collections.ArrayList }; [void]$bySize[$k].Add($f) }
    $roles = [ordered]@{}; $amb = New-Object System.Collections.ArrayList; $used = @{}; $runFolders = @()
    $rel = { param($p) $p.Substring([Math]::Min($proj.Length + 1, $p.Length)) }
    # 1. declared runs: RUN_MANIFEST.csv by declared size and SHA-256, then every manifest-listed file verified in place
    foreach ($dr in $script:DeclaredRuns.Keys) {
        $spec = $script:DeclaredRuns[$dr]; $entry = $null
        if ($null -ne $Declaration) { $entry = @($Declaration.runs | Where-Object { $_.declared_run -eq $dr }) | Select-Object -First 1 }
        $st = $null
        if ($null -eq $Declaration) { $st = [pscustomobject]@{ Status = 'NOT_DECLARED'; Reason = 'no input-identification declaration is installed (config/project_inputs_declaration_v1.json)'; Folder = ''; Files = @() } }
        elseif ($null -eq $entry) { $st = [pscustomobject]@{ Status = 'NOT_DECLARED'; Reason = ('the declaration has no entry for the ' + $spec.Label); Folder = ''; Files = @() } }
        elseif ([string]$entry.status -ne 'MANIFEST_VERIFIED') { $st = [pscustomobject]@{ Status = 'UNCONFIRMED'; Reason = ('declaration status ' + $entry.status + ': ' + [string]$entry.reason); Folder = ''; Files = @() } }
        else {
            $want = ([string]$entry.run_manifest.sha256).ToUpper(); $sz = [string]$entry.run_manifest.bytes
            $cands = @(); if ($bySize.ContainsKey($sz)) { $cands = @($bySize[$sz] | Where-Object { $_.Name -eq 'RUN_MANIFEST.csv' -and (& $H $_) -eq $want }) }
            if ($cands.Count -eq 0) { $st = [pscustomobject]@{ Status = 'ABSENT'; Reason = ('the ' + $spec.Label + ' was not found in the project folder (its RUN_MANIFEST.csv hash did not match any file)'); Folder = ''; Files = @() } }
            elseif ($cands.Count -gt 1) { [void]$amb.Add(('{0}: {1} copies found ({2})' -f $spec.Label, $cands.Count, (($cands | ForEach-Object { & $rel $_.DirectoryName }) -join '; '))); $st = [pscustomobject]@{ Status = 'AMBIGUOUS'; Reason = 'more than one copy'; Folder = ''; Files = @() } }
            else {
                $folder = $cands[0].DirectoryName; $v = Test-SiewRunManifest $folder; $miss = @($spec.Required | Where-Object { -not (Test-Path -LiteralPath (Join-Path $folder $_) -PathType Leaf) })
                if (-not $v.Ok) { $st = [pscustomobject]@{ Status = 'NOT_VERIFIED'; Reason = ('manifest-listed files changed: ' + (($v.Problems | Select-Object -First 5) -join '; ')); Folder = $folder; Files = @() } }
                elseif ($miss.Count) { $st = [pscustomobject]@{ Status = 'INCOMPLETE'; Reason = ('required files missing: ' + ($miss -join ', ')); Folder = $folder; Files = @() } }
                else { $st = [pscustomobject]@{ Status = 'FOUND'; Reason = ('RUN_MANIFEST.csv SHA-256 matches the declaration; ' + $v.Files + ' listed files verified'); Folder = $folder; Files = @([pscustomobject]@{ file = 'RUN_MANIFEST.csv'; path = $cands[0].FullName; bytes = $cands[0].Length; sha256 = $want }) }; $runFolders += $folder }
            }
        }
        foreach ($role in $spec.Roles) { $roles[$role] = $st }
    }
    $inRun = { param($f) foreach ($rf in $runFolders) { if (Test-SiewInside $f.FullName $rf) { return $true } }; return $false }
    $pool = @($all | Where-Object { -not (& $inRun $_) })
    $poolBySize = @{}; foreach ($f in $pool) { $k = [string]$f.Length; if (-not $poolBySize.ContainsKey($k)) { $poolBySize[$k] = New-Object System.Collections.ArrayList }; [void]$poolBySize[$k].Add($f) }
    # 2. reference roles: every declared file by declared size, then SHA-256
    foreach ($spec in (Get-SiewRoleSpecs $Repo)) {
        $role = $spec.Role; $files = $spec.Files; $why = ''
        if ($spec.Source -eq 'declaration') {
            $de = $null; if ($null -ne $Declaration) { $de = @($Declaration.bundles | Where-Object { $_.role -eq $role }) | Select-Object -First 1 }
            if ($null -eq $Declaration) { $roles[$role] = [pscustomobject]@{ Status = 'NOT_DECLARED'; Reason = 'no input-identification declaration is installed (config/project_inputs_declaration_v1.json)'; Folder = ''; Files = @() }; continue }
            if ($null -eq $de) { $roles[$role] = [pscustomobject]@{ Status = 'NOT_DECLARED'; Reason = 'the declaration has no entry for this bundle'; Folder = ''; Files = @() }; continue }
            $badFiles = @(@($de.files) | Where-Object { $_.PSObject.Properties['status'] -and $script:AcceptedBundleStatuses -notcontains [string]$_.status })
            if ($script:AcceptedBundleStatuses -notcontains [string]$de.status -or $badFiles.Count) { $roles[$role] =[pscustomobject]@{ Status = 'UNCONFIRMED'; Reason = ('declaration status ' + $de.status + ': ' + [string]$de.reason); Folder = ''; Files = @() }; continue }
            $files = @(foreach ($sf in $spec.Files) { $x = @($de.files | Where-Object { $_.file -eq $sf.file }) | Select-Object -First 1; if ($null -ne $x) { [pscustomobject]@{ file = $sf.file; group = $sf.group; group_required = $sf.group_required; sha256 = ([string]$x.sha256).ToUpper(); bytes = [int64]$x.bytes } } })
        }
        $found = New-Object System.Collections.ArrayList; $isAmb = $false
        foreach ($sf in $files) {
            $k = [string]$sf.bytes; $c = @(); if ($poolBySize.ContainsKey($k)) { $c = @($poolBySize[$k] | Where-Object { (& $H $_) -eq $sf.sha256 }) }
            if ($c.Count -gt 1) { $isAmb = $true; [void]$amb.Add(('{0} / {1}: {2} identical candidates ({3})' -f $role, $sf.file, $c.Count, (($c | ForEach-Object { & $rel $_.FullName }) -join '; '))) }
            elseif ($c.Count -eq 1) { [void]$found.Add([pscustomobject]@{ file = $sf.file; group = $sf.group; path = $c[0].FullName; bytes = $c[0].Length; sha256 = $sf.sha256 }); $used[$c[0].FullName] = $role }
        }
        $groups = @($files | Where-Object { $_.group_required } | ForEach-Object { $_.group } | Select-Object -Unique)
        $missing = @($groups | Where-Object { $g = $_; -not @($found | Where-Object { $_.group -eq $g }).Count })
        if ($isAmb) { $roles[$role] = [pscustomobject]@{ Status = 'AMBIGUOUS'; Reason = 'more than one candidate for a declared file'; Folder = ''; Files = $found.ToArray() } }
        elseif ($found.Count -eq 0) { $roles[$role] = [pscustomobject]@{ Status = 'ABSENT'; Reason = ('none of the declared files was found (' + $(if ($spec.Source -eq 'protocol') { 'hashes declared in ' + $spec.SourceFile } else { 'hashes from the input-identification declaration' }) + ')'); Folder = ''; Files = @() } }
        elseif ($missing.Count) { $roles[$role] = [pscustomobject]@{ Status = 'INCOMPLETE'; Reason = ('required file(s) not found: ' + (($missing | ForEach-Object { $g = $_; (@($files | Where-Object { $_.group -eq $g }) | ForEach-Object { $_.file }) -join ' or ' }) -join '; ')); Folder = ''; Files = $found.ToArray() } }
        else {
            $idStatus = 'CONFIRMED_BY_PROTOCOL'; $idEv = @()
            if ($spec.Source -ne 'protocol') {
                $idStatus = [string]$de.status
                foreach ($x in @($de.files | Where-Object { @('CONFIRMED_BY_FROZEN_SOURCE', 'AUTHOR_ACCEPTED_EVIDENCE') -contains [string]$_.status })) {
                    $idEv += ('{0} [{1}]: {2}{3}' -f $x.file, $x.status, $x.evidence, $(if ([string]$x.status -eq 'AUTHOR_ACCEPTED_EVIDENCE') { ' (author: ' + $x.author + ', ' + $x.author_date + ')' } else { '' }))
                }
            }
            $roles[$role] = [pscustomobject]@{ Status = 'FOUND'; IdStatus = $idStatus; Evidence = ($idEv -join ' | '); Reason = ('all required files identified by SHA-256 (' + $(if ($spec.Source -eq 'protocol') { 'declared in ' + $spec.SourceFile } else { 'input-identification declaration, ' + $idStatus }) + ')'); Folder = ''; Files = $found.ToArray() }
        }
    }
    # 3. the data CSV: accepted SHA-256 (synthetic SHA-256 in demo); otherwise a header-only check for the canonical columns
    $acc = $(if ($Demo) { $script:SyntheticInputSha } else { $script:AcceptedInputSha }); $dbytes = ''
    if (-not $Demo -and $null -ne $Declaration -and $null -ne $Declaration.data_csv -and ([string]$Declaration.data_csv.sha256).ToUpper() -eq $acc) { $dbytes = [string]$Declaration.data_csv.bytes }
    $csvs = @($pool | Where-Object { $_.Extension -ieq '.csv' -and -not $used.ContainsKey($_.FullName) })
    $dc = @(); if ($dbytes -ne '') { $dc = @($csvs | Where-Object { [string]$_.Length -eq $dbytes -and (& $H $_) -eq $acc }) } else { $dc = @($csvs | Where-Object { (& $H $_) -eq $acc }) }
    $data = $null
    if ($dc.Count -gt 1) { [void]$amb.Add(('data CSV: {0} identical copies of the expected input ({1})' -f $dc.Count, (($dc | ForEach-Object { & $rel $_.FullName }) -join '; '))); $data = [pscustomobject]@{ Status = 'AMBIGUOUS'; Path = ''; Sha = $acc; Bytes = 0; Reason = 'more than one copy' } }
    elseif ($dc.Count -eq 1) { $data = [pscustomobject]@{ Status = $(if ($Demo) { 'DEMO_SYNTHETIC_INPUT' } else { 'ACCEPTED' }); Path = $dc[0].FullName; Sha = $acc; Bytes = $dc[0].Length; Reason = $(if ($Demo) { 'SHA-256 equals the shipped synthetic CSV' } else { 'SHA-256 equals the accepted canonical CSV' }) }; $used[$dc[0].FullName] = 'data_csv' }
    else {
        $hc = @($csvs | Where-Object { Test-SiewDataHeader $_.FullName })
        if ($hc.Count -gt 1) { [void]$amb.Add(('data CSV: no file has the expected SHA-256 and {0} files have the canonical column header ({1})' -f $hc.Count, (($hc | ForEach-Object { & $rel $_.FullName }) -join '; '))); $data = [pscustomobject]@{ Status = 'AMBIGUOUS'; Path = ''; Sha = ''; Bytes = 0; Reason = 'more than one candidate data file' } }
        elseif ($hc.Count -eq 1) { $data = [pscustomobject]@{ Status = 'NOT_THE_ACCEPTED_INPUT'; Path = $hc[0].FullName; Sha = (& $H $hc[0]); Bytes = $hc[0].Length; Reason = 'the canonical columns are present but the SHA-256 is not the expected input' }; $used[$hc[0].FullName] = 'data_csv' }
        else { $data = [pscustomobject]@{ Status = 'ABSENT'; Path = ''; Sha = ''; Bytes = 0; Reason = 'no CSV with the expected SHA-256 or the canonical column header was found' } }
    }
    # 4. nearest-station histories (participant-level, post hoc step 26), never staged or copied: the export variants declared in the post hoc protocol by size and SHA-256
    #    (each found variant is run separately); when no declared variant is present, one CSV identified by its header line only (provenance REVIEW in the step)
    $vf = New-Object System.Collections.ArrayList
    foreach ($v in (Get-SiewNearestVariants $Repo)) {
        $k = [string]$v.bytes; $c = @(); if ($poolBySize.ContainsKey($k)) { $c = @($poolBySize[$k] | Where-Object { -not $used.ContainsKey($_.FullName) -and (& $H $_) -eq $v.sha256 }) }
        if ($c.Count -gt 1) { [void]$amb.Add(('nearest-station export {0}: {1} identical copies ({2})' -f $v.id, $c.Count, (($c | ForEach-Object { & $rel $_.FullName }) -join '; '))) }
        elseif ($c.Count -eq 1) { [void]$vf.Add([pscustomobject]@{ file = $c[0].Name; group = ('nearest variant ' + $v.id); variant = $v.id; path = $c[0].FullName; bytes = $c[0].Length; sha256 = $v.sha256 }); $used[$c[0].FullName] = 'nearest_station_histories' }
    }
    $nc = @($pool | Where-Object { $_.Extension -ieq '.csv' -and -not $used.ContainsKey($_.FullName) -and (Test-SiewNearestHeader $_.FullName) })
    if (@($amb | Where-Object { $_ -like 'nearest-station export *' }).Count) { $roles['nearest_station_histories'] = [pscustomobject]@{ Status = 'AMBIGUOUS'; Reason = 'more than one copy of a declared export variant'; Folder = ''; Files = @() } }
    elseif ($vf.Count) { $roles['nearest_station_histories'] = [pscustomobject]@{ Status = 'FOUND'; IdStatus = 'CONFIRMED_BY_PROTOCOL'; Evidence = ''; Reason = ('{0} declared export variant(s) identified by SHA-256 ({1}); each is run separately; participant-level, local only' -f $vf.Count, ((@($vf) | ForEach-Object { $_.variant }) -join ', ')); Folder = ''; Files = $vf.ToArray() } }
    elseif ($nc.Count -gt 1) { [void]$amb.Add(('nearest-station histories: {0} files have the declared header ({1})' -f $nc.Count, (($nc | ForEach-Object { & $rel $_.FullName }) -join '; '))); $roles['nearest_station_histories'] = [pscustomobject]@{ Status = 'AMBIGUOUS'; Reason = 'more than one candidate'; Folder = ''; Files = @() } }
    elseif ($nc.Count -eq 1) { $roles['nearest_station_histories'] = [pscustomobject]@{ Status = 'FOUND'; IdStatus = 'IDENTIFIED_BY_HEADER'; Evidence = 'header line only (participant-level file; local only; no declared hash)'; Reason = ('header columns participant_code (or FEATURE_ID), ' + ($script:NearestHeaderColumns -join ', ') + ' and a local date column; participant-level, local only; not a declared export variant (provenance REVIEW)'); Folder = ''; Files = @([pscustomobject]@{ file = $nc[0].Name; group = 'nearest'; path = $nc[0].FullName; bytes = $nc[0].Length; sha256 = '' }) }; $used[$nc[0].FullName] = 'nearest_station_histories' }
    else { $roles['nearest_station_histories'] = [pscustomobject]@{ Status = 'ABSENT'; Reason = 'no declared nearest-station export (SHA-256) and no CSV with the nearest-station header (participant_code or FEATURE_ID, a local date, TD_MEAN, RH_MEAN, RAIN_TOTAL, RAIN_DAYS1) was found'; Folder = ''; Files = @() } }
    $readme = Join-Path $proj 'README_FIRST.txt'
    $unknown = @($pool | Where-Object { -not $used.ContainsKey($_.FullName) -and $_.FullName -ne $readme } | ForEach-Object { [pscustomobject]@{ path = (& $rel $_.FullName); bytes = $_.Length } })
    return [pscustomobject]@{ Project = $proj; Repo = (ConvertTo-SiewFullPath $Repo); Roles = $roles; Data = $data; Ambiguities = $amb.ToArray(); Unknown = $unknown; FilesScanned = $all.Count; FilesHashed = $hash.Count; Demo = [bool]$Demo; Declaration = $(if ($null -ne $Declaration) { $Declaration._path } else { '' }) }
}

# Plan: mode, keys for run_all, NOT_TESTED steps with reasons, blocking problems, estimate. Pure apart from the free-space probe.
function New-SiewProjectPlan {
    param([Parameter(Mandatory = $true)]$Find, [double]$MinFreeGB = 2)
    $stops = New-Object System.Collections.ArrayList
    foreach ($a in $Find.Ambiguities) { [void]$stops.Add(('ambiguous: ' + $a + '. Keep one copy and start again.')) }
    $d = $Find.Data
    if ($d.Status -eq 'ABSENT') { [void]$stops.Add('no data CSV found: copy the data CSV into the project folder (for example into data\).') }
    elseif ($d.Path -ne '' -and (Split-Path -Parent $d.Path).TrimEnd('\').ToLowerInvariant() -eq $Find.Project.TrimEnd('\').ToLowerInvariant()) { [void]$stops.Add('the data CSV is directly in the project folder: move it into a subfolder such as data\ (outputs are written to the project''s outputs\ folder and must not sit inside the data folder).') }
    if ($Find.Project.Length -gt $script:ProjectPathBudget) { [void]$stops.Add(('the project folder path is {0} characters long. Windows limits a full file path to 260 characters and the run''s own nested files need about 150 of them, so the project folder path must be at most {1} characters. Move or rename the project folder (for example C:\SIECC_project) and start again.' -f $Find.Project.Length, $script:ProjectPathBudget)) }
    if (Test-SiewInside $Find.Project $Find.Repo) { [void]$stops.Add('the project folder is inside the package folder: choose or create a project folder elsewhere.') }
    $free = Get-SiewFreeGB $Find.Project
    if ($free -lt $MinFreeGB) { [void]$stops.Add(('only {0} GB free on the project drive; at least {1} GB is required.' -f $free, $MinFreeGB)) }
    $notAccepted = ($d.Status -eq 'NOT_THE_ACCEPTED_INPUT')
    $keys = [ordered]@{}; $reasons = [ordered]@{}
    foreach ($role in @($script:BundleRoles) + @('anom_failed_run', 'dlm_accepted_run', 'batch_b_legacy_run', 'batch_c_accepted_run', 'nearest_station_histories', 'gis_ims_v8_1_reference')) {
        $r = $Find.Roles[$role]
        if ($null -eq $r) { $keys[$role] = ''; $reasons[$role] = 'not identified'; continue }
        if ($notAccepted) { $keys[$role] = ''; $reasons[$role] = 'the data file is NOT_THE_ACCEPTED_INPUT: validation against the frozen references is not meaningful'; continue }
        if ($r.Status -eq 'FOUND') { $keys[$role] = $(if ($script:BundleRoles -contains $role) { '<staged>' } elseif ($role -in @('nearest_station_histories', 'gis_ims_v8_1_reference')) { (@($r.Files) | ForEach-Object { $_.path }) -join '|' } else { $r.Folder }) } else { $keys[$role] = ''; $reasons[$role] = ($r.Status + ': ' + $r.Reason) }
    }
    $anyRef = @($keys.Keys | Where-Object { $keys[$_] -ne '' }).Count -gt 0
    $mode = $(if ($anyRef -and -not $notAccepted) { 'FULL_VALIDATION' } else { 'ANALYSIS' })
    $steps = New-Object System.Collections.ArrayList; $mins = 1
    foreach ($s in $script:StepTable) {
        $why = ''
        if ($script:StepInputs.ContainsKey($s.Name) -and -not ($Find.Demo -and $script:PosthocStepRoles.ContainsKey($s.Name))) { foreach ($k in $script:StepInputs[$s.Name]) { if ($keys[$k] -eq '' -and $why -eq '') { $why = ('input absent (' + $k + '): ' + $reasons[$k]) } } }
        if ($s.Name -eq 'DLM_V21' -and @($steps | Where-Object { $_.Name -eq 'DLM_V2' -and $_.Runs }).Count -eq 0) { $why = 'dependency: DLM_V2 does not run' }
        $runs = ($why -eq ''); if ($runs) { $mins += $script:StepMinutes[$s.Name] }
        # identification statuses beyond acceptance/protocol are shown next to every step that uses them
        $idn = @(); if ($runs -and $script:StepInputs.ContainsKey($s.Name)) { foreach ($k in $script:StepInputs[$s.Name]) { $rr = $Find.Roles[$k]; if ($null -ne $rr -and @('CONFIRMED_BY_FROZEN_SOURCE', 'AUTHOR_ACCEPTED_EVIDENCE', 'IDENTIFIED_BY_HEADER') -contains [string]$rr.IdStatus) { $idn += ('{0}: {1} - {2}' -f $k, $rr.IdStatus, $rr.Evidence) } } }
        [void]$steps.Add([pscustomobject]@{ Id = $s.Id; Name = $s.Name; Runs = $runs; Reason = $why; Identification = $idn })
    }
    $idmap = [ordered]@{}; foreach ($role in $Find.Roles.Keys) { $rr = $Find.Roles[$role]; if (@('CONFIRMED_BY_FROZEN_SOURCE', 'AUTHOR_ACCEPTED_EVIDENCE', 'IDENTIFIED_BY_HEADER') -contains [string]$rr.IdStatus) { $idmap[$role] = [ordered]@{ status = [string]$rr.IdStatus; evidence = [string]$rr.Evidence } } }
    $notes = @(); if ($keys['dlm_reference'] -ne '' -and $keys['dlm_accepted_run'] -eq '') { $notes += 'DLM V2 will compare with the paper output of this same run (self-reference; proves the wiring only), because the historical accepted paper run was not identified.' }
    return [pscustomobject]@{ Find = $Find; Mode = $mode; InputStatus = $d.Status; Keys = $keys; Reasons = $reasons; IdentificationEvidence = $idmap; Steps = $steps.ToArray(); EstimateMinutes = $mins; FreeGB = $free; Stops = $stops.ToArray(); CanRun = ($stops.Count -eq 0); Notes = $notes }
}

function Format-SiewPlan {
    param($Plan)
    $f = $Plan.Find; $L = New-Object System.Collections.ArrayList; $add = { param($x) [void]$L.Add($x) }
    & $add ('PROJECT FOLDER: ' + $f.Project)
    & $add ('Files scanned: {0} (outputs\ excluded); files hashed: {1} (others skipped by size). Declaration: {2}' -f $f.FilesScanned, $f.FilesHashed, $(if ($f.Declaration -ne '') { $f.Declaration } else { 'none installed' }))
    & $add ''; & $add 'FILES FOUND (identified by content: SHA-256, not by name)'
    $rel = { param($p) if ($p -eq '') { '' } else { $p.Substring([Math]::Min($f.Project.Length + 1, $p.Length)) } }
    & $add ('  {0,-46} {1}  {2}' -f ($script:RoleLabels['data_csv'] + ':'), $f.Data.Status, $(if ($f.Data.Path -ne '') { (& $rel $f.Data.Path) + ' (' + $f.Data.Bytes + ' bytes)' } else { $f.Data.Reason }))
    foreach ($role in $f.Roles.Keys) { $r = $f.Roles[$role]; $where = $(if (@('CONFIRMED_BY_FROZEN_SOURCE', 'AUTHOR_ACCEPTED_EVIDENCE') -contains [string]$r.IdStatus -and $r.Status -eq 'FOUND') { '[' + $r.IdStatus + '] ' } else { '' }) +$(if ($r.Folder -ne '') { & $rel $r.Folder } elseif ($r.Files.Count) { (@($r.Files | ForEach-Object { (& $rel $_.path) + $(if ($null -ne $_.PSObject.Properties['variant']) { ' [declared export ' + $_.variant + ']' } else { '' }) }) -join ', ') } else { $r.Reason }); & $add ('  {0,-46} {1}  {2}' -f ($script:RoleLabels[$role] + ':'), $r.Status, $where) }
    & $add ''; & $add ('MODE: {0}{1}' -f $Plan.Mode, $(if ($Plan.Mode -eq 'ANALYSIS') { ' (paper analysis and coded model record; validation steps NOT_TESTED)' } else { ' (paper analysis plus every validation whose inputs were identified)' }))
    & $add ('INPUT: {0}' -f $(switch ($Plan.InputStatus) { 'ACCEPTED' { 'the accepted canonical CSV (SHA-256 checked)' } 'DEMO_SYNTHETIC_INPUT' { 'the shipped SYNTHETIC CSV (demo; numbers have no scientific meaning)' } 'NOT_THE_ACCEPTED_INPUT' { 'NOT_THE_ACCEPTED_INPUT - analysis for exploration only; every output labelled; every validation NOT_TESTED' } default { $Plan.InputStatus } }))
    & $add ''; & $add 'STEPS'
    foreach ($s in $Plan.Steps) { & $add ('  {0} {1,-27} {2}' -f $s.Id, $s.Name, $(if ($s.Runs) { 'will run' } else { 'NOT_TESTED - ' + $s.Reason })); foreach ($x in @($s.Identification)) { & $add ('       input identification - ' + $x) } }
    foreach ($n in $Plan.Notes) { & $add ('  NOTE: ' + $n) }
    if ($f.Unknown.Count) { & $add ''; & $add ('UNKNOWN FILES (not identified; ignored): {0}' -f $f.Unknown.Count); foreach ($u in ($f.Unknown | Select-Object -First 10)) { & $add ('  ' + $u.path + ' (' + $u.bytes + ' bytes)') }; if ($f.Unknown.Count -gt 10) { & $add '  ... (full list in the run folder: launcher\unknown_files.csv)' } }
    & $add ''; & $add ('FREE DISK: {0} GB. ESTIMATED TIME: about {1}-{2} minutes.' -f $Plan.FreeGB, $Plan.EstimateMinutes, (2 * $Plan.EstimateMinutes))
    if (-not $Plan.CanRun) { & $add ''; & $add 'CANNOT START:'; foreach ($s in $Plan.Stops) { & $add ('  - ' + $s) } }
    return $L.ToArray()
}

function New-SiewProjectFolders {
    param([Parameter(Mandatory = $true)][string]$Path, [Parameter(Mandatory = $true)][string]$Repo)
    $p = ConvertTo-SiewFullPath $Path
    foreach ($d in @('data', 'references', 'outputs')) { New-Item -ItemType Directory -Path (Join-Path $p $d) -Force | Out-Null }
    $rf = Join-Path $p 'README_FIRST.txt'
    if (-not (Test-Path -LiteralPath $rf)) { $t = Get-Content -LiteralPath (Join-Path $Repo 'inst\extdata\project_README_FIRST.txt') -Raw -Encoding UTF8; [System.IO.File]::WriteAllText($rf, $t, (New-Object System.Text.UTF8Encoding($false))) }
    return $p
}

function ConvertTo-SiewHtml { param([string]$s) return [System.Net.WebUtility]::HtmlEncode([string]$s) }

# Self-contained results page (no script, no internet, no participant-level table): statuses verbatim, meanings, links to the main aggregate tables.
function Write-SiewResultsHtml {
    param([Parameter(Mandatory = $true)][string]$RunDir, [string]$PlanText = '')
    $E = { param($x) ConvertTo-SiewHtml $x }
    $st = @(); $sl = @(); $par = Read-SiewKeyValue (Join-Path $RunDir 'RUN_PARAMETERS.txt')
    if (Test-Path -LiteralPath (Join-Path $RunDir 'FULL_RUN_STATUS.csv')) { $st = @(Import-Csv -LiteralPath (Join-Path $RunDir 'FULL_RUN_STATUS.csv')) }
    if (Test-Path -LiteralPath (Join-Path $RunDir 'FULL_RUN_STATUS_LINES.csv')) { $sl = @(Import-Csv -LiteralPath (Join-Path $RunDir 'FULL_RUN_STATUS_LINES.csv')) }
    $envv = @{}; $pf = @(Get-ChildItem -LiteralPath (Join-Path $RunDir 'preflight') -Filter 'ENVIRONMENT_*.txt' -ErrorAction SilentlyContinue | Sort-Object Name); if ($pf.Count) { $envv = Read-SiewKeyValue $pf[-1].FullName }
    $outcome = 'UNKNOWN'; $code = ''; $sum = Join-Path $RunDir 'FULL_RUN_SUMMARY.md'
    if (Test-Path -LiteralPath $sum) { $m = [regex]::Match([System.IO.File]::ReadAllText($sum), '\*\*Outcome: ([A-Z_]+)\*\* \(exit code (\d+)\)'); if ($m.Success) { $outcome = $m.Groups[1].Value; $code = $m.Groups[2].Value } }
    $plain = switch ($outcome) {
        'ALL_PASS' { 'Every step passed.' } 'DEMO_GREEN' { 'The demonstration completed: every executed step passed; steps needing study references were not tested (by design).' }
        'COMPLETED_WITH_REVIEW_NOT_TESTED_OR_DIAGNOSTIC' { 'The run completed with no failure, but some steps were not tested, need review, or are diagnostic only. Nothing below counts as passed unless it says PASS.' }
        'COMPLETED_WITH_FAIL' { 'The run completed and at least one step FAILED. Read the FAIL rows below. (ANOM_ALT is governed by ANOM_VERIFICATION_STATUS_V2 since protocol v2; see the ANOM note.)' }
        'INCOMPLETE' { 'The run did not complete. Fix the reason shown and run the launcher again (or resume).' } default { 'The outcome could not be read.' } }
    $istat = [string]$par['project_input_status']
    $idIn = @(); $idStep = @{}
    $pj = Join-Path $RunDir 'launcher\paths.json'
    if (Test-Path -LiteralPath $pj -PathType Leaf) {
        try {
            $pjo = Get-Content -LiteralPath $pj -Raw -Encoding UTF8 | ConvertFrom-Json
            if ($null -ne $pjo.input_identification) { foreach ($q in $pjo.input_identification.PSObject.Properties) { $idIn += [pscustomobject]@{ role = $q.Name; status = [string]$q.Value.status; evidence = [string]$q.Value.evidence } } }
            if ($null -ne $pjo.step_identification) { foreach ($q in $pjo.step_identification.PSObject.Properties) { $idStep[$q.Name] = @($q.Value) } }
        } catch { }
    }
    $cls = @{ PASS = '#1b7f3b'; FAIL = '#b3261e'; REVIEW = '#a15c00'; NOT_TESTED = '#5f6368'; DIAGNOSTIC = '#1a5fb4'; EXECUTION_FAILURE = '#b3261e'; ARGUMENT_ERROR = '#b3261e' }
    $badge = { param($c) $col = $(if ($cls.ContainsKey($c)) { $cls[$c] } else { '#5f6368' }); '<span class="b" style="background:' + $col + '">' + (& $E $c) + '</span>' }
    $h = New-Object System.Collections.ArrayList; $w = { param($x) [void]$h.Add($x) }
    & $w '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>SIECC run results</title><style>'
    & $w 'body{font-family:Segoe UI,Arial,sans-serif;margin:24px;max-width:1100px;color:#1f1f1f;background:#fff}h1{font-size:22px}h2{font-size:18px;margin-top:28px}table{border-collapse:collapse;width:100%}td,th{border:1px solid #ddd;padding:6px;vertical-align:top;font-size:14px;text-align:left}.b{color:#fff;border-radius:4px;padding:2px 6px;font-size:12px;font-weight:600}.warn{background:#fff4e5;border:1px solid #a15c00;padding:10px;border-radius:6px}.note{background:#eef4ff;border:1px solid #1a5fb4;padding:10px;border-radius:6px}code,pre{font-family:Consolas,monospace;font-size:12px}pre{white-space:pre-wrap;background:#f6f8fa;padding:8px;border-radius:4px}'
    & $w '</style></head><body>'
    & $w ('<h1>SIECC winter package: run results</h1><p><b>Outcome: ' + (& $E $outcome) + '</b> (exit code ' + (& $E $code) + '). ' + (& $E $plain) + '</p>')
    & $w ('<p>Mode: <b>' + (& $E ([string]$par['project_mode_label'])) + '</b>. Input: <b>' + (& $E $istat) + '</b>.</p>')
    if ($istat -eq 'NOT_THE_ACCEPTED_INPUT') { & $w '<p class="warn"><b>NOT_THE_ACCEPTED_INPUT.</b> The data file is not the accepted canonical CSV. Every output of this run is exploratory and labelled; no validation step was run.</p>' }
    if ($istat -eq 'DEMO_SYNTHETIC_INPUT') { & $w '<p class="warn"><b>Synthetic demonstration.</b> The input is the shipped synthetic CSV. The numbers have no scientific meaning.</p>' }
    & $w '<h2>What the statuses mean</h2><table><tr><th>Status</th><th>Meaning</th></tr>'
    foreach ($row in @(@('PASS', 'The check was made and met its declared criterion. Only PASS counts as passed.'), @('FAIL', 'The check was made and did not meet its criterion, or required evidence was missing. Results are never changed to fit a reference.'), @('REVIEW', 'Required statuses passed, but a documented item needs human judgement. Not a pass.'), @('NOT_TESTED', 'Not compared (input absent, dependency not run, or out of scope). Not a pass.'), @('DIAGNOSTIC', 'Informational post hoc result; changes no status.'), @('EXECUTION_FAILURE', 'The step did not run to completion.'))) { & $w ('<tr><td>' + (& $badge $row[0]) + '</td><td>' + (& $E $row[1]) + '</td></tr>') }
    & $w '</table>'
    & $w '<h2>Steps</h2><table><tr><th>Step</th><th>Status</th><th>Meaning</th><th>Folder</th></tr>'
    foreach ($s in $st) { $lnk = $(if ([string]$s.run_folder -ne '') { '<a href="' + (& $E (([string]$s.run_folder) -replace '\\', '/')) + '/">' + (& $E $s.run_folder) + '</a>' } else { '' }); & $w ('<tr><td>' + (& $E ($s.step_id + ' ' + $s.step)) + '</td><td>' + (& $badge $s.class) + '</td><td>' + (& $E $s.meaning) + $(if ($idStep.ContainsKey([string]$s.step)) { '<br><span class="idn"><b>Input identification:</b> ' + (& $E (($idStep[[string]$s.step]) -join ' | ')) + '</span>' } else { '' }) + '</td><td>' + $lnk + '</td></tr>') }
    & $w '</table>'
    if ($idIn.Count) {
        & $w '<h2>Input identification (evidence behind the references)</h2><p>These reference inputs are identified by evidence other than an acceptance-time hash or a protocol hash. The identification status does not change any step status; only a step''s own status counts.</p><table><tr><th>Reference</th><th>Identification status</th><th>Evidence</th></tr>'
        foreach ($x in $idIn) { & $w ('<tr><td>' + (& $E $x.role) + '</td><td><b>' + (& $E $x.status) + '</b></td><td>' + (& $E $x.evidence) + '</td></tr>') }
        & $w '</table>'
    }
    & $w '<p class="note"><b>ANOM note.</b> Since the post hoc protocol v2 (author decision, 12 Oct 2026), step 14 ANOM_ALT is governed by <code>ANOM_VERIFICATION_STATUS_V2</code>: the gaussian implied SD of every row is compared with the directly measured SD of that row''s own complete-case sample (tolerance 1e-9). <code>ANOM_VERIFICATION_STATUS</code> (the v1 composition) is still shown, superseded, kept for the record. The earlier runs anomhist_20261004_141846_28bd and run_20261012_114559_4737 keep ANOM_VERIFICATION_STATUS=FAIL in their own records. Step 15 (ANOM_SAMPLE_SD_DIAGNOSTIC) is informational; it changes no status.</p>'
    & $w '<h2>Main tables (aggregate only)</h2><ul>'
    $pr = @($st | Where-Object { $_.step -eq 'PAPER' -and [string]$_.run_folder -ne '' }) | Select-Object -First 1
    $links = @(@('FULL_RUN_SUMMARY.md', 'full run summary'), @('FULL_RUN_STATUS.csv', 'one row per step'), @('FULL_RUN_STATUS_LINES.csv', 'every status line, verbatim'), @('run_progress.txt', 'progress log'))
    if ($null -ne $pr) { foreach ($t in @(@('cumulative_results.csv', 'cumulative grid (1,680 specifications)'), @('dlm_final_summary.csv', 'nine final L01-L56 distributed-lag models'), @('dlm_final_lag_curves.csv', 'lag curves'), @('dlm_final_cumulative_curves.csv', 'cumulative curves'), @('input_validation_report.csv', 'input checks (counts only)'), @('RUN_SUMMARY.md', 'paper run summary'))) { $links += , @((([string]$pr.run_folder) + '\' + $t[0]), $t[1]) } }
    $cr = @(Get-ChildItem -LiteralPath $RunDir -Directory -Filter 'coded_model_record_*' -ErrorAction SilentlyContinue) | Select-Object -First 1; if ($null -ne $cr) { $links += , @(($cr.Name + '\SIECC_coded_model_record_v2.csv'), 'coded model record v2') }
    foreach ($l in $links) { if (Test-Path -LiteralPath (Join-Path $RunDir $l[0])) { & $w ('<li><a href="' + (& $E ($l[0] -replace '\\', '/')) + '">' + (& $E $l[0]) + '</a> - ' + (& $E $l[1]) + '</li>') } }
    & $w '</ul><p>Figures: this package version draws no figures; the tables above are its outputs.</p>'
    # post hoc steps (labelled; they cannot change q90, q9 or any accepted status)
    $phMain = [ordered]@{ EXPOSURE_EXPORT = 'exposure_aggregate_export.csv'; PH_DELETION = 'selected_deletion_refits_with_intervals.csv'; PH_L59_BOOTSTRAP = 'l59_dlm_bootstrap_summary.csv'; PH_NO_APRIL = 'no_april_cumulative_90.csv'; PH_SOURCE_AGREEMENT = 'weather_source_agreement_80.csv'
        PH_EXPOSURE_RESIDUALS = 'exposure_residual_diagnostics_27.csv'; PH_PROFILE_LIKELIHOOD = 'profile_likelihood_integration_30.csv'; PH_EARLY_LATE = 'early_late_subgroups_180.csv'; PH_H3_NESTED = 'H3_NESTED_TEST.csv'; PH_NEAREST_STATION = 'nearest_station_variant_summary.csv'
        PH_EXPOSURE_QA = 'exposure_qa_station_coverage.csv'; PH_VIF_SUMMARY = 'vif_per_model_m2l.csv'; PH_CALENDAR_SPLINE = 'calendar_spline_cumulative_180.csv'; PH_AGE65 = 'age65_cumulative_90.csv' }
    $phRows = @($st | Where-Object { $phMain.Contains([string]$_.step) })
    if ($phRows.Count) {
        & $w '<h2>Post hoc steps (labelled; primary inference unchanged)</h2><p class="note">Steps 17-30 follow <code>config/posthoc_sensitivity_protocol_v2.json</code> (steps 17-26 declared 13 Oct 2026, steps 27-30 on 14 Oct 2026, after the 30 July 2026 family lock). Each is labelled SENSITIVITY, POST_HOC or RECONSTRUCTION. They read the primary results and never write them: q90, q9, q9-nested and every accepted status are unchanged by them. The SI concordance column links, for each step, the comparison of package values with the manuscript/SI values (a differing value is reported, never tuned away). REVIEW and NOT_TESTED are never counted as PASS.</p>'
        & $w '<table><tr><th>Step</th><th>Label</th><th>Status</th><th>Main table</th><th>SI concordance</th></tr>'
        foreach ($s in $phRows) { $lab = @($sl | Where-Object { $_.step_id -eq $s.step_id -and $_.key -eq 'POSTHOC_LABEL' } | ForEach-Object { $_.value }) | Select-Object -First 1; $mt = ''
            if ([string]$s.run_folder -ne '') { $rel = ([string]$s.run_folder) + '\' + $phMain[[string]$s.step]; if (Test-Path -LiteralPath (Join-Path $RunDir $rel)) { $mt = '<a href="' + (& $E ($rel -replace '\\', '/')) + '">' + (& $E $phMain[[string]$s.step]) + '</a>' } }
            $sc = ''; if ([string]$s.run_folder -ne '') { foreach ($cf in @(Get-ChildItem -LiteralPath (Join-Path $RunDir ([string]$s.run_folder)) -File -Filter '*_si_concordance*.csv' -ErrorAction SilentlyContinue | Sort-Object Name)) { $cr = ([string]$s.run_folder) + '\' + $cf.Name; $sc += '<a href="' + (& $E ($cr -replace '\\', '/')) + '">' + (& $E $cf.Name) + '</a> ' } }
            & $w ('<tr><td>' + (& $E ($s.step_id + ' ' + $s.step)) + '</td><td>' + (& $E $lab) + '</td><td>' + (& $badge $s.class) + '</td><td>' + $mt + '</td><td>' + $sc + '</td></tr>') }
        & $w '</table>'
        $ex = @($phRows | Where-Object { $_.step -eq 'EXPOSURE_EXPORT' -and [string]$_.run_folder -ne '' }) | Select-Object -First 1
        if ($null -ne $ex) { $ef = Join-Path $RunDir (([string]$ex.run_folder) + '\exposure_aggregate_export.csv')
            if (Test-Path -LiteralPath $ef) { & $w '<h3>Aggregate exposure export (L01-L56; aggregates only)</h3><table><tr><th>Quantity</th><th>Value</th><th>Unit</th></tr>'
                foreach ($q in (Import-Csv -LiteralPath $ef)) { $v = $(if ([string]$q.value_text -ne '') { [string]$q.value_text } elseif ([string]$q.unit -eq 'percent' -or [string]$q.unit -eq 'degC') { '{0:N2}' -f [double]$q.value } else { [string]$q.value }); & $w ('<tr><td>' + (& $E $q.quantity) + '</td><td>' + (& $E $v) + '</td><td>' + (& $E $q.unit) + '</td></tr>') }
                & $w '</table>' } } }
    & $w '<h2>Every status, verbatim</h2>'
    foreach ($s in $st) { $lines = @($sl | Where-Object { $_.step_id -eq $s.step_id }); & $w ('<details><summary>' + (& $E ($s.step_id + ' ' + $s.step)) + ' ' + (& $badge $s.class) + ' (' + $lines.Count + ' lines)</summary><pre>' + (& $E ((@($lines | ForEach-Object { $_.key + '=' + $_.value })) -join "`n")) + '</pre></details>') }
    & $w '<h2>Fingerprints</h2><table>'
    foreach ($k in @('INPUT_SHA256', 'INPUT_SHA256_EXPECTED', 'CONFIG_SHA256', 'SOURCE_FINGERPRINT', 'SOURCE_FILES', 'PACKAGE_VERSION', 'R_VERSION')) { if ($envv.Contains($k)) { & $w ('<tr><td>' + (& $E $k) + '</td><td><code>' + (& $E $envv[$k]) + '</code></td></tr>') } }
    & $w '</table>'
    if ($PlanText -ne '') { & $w ('<h2>Plan shown before the run</h2><pre>' + (& $E $PlanText) + '</pre>') }
    & $w '<p>This page is self-contained (no internet, no scripts). It shows aggregate statuses only; no participant code or row value appears in any output.</p></body></html>'
    $f = Join-Path $RunDir 'RESULTS_SUMMARY.html'; [System.IO.File]::WriteAllLines($f, [string[]]$h.ToArray(), (New-Object System.Text.UTF8Encoding($false))); return $f
}

# Stage identified reference files under their declared names (byte-identical copies, verified) and write the project-mode paths file. Returns the paths-file path.
function New-SiewProjectRun {
    param([Parameter(Mandatory = $true)]$Plan, [string]$PlanText = '')
    $f = $Plan.Find; $outRoot = Join-Path $f.Project 'outputs'; New-Item -ItemType Directory -Path $outRoot -Force | Out-Null
    $stamp = Get-Date -Format 'yyyyMMdd_HHmmss'; $runDir = $null
    for ($i = 0; $i -lt 50; $i++) { $c = Join-Path $outRoot ('run_{0}_{1}' -f $stamp, ('{0:x4}' -f (Get-Random -Maximum 65536))); if (-not (Test-Path -LiteralPath $c)) { $runDir = $c; break } }
    New-Item -ItemType Directory -Path (Join-Path $runDir 'launcher') -Force | Out-Null
    $keys = [ordered]@{}; foreach ($k in $Plan.Keys.Keys) { $keys[$k] = $Plan.Keys[$k] }
    $idRows = @('role,status,declared_file,project_path,bytes,sha256,reason')
    $rel = { param($p) if ($p -eq '') { '' } else { $p.Substring([Math]::Min($f.Project.Length + 1, $p.Length)) } }
    $q = { param($x) '"' + ([string]$x -replace '"', "'") + '"' }
    $idRows += ('data_csv,{0},,{1},{2},{3},{4}' -f (& $q $f.Data.Status), (& $q (& $rel $f.Data.Path)), $f.Data.Bytes, $f.Data.Sha, (& $q $f.Data.Reason))
    foreach ($role in $f.Roles.Keys) {
        $r = $f.Roles[$role]
        if (@($r.Files).Count -eq 0) { $idRows += ('{0},{1},,{2},,,{3}' -f $role, $r.Status, (& $q (& $rel $r.Folder)), (& $q $r.Reason)) }
        foreach ($x in $r.Files) { $idRows += ('{0},{1},{2},{3},{4},{5},{6}' -f $role, $r.Status, (& $q $x.file), (& $q (& $rel $x.path)), $x.bytes, $x.sha256, (& $q $r.Reason)) }
        if ($keys[$role] -eq '<staged>') {
            $dest = Join-Path $runDir ('staged_references\' + $role); New-Item -ItemType Directory -Path $dest -Force | Out-Null
            foreach ($x in $r.Files) { $t = Join-Path $dest $x.file; Copy-Item -LiteralPath $x.path -Destination $t -Force; if ((Get-SiewSha256 $t) -ne $x.sha256) { throw ('SIW_PROJECT_STAGING_COPY_DIFFERS: ' + $role + ' ' + $x.file) } }
            $keys[$role] = $dest
        }
    }
    Write-SiewAtomic (Join-Path $runDir 'launcher\identification.csv') $idRows
    $uk = @('project_path,bytes'); foreach ($u in $f.Unknown) { $uk += ('{0},{1}' -f (& $q $u.path), $u.bytes) }; Write-SiewAtomic (Join-Path $runDir 'launcher\unknown_files.csv') $uk
    Write-SiewAtomic (Join-Path $runDir 'launcher\PROJECT_PLAN.txt') @($PlanText -split "`r?`n")
    $paths = [ordered]@{ _comment = 'written by the no-code launcher; project mode of run_all.ps1'; project_mode = $true; demo = [bool]$f.Demo; mode_label = $Plan.Mode; input_status = $(if ($Plan.InputStatus -eq 'NOT_THE_ACCEPTED_INPUT') { 'NOT_THE_ACCEPTED_INPUT' } else { 'ACCEPTED' }); data_csv = $f.Data.Path; out_root = $outRoot; run_dir = $runDir }
    foreach ($k in $keys.Keys) { $paths[$k] = $keys[$k] }
    $paths['not_tested_reasons'] = $Plan.Reasons
    $paths['input_identification'] = $Plan.IdentificationEvidence
    $paths['step_identification'] = [ordered]@{}; foreach ($s in $Plan.Steps) { if (@($s.Identification).Count) { $paths['step_identification'][$s.Name] = @($s.Identification) } }
    $pf = Join-Path $runDir 'launcher\paths.json'; [System.IO.File]::WriteAllText($pf, ($paths | ConvertTo-Json -Depth 4), (New-Object System.Text.UTF8Encoding($false)))
    return [pscustomobject]@{ RunDir = $runDir; PathsFile = $pf }
}

# One run_progress.txt line in plain words: "Step 05 of 16: BATCH_A - finished: PASS (exit 0), 41.2 s (elapsed 03:10)". Pure function.
function Format-SiewProgressLine {
    param([string]$Line, [TimeSpan]$Elapsed)
    $el = '{0:00}:{1:00}' -f [math]::Floor($Elapsed.TotalMinutes), $Elapsed.Seconds
    $m = [regex]::Match($Line, '\| step (\d\d(?:-\d\d)?)/(\d+) ([A-Z_0-9]+) \| (START|END|RESTORED|NOT_TESTED|STOPPED)(?: \| ([^|]*?))?(?: \| ([0-9.]+) s)? \| free ([0-9.,]+) GB$')
    if ($m.Success) {
        $what = switch ($m.Groups[4].Value) { 'START' { 'started' } 'END' { 'finished: ' + $m.Groups[5].Value.Trim() + $(if ($m.Groups[6].Success) { ', ' + $m.Groups[6].Value + ' s' } else { '' }) } 'RESTORED' { 'restored from the earlier attempt: ' + $m.Groups[5].Value.Trim() } 'NOT_TESTED' { 'NOT_TESTED (' + $m.Groups[5].Value.Trim() + ')' } 'STOPPED' { 'STOPPED: ' + $m.Groups[5].Value.Trim() } }
        return ('Step {0} of {1}: {2} - {3} (elapsed {4}; free {5} GB)' -f $m.Groups[1].Value, $m.Groups[2].Value, $m.Groups[3].Value, $what, $el, $m.Groups[7].Value)
    }
    if ($Line -match '\| RUN END \| ([^|]+)') { return ('Run finished: {0} (elapsed {1})' -f $matches[1].Trim(), $el) }
    if ($Line -match '\| (RUN START|RUN RESUME) \|') { return ('Run started (elapsed {0})' -f $el) }
    if ($Line -match '\| NOTE \| (.*) \| free') { return ('NOTE: ' + $matches[1]) }
    return $Line
}

# Run run_all.ps1 -PathsFile in a child process; show "step x of n, name, elapsed" from run_progress.txt. Returns the exit code.
function Invoke-SiewProjectEngine {
    param([Parameter(Mandatory = $true)][string]$Repo, [Parameter(Mandatory = $true)]$Run, [double]$MinFreeGB = 2)
    $log = Join-Path $Run.RunDir 'launcher\run_all_console.log'; $err = Join-Path $Run.RunDir 'launcher\run_all_console_err.log'
    $argLine = '-NoProfile -NonInteractive -ExecutionPolicy Bypass -File "{0}" -PathsFile "{1}" -MinFreeGB {2}' -f (Join-Path $Repo 'run_all.ps1'), $Run.PathsFile, ([string]$MinFreeGB).Replace(',', '.')
    $oldMod = $env:PSModulePath; $env:PSModulePath = Get-SiewWinPSModulePath
    try { $p = Start-Process -FilePath $script:PowerShellExe -ArgumentList $argLine -WorkingDirectory $Repo -NoNewWindow -PassThru -RedirectStandardOutput $log -RedirectStandardError $err } finally { $env:PSModulePath = $oldMod }
    $null = $p.Handle                     # caches the process handle so that ExitCode is available after the process ends
    $t0 = Get-Date; $seen = 0; $beat = Get-Date; $prog = Join-Path $Run.RunDir 'run_progress.txt'
    $show = {
        if (-not (Test-Path -LiteralPath $prog)) { return }
        $ln = @(Get-Content -LiteralPath $prog -Encoding UTF8 -ErrorAction SilentlyContinue)
        for ($i = $script:SiewSeen; $i -lt $ln.Count; $i++) { Write-Host ('  ' + (Format-SiewProgressLine $ln[$i] ((Get-Date) - $t0))) }
        if ($ln.Count -gt $script:SiewSeen) { $script:SiewSeen = $ln.Count; $script:SiewBeat = Get-Date }
    }
    $script:SiewSeen = 0; $script:SiewBeat = Get-Date
    while (-not $p.HasExited) {
        Start-Sleep -Milliseconds 1500; & $show
        if (((Get-Date) - $script:SiewBeat).TotalSeconds -ge 60) { Write-Host ('  ... still working (elapsed {0:mm\:ss})' -f ((Get-Date) - $t0)); $script:SiewBeat = Get-Date }
    }
    $p.WaitForExit(); & $show
    $rc = $p.ExitCode; if ($null -eq $rc) { $rc = 1 }
    return [int]$rc
}

# Hashes of every launcher-written file added after FULL_RUN_MANIFEST.csv (the results page, the console logs).
function Write-SiewLauncherManifest {
    param([string]$RunDir)
    $rows = @('file,bytes,sha256')
    foreach ($x in @(Get-ChildItem -LiteralPath (Join-Path $RunDir 'launcher') -File) + @(Get-Item -LiteralPath (Join-Path $RunDir 'RESULTS_SUMMARY.html') -ErrorAction SilentlyContinue)) { if ($null -ne $x -and $x.Name -ne 'LAUNCHER_MANIFEST.csv') { $rows += ('"{0}",{1},{2}' -f $x.FullName.Substring($RunDir.Length + 1), $x.Length, (Get-SiewSha256 $x.FullName)) } }
    Write-SiewAtomic (Join-Path $RunDir 'launcher\LAUNCHER_MANIFEST.csv') $rows
}
