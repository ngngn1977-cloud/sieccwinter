<#
.SYNOPSIS
  Writes the post hoc input-identification record project_inputs_declaration_v1.json (run by the author; started by DECLARE_SIECC_INPUTS.cmd).
  Pick (or paste) the data CSV, each reference bundle folder, the failed ANOM run, the historical accepted paper run and Batch B run, and the folders holding
  acceptance-time records (earlier validation runs). Records ONLY file names, byte sizes and SHA-256; for runs the RUN_MANIFEST.csv hash after verifying every
  manifest-listed file. Never prints or copies file contents. Each bundle file is cross-checked against acceptance-time records: CONFIRMED_AT_ACCEPTANCE
  (hash equals a recorded hash; the run and record file are cited) or UNCONFIRMED (no record, or a different recorded hash; both hashes shown).
  The four protocol-declared bundles are compared with their protocol hashes (CONFIRMED_BY_PROTOCOL or UNCONFIRMED).
  Two further statuses exist for the declaration-sourced bundles; a file is tried in this order: CONFIRMED_AT_ACCEPTANCE, then CONFIRMED_BY_FROZEN_SOURCE (-FrozenSourceFolder:
  the same-named file in the frozen source folder has the same SHA-256; the evidence cites that path and hash; a same-named file with another hash gives UNCONFIRMED), then
  AUTHOR_ACCEPTED_EVIDENCE (-AuthorAcceptedEvidence @{ 'role/file' = 'evidence text' } with -AuthorName and -AuthorDate: accepted by the author on cited evidence, no acceptance-time hash;
  refused, nothing written, when the evidence text, the author or the date is missing, and never applied over a contradicting recorded hash). A bundle takes its weakest file status.
.NOTES
  Writes three files into -OutDir (never into the package): project_inputs_declaration_v1.json, DECLARATION_REPORT.txt, DECLARATION_REPORT.csv. An existing
  declaration in -OutDir is never overwritten. Review the report, then import with scripts\import_project_declaration.ps1 (or copy the JSON into config\ and
  inst\extdata\ yourself). No protocol, plan, tolerance or accepted result is changed. Exit codes: 0 written; 2 refused (nothing written).
#>
[CmdletBinding()]
param(
    [string]$DataCsv = '', [string]$CoreReference = '', [string]$BatchAReference = '', [string]$BatchBReference = '', [string]$BatchCReference = '', [string]$DlmReference = '',
    [string]$ArchivedDlmReference = '', [string]$ColdBurdenReference = '', [string]$DailyLayerReference = '', [string]$AnomhistReference = '',
    [string]$AnomFailedRun = '', [string]$PaperRun = '', [string]$BatchBRun = '', [string[]]$AcceptanceRecordRuns = @(), [string]$OutDir = '', [switch]$NonInteractive,
    [string]$FrozenSourceFolder = '', [hashtable]$AuthorAcceptedEvidence = @{}, [string]$AuthorName = '', [string]$AuthorDate = '', [string[]]$AuthorAccepted = @()
)
# command-line form of -AuthorAcceptedEvidence: one item per file, "role/file=evidence text" (split at the first '=')
foreach ($aa in @($AuthorAccepted | ForEach-Object { ([string]$_) -split [string][char]31 })) { $i = ([string]$aa).IndexOf('='); if ($i -lt 1) { $AuthorAcceptedEvidence[([string]$aa).Trim()] = '' } else { $AuthorAcceptedEvidence[([string]$aa).Substring(0, $i).Trim()] = ([string]$aa).Substring($i + 1) } }
$ErrorActionPreference = 'Stop'
$AcceptanceRecordRuns = @($AcceptanceRecordRuns | ForEach-Object { ([string]$_) -split ';' } | ForEach-Object { $_.Trim().Trim('"') } | Where-Object { $_ -ne '' })   # several folders may be given as one ';'-separated value (powershell -File)
$repo = Split-Path -Parent $PSScriptRoot
. (Join-Path $PSScriptRoot '_siecc_project.ps1')
if ($PSVersionTable.PSVersion.Major -ne 5) { exit (Invoke-SiewRelaunchInWinPS -ScriptPath $PSCommandPath -Bound $PSBoundParameters) }   # started from PowerShell 7 or another host: run in Windows PowerShell 5.1
try { [Console]::OutputEncoding = New-Object System.Text.UTF8Encoding($false) } catch { }

function Select-SiewPath {
    param([string]$Title, [switch]$File)
    try {
        Add-Type -AssemblyName System.Windows.Forms
        if ($File) { $d = New-Object System.Windows.Forms.OpenFileDialog; $d.Title = $Title; $d.Filter = 'CSV files (*.csv)|*.csv|All files (*.*)|*.*'; if ($d.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) { return $d.FileName } }
        else { $d = New-Object System.Windows.Forms.FolderBrowserDialog; $d.Description = $Title; if ($d.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) { return $d.SelectedPath } }
    } catch { Write-Host 'The window could not be opened here.' }
    $p = Read-Host ($Title + ' - paste the path, or press Enter to skip'); return ([string]$p).Trim().Trim('"')
}

$roleParams = [ordered]@{ core_reference = 'CoreReference'; batch_a_reference = 'BatchAReference'; batch_b_reference = 'BatchBReference'; batch_c_reference = 'BatchCReference'; dlm_reference = 'DlmReference'; archived_dlm_reference = 'ArchivedDlmReference'; cold_burden_reference = 'ColdBurdenReference'; daily_layer_reference = 'DailyLayerReference'; anomhist_reference = 'AnomhistReference' }
$runParams = [ordered]@{ anom_failed_run = 'AnomFailedRun'; paper_accepted_run = 'PaperRun'; batchb_accepted_run = 'BatchBRun' }
$sel = @{}; foreach ($k in $roleParams.Keys) { $sel[$k] = (Get-Variable -Name $roleParams[$k] -ValueOnly) }; foreach ($k in $runParams.Keys) { $sel[$k] = (Get-Variable -Name $runParams[$k] -ValueOnly) }
Write-Host 'SIECC input-identification declaration (post hoc record; changes no protocol, plan, tolerance or accepted result)' -ForegroundColor Cyan
if (-not $NonInteractive) {
    Write-Host 'For each item a window opens. Cancel (or press Enter at the paste prompt) to skip an item you do not have.'
    if ($DataCsv -eq '') { $DataCsv = Select-SiewPath 'The study data CSV (siecc_winter_canonical.csv)' -File }
    foreach ($k in $roleParams.Keys) { if ($sel[$k] -eq '') { $sel[$k] = Select-SiewPath ('Folder of the ' + $script:RoleLabels[$k]) } }
    foreach ($k in $runParams.Keys) { if ($sel[$k] -eq '') { $sel[$k] = Select-SiewPath ('Folder of the ' + $script:DeclaredRuns[$k].Label) } }
    $acc = New-Object System.Collections.ArrayList; foreach ($a in $AcceptanceRecordRuns) { [void]$acc.Add($a) }
    while ($true) { $more = ([string](Read-Host 'Add a folder with acceptance-time records (an earlier validation run, e.g. validate_..., batcha_..., batchb_..., batchc_..., dlmvalidate_...)? (Y/N)')).Trim(); if ($more -notmatch '^(?i)y') { break }; $x = Select-SiewPath 'Folder with acceptance-time records'; if ($x -ne '') { [void]$acc.Add($x) } }
    $AcceptanceRecordRuns = $acc.ToArray()
    if ($OutDir -eq '') { $OutDir = Select-SiewPath 'Folder where the declaration and its report are written (outside the package)' }
}
if ($OutDir -eq '') { Write-Host 'No output folder chosen. Nothing was written.' -ForegroundColor Red; exit 2 }
foreach ($ak in @($AuthorAcceptedEvidence.Keys)) {
    if (([string]$AuthorAcceptedEvidence[$ak]).Trim() -eq '') { Write-Host ('AUTHOR_ACCEPTED_EVIDENCE for ' + $ak + ' has no evidence text. Nothing was written.') -ForegroundColor Red; exit 2 }
    if (([string]$AuthorAcceptedEvidence[$ak]) -match '<[^>]*>') { Write-Host ('The evidence text for ' + $ak + ' contains <...> (placeholder). Nothing was written.') -ForegroundColor Red; exit 2 }
    if ($AuthorName.Trim() -eq '' -or $AuthorDate.Trim() -eq '') { Write-Host 'AUTHOR_ACCEPTED_EVIDENCE requires -AuthorName and -AuthorDate. Nothing was written.' -ForegroundColor Red; exit 2 }
}
if ($FrozenSourceFolder -ne '' -and -not (Test-Path -LiteralPath $FrozenSourceFolder -PathType Container)) { Write-Host ('The frozen source folder was not found: ' + $FrozenSourceFolder + '. Nothing was written.') -ForegroundColor Red; exit 2 }
$out = ConvertTo-SiewFullPath $OutDir
if (Test-SiewInside $out (ConvertTo-SiewFullPath $repo)) { Write-Host 'The output folder must be outside the package folder (review the declaration first, then import it). Nothing was written.' -ForegroundColor Red; exit 2 }
New-Item -ItemType Directory -Path $out -Force | Out-Null
if (Test-Path -LiteralPath (Join-Path $out $script:DeclarationFileName)) { Write-Host 'A declaration already exists in that folder; it is never overwritten. Choose another folder. Nothing was written.' -ForegroundColor Red; exit 2 }

# acceptance-time records: (file name, hash) pairs and every 64-hex value, from provenance CSVs, manifests and metadata JSON; contents are never printed
$recPairs = New-Object System.Collections.ArrayList; $recAny = @{}
$bundleDirs = @($roleParams.Keys | ForEach-Object { [string]$sel[$_] } | Where-Object { $_ -ne '' -and (Test-Path -LiteralPath $_ -PathType Container) } | ForEach-Object { ConvertTo-SiewFullPath $_ })
foreach ($rr in $AcceptanceRecordRuns) {
    if ($rr -eq '' -or -not (Test-Path -LiteralPath $rr -PathType Container)) { Write-Host ('Skipped (not a folder): ' + $rr) -ForegroundColor Yellow; continue }
    $rn = Split-Path -Leaf (ConvertTo-SiewFullPath $rr)
    # a bundle never confirms itself: record files inside a chosen bundle folder are ignored
    foreach ($f in @(Get-ChildItem -LiteralPath $rr -Recurse -File | Where-Object { $_.Name -match '(?i)(provenance.*\.csv|^RUN_MANIFEST\.csv$|^MANIFEST\.csv$|metadata.*\.json$|source_manifest.*\.csv$)' } | Where-Object { $ff = $_.FullName; -not @($bundleDirs | Where-Object { Test-SiewInside $ff $_ }).Count })) {
        $cite = $rn + '\' + $f.FullName.Substring((ConvertTo-SiewFullPath $rr).Length + 1); $txt = [System.IO.File]::ReadAllText($f.FullName)
        foreach ($m in [regex]::Matches($txt, '(?i)\b[0-9a-f]{64}\b')) { $h = $m.Value.ToUpper(); if (-not $recAny.ContainsKey($h)) { $recAny[$h] = $cite } }
        if ($f.Extension -ieq '.csv') { foreach ($row in (Import-Csv -LiteralPath $f.FullName)) { $vals = @($row.PSObject.Properties | ForEach-Object { [string]$_.Value }); $hs = @($vals | Where-Object { $_ -match '^(?i)[0-9a-f]{64}$' }); $ns = @($vals | Where-Object { $_ -match '\.[A-Za-z0-9]{1,5}$' -and $_ -notmatch '^(?i)[0-9a-f]{64}$' }); foreach ($h in $hs) { foreach ($n in $ns) { [void]$recPairs.Add([pscustomobject]@{ name = (Split-Path -Leaf ($n -replace '/', '\')); sha = $h.ToUpper(); cite = $cite }) } } } }
        else {
            foreach ($m in [regex]::Matches($txt, '"([^"]+\.[A-Za-z0-9]{1,5})"\s*:\s*"([0-9A-Fa-f]{64})"')) { [void]$recPairs.Add([pscustomobject]@{ name = (Split-Path -Leaf ($m.Groups[1].Value -replace '/', '\')); sha = $m.Groups[2].Value.ToUpper(); cite = $cite }) }
            foreach ($m in [regex]::Matches($txt, '"file"\s*:\s*"([^"]+)"[^{}]*?"(?:sha256|hash)"\s*:\s*"([0-9A-Fa-f]{64})"')) { [void]$recPairs.Add([pscustomobject]@{ name = (Split-Path -Leaf ($m.Groups[1].Value -replace '/', '\')); sha = $m.Groups[2].Value.ToUpper(); cite = $cite }) }
        }
    }
}
function Get-SiewAcceptanceStatus {
    param([string]$Name, [string]$Sha, [string]$Role = '')
    $same = @($recPairs | Where-Object { $_.name -eq $Name })
    $hit = @($same | Where-Object { $_.sha -eq $Sha }) | Select-Object -First 1
    if ($null -ne $hit) { return [pscustomobject]@{ status = 'CONFIRMED_AT_ACCEPTANCE'; evidence = ('recorded with this file name in ' + $hit.cite) } }
    if ($recAny.ContainsKey($Sha)) { return [pscustomobject]@{ status = 'CONFIRMED_AT_ACCEPTANCE'; evidence = ('hash recorded in ' + $recAny[$Sha]) } }
    if ($same.Count) { $o = $same[0]; return [pscustomobject]@{ status = 'UNCONFIRMED'; evidence = ('MISMATCH: declared ' + $Sha + '; acceptance record ' + $o.sha + ' (' + $o.cite + ')') } }
    # frozen source: the same-named file in the frozen source folder, compared by SHA-256 (a different hash is a contradiction, never overridden by the author)
    if ($FrozenSourceFolder -ne '') {
        $fp = Join-Path $FrozenSourceFolder $Name
        if (Test-Path -LiteralPath $fp -PathType Leaf) {
            $fh = Get-SiewSha256 $fp
            if ($fh -eq $Sha) { return [pscustomobject]@{ status = 'CONFIRMED_BY_FROZEN_SOURCE'; evidence = ('byte-identical (SHA-256 ' + $Sha + ') to the frozen source file ' + $fp); source_path = $fp; source_sha256 = $fh } }
            return [pscustomobject]@{ status = 'UNCONFIRMED'; evidence = ('MISMATCH with the frozen source: declared ' + $Sha + '; frozen file ' + $fh + ' (' + $fp + ')') }
        }
    }
    $ak = $Role + '/' + $Name
    if ($AuthorAcceptedEvidence.ContainsKey($ak)) { return [pscustomobject]@{ status = 'AUTHOR_ACCEPTED_EVIDENCE'; evidence = [string]$AuthorAcceptedEvidence[$ak]; author = $AuthorName.Trim(); author_date = $AuthorDate.Trim() } }
    return [pscustomobject]@{ status = 'UNCONFIRMED'; evidence = 'no acceptance-time record of this file was found in the record folders given' }
}

$report = New-Object System.Collections.ArrayList; $csv = @('section,role,file,bytes,sha256,status,evidence'); $q = { param($x) '"' + ([string]$x -replace '"', "'") + '"' }
$decl = [ordered]@{ declaration_version = '1'; status_of_this_document = $script:DeclarationMarking; created_local = (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'); created_with = 'scripts/declare_project_inputs.ps1'
    package_version = ((Get-Content -LiteralPath (Join-Path $repo 'DESCRIPTION') | Where-Object { $_ -like 'Version:*' }) -replace '^Version:\s*', ''); config_sha256 = (Get-SiewSha256 (Join-Path (Get-SiewConfigDir $repo) 'sieccwinter_config.json'))
    accepted_input_sha256 = $script:AcceptedInputSha; privacy = 'file names, byte sizes and SHA-256 only; no file content was copied or recorded' }
if ($DataCsv -ne '' -and (Test-Path -LiteralPath $DataCsv -PathType Leaf)) {
    $fi = Get-Item -LiteralPath $DataCsv; $h = Get-SiewSha256 $fi.FullName; $st = $(if ($h -eq $script:AcceptedInputSha) { 'ACCEPTED_SHA_MATCH' } else { 'NOT_THE_ACCEPTED_INPUT' })
    $decl.data_csv = [ordered]@{ file = $fi.Name; bytes = $fi.Length; sha256 = $h; status = $st }; $csv += ('data,data_csv,{0},{1},{2},{3},' -f (& $q $fi.Name), $fi.Length, $h, $st); [void]$report.Add(('data CSV: {0} ({1} bytes) {2}' -f $fi.Name, $fi.Length, $st))
} else { [void]$report.Add('data CSV: not given') }
$bundles = @(); $authorUsed = @{}
foreach ($spec in (Get-SiewRoleSpecs $repo)) {
    $role = $spec.Role; $dir = [string]$sel[$role]
    if ($dir -eq '') { [void]$report.Add(('{0}: not given' -f $role)); continue }
    if (-not (Test-Path -LiteralPath $dir -PathType Container)) { [void]$report.Add(('{0}: folder not found ({1})' -f $role, $dir)); continue }
    $files = @(); $issues = @()
    foreach ($sf in $spec.Files) {
        $p = Join-Path $dir $sf.file
        if (-not (Test-Path -LiteralPath $p -PathType Leaf)) { if ($sf.group_required -and @($spec.Files | Where-Object { $_.group -eq $sf.group -and (Test-Path -LiteralPath (Join-Path $dir $_.file) -PathType Leaf) }).Count -eq 0) { $issues += ('required file missing: ' + $sf.file) }; continue }
        $fi = Get-Item -LiteralPath $p; $h = Get-SiewSha256 $fi.FullName
        if ($spec.Source -eq 'protocol') { $a = $(if ($h -eq $sf.sha256) { [pscustomobject]@{ status = 'CONFIRMED_BY_PROTOCOL'; evidence = ('equals the hash declared in ' + $spec.SourceFile) } } else { [pscustomobject]@{ status = 'UNCONFIRMED'; evidence = ('MISMATCH: file ' + $h + '; ' + $spec.SourceFile + ' declares ' + $sf.sha256) } }) }
        else { $a = Get-SiewAcceptanceStatus $sf.file $h $role }
        if ($a.status -eq 'UNCONFIRMED') { $issues += ($sf.file + ': ' + $a.evidence) }
        $fe = [ordered]@{ file = $sf.file; bytes = $fi.Length; sha256 = $h; status = $a.status; evidence = $a.evidence }
        if ($a.status -eq 'CONFIRMED_BY_FROZEN_SOURCE') { $fe.source_path = $a.source_path; $fe.source_sha256 = $a.source_sha256 }
        if ($a.status -eq 'AUTHOR_ACCEPTED_EVIDENCE') { $fe.author = $a.author; $fe.author_date = $a.author_date; $authorUsed[$role + '/' + $sf.file] = $true }
        $files += $fe
        $csv += ('bundle,{0},{1},{2},{3},{4},{5}' -f $role, (& $q $sf.file), $fi.Length, $h, $a.status, (& $q ($a.evidence + $(if ($a.status -eq 'AUTHOR_ACCEPTED_EVIDENCE') { ' [author: ' + $a.author + ', ' + $a.author_date + ']' } else { '' }))))
    }
    # a bundle takes its weakest file status
    $fst = @($files | ForEach-Object { $_.status })
    $rs = $(if ($issues.Count) { 'UNCONFIRMED' } elseif ($spec.Source -eq 'protocol') { 'CONFIRMED_BY_PROTOCOL' } elseif ($fst -contains 'AUTHOR_ACCEPTED_EVIDENCE') { 'AUTHOR_ACCEPTED_EVIDENCE' } elseif ($fst -contains 'CONFIRMED_BY_FROZEN_SOURCE') { 'CONFIRMED_BY_FROZEN_SOURCE' } else { 'CONFIRMED_AT_ACCEPTANCE' })
    $bundles += [ordered]@{ role = $role; source = $spec.Source; folder_name = (Split-Path -Leaf (ConvertTo-SiewFullPath $dir)); status = $rs; reason = ($issues -join ' | '); files = $files }
    [void]$report.Add(('{0}: {1} ({2} file(s)){3}' -f $role, $rs, $files.Count, $(if ($issues.Count) { ' - ' + ($issues -join ' | ') } else { '' })))
}
$decl.bundles = $bundles
foreach ($ak in @($AuthorAcceptedEvidence.Keys)) { if (-not $authorUsed.ContainsKey($ak)) { [void]$report.Add(('NOTE: author-accepted evidence given for ' + $ak + ' was not applied (no such file, or a hash record or frozen-source file decided it first).')) } }
$runs = @()
foreach ($dr in $script:DeclaredRuns.Keys) {
    $spec = $script:DeclaredRuns[$dr]; $dir = [string]$sel[$dr]
    if ($dir -eq '') { [void]$report.Add(('{0}: not given' -f $dr)); continue }
    if (-not (Test-Path -LiteralPath (Join-Path $dir 'RUN_MANIFEST.csv') -PathType Leaf)) { [void]$report.Add(('{0}: RUN_MANIFEST.csv not found in {1}' -f $dr, $dir)); continue }
    $mf = Get-Item -LiteralPath (Join-Path $dir 'RUN_MANIFEST.csv'); $mh = Get-SiewSha256 $mf.FullName; $v = Test-SiewRunManifest $dir
    $miss = @($spec.Required | Where-Object { -not (Test-Path -LiteralPath (Join-Path $dir $_) -PathType Leaf) })
    $st = $(if (-not $v.Ok) { 'MANIFEST_NOT_VERIFIED' } elseif ($miss.Count) { 'MISSING_REQUIRED_FILES' } else { 'MANIFEST_VERIFIED' })
    $why = $(if (-not $v.Ok) { ($v.Problems | Select-Object -First 5) -join '; ' } elseif ($miss.Count) { 'missing: ' + ($miss -join ', ') } else { ('' + $v.Files + ' manifest-listed files verified') })
    $ev = $(if ($recAny.ContainsKey($mh)) { 'RUN_MANIFEST.csv hash recorded in ' + $recAny[$mh] } else { 'no acceptance-time record of this run''s manifest hash in the record folders given (informational)' })
    $runs += [ordered]@{ declared_run = $dr; roles = $spec.Roles; folder_name = (Split-Path -Leaf (ConvertTo-SiewFullPath $dir)); run_manifest = [ordered]@{ bytes = $mf.Length; sha256 = $mh }; manifest_files = $v.Files; required_files = $spec.Required; status = $st; reason = $why; acceptance_evidence = $ev }
    $csv += ('run,{0},RUN_MANIFEST.csv,{1},{2},{3},{4}' -f $dr, $mf.Length, $mh, $st, (& $q ($why + ' | ' + $ev)))
    [void]$report.Add(('{0}: {1} - {2} - {3}' -f $dr, $st, $why, $ev))
}
$decl.runs = $runs
$decl.acceptance_record_runs = @($AcceptanceRecordRuns | Where-Object { $_ -ne '' } | ForEach-Object { Split-Path -Leaf (ConvertTo-SiewFullPath $_) })
$jf = Join-Path $out $script:DeclarationFileName
[System.IO.File]::WriteAllText($jf, ($decl | ConvertTo-Json -Depth 6), (New-Object System.Text.UTF8Encoding($false)))
$hdr = @('SIECC INPUT-IDENTIFICATION DECLARATION - REPORT', ('Marking: ' + $script:DeclarationMarking), ('Written: ' + $decl.created_local), 'Only names, byte sizes and SHA-256 were recorded; no file content was copied.', '',
    'Status meanings: CONFIRMED_AT_ACCEPTANCE = the hash equals an acceptance-time record (cited); CONFIRMED_BY_PROTOCOL = equals the hash in the protocol;', 'CONFIRMED_BY_FROZEN_SOURCE = byte-identical (SHA-256) to a named file in the frozen source folder (path and hash cited); AUTHOR_ACCEPTED_EVIDENCE = accepted by the named author on the cited evidence, no acceptance-time hash;', 'UNCONFIRMED = no record or a different recorded hash (the launcher then reports the role NOT_TESTED, never PASS); MANIFEST_VERIFIED = every file listed in the run''s RUN_MANIFEST.csv verified.', '')
Write-SiewAtomic (Join-Path $out 'DECLARATION_REPORT.txt') (@($hdr) + @($report.ToArray()) + @('', ('Declaration: ' + $jf), 'Next: review this report. Then import with scripts\import_project_declaration.ps1 -File "<this declaration>" (or copy it into config\ and inst\extdata\).'))
Write-SiewAtomic (Join-Path $out 'DECLARATION_REPORT.csv') $csv
Write-Host ''; foreach ($l in $report) { Write-Host ('  ' + $l) -ForegroundColor $(if ($l -like '*UNCONFIRMED*' -or $l -like '*NOT_VERIFIED*' -or $l -like '*MISSING*' -or $l -like '*NOT_THE_ACCEPTED*') { 'Yellow' } else { 'Gray' }) }
Write-Host ''; Write-Host ('Written: {0}' -f $jf) -ForegroundColor Green; Write-Host ('Report:  {0}' -f (Join-Path $out 'DECLARATION_REPORT.txt'))
exit 0
