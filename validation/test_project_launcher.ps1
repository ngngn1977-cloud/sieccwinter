<#
.SYNOPSIS
  Tests the no-code interface on SYNTHETIC data only: the declaration helper, the import helper and the launcher (RUN_SIECC.cmd's script) in non-interactive mode.
  Every project folder lives under a path with spaces and Hebrew characters. Writes only under -WorkDir; checks that nothing in the package changes.
.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File .\validation\test_project_launcher.ps1
#>
param([string]$WorkDir = '', [string]$ResultCsv = '', [switch]$Keep)
$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent $PSScriptRoot
. (Join-Path $repo 'scripts\_siecc_project.ps1')
if ($WorkDir -eq '') { $WorkDir = Join-Path $env:TEMP 'siw_pt' }
function Remove-Tree { param([string]$D) if ([System.IO.Directory]::Exists($D)) { [System.IO.Directory]::Delete('\\?\' + [System.IO.Path]::GetFullPath($D), $true) } }
Remove-Tree $WorkDir; New-Item -ItemType Directory $WorkDir -Force | Out-Null; $WorkDir = (Resolve-Path -LiteralPath $WorkDir).Path
$W = Join-Path $WorkDir ('lt proj ' + [char]0x05E9 + [char]0x05EA + ' x'); New-Item -ItemType Directory $W -Force | Out-Null
$results = New-Object System.Collections.ArrayList
function Check { param([string]$Name, $Expected, $Actual) $ok = ([string]$Expected -eq [string]$Actual); [void]$script:results.Add([pscustomobject]@{ test = $Name; expected = [string]$Expected; actual = [string]$Actual; pass = $ok }); Write-Host ("  [{0}] {1}: expected {2}, got {3}" -f $(if ($ok) { 'ok' } else { 'FAIL' }), $Name, $Expected, $Actual) -ForegroundColor $(if ($ok) { 'Green' } else { 'Red' }) }
$PS = Join-Path $PSHOME 'powershell.exe'
function Run-Script { param([string]$Script, [string[]]$A) $o = & $PS -NoProfile -ExecutionPolicy Bypass -File (Join-Path $repo $Script) @A 2>&1 | ForEach-Object { [string]$_ }; return [pscustomobject]@{ Code = $LASTEXITCODE; Out = $o; Text = ($o -join "`n") } }
function Launch { param([string[]]$A) $x = @('-NoOpen') + $A; if ($A -notcontains '-DeclarationFile') { $x += '-NoDeclaration' }; return (Run-Script 'scripts\siecc_launcher.ps1' $x) }   # the tests never depend on a declaration installed in the package
function Pkg-Snapshot { $h = @{}; foreach ($d in 'R', 'scripts', 'config', 'inst', 'tests', 'validation', 'man') { foreach ($f in Get-ChildItem -LiteralPath (Join-Path $repo $d) -Recurse -File) { $h[$f.FullName] = (Get-FileHash -LiteralPath $f.FullName -Algorithm SHA256).Hash } }; foreach ($f in Get-ChildItem -LiteralPath $repo -File) { $h[$f.FullName] = (Get-FileHash -LiteralPath $f.FullName -Algorithm SHA256).Hash }; return $h }
$before = Pkg-Snapshot
$syn = Join-Path $repo 'inst\extdata\synthetic_siecc_winter.csv'

Write-Host '0. pure functions'
Check 'progress line: END' 'Step 05 of 16: BATCH_A - finished: PASS (exit 0), 41.2 s (elapsed 03:10; free 12.5 GB)' (Format-SiewProgressLine '2026-10-07 10:00:00 | step 05/16 BATCH_A | END | PASS (exit 0) | 41.2 s | free 12.5 GB' ([TimeSpan]::FromSeconds(190)))
Check 'progress line: START' 'Step 03 of 16: PAPER - started (elapsed 00:05; free 9 GB)' (Format-SiewProgressLine '2026-10-07 10:00:00 | step 03/16 PAPER | START | free 9 GB' ([TimeSpan]::FromSeconds(5)))
Check 'progress line: NOT_TESTED' $true ((Format-SiewProgressLine '2026-10-07 10:00:00 | step 11/16 ARCHIVED_DLM | NOT_TESTED | input absent (archived_dlm_reference) | free 9 GB' ([TimeSpan]::FromSeconds(5))) -like 'Step 11 of 16: ARCHIVED_DLM - NOT_TESTED (input absent (archived_dlm_reference))*')
$specs = Get-SiewRoleSpecs $repo; Check 'role specs: 12 reference roles (9 + 3 post hoc)' 12 $specs.Count; Check 'role specs: post hoc roles from the post hoc protocol' 'posthoc_v14_reference:2,archive_engine_reference:5,gis_ims_v8_1_reference:11' ((@($specs | Where-Object { $_.SourceFile -eq 'posthoc_sensitivity_protocol_v2.json' }) | ForEach-Object { $_.Role + ':' + @($_.Files).Count }) -join ',')
Check 'role specs: protocol roles carry hash and size' $true (@($specs | Where-Object { $_.Source -eq 'protocol' } | ForEach-Object { $_.Files } | Where-Object { $_.sha256 -notmatch '^[0-9A-F]{64}$' -or $_.bytes -le 0 }).Count -eq 0)
Check 'role specs: the core plan''s out-of-scope archived file is not part of the core role' 0 @(($specs | Where-Object { $_.Role -eq 'core_reference' }).Files | Where-Object { $_.file -eq 'fixed_DLM_summary.csv' }).Count
$fake = [pscustomobject]@{ Project = (Join-Path $repo 'some_project'); Repo = $repo; Roles = [ordered]@{}; Data = [pscustomobject]@{ Status = 'DEMO_SYNTHETIC_INPUT'; Path = (Join-Path $repo 'some_project\data\x.csv'); Sha = ''; Bytes = 1; Reason = '' }; Ambiguities = @(); Unknown = @(); FilesScanned = 0; FilesHashed = 0; Demo = $true; Declaration = '' }
foreach ($r in @($script:BundleRoles) + @('anom_failed_run', 'dlm_accepted_run', 'batch_b_legacy_run', 'batch_c_accepted_run', 'nearest_station_histories', 'gis_ims_v8_1_reference')) { $fake.Roles[$r] = [pscustomobject]@{ Status = 'ABSENT'; Reason = 'x'; Folder = ''; Files = @() } }
$fake.Demo = $false; $fake.Data.Status = 'ACCEPTED'; $fake.Roles['gis_ims_v8_1_reference'] = [pscustomobject]@{ Status = 'FOUND'; IdStatus = 'CONFIRMED_BY_PROTOCOL'; Evidence = ''; Reason = 'x'; Folder = ''; Files = @([pscustomobject]@{ file = 'a'; path = 'C:\p\a.csv' }, [pscustomobject]@{ file = 'b'; path = 'C:\p\b.csv.gz' }) }
$plg = New-SiewProjectPlan -Find $fake -MinFreeGB 0.5; Check 'plan: the GIS role is passed as the identified files separated by | (never staged)' 'C:\p\a.csv|C:\p\b.csv.gz' $plg.Keys['gis_ims_v8_1_reference']
Check 'plan: step 27 runs with the GIS role, 28 needs no reference, 29 needs the archive-engine role, 30 needs no reference' 'True|True|False|True' ((@('PH_EXPOSURE_QA', 'PH_VIF_SUMMARY', 'PH_CALENDAR_SPLINE', 'PH_AGE65') | ForEach-Object { $n = $_; [string](@($plg.Steps | Where-Object { $_.Name -eq $n })[0].Runs) }) -join '|')
$fake.Demo = $true; $fake.Data.Status = 'DEMO_SYNTHETIC_INPUT'; $fake.Roles['gis_ims_v8_1_reference'] = [pscustomobject]@{ Status = 'ABSENT'; Reason = 'x'; Folder = ''; Files = @() }
$pl = New-SiewProjectPlan -Find $fake -MinFreeGB 0.5; Check 'plan: a project folder inside the package cannot start' $true ((-not $pl.CanRun) -and (@($pl.Stops | Where-Object { $_ -like '*inside the package folder*' }).Count -eq 1))

Write-Host '1. synthetic fixtures (a paper run as the historical accepted run, a core bundle, a Batch B bundle, acceptance records)'
$fx = Join-Path $W 'fixtures'; New-Item -ItemType Directory $fx -Force | Out-Null
$pr = Run-Script 'run_paper.ps1' @('-DataCsv', $syn, '-OutRoot', (Join-Path $fx 'runs')); Check 'fixture: synthetic paper run (exit 0)' 0 $pr.Code
$paperRun = (Get-ChildItem -LiteralPath (Join-Path $fx 'runs') -Directory | Select-Object -First 1).FullName
$core = Join-Path $fx 'core bundle'; New-Item -ItemType Directory $core -Force | Out-Null
Copy-Item -LiteralPath (Join-Path $paperRun 'cumulative_results.csv') -Destination (Join-Path $core 'cumulative_regression_grid.csv'); Copy-Item -LiteralPath (Join-Path $paperRun 'dlm_final_summary.csv') -Destination (Join-Path $core 'DLM_L01_L56_nine_representative_summary.csv')
$bb = Join-Path $fx 'batchB bundle'; New-Item -ItemType Directory $bb -Force | Out-Null; $i = 0; foreach ($n in 'batchB_firth_reference.csv', 'batchB_ordinal_reference.csv', 'batchB_transformations_reference.csv', 'batchB_nonlinear_reference.csv') { $i++; ('a,b' + "`r`n" + $i + ',' + ($i * 7)) | Set-Content -LiteralPath (Join-Path $bb $n) -Encoding ASCII }
$rec = Join-Path $fx 'validate_accepted_x'; New-Item -ItemType Directory $rec -Force | Out-Null
$rows = @('file,sha256'); foreach ($n in 'cumulative_regression_grid.csv', 'DLM_L01_L56_nine_representative_summary.csv') { $rows += ('{0},{1}' -f $n, (Get-FileHash -LiteralPath (Join-Path $core $n) -Algorithm SHA256).Hash) }; $rows | Set-Content -LiteralPath (Join-Path $rec 'core_reference_provenance.csv') -Encoding ASCII
$rec2 = Join-Path $fx 'batchb_records_x'; New-Item -ItemType Directory $rec2 -Force | Out-Null; ('file,sha256' + "`r`n" + 'batchB_firth_reference.csv,' + ('AB' * 32)) | Set-Content -LiteralPath (Join-Path $rec2 'batchB_reference_provenance.csv') -Encoding ASCII
$tam = Join-Path $fx 'tampered anom run'; New-Item -ItemType Directory $tam -Force | Out-Null; Copy-Item -Path (Join-Path $paperRun '*') -Destination $tam -Recurse; Add-Content -LiteralPath (Join-Path $tam 'cumulative_results.csv') 'x'

Write-Host '2. declaration helper (records names, sizes and SHA-256 only)'
$dOut = Join-Path $W 'declaration out'
$d = Run-Script 'scripts\declare_project_inputs.ps1' @('-NonInteractive', '-DataCsv', $syn, '-CoreReference', $core, '-BatchBReference', $bb, '-PaperRun', $paperRun, '-AnomFailedRun', $tam, '-AcceptanceRecordRuns', ($rec + ';' + $rec2), '-OutDir', $dOut)
Check 'declare: exit 0' 0 $d.Code
$dj = Join-Path $dOut 'project_inputs_declaration_v1.json'; Check 'declare: declaration written' $true (Test-Path -LiteralPath $dj)
Check 'declare: exactly three files written (no content copied)' 3 @(Get-ChildItem -LiteralPath $dOut -File).Count
$D = Get-Content -LiteralPath $dj -Raw -Encoding UTF8 | ConvertFrom-Json
Check 'declare: marking' $script:DeclarationMarking ([string]$D.status_of_this_document)
Check 'declare: synthetic data is NOT_THE_ACCEPTED_INPUT' 'NOT_THE_ACCEPTED_INPUT' $D.data_csv.status
$dc = @($D.bundles | Where-Object { $_.role -eq 'core_reference' })[0]; Check 'declare: core bundle CONFIRMED_AT_ACCEPTANCE' 'CONFIRMED_AT_ACCEPTANCE' $dc.status
Check 'declare: core evidence cites the record run and file' $true ([string]$dc.files[0].evidence -like '*validate_accepted_x\core_reference_provenance.csv*')
$db = @($D.bundles | Where-Object { $_.role -eq 'batch_b_reference' })[0]; Check 'declare: Batch B bundle UNCONFIRMED' 'UNCONFIRMED' $db.status
$fi = @($db.files | Where-Object { $_.file -eq 'batchB_firth_reference.csv' })[0]; Check 'declare: mismatch shows both hashes' $true ([string]$fi.evidence -like ('MISMATCH: declared ' + $fi.sha256 + '; acceptance record ' + ('AB' * 32) + '*'))
Check 'declare: file without record says so' $true ([string](@($db.files | Where-Object { $_.file -eq 'batchB_ordinal_reference.csv' })[0].evidence) -like 'no acceptance-time record*')
$rp = @($D.runs | Where-Object { $_.declared_run -eq 'paper_accepted_run' })[0]; Check 'declare: paper run MANIFEST_VERIFIED' 'MANIFEST_VERIFIED' $rp.status
Check 'declare: paper run manifest hash recorded' (Get-FileHash -LiteralPath (Join-Path $paperRun 'RUN_MANIFEST.csv') -Algorithm SHA256).Hash ([string]$rp.run_manifest.sha256)
Check 'declare: tampered run MANIFEST_NOT_VERIFIED' 'MANIFEST_NOT_VERIFIED' (@($D.runs | Where-Object { $_.declared_run -eq 'anom_failed_run' })[0].status)
Check 'declare: no participant code in the declaration or report' 0 @(Get-ChildItem -LiteralPath $dOut -File | Select-String -Pattern 'SYN0[0-9]' -List).Count
Check 'declare: refuses an output folder inside the package' 2 (Run-Script 'scripts\declare_project_inputs.ps1' @('-NonInteractive', '-DataCsv', $syn, '-OutDir', (Join-Path $repo 'siw_never_written'))).Code
Check 'declare: nothing written inside the package' $false (Test-Path -LiteralPath (Join-Path $repo 'siw_never_written'))
Check 'declare: never overwrites an existing declaration' 2 (Run-Script 'scripts\declare_project_inputs.ps1' @('-NonInteractive', '-DataCsv', $syn, '-OutDir', $dOut)).Code
# a bundle never confirms itself: its own folder given as the record folder
$self = Join-Path $W 'decl self'; $ds = Run-Script 'scripts\declare_project_inputs.ps1' @('-NonInteractive', '-CoreReference', $core, '-AcceptanceRecordRuns', $core, '-OutDir', $self)
Check 'declare: a bundle given as its own record stays UNCONFIRMED' 'UNCONFIRMED' (@((Get-Content -LiteralPath (Join-Path $self 'project_inputs_declaration_v1.json') -Raw | ConvertFrom-Json).bundles)[0].status)

Write-Host '3. import helper (into a scratch copy of the package skeleton, never the real package)'
$sk = Join-Path $W 'pkg skeleton'; foreach ($x in 'scripts', 'config', 'inst\extdata') { New-Item -ItemType Directory (Join-Path $sk $x) -Force | Out-Null }
foreach ($x in '_run_full.ps1', '_siecc_project.ps1', 'import_project_declaration.ps1') { Copy-Item -LiteralPath (Join-Path $repo ('scripts\' + $x)) -Destination (Join-Path $sk 'scripts') }
Copy-Item -LiteralPath (Join-Path $repo 'config\sieccwinter_config.json') -Destination (Join-Path $sk 'config')
$imp = { param($f, [switch]$Replace) $a = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', (Join-Path $sk 'scripts\import_project_declaration.ps1'), '-File', $f); if ($Replace) { $a += '-Replace' }; $o = & $PS @a 2>&1; return $LASTEXITCODE }
Check 'import: installs both copies (exit 0)' 0 (& $imp $dj)
Check 'import: copies byte-identical to the source' $true (((Get-FileHash (Join-Path $sk 'config\project_inputs_declaration_v1.json')).Hash -eq (Get-FileHash $dj).Hash) -and ((Get-FileHash (Join-Path $sk 'inst\extdata\project_inputs_declaration_v1.json')).Hash -eq (Get-FileHash $dj).Hash))
$alt = Join-Path $W 'alt decl'; New-Item -ItemType Directory $alt -Force | Out-Null; $altf = Join-Path $alt 'project_inputs_declaration_v1.json'; ((Get-Content -LiteralPath $dj -Raw) -replace '"created_local":\s*"[^"]*"', '"created_local": "2026-10-07 00:00:00"') | Set-Content -LiteralPath $altf -Encoding UTF8
Check 'import: a different installed declaration is not replaced without -Replace' 2 (& $imp $altf)
Check 'import: -Replace replaces it' 0 (& $imp $altf -Replace)
$bad = Join-Path $W 'bad decl'; New-Item -ItemType Directory $bad -Force | Out-Null; ((Get-Content -LiteralPath $dj -Raw) -replace [regex]::Escape($script:DeclarationMarking), 'something else') | Set-Content -LiteralPath (Join-Path $bad 'project_inputs_declaration_v1.json') -Encoding UTF8
Check 'import: wrong marking refused' 2 (& $imp (Join-Path $bad 'project_inputs_declaration_v1.json'))

# the synthetic declaration used by the launcher tests: the core bundle and the paper run are confirmed; Batch B is UNCONFIRMED
$DECL = $dj

Write-Host '3b. CONFIRMED_BY_FROZEN_SOURCE and AUTHOR_ACCEPTED_EVIDENCE (synthetic)'
$frz = Join-Path $W 'frozen source'; New-Item -ItemType Directory $frz -Force | Out-Null; Copy-Item -LiteralPath (Join-Path $core 'cumulative_regression_grid.csv') -Destination $frz
$frzBad = Join-Path $W 'frozen source differing'; New-Item -ItemType Directory $frzBad -Force | Out-Null; Copy-Item -LiteralPath (Join-Path $core 'cumulative_regression_grid.csv') -Destination $frzBad; Add-Content -LiteralPath (Join-Path $frzBad 'cumulative_regression_grid.csv') 'x'
$evText = 'synthetic test evidence: the validation summary of the earlier runs shows PASS for this file; author decision recorded'
$akey = 'core_reference/DLM_L01_L56_nine_representative_summary.csv=' + $evText
$fOut = Join-Path $W 'decl frozen author'
$fd = Run-Script 'scripts\declare_project_inputs.ps1' @('-NonInteractive', '-CoreReference', $core, '-FrozenSourceFolder', $frz, '-AuthorAccepted', $akey, '-AuthorName', 'Example Declarant', '-AuthorDate', '2026-10-11', '-OutDir', $fOut)
Check 'new statuses: declare exit 0' 0 $fd.Code
$FD = Get-Content -LiteralPath (Join-Path $fOut 'project_inputs_declaration_v1.json') -Raw -Encoding UTF8 | ConvertFrom-Json
$fb = @($FD.bundles | Where-Object { $_.role -eq 'core_reference' })[0]
$f1 = @($fb.files | Where-Object { $_.file -eq 'cumulative_regression_grid.csv' })[0]; $f2 = @($fb.files | Where-Object { $_.file -eq 'DLM_L01_L56_nine_representative_summary.csv' })[0]
Check 'frozen: file status CONFIRMED_BY_FROZEN_SOURCE' 'CONFIRMED_BY_FROZEN_SOURCE' $f1.status
Check 'frozen: evidence cites the source path and the hash' $true (([string]$f1.evidence).Contains((Join-Path $frz 'cumulative_regression_grid.csv')) -and ([string]$f1.evidence).Contains([string]$f1.sha256) -and $f1.source_path -eq (Join-Path $frz 'cumulative_regression_grid.csv') -and $f1.source_sha256 -eq $f1.sha256)
Check 'author: file status AUTHOR_ACCEPTED_EVIDENCE with evidence, author and date' $true ($f2.status -eq 'AUTHOR_ACCEPTED_EVIDENCE' -and $f2.evidence -eq $evText -and $f2.author -eq 'Example Declarant' -and $f2.author_date -eq '2026-10-11')
Check 'new statuses: the bundle takes its weakest file status' 'AUTHOR_ACCEPTED_EVIDENCE' $fb.status
Check 'new statuses: report lists the statuses and the author' $true $(& { $rc = (Get-Content -LiteralPath (Join-Path $fOut 'DECLARATION_REPORT.csv') -Raw); $rc.Contains('CONFIRMED_BY_FROZEN_SOURCE') -and $rc.Contains('AUTHOR_ACCEPTED_EVIDENCE') -and $rc.Contains('[author: Example Declarant, 2026-10-11]') })
$fo2 = Join-Path $W 'decl frozen only'; $null = Run-Script 'scripts\declare_project_inputs.ps1' @('-NonInteractive', '-CoreReference', $core, '-FrozenSourceFolder', $frz, '-OutDir', $fo2)
$fb2 = @((Get-Content -LiteralPath (Join-Path $fo2 'project_inputs_declaration_v1.json') -Raw | ConvertFrom-Json).bundles | Where-Object { $_.role -eq 'core_reference' })[0]
Check 'frozen only: the DLM summary without any record keeps the bundle UNCONFIRMED' 'UNCONFIRMED' $fb2.status
$fo3 = Join-Path $W 'decl frozen mismatch'; $null = Run-Script 'scripts\declare_project_inputs.ps1' @('-NonInteractive', '-CoreReference', $core, '-FrozenSourceFolder', $frzBad, '-AuthorAccepted', ('core_reference/cumulative_regression_grid.csv=' + $evText), '-AuthorName', 'Example Declarant', '-AuthorDate', '2026-10-11', '-OutDir', $fo3)
$fm = @(@((Get-Content -LiteralPath (Join-Path $fo3 'project_inputs_declaration_v1.json') -Raw | ConvertFrom-Json).bundles | Where-Object { $_.role -eq 'core_reference' })[0].files | Where-Object { $_.file -eq 'cumulative_regression_grid.csv' })[0]
Check 'frozen mismatch: UNCONFIRMED (author evidence does not override a contradicting source)' 'UNCONFIRMED' $fm.status
Check 'frozen mismatch: both hashes shown' $true ([string]$fm.evidence -like 'MISMATCH with the frozen source: declared * frozen file *')
$fo4 = Join-Path $W 'decl no evidence'; $r = Run-Script 'scripts\declare_project_inputs.ps1' @('-NonInteractive', '-CoreReference', $core, '-AuthorAccepted', 'core_reference/DLM_L01_L56_nine_representative_summary.csv=', '-AuthorName', 'Example Declarant', '-AuthorDate', '2026-10-11', '-OutDir', $fo4)
Check 'author: missing evidence text refused (exit 2, nothing written)' $true ($r.Code -eq 2 -and -not (Test-Path -LiteralPath $fo4))
$fo5 = Join-Path $W 'decl no author'; $r = Run-Script 'scripts\declare_project_inputs.ps1' @('-NonInteractive', '-CoreReference', $core, '-AuthorAccepted', $akey, '-AuthorDate', '2026-10-11', '-OutDir', $fo5)
Check 'author: missing author refused (exit 2, nothing written)' $true ($r.Code -eq 2 -and -not (Test-Path -LiteralPath $fo5))
$fo6 = Join-Path $W 'decl no date'; $r = Run-Script 'scripts\declare_project_inputs.ps1' @('-NonInteractive', '-CoreReference', $core, '-AuthorAccepted', $akey, '-AuthorName', 'Example Declarant', '-OutDir', $fo6)
Check 'author: missing date refused (exit 2, nothing written)' $true ($r.Code -eq 2 -and -not (Test-Path -LiteralPath $fo6))
# an edited declaration that drops the author, or whose frozen-source hash differs from the file hash, cannot be read by the launcher
$ed = Join-Path $W 'decl edited'; New-Item -ItemType Directory $ed -Force | Out-Null; $edf = Join-Path $ed 'project_inputs_declaration_v1.json'; $fj = Get-Content -LiteralPath (Join-Path $fOut 'project_inputs_declaration_v1.json') -Raw
$P6e = Join-Path $W 'P6e'; New-Item -ItemType Directory (Join-Path $P6e 'data') -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P6e 'data\d.csv')
($fj -replace '"author":\s*"Example Declarant"', '"author": ""') | Set-Content -LiteralPath $edf -Encoding UTF8
$r = Launch @('-ProjectDir', $P6e, '-Demo', '-PlanOnly', '-DeclarationFile', $edf); Check 'declaration without the author of an author-accepted file: refused' $true ($r.Code -eq 2 -and $r.Text -like '*AUTHOR_EVIDENCE_INCOMPLETE*')
($fj -replace '"source_sha256":\s*"[0-9A-F]{64}"', ('"source_sha256": "' + ('0' * 64) + '"')) | Set-Content -LiteralPath $edf -Encoding UTF8
$r = Launch @('-ProjectDir', $P6e, '-Demo', '-PlanOnly', '-DeclarationFile', $edf); Check 'frozen-source entry whose source hash differs: refused' $true ($r.Code -eq 2 -and $r.Text -like '*FROZEN_SOURCE_INCOMPLETE*')

Write-Host '4. launcher: create a project folder'
$c = Launch @('-CreateProject', (Join-Path $W 'new project')); Check 'create: exit 0' 0 $c.Code
Check 'create: data, references, outputs and README_FIRST.txt' 4 @(@('data', 'references', 'outputs', 'README_FIRST.txt') | Where-Object { Test-Path -LiteralPath (Join-Path (Join-Path $W 'new project') $_) }).Count

Write-Host '5. (i) data only -> ANALYSIS, end to end'
$P1 = Join-Path $W 'P1 data only'; Launch @('-CreateProject', $P1) | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P1 'data\my data.csv'); 'note' | Set-Content -LiteralPath (Join-Path $P1 'references\unrelated notes.txt')
$p = Launch @('-ProjectDir', $P1, '-Demo', '-PlanOnly'); Check '(i) plan: exit 0' 0 $p.Code; Check '(i) plan: mode ANALYSIS' $true ($p.Text -like '*MODE: ANALYSIS*')
Check '(i) plan: without a declaration the undeclared roles are NOT_DECLARED' $true ($p.Text -like '*core reference bundle:*NOT_DECLARED*')
Check '(i) plan: unknown file listed and ignored' $true ($p.Text -like '*UNKNOWN FILES (not identified; ignored): 1*unrelated notes.txt*')
Check '(i) plan: README_FIRST.txt is not reported as unknown' $false ($p.Text -like '*README_FIRST.txt (*')
Check '(i) plan: estimated time and free disk shown' $true ($p.Text -match 'FREE DISK: [0-9.]+ GB\. ESTIMATED TIME: about \d+-\d+ minutes')
$r1 = Launch @('-ProjectDir', $P1, '-Demo', '-Yes'); Check '(i) run: exit 3 (NOT_TESTED present, never PASS)' 3 $r1.Code
Check '(i) run: progress shown as step x of n' $true ($r1.Text -like '*Step 03 of 30: PAPER - finished: PASS (exit 0)*')
Check '(i) run: the post hoc steps ran on the synthetic demo input' 14 @($r1.Text -split "`n" | Where-Object { $_ -match 'Step (1[7-9]|2[0-9]|30) of 30: [A-Z_0-9]+ - finished: (PASS|REVIEW)' }).Count
$rd1 = @(Get-ChildItem -LiteralPath (Join-Path $P1 'outputs') -Directory); Check '(i) run: one run folder under outputs\' 1 $rd1.Count; $rd1 = $rd1[0].FullName
Check '(i) run: run folder named run_<stamp>' $true ((Split-Path -Leaf $rd1) -match '^run_\d{8}_\d{6}_[0-9a-f]{4}$')
foreach ($f in 'RESULTS_SUMMARY.html', 'FULL_RUN_SUMMARY.md', 'FULL_RUN_MANIFEST.csv', 'launcher\PROJECT_PLAN.txt', 'launcher\identification.csv', 'launcher\paths.json', 'launcher\LAUNCHER_MANIFEST.csv') { Check ("(i) run: $f") $true (Test-Path -LiteralPath (Join-Path $rd1 $f)) }
$st1 = Import-Csv -LiteralPath (Join-Path $rd1 'FULL_RUN_STATUS.csv'); Check '(i) run: PAPER PASS' 'PASS' ($st1 | Where-Object { $_.step -eq 'PAPER' }).class
Check '(i) run: validations NOT_TESTED with "input absent"' 11 @($st1 | Where-Object { $_.class -eq 'NOT_TESTED' -and $_.meaning -like 'input absent*' }).Count
$h1 = [System.IO.File]::ReadAllText((Join-Path $rd1 'RESULTS_SUMMARY.html'))
Check '(ix) html: no participant code' 0 ([regex]::Matches($h1, 'SYN0\d\d').Count); Check '(ix) html: no script, no internet link' 0 ([regex]::Matches($h1, '<script|https?://').Count)
Check '(ix) html: synthetic label and ANOM note' $true ($h1 -like '*Synthetic demonstration*' -and $h1 -like '*ANOM_VERIFICATION_STATUS*')
Check '(ix) html: every link resolves' 0 @([regex]::Matches($h1, 'href="([^"]+)"') | Where-Object { -not (Test-Path -LiteralPath (Join-Path $rd1 ([System.Net.WebUtility]::HtmlDecode($_.Groups[1].Value) -replace '/', '\'))) }).Count
Check '(ix) html: no row value of the data (first participant row absent)' $false ($h1.Contains(((Get-Content -LiteralPath $syn -TotalCount 2)[1]).Substring(0, 40)))
$mf = Import-Csv -LiteralPath (Join-Path $rd1 'FULL_RUN_MANIFEST.csv'); Check '(i) run: FULL_RUN_MANIFEST verifies' 0 @($mf | Where-Object { (Get-SiewSha256 (Join-Path $rd1 $_.file)) -ne $_.sha256 }).Count
Check '(i) run: nothing written outside the project folder (package unchanged so far)' 0 @((Pkg-Snapshot).GetEnumerator() | Where-Object { $before[$_.Key] -ne $_.Value }).Count

Write-Host '6. (ii)+(iii) data plus references, renamed and nested -> FULL_VALIDATION, end to end'
$P2 = Join-Path $W 'P2 full'; New-Item -ItemType Directory (Join-Path $P2 'data\deep\a b') -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P2 'data\deep\a b\renamed input.csv')
New-Item -ItemType Directory (Join-Path $P2 'references\x'), (Join-Path $P2 'references\y\z'), (Join-Path $P2 'references\bb') -Force | Out-Null
Copy-Item -LiteralPath (Join-Path $core 'cumulative_regression_grid.csv') -Destination (Join-Path $P2 'references\x\grid with another name.csv'); Copy-Item -LiteralPath (Join-Path $core 'DLM_L01_L56_nine_representative_summary.csv') -Destination (Join-Path $P2 'references\y\z\s.csv')
Copy-Item -Path (Join-Path $bb '*') -Destination (Join-Path $P2 'references\bb'); Copy-Item -LiteralPath $paperRun -Destination (Join-Path $P2 'references\old runs') -Recurse
$p = Launch @('-ProjectDir', $P2, '-Demo', '-PlanOnly', '-DeclarationFile', $DECL); Check '(ii) plan: exit 0' 0 $p.Code; Check '(ii) plan: mode FULL_VALIDATION' $true ($p.Text -like '*MODE: FULL_VALIDATION*')
Check '(iii) plan: renamed and nested core files found by content' $true ($p.Text -like '*core reference bundle:*FOUND*grid with another name.csv*s.csv*')
Check '(ii) plan: the historical accepted paper run found by its manifest hash' $true ($p.Text -like '*historical accepted paper run:*FOUND*old runs*')
Check '(ii) plan: UNCONFIRMED Batch B -> NOT_TESTED with that reason' $true ($p.Text -like '*06 BATCH_B*NOT_TESTED*UNCONFIRMED*')
Check '(ii) plan: a run folder''s copy of the core file does not make the role ambiguous' $false ($p.Text -like '*ambiguous*')
$r2 = Launch @('-ProjectDir', $P2, '-Demo', '-Yes', '-DeclarationFile', $DECL); Check '(ii) run: exit 3' 3 $r2.Code
$rd2 = (@(Get-ChildItem -LiteralPath (Join-Path $P2 'outputs') -Directory))[0].FullName; $st2 = Import-Csv -LiteralPath (Join-Path $rd2 'FULL_RUN_STATUS.csv')
Check '(ii) run: CORE_VALIDATION PASS against the staged, renamed references' 'PASS' ($st2 | Where-Object { $_.step -eq 'CORE_VALIDATION' }).class
Check '(ii) run: staged files carry the declared names and identical bytes' $true (((Get-FileHash (Join-Path $rd2 'staged_references\core_reference\cumulative_regression_grid.csv')).Hash -eq (Get-FileHash (Join-Path $core 'cumulative_regression_grid.csv')).Hash))
Check '(ii) run: ORDINAL_REFINED (needs only Batch B) NOT_TESTED because the declaration is UNCONFIRMED' $true (($st2 | Where-Object { $_.step -eq 'ORDINAL_REFINED' }).meaning -like 'input absent (batch_b_reference): UNCONFIRMED*')
Check '(ii) run: BATCH_B NOT_TESTED, naming its first absent input' $true (($st2 | Where-Object { $_.step -eq 'BATCH_B' }).meaning -like 'input absent (batch_a_reference): NOT_DECLARED*')
Check '(ii) run: the original project files are untouched' (Get-FileHash -LiteralPath (Join-Path $core 'cumulative_regression_grid.csv')).Hash (Get-FileHash -LiteralPath (Join-Path $P2 'references\x\grid with another name.csv')).Hash

Write-Host '6b. post hoc: nearest-station histories identified by the header line only (synthetic file)'
$P5 = Join-Path $W 'P5 nearest'; New-Item -ItemType Directory (Join-Path $P5 'data'), (Join-Path $P5 'references\near') -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P5 'data\d.csv')
@('participant_code,LOCAL_DATE,TD_MEAN,RH_MEAN,RAIN_TOTAL,RAIN_DAYS1', 'SYN001,2026-01-01,10,70,0,0') | Set-Content -LiteralPath (Join-Path $P5 'references\near\nearest histories.csv')
$p = Launch @('-ProjectDir', $P5, '-Demo', '-PlanOnly'); Check '(post hoc) plan: exit 0' 0 $p.Code
Check '(post hoc) plan: nearest-station histories FOUND by header' $true ($p.Text -like '*nearest-station histories (participant-level):*FOUND*nearest histories.csv*')
Check '(post hoc) plan: the nearest file is not an unknown file' $false ($p.Text -like '*UNKNOWN FILES*nearest histories.csv*')
Check '(post hoc) plan: post hoc steps will run in the demo' $true ($p.Text -like '*25 PH_H3_NESTED*will run*')
Copy-Item -LiteralPath (Join-Path $P5 'references\near\nearest histories.csv') -Destination (Join-Path $P5 'references\near\second.csv')
$p = Launch @('-ProjectDir', $P5, '-Demo', '-PlanOnly'); Check '(post hoc) two nearest files: refused (exit 2)' 2 $p.Code; Check '(post hoc) two nearest files: named as ambiguous' $true ($p.Text -like '*ambiguous*nearest-station histories: 2 files*')

Write-Host '6c. post hoc: declared nearest export variants identified by SHA-256 and all passed on (in-process; synthetic variants declared for this test only)'
Check '(post hoc) the protocol declares the two nearest export variants' 'jul19,jul25' ((@(Get-SiewNearestVariants $repo) | ForEach-Object { $_.id }) -join ',')
$P6 = Join-Path $W 'P6 variants'; New-Item -ItemType Directory (Join-Path $P6 'data'), (Join-Path $P6 'references\nearest') -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P6 'data\d.csv')
$va = Join-Path $P6 'references\nearest\export a.csv'; $vb = Join-Path $P6 'references\nearest\export b.csv'
@('FEATURE_ID,METHOD,JERUSALEM_LOCAL_DATE,TD_MEAN,RH_MEAN,RAIN_TOTAL,RAIN_DAYS1', 'SYN001,NEAREST,2026-01-01,10,70,0,0') | Set-Content -LiteralPath $va
@('FEATURE_ID,METHOD,LOCAL_DATE,TIME_ZONE_NAME,TD_MEAN,RH_MEAN,RAIN_TOTAL,RAIN_DAYS1', 'SYN001,NEAREST,2026-01-01,Asia/Jerusalem,10,70,0,0') | Set-Content -LiteralPath $vb
Check '(post hoc) header alias: FEATURE_ID and JERUSALEM_LOCAL_DATE are accepted' $true (Test-SiewNearestHeader $va)
$script:NearestVariantsOverride = @(foreach ($x in @(@('va', $va), @('vb', $vb))) { [pscustomobject]@{ id = $x[0]; bytes = (Get-Item -LiteralPath $x[1]).Length; sha256 = (Get-FileHash -LiteralPath $x[1] -Algorithm SHA256).Hash } })
$fd6 = Find-SiewProjectInputs -ProjectDir $P6 -Repo $repo -Declaration $null -Demo; $pl6 = New-SiewProjectPlan -Find $fd6 -MinFreeGB 0.5; $r6 = $fd6.Roles['nearest_station_histories']
Check '(post hoc) variants: both FOUND by SHA-256, in declared order' 'FOUND|CONFIRMED_BY_PROTOCOL|va,vb' ('{0}|{1}|{2}' -f $r6.Status, $r6.IdStatus, ((@($r6.Files) | ForEach-Object { $_.variant }) -join ','))
Check '(post hoc) variants: the run_all key lists both files separated by |' ($va + '|' + $vb) $pl6.Keys['nearest_station_histories']
Check '(post hoc) variants: the plan names each declared export' $true (((Format-SiewPlan $pl6) -join "`n") -like '*export a.csv `[declared export va`], *export b.csv `[declared export vb`]*')
Copy-Item -LiteralPath $va -Destination (Join-Path $P6 'references\nearest\copy of a.csv'); $fd6b = Find-SiewProjectInputs -ProjectDir $P6 -Repo $repo -Declaration $null -Demo
Check '(post hoc) variants: two copies of one declared export are ambiguous' 'AMBIGUOUS|1' ('{0}|{1}' -f $fd6b.Roles['nearest_station_histories'].Status, @($fd6b.Ambiguities | Where-Object { $_ -like 'nearest-station export va: 2 identical copies*' }).Count)
$script:NearestVariantsOverride = $null
$rp6 = Join-Path $P6 'paths.json'; $rr6 = Join-Path $P6 'runs'; New-Item -ItemType Directory (Join-Path $rr6 'run1') -Force | Out-Null
@{ data_csv = (Join-Path $P6 'data\d.csv'); out_root = $rr6; project_mode = $true; demo = $true; run_dir = (Join-Path $rr6 'run1'); nearest_station_histories = @($va, $vb) } | ConvertTo-Json | Set-Content -LiteralPath $rp6 -Encoding UTF8
$q6 = $null; $e6 = ''; try { $q6 = Get-SiewRunPaths -RepoDir $repo -PathsFile $rp6 } catch { $e6 = $_.Exception.Message }
Check '(post hoc) run_all paths: a JSON array of nearest files becomes one |-separated list' ($va + '|' + $vb) $(if ($null -ne $q6) { $q6.nearest_station_histories } else { $e6 })
@{ data_csv = (Join-Path $P6 'data\d.csv'); out_root = $rr6; project_mode = $true; demo = $true; run_dir = (Join-Path $rr6 'run1'); nearest_station_histories = ($va + '|' + (Join-Path $P6 'references\nearest\missing.csv')) } | ConvertTo-Json | Set-Content -LiteralPath $rp6 -Encoding UTF8
$e6 = ''; try { $null = Get-SiewRunPaths -RepoDir $repo -PathsFile $rp6 } catch { $e6 = $_.Exception.Message }; Check '(post hoc) run_all paths: a missing nearest file is refused' $true ($e6 -like '*nearest_station_histories (file not found)*')

Write-Host '7. (iv) a duplicate candidate stops the run'
$P3 = Join-Path $W 'P3 dup'; New-Item -ItemType Directory (Join-Path $P3 'data'), (Join-Path $P3 'references\one'), (Join-Path $P3 'references\two') -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P3 'data\d.csv')
Copy-Item -LiteralPath (Join-Path $core 'cumulative_regression_grid.csv') -Destination (Join-Path $P3 'references\one\a.csv'); Copy-Item -LiteralPath (Join-Path $core 'cumulative_regression_grid.csv') -Destination (Join-Path $P3 'references\two\b.csv'); Copy-Item -LiteralPath (Join-Path $core 'DLM_L01_L56_nine_representative_summary.csv') -Destination (Join-Path $P3 'references\one\c.csv')
$p = Launch @('-ProjectDir', $P3, '-Demo', '-PlanOnly', '-DeclarationFile', $DECL); Check '(iv) plan: exit 2' 2 $p.Code; Check '(iv) plan: both candidates named' $true ($p.Text -like '*ambiguous*core_reference / cumulative_regression_grid.csv: 2 identical candidates*one\a.csv*two\b.csv*')
$r = Launch @('-ProjectDir', $P3, '-Demo', '-Yes', '-DeclarationFile', $DECL); Check '(iv) run: refused (exit 2)' 2 $r.Code; Check '(iv) run: no run folder created' 0 @(Get-ChildItem -LiteralPath (Join-Path $P3 'outputs') -Directory -ErrorAction SilentlyContinue).Count
$P3b = Join-Path $W 'P3b two data'; New-Item -ItemType Directory (Join-Path $P3b 'data\a'), (Join-Path $P3b 'data\b') -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P3b 'data\a\x.csv'); Copy-Item -LiteralPath $syn -Destination (Join-Path $P3b 'data\b\y.csv')
Check '(iv) two copies of the data CSV: refused' 2 (Launch @('-ProjectDir', $P3b, '-Demo', '-PlanOnly')).Code

Write-Host '8. (v) a data file that is not the accepted input'
$P4 = Join-Path $W 'P4 wrong data'; New-Item -ItemType Directory (Join-Path $P4 'data'), (Join-Path $P4 'references') -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P4 'data\study.csv'); Copy-Item -LiteralPath (Join-Path $core '*') -Destination (Join-Path $P4 'references')
$p = Launch @('-ProjectDir', $P4, '-PlanOnly', '-DeclarationFile', $DECL); Check '(v) plan: exit 0 (analysis may run)' 0 $p.Code
Check '(v) plan: labelled NOT_THE_ACCEPTED_INPUT, mode ANALYSIS' $true ($p.Text -like '*study data CSV:*NOT_THE_ACCEPTED_INPUT*' -and $p.Text -like '*MODE: ANALYSIS*')
Check '(v) plan: core reference present but CORE_VALIDATION NOT_TESTED' $true ($p.Text -like '*04 CORE_VALIDATION*NOT_TESTED*NOT_THE_ACCEPTED_INPUT*')
$r4 = Launch @('-ProjectDir', $P4, '-Yes', '-DeclarationFile', $DECL); Check '(v) run: exit 4 (INPUT_HASH FAIL: not the accepted input)' 4 $r4.Code
$rd4 = (@(Get-ChildItem -LiteralPath (Join-Path $P4 'outputs') -Directory))[0].FullName; $st4 = Import-Csv -LiteralPath (Join-Path $rd4 'FULL_RUN_STATUS.csv')
Check '(v) run: INPUT_HASH FAIL labelled' $true (($st4 | Where-Object { $_.step -eq 'INPUT_HASH' }).meaning -like 'NOT_THE_ACCEPTED_INPUT*')
Check '(v) run: every validation and post hoc step NOT_TESTED' 26 @($st4 | Where-Object { $_.class -eq 'NOT_TESTED' }).Count
Check '(v) run: label file in the run folder' $true (Test-Path -LiteralPath (Join-Path $rd4 'NOT_THE_ACCEPTED_INPUT.txt'))
Check '(v) run: label file in the paper step folder' $true (Test-Path -LiteralPath (Join-Path (Join-Path $rd4 ($st4 | Where-Object { $_.step -eq 'PAPER' }).run_folder) 'NOT_THE_ACCEPTED_INPUT.txt'))
Check '(v) html: NOT_THE_ACCEPTED_INPUT banner' $true ([System.IO.File]::ReadAllText((Join-Path $rd4 'RESULTS_SUMMARY.html')) -like '*NOT_THE_ACCEPTED_INPUT.</b> The data file is not the accepted canonical CSV*')

Write-Host '9. (vii) missing R, low disk, other refusals'
$r = Launch @('-ProjectDir', $P1, '-PlanOnly', '-RscriptPath', 'C:\no such folder\Rscript.exe'); Check '(vii) missing R: exit 2' 2 $r.Code; Check '(vii) missing R: says what to do' $true ($r.Text -like '*R 4.4.3 was not found*install R version 4.4.3*')
$r = Launch @('-ProjectDir', $P1, '-Demo', '-PlanOnly', '-MinFreeGB', '1000000'); Check '(vii) low disk: plan refuses (exit 2)' 2 $r.Code; Check '(vii) low disk: message' $true ($r.Text -like '*GB free on the project drive; at least 1000000 GB is required*')
$n = @(Get-ChildItem -LiteralPath (Join-Path $P1 'outputs') -Directory).Count; $r = Launch @('-ProjectDir', $P1, '-Demo', '-Yes', '-MinFreeGB', '1000000'); Check '(vii) low disk: run refused' 2 $r.Code; Check '(vii) low disk: no new run folder' $n @(Get-ChildItem -LiteralPath (Join-Path $P1 'outputs') -Directory).Count
$P5 = Join-Path $W 'P5 root data'; New-Item -ItemType Directory $P5 -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P5 'd.csv'); $r = Launch @('-ProjectDir', $P5, '-Demo', '-PlanOnly'); Check 'data CSV directly in the project folder: refused' $true ($r.Code -eq 2 -and $r.Text -like '*move it into a subfolder such as data*')
$P6 = Join-Path $W 'P6 empty'; New-Item -ItemType Directory $P6 -Force | Out-Null; $r = Launch @('-ProjectDir', $P6, '-Demo', '-PlanOnly'); Check 'no data CSV: refused with a message' $true ($r.Code -eq 2 -and $r.Text -like '*no data CSV found*')
$r = Launch @('-ProjectDir', (Join-Path $W 'does not exist'), '-PlanOnly'); Check 'missing project folder: exit 2' 2 $r.Code
$P7 = Join-Path $W ('P7 ' + ('long folder name ' * 6)).Trim(); New-Item -ItemType Directory (Join-Path $P7 'data') -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P7 'data\d.csv'); $r = Launch @('-ProjectDir', $P7, '-Demo', '-PlanOnly'); Check 'project path over 110 characters: refused with a plain message' $true ($r.Code -eq 2 -and $r.Text -like '*Windows limits a full file path to 260 characters*C:\SIECC_project*')
$ph = Join-Path $W 'decl placeholder'; New-Item -ItemType Directory $ph -Force | Out-Null; $phf = Join-Path $ph 'project_inputs_declaration_v1.json'; ((Get-Content -LiteralPath $dj -Raw) -replace '"config_sha256":\s*"[^"]*"', '"config_sha256": "<fill me>"') | Set-Content -LiteralPath $phf -Encoding UTF8
$r = Launch @('-ProjectDir', $P1, '-Demo', '-PlanOnly', '-DeclarationFile', $phf); Check 'declaration with a placeholder: refused' $true ($r.Code -eq 2 -and $r.Text -like '*PLACEHOLDERS*')
Check 'R entry point exists and uses the same launcher' $true ((Get-Content -LiteralPath (Join-Path $repo 'R\project.R') -Raw) -like '*scripts", "siecc_launcher.ps1"*')
Check 'RUN_SIECC.cmd calls the launcher with -STA' $true ((Get-Content -LiteralPath (Join-Path $repo 'RUN_SIECC.cmd') -Raw) -like '*-STA -File "%~dp0scripts\siecc_launcher.ps1"*')

Write-Host '9b. launcher with a CONFIRMED_BY_FROZEN_SOURCE + AUTHOR_ACCEPTED_EVIDENCE core bundle, end to end'
$P8 = Join-Path $W 'P8 evidence'; New-Item -ItemType Directory (Join-Path $P8 'data'), (Join-Path $P8 'references\x') -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P8 'data\d.csv')
Copy-Item -LiteralPath (Join-Path $core 'cumulative_regression_grid.csv') -Destination (Join-Path $P8 'references\x\g.csv'); Copy-Item -LiteralPath (Join-Path $core 'DLM_L01_L56_nine_representative_summary.csv') -Destination (Join-Path $P8 'references\x\s.csv')
$DECL8 = Join-Path $fOut 'project_inputs_declaration_v1.json'
$p = Launch @('-ProjectDir', $P8, '-Demo', '-PlanOnly', '-DeclarationFile', $DECL8); Check '(9b) plan: exit 0' 0 $p.Code
Check '(9b) plan: core role FOUND with its identification status' $true ($p.Text -like '*core reference bundle:*FOUND*' -or $p.Text -like '*core reference bundle:*[AUTHOR_ACCEPTED_EVIDENCE]*')
Check '(9b) plan: status and evidence shown next to the step' $true ($p.Text -like '*04 CORE_VALIDATION*will run*input identification - core_reference: AUTHOR_ACCEPTED_EVIDENCE - *' -and $p.Text -like ('*' + $evText + '*(author: Example Declarant, 2026-10-11)*') -and $p.Text -like '*CONFIRMED_BY_FROZEN_SOURCE*byte-identical*')
$r8 = Launch @('-ProjectDir', $P8, '-Demo', '-Yes', '-DeclarationFile', $DECL8); Check '(9b) run: exit 3' 3 $r8.Code
$rd8 = (@(Get-ChildItem -LiteralPath (Join-Path $P8 'outputs') -Directory))[0].FullName; $st8 = Import-Csv -LiteralPath (Join-Path $rd8 'FULL_RUN_STATUS.csv')
Check '(9b) run: CORE_VALIDATION ran and PASSed (both statuses let the step run)' 'PASS' ($st8 | Where-Object { $_.step -eq 'CORE_VALIDATION' }).class
$h8 = [System.IO.File]::ReadAllText((Join-Path $rd8 'RESULTS_SUMMARY.html')); $m8 = [System.IO.File]::ReadAllText((Join-Path $rd8 'FULL_RUN_SUMMARY.md'))
Check '(9b) html: statuses and evidence shown, next to the step and in the table' $true ($h8 -like '*Input identification (evidence behind the references)*' -and $h8 -like '*AUTHOR_ACCEPTED_EVIDENCE*' -and $h8 -like ('*' + [System.Net.WebUtility]::HtmlEncode($evText) + '*') -and $h8 -like '*CORE_VALIDATION*Input identification:*')
Check '(9b) run summary: identification section with evidence and the steps using it' $true ($m8 -like '*## Input identification (post hoc declaration)*' -and $m8 -like ('*' + $evText + '*') -and $m8 -like '*CORE_VALIDATION*')
Check '(9b) html: still no script or internet link' 0 ([regex]::Matches($h8, '<script|https?://').Count)
# an UNCONFIRMED core bundle still means NOT_TESTED
$p = Launch @('-ProjectDir', $P8, '-Demo', '-PlanOnly', '-DeclarationFile', (Join-Path $fo2 'project_inputs_declaration_v1.json')); Check '(9b) UNCONFIRMED core bundle: CORE_VALIDATION NOT_TESTED' $true ($p.Text -like '*04 CORE_VALIDATION*NOT_TESTED*UNCONFIRMED*')

Write-Host '9c. Windows PowerShell 5.1 with the 5.1 module path, whatever shell the user starts from'
foreach ($cmdName in 'RUN_SIECC.cmd', 'DECLARE_SIECC_INPUTS.cmd') {
    $ct = Get-Content -LiteralPath (Join-Path $repo $cmdName) -Raw
    Check "(9c) $cmdName resets PSModulePath to the 5.1 defaults" $true ($ct -match 'set "PSModulePath=%USERPROFILE%\\Documents\\WindowsPowerShell\\Modules;%ProgramFiles%\\WindowsPowerShell\\Modules;%SystemRoot%\\System32\\WindowsPowerShell\\v1\.0\\Modules"')
    Check "(9c) $cmdName starts powershell.exe 5.1 by absolute path (never a PATH lookup, never pwsh)" $true ($ct -like '*"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" -NoProfile*' -and $ct -notmatch '(?im)^\s*(powershell|pwsh)(\.exe)?\s')
}
Check '(9c) every child PowerShell of the package is the 5.1 executable' $true (($script:PowerShellExe -like '*\WindowsPowerShell\v1.0\powershell.exe') -and ((Get-SiewWinPSModulePath) -like '*WindowsPowerShell\Modules*System32\WindowsPowerShell\v1.0\Modules'))
$P9 = Join-Path $W 'P9 cmd'; New-Item -ItemType Directory (Join-Path $P9 'data') -Force | Out-Null; Copy-Item -LiteralPath $syn -Destination (Join-Path $P9 'data\d.csv')
function Run-Foreign { param([string]$Exe, [string[]]$A) $ErrorActionPreference = 'Continue'; $saved = $env:PSModulePath; $env:PSModulePath = 'C:\no such module folder'; try { $raw = & $Exe @A 2>&1; $c = $LASTEXITCODE } finally { $env:PSModulePath = $saved }; return [pscustomobject]@{ Code = $c; Text = ((@($raw) | ForEach-Object { [string]$_ }) -join "`n") } }   # a foreign PSModulePath, as a shell that exports one would hand over
function Run-ForeignCmd { param([string]$CmdLine) $saved = $env:PSModulePath; $env:PSModulePath = 'C:\no such module folder'; $of = Join-Path $W 'foreign_cmd_out.txt'; try { $p = Start-Process -FilePath 'cmd.exe' -ArgumentList $CmdLine -NoNewWindow -Wait -PassThru -RedirectStandardOutput $of } finally { $env:PSModulePath = $saved }; return [pscustomobject]@{ Code = $p.ExitCode; Text = [System.IO.File]::ReadAllText($of) } }
$co = Run-ForeignCmd ('/d /c ""' + (Join-Path $repo 'RUN_SIECC.cmd') + '" -ProjectDir "' + $P9 + '" -Demo -PlanOnly -NoDeclaration <nul"')
Check '(9c) RUN_SIECC.cmd with a foreign PSModulePath: plan shown, exit 0' $true ($co.Code -eq 0 -and $co.Text -like '*MODE: ANALYSIS*')
$pw = Get-Command pwsh -ErrorAction SilentlyContinue
if ($null -eq $pw) { Write-Host '  NOTE: pwsh (PowerShell 7) is not installed on this machine; the PowerShell 7 launch tests were NOT run.'; Check '(9c) pwsh present (otherwise the PowerShell 7 tests are reported as not run)' 'not run: pwsh absent' 'not run: pwsh absent' }
else {
    $pv7 = (& $pw.Source -NoProfile -Command '$PSVersionTable.PSVersion.ToString()'); Write-Host ('  pwsh found: ' + $pw.Source + ' (version ' + $pv7 + ')')
    $po = Run-Foreign $pw.Source @('-NoProfile', '-File', (Join-Path $repo 'scripts\siecc_launcher.ps1'), '-ProjectDir', $P9, '-Demo', '-PlanOnly', '-NoDeclaration')
    Check '(9c) from pwsh: launcher plan-only (non-interactive demo) exit 0' $true ($po.Code -eq 0 -and $po.Text -like '*MODE: ANALYSIS*')
    $pr9 = Run-Foreign $pw.Source @('-NoProfile', '-File', (Join-Path $repo 'scripts\siecc_launcher.ps1'), '-ProjectDir', $P9, '-Demo', '-Yes', '-NoOpen', '-NoDeclaration')
    Check '(9c) from pwsh: non-interactive demo run exit 3 (PASS steps plus NOT_TESTED, same as from powershell.exe)' 3 $pr9.Code
    Check '(9c) from pwsh: progress shown and the run ran PAPER to PASS' $true ($pr9.Text -like '*Step 03 of 30: PAPER - finished: PASS (exit 0)*')
    $rd9 = @(Get-ChildItem -LiteralPath (Join-Path $P9 'outputs') -Directory); Check '(9c) from pwsh: one run folder with RESULTS_SUMMARY.html and a verifying manifest' $true ($rd9.Count -eq 1 -and (Test-Path -LiteralPath (Join-Path $rd9[0].FullName 'RESULTS_SUMMARY.html')) -and @(Import-Csv -LiteralPath (Join-Path $rd9[0].FullName 'FULL_RUN_MANIFEST.csv') | Where-Object { (Get-SiewSha256 (Join-Path $rd9[0].FullName $_.file)) -ne $_.sha256 }).Count -eq 0)
    $pdo = Join-Path $W 'decl from pwsh'
    $pd = Run-Foreign $pw.Source @('-NoProfile', '-File', (Join-Path $repo 'scripts\declare_project_inputs.ps1'), '-NonInteractive', '-CoreReference', $core, '-AcceptanceRecordRuns', ($rec + ';' + $rec2), '-OutDir', $pdo)
    Check '(9c) from pwsh: declaration helper exit 0 and the core bundle CONFIRMED_AT_ACCEPTANCE' $true ($pd.Code -eq 0 -and (@((Get-Content -LiteralPath (Join-Path $pdo 'project_inputs_declaration_v1.json') -Raw | ConvertFrom-Json).bundles)[0].status -eq 'CONFIRMED_AT_ACCEPTANCE'))
}

Write-Host '10. the package was never written'
Check 'package files unchanged by every test above' 0 @((Pkg-Snapshot).GetEnumerator() | Where-Object { $before[$_.Key] -ne $_.Value }).Count
Check 'no file added to the package' $before.Count (Pkg-Snapshot).Count

$n = $results.Count; $np = @($results | Where-Object { $_.pass }).Count; Write-Host ("`nTESTS: {0}; PASSED: {1}; FAILED: {2}" -f $n, $np, ($n - $np))
if ($ResultCsv -ne '') { $results | Export-Csv -NoTypeInformation -Encoding UTF8 -Path $ResultCsv }
if (-not $Keep -and $n -eq $np) { Remove-Tree $WorkDir }
exit $(if ($n -eq $np) { 0 } else { 1 })
