<#
.SYNOPSIS
  No-code launcher (started by RUN_SIECC.cmd, or by siw_run_project() in R). Choose a project folder; the program identifies every input by content,
  shows a plan, asks "Start? (Y/N)", runs run_all.ps1 in project mode with a progress display, writes RESULTS_SUMMARY.html and opens the run folder.
.NOTES
  Non-interactive use (tests, R): -ProjectDir <folder> [-Yes] [-PlanOnly] [-NoOpen] [-Demo]. -CreateProject <folder> makes an empty project folder.
  -DeclarationFile, -NoDeclaration (ignore the installed declaration) and -RscriptPath are for the synthetic tests only. Exit codes: those of run_all.ps1 after a run (0 all PASS; 3 REVIEW/NOT_TESTED/DIAGNOSTIC;
  4 a FAIL; 1 incomplete); 0 plan shown (-PlanOnly) or project created or "N" answered; 2 refused before any analysis (R missing, folder, plan problem, disk).
#>
[CmdletBinding()]
param(
    [string]$ProjectDir = '',
    [string]$CreateProject = '',
    [switch]$Yes,
    [switch]$PlanOnly,
    [switch]$NoOpen,
    [switch]$Demo,
    [string]$DeclarationFile = '',
    [switch]$NoDeclaration,
    [string]$RscriptPath = '',
    [double]$MinFreeGB = 2
)
$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent $PSScriptRoot
. (Join-Path $PSScriptRoot '_siecc_project.ps1')
if ($PSVersionTable.PSVersion.Major -ne 5) { exit (Invoke-SiewRelaunchInWinPS -ScriptPath $PSCommandPath -Bound $PSBoundParameters) }   # started from PowerShell 7 or another host: run in Windows PowerShell 5.1
try { [Console]::OutputEncoding = New-Object System.Text.UTF8Encoding($false) } catch { }

function Select-SiewFolder {
    param([string]$Title)
    try {
        Add-Type -AssemblyName System.Windows.Forms
        $d = New-Object System.Windows.Forms.FolderBrowserDialog; $d.Description = $Title; $d.ShowNewFolderButton = $true
        if ($d.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) { return $d.SelectedPath }
        Write-Host 'No folder was chosen in the window.'
    } catch { Write-Host 'The folder window could not be opened here.' }
    $p = Read-Host 'Paste the folder path here and press Enter (or just press Enter to cancel)'
    return ([string]$p).Trim().Trim('"')
}

Write-Host ''
Write-Host 'SIECC winter package - run on a project folder' -ForegroundColor Cyan
Write-Host '----------------------------------------------'
$rs = $(if ($RscriptPath -ne '') { $RscriptPath } else { $script:RscriptExe })
if (-not (Test-Path -LiteralPath $rs -PathType Leaf)) {
    Write-Host ''
    Write-Host ('R 4.4.3 was not found (expected: {0}).' -f $rs) -ForegroundColor Red
    Write-Host 'What to do: install R version 4.4.3 for Windows from the CRAN website (choose the "old releases" page, version 4.4.3),'
    Write-Host 'keep the default installation folder C:\Program Files\R\R-4.4.3, then double-click RUN_SIECC.cmd again.'
    Write-Host 'Nothing was run.'
    exit 2
}

if ($CreateProject -eq '' -and $ProjectDir -eq '' -and -not $Yes -and -not $PlanOnly) {
    Write-Host 'What would you like to do?'
    Write-Host '  1 = run on a project folder (the usual choice)'
    Write-Host '  2 = create a new, empty project folder'
    $c = ([string](Read-Host 'Type 1 or 2 and press Enter')).Trim()
    if ($c -eq '2') { $CreateProject = Select-SiewFolder 'Choose (or create) the folder that will become the new project folder' }
    else { $ProjectDir = Select-SiewFolder 'Choose the project folder (it holds your data CSV and, optionally, reference folders)' }
    if ($CreateProject -eq '' -and $ProjectDir -eq '') { Write-Host 'Nothing was chosen. Nothing was run.'; exit 2 }
}

if ($CreateProject -ne '') {
    try { $pp = New-SiewProjectFolders -Path $CreateProject -Repo $repo } catch { Write-Host ('Could not create the project folder: ' + $_.Exception.Message) -ForegroundColor Red; exit 2 }
    Write-Host ('Project folder ready: {0}' -f $pp) -ForegroundColor Green
    Write-Host 'Copy the data CSV into data\ and, optionally, reference or run folders into references\. Then run RUN_SIECC.cmd again and choose this folder.'
    Write-Host 'README_FIRST.txt in that folder explains what goes where.'
    if (-not $NoOpen) { try { Start-Process -FilePath 'explorer.exe' -ArgumentList ('"' + $pp + '"') } catch { } }
    exit 0
}

if (-not (Test-Path -LiteralPath $ProjectDir -PathType Container)) { Write-Host ('The project folder was not found: {0}' -f $ProjectDir) -ForegroundColor Red; Write-Host 'Nothing was run.'; exit 2 }
$script:RscriptExe = $rs
try { $decl = $(if ($NoDeclaration) { $null } else { Read-SiewDeclaration -Repo $repo -Path $DeclarationFile }) } catch { Write-Host ('The input-identification declaration cannot be used: ' + $_.Exception.Message) -ForegroundColor Red; Write-Host 'Nothing was run.'; exit 2 }

Write-Host ''
Write-Host 'Looking at the files in the project folder (identification by content; this can take a minute) ...'
$find = Find-SiewProjectInputs -ProjectDir $ProjectDir -Repo $repo -Declaration $decl -Demo:$Demo
$plan = New-SiewProjectPlan -Find $find -MinFreeGB $MinFreeGB
$planText = Format-SiewPlan $plan
Write-Host ''
foreach ($l in $planText) { $col = $(if ($l -like '*NOT_TESTED*' -or $l -like 'UNKNOWN*') { 'Yellow' } elseif ($l -like 'CANNOT START*' -or $l -like '  - *' -or $l -like '*NOT_THE_ACCEPTED_INPUT*') { 'Red' } else { 'Gray' }); Write-Host $l -ForegroundColor $col }
Write-Host ''
if ($PlanOnly) { if ($plan.CanRun) { Write-Host 'PLAN_ONLY: the run can start.'; exit 0 } else { Write-Host 'PLAN_ONLY: the run cannot start (see above).'; exit 2 } }
if (-not $plan.CanRun) { Write-Host 'The run cannot start. Fix the points above and start again. Nothing was run.' -ForegroundColor Red; exit 2 }
if (-not $Yes) {
    $a = ([string](Read-Host 'Start? (Y/N)')).Trim()
    if ($a -notmatch '^(?i)y(es)?$') { Write-Host 'Nothing was run.'; exit 0 }
}

$run = New-SiewProjectRun -Plan $plan -PlanText ($planText -join "`r`n")
Write-Host ('Run folder: {0}' -f $run.RunDir)
Write-Host 'Running. Progress (step x of n, name, elapsed):'
$code = Invoke-SiewProjectEngine -Repo $repo -Run $run -MinFreeGB $MinFreeGB
$html = Write-SiewResultsHtml -RunDir $run.RunDir -PlanText ($planText -join "`r`n")
Write-SiewLauncherManifest -RunDir $run.RunDir
$sum = Join-Path $run.RunDir 'FULL_RUN_SUMMARY.md'
$oc = ''; if (Test-Path -LiteralPath $sum) { $m = [regex]::Match([System.IO.File]::ReadAllText($sum), '\*\*Outcome: ([A-Z_]+)\*\*'); if ($m.Success) { $oc = $m.Groups[1].Value } }
Write-Host ''
Write-Host ('Finished. Outcome: {0} (exit code {1}).' -f $oc, $code) -ForegroundColor $(switch ($code) { 0 { 'Green' } 3 { 'Yellow' } default { 'Red' } })
Write-Host ('Open this first: {0}' -f $html)
if ($code -eq 4) { Write-Host 'A FAIL is listed: read the FAIL rows in the results page. (ANOM_ALT is governed by ANOM_VERIFICATION_STATUS_V2 since protocol v2; see the ANOM note.)' }
if (-not $NoOpen) { try { Start-Process -FilePath 'explorer.exe' -ArgumentList ('"' + $run.RunDir + '"') } catch { } }
exit $code
