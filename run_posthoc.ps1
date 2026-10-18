<#
.SYNOPSIS
  Post hoc steps of config/posthoc_sensitivity_protocol_v2.json (version 1 of 13 Oct 2026 plus steps 27-30 of 14 Oct 2026, after the 30 July 2026 family lock), each labelled
  SENSITIVITY, POST_HOC or RECONSTRUCTION: exposure_export, deletion, l59_bootstrap, no_april, source_agreement, exposure_residuals, profile_likelihood, early_late, h3_nested,
  nearest_station, exposure_qa, vif_summary, calendar_spline, age65.
  None of them changes an estimator, rule, seed, tolerance, family or accepted result, and none can change q90, q9 or any accepted status.
  Creates a NEW run folder; the data file and the reference folders are only read; no result is altered to fit a reference.
.EXAMPLE
  .\run_posthoc.ps1 -Item h3_nested -DataCsv "C:\path\to\siecc_winter_canonical.csv" -CoreReferenceDir "C:\path\to\reference_bundle_01" -OutRoot "C:\path\to\runs"
.NOTES
  Reference folders by role: -V14ReferenceDir (posthoc_v14_reference: deletion, l59_bootstrap), -ArchiveReferenceDir (archive_engine_reference: exposure_export, no_april, source_agreement,
  exposure_residuals, profile_likelihood, early_late, calendar_spline), -CoreReferenceDir (core_reference: h3_nested), -GisReference (gis_ims_v8_1_reference: exposure_qa;
  participant- and station-level, local only: a folder holding the declared files, or the files separated by | - matched by SHA-256), -NearestHistoriesCsv (nearest_station_histories: nearest_station; participant-level, local only: one CSV, or several separated by | - each declared
  export variant is run separately and the variant that reproduces SI Tables S9/S10 is named).
  Every reference file is checked against the SHA-256 and size declared in the protocol (core_reference: in the input-identification declaration). -SyntheticDemo is accepted only for the shipped synthetic CSV.
  Exit codes: 0 all PASS; 3 REVIEW (a REVIEW or NOT_TESTED status, no FAIL); 4 a status FAILED; 1 execution failure; 2 invalid arguments or R missing.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$Item,
    [Parameter(Mandatory = $true)][string]$DataCsv,
    [string]$OutRoot = '', [string]$V14ReferenceDir = '', [string]$ArchiveReferenceDir = '', [string]$CoreReferenceDir = '', [string]$NearestHistoriesCsv = '', [string]$GisReference = '', [switch]$SyntheticDemo
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_posthoc.ps1')
if (-not (Test-Path -LiteralPath $DataCsv -PathType Leaf)) { Write-Host 'FAIL: the data file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($DataCsv).ToLower() -ne '.csv') { Write-Host 'FAIL: the data file must be a .csv.' -ForegroundColor Red; exit 2 }
$dirs = @{}
foreach ($kv in @(@('V14ReferenceDir', $V14ReferenceDir), @('ArchiveReferenceDir', $ArchiveReferenceDir), @('CoreReferenceDir', $CoreReferenceDir))) {
    if ($kv[1] -ne '') { if (-not (Test-Path -LiteralPath $kv[1] -PathType Container)) { Write-Host ('FAIL: -{0} was not found.' -f $kv[0]) -ForegroundColor Red; exit 2 }; $dirs[$kv[0]] = (Resolve-Path -LiteralPath $kv[1]).Path } else { $dirs[$kv[0]] = '' }
}
$nearList = @(); foreach ($nf in @($NearestHistoriesCsv -split '\|' | Where-Object { $_ -ne '' })) { if (-not (Test-Path -LiteralPath $nf -PathType Leaf) -or [System.IO.Path]::GetExtension($nf).ToLower() -ne '.csv') { Write-Host 'FAIL: -NearestHistoriesCsv must be one existing .csv file, or several separated by |.' -ForegroundColor Red; exit 2 }; $nearList += (Resolve-Path -LiteralPath $nf).Path }
$near = $nearList -join '|'
$gisList = @(); foreach ($gf in @($GisReference -split '\|' | Where-Object { $_ -ne '' })) { if (-not (Test-Path -LiteralPath $gf)) { Write-Host 'FAIL: -GisReference must be an existing folder, or existing files separated by |.' -ForegroundColor Red; exit 2 }; $gisList += (Resolve-Path -LiteralPath $gf).Path }
$gis = $gisList -join '|'
if ($OutRoot -ne '') { $outFull = [System.IO.Path]::GetFullPath($OutRoot); foreach ($d in @($dirs.Values) + @($nearList | ForEach-Object { Split-Path -Parent $_ }) + @($gisList | ForEach-Object { if (Test-Path -LiteralPath $_ -PathType Container) { $_ } else { Split-Path -Parent $_ } })) { if ($d -ne '' -and $outFull.TrimEnd('\').StartsWith($d.TrimEnd('\') + '\', [System.StringComparison]::OrdinalIgnoreCase)) { Write-Host 'FAIL: -OutRoot must not be inside a reference or input folder.' -ForegroundColor Red; exit 2 } } }
$code = Invoke-SiewPosthoc -RepoDir $repo -Item $Item -DataCsv (Resolve-Path -LiteralPath $DataCsv).Path -OutRoot $OutRoot -V14ReferenceDir $dirs['V14ReferenceDir'] -ArchiveReferenceDir $dirs['ArchiveReferenceDir'] -CoreReferenceDir $dirs['CoreReferenceDir'] -NearestHistoriesCsv $near -GisReference $gis -SyntheticDemo:$SyntheticDemo
exit $code
