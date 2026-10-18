<#
.SYNOPSIS
  PRIVATE VALIDATION KIT (local only). Restores the full source tree on top of an extracted PUBLIC package: the full declarations (SHA-256 values, sizes and locations of the accepted
  input and of the frozen references), the input declaration, the private-run wrappers and the launcher (RUN_SIECC.cmd), the documentation and the test constants.
  Every overlay file is checked against PRIVATE_KIT_MANIFEST.csv before it is copied; inst\extdata\PUBLIC_BUILD.json is removed; the source fingerprint is then compared with the
  fingerprint of the development tree the kit was built from (KIT_FINGERPRINT.txt).
.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File .\install_private_kit.ps1 -Target "C:\SIECC_full\sieccwinter"
.NOTES
  The target is the folder created by extracting the public tarball (it contains DESCRIPTION and R\). Nothing outside the target is written. Exit 0 = installed and the
  fingerprint matches; 4 = installed but the fingerprint differs (reported); 2 = refused (a kit file does not match its manifest, or the target is not a public package).
#>
[CmdletBinding()]
param([Parameter(Mandatory = $true)][string]$Target, [string]$RscriptPath = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe')
$ErrorActionPreference = 'Stop'
$kit = $PSScriptRoot; $ov = Join-Path $kit 'overlay'
if (-not (Test-Path -LiteralPath (Join-Path $Target 'DESCRIPTION')) -or -not (Test-Path -LiteralPath (Join-Path $Target 'R') -PathType Container)) { Write-Host 'FAIL: -Target is not an extracted package folder (DESCRIPTION and R\ expected).' -ForegroundColor Red; exit 2 }
$Target = (Resolve-Path -LiteralPath $Target).Path
$sha = { param($p) $h = [System.Security.Cryptography.SHA256]::Create(); $s = [System.IO.File]::OpenRead($p); try { ([System.BitConverter]::ToString($h.ComputeHash($s)) -replace '-', '') } finally { $s.Dispose(); $h.Dispose() } }
$m = Import-Csv -LiteralPath (Join-Path $kit 'PRIVATE_KIT_MANIFEST.csv'); $bad = @()
foreach ($r in $m) { $f = Join-Path $ov ($r.file -replace '/', '\'); if (-not (Test-Path -LiteralPath $f -PathType Leaf) -or (& $sha $f) -ne $r.sha256) { $bad += $r.file } }
if ($bad.Count) { Write-Host ('FAIL: {0} kit file(s) do not match PRIVATE_KIT_MANIFEST.csv; nothing was installed: {1}' -f $bad.Count, (($bad | Select-Object -First 5) -join ', ')) -ForegroundColor Red; exit 2 }
foreach ($r in $m) { $src = Join-Path $ov ($r.file -replace '/', '\'); $dst = Join-Path $Target ($r.file -replace '/', '\'); New-Item -ItemType Directory -Path (Split-Path -Parent $dst) -Force | Out-Null; Copy-Item -LiteralPath $src -Destination $dst -Force }
$pb = Join-Path $Target 'inst\extdata\PUBLIC_BUILD.json'; if (Test-Path -LiteralPath $pb) { Remove-Item -LiteralPath $pb -Force }
Write-Host ('Installed {0} kit files into {1}; PUBLIC_BUILD.json removed.' -f $m.Count, $Target)
$want = ((Get-Content -LiteralPath (Join-Path $kit 'KIT_FINGERPRINT.txt')) | Where-Object { $_ -like 'DEV_SOURCE_FINGERPRINT=*' }) -replace '^DEV_SOURCE_FINGERPRINT=', ''
if (-not (Test-Path -LiteralPath $RscriptPath)) { Write-Host 'NOTE: R not found; the source fingerprint was not checked.' -ForegroundColor Yellow; exit 4 }
$fp = (& $RscriptPath -e ("source('{0}/scripts/load_package.R'); siw_dev_load('{0}'); cat(siw_source_manifest()`$fingerprint)" -f ($Target -replace '\\', '/')) 2>$null | Select-Object -Last 1)
Write-Host ('Source fingerprint: {0}' -f $fp); Write-Host ('Kit fingerprint:    {0}' -f $want)
if ($fp -eq $want) { Write-Host 'PASS: the restored tree has the fingerprint of the development tree the kit was built from. RUN_SIECC.cmd can be used from this folder.' -ForegroundColor Green; exit 0 }
Write-Host 'FAIL: the restored tree does not have the expected source fingerprint (see KIT_FINGERPRINT.txt).' -ForegroundColor Red; exit 4
