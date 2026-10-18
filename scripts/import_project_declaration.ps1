<#
.SYNOPSIS
  Installs a reviewed project_inputs_declaration_v1.json into config\ and inst\extdata\ (byte-identical copies). Checks version, marking and that no placeholder
  remains. Refuses to replace a different existing declaration unless -Replace is given. Changes no protocol, plan, configuration or tolerance; the package
  source fingerprint changes (a new file in its scope), the configuration hash does not.
.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\import_project_declaration.ps1 -File "<folder>\project_inputs_declaration_v1.json"
#>
[CmdletBinding()]
param([Parameter(Mandatory = $true)][string]$File, [switch]$Replace)
$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent $PSScriptRoot
. (Join-Path $PSScriptRoot '_siecc_project.ps1')
try { $d = Read-SiewDeclaration -Repo $repo -Path $File } catch { Write-Host ('Not imported: ' + $_.Exception.Message) -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetFileName($File) -ne $script:DeclarationFileName) { Write-Host ('Not imported: the file must be named ' + $script:DeclarationFileName) -ForegroundColor Red; exit 2 }
$bytes = [System.IO.File]::ReadAllBytes((ConvertTo-SiewFullPath $File)); $src = Get-SiewSha256 (ConvertTo-SiewFullPath $File)
$targets = @((Join-Path $repo ('config\' + $script:DeclarationFileName)), (Join-Path $repo ('inst\extdata\' + $script:DeclarationFileName)))
foreach ($t in $targets) { if ((Test-Path -LiteralPath $t) -and (Get-SiewSha256 $t) -ne $src -and -not $Replace) { Write-Host ('Not imported: a different declaration is already installed at ' + $t + ' (use -Replace to replace it deliberately).') -ForegroundColor Red; exit 2 } }
foreach ($t in $targets) { [System.IO.File]::WriteAllBytes($t, $bytes); if ((Get-SiewSha256 $t) -ne $src) { Write-Host ('Copy differs: ' + $t) -ForegroundColor Red; exit 1 } }
Write-Host ('Imported (SHA-256 {0}) into config\ and inst\extdata\.' -f $src) -ForegroundColor Green
Write-Host 'The package source fingerprint changes (a new file in its scope); the configuration hash and every protocol are unchanged.'
exit 0
