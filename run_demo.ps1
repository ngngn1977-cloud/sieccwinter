<#
.SYNOPSIS
  Runs the sieccwinter demonstration on the shipped fully synthetic data. No arguments needed.
.EXAMPLE
  .\run_demo.ps1
#>
[CmdletBinding()]
param(
    [string]$OutRoot = ''   # optional parent folder for the run folder (default: <repository>\runs)
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
. (Join-Path $repo 'scripts\_run_common.ps1')
$code = Invoke-SiewRun -RepoDir $repo -Mode 'demo' -ModeArgs @() -OutRoot $OutRoot
exit $code
