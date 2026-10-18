<#
.SYNOPSIS
  Converts the confidential SPSS (.sav) file to the canonical private CSV used by run_paper.ps1 and run_validate.ps1.
  Writes ONLY inside -OutDir, which must be outside this repository. Never prints participant records. Never installs packages.
.EXAMPLE
  .\prepare_private_csv.ps1 -SavPath "C:\path\to\study.sav" -OutDir "D:\private\siecc_conversion_01"
.NOTES
  Exit codes: 0 success; 1 conversion or validation failure; 2 invalid arguments, missing software, or refusal to overwrite / unsafe output folder.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string]$SavPath,
    [Parameter(Mandatory = $true)][string]$OutDir,
    [switch]$Overwrite,                 # replace an existing conversion in OutDir (otherwise refused)
    [string]$ExpectedSavSha256 = '',    # optional: stop if the SAV hash differs
    [int]$ExpectedRows = 0              # optional: stop if the row count differs (0 = not checked)
)
$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
$rscript = 'C:\Program Files\R\R-4.4.3\bin\Rscript.exe'
$script = Join-Path $repo 'scripts\prepare_private_csv.R'

if (-not (Test-Path -LiteralPath $rscript -PathType Leaf)) { Write-Host "FAIL: R was not found at $rscript" -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $script -PathType Leaf)) { Write-Host 'FAIL: scripts\prepare_private_csv.R was not found next to this script.' -ForegroundColor Red; exit 2 }
if (-not (Test-Path -LiteralPath $SavPath -PathType Leaf)) { Write-Host 'FAIL: the SAV file was not found.' -ForegroundColor Red; exit 2 }
if ([System.IO.Path]::GetExtension($SavPath).ToLower() -ne '.sav') { Write-Host 'FAIL: the input must be an SPSS .sav file.' -ForegroundColor Red; exit 2 }
if ([string]::IsNullOrWhiteSpace($OutDir)) { Write-Host 'FAIL: -OutDir is required.' -ForegroundColor Red; exit 2 }
if ($ExpectedSavSha256 -ne '' -and $ExpectedSavSha256 -notmatch '^[0-9A-Fa-f]{64}$') { Write-Host 'FAIL: -ExpectedSavSha256 must be 64 hexadecimal characters.' -ForegroundColor Red; exit 2 }
if ($ExpectedRows -lt 0) { Write-Host 'FAIL: -ExpectedRows must not be negative.' -ForegroundColor Red; exit 2 }

# packages are only checked, never installed
$probe = & $rscript --vanilla -e "cat(all(vapply(c('foreign','jsonlite','MASS'), requireNamespace, logical(1), quietly = TRUE)))"
if ($LASTEXITCODE -ne 0 -or $probe -ne 'TRUE') {
    Write-Host 'FAIL: a required R package (foreign, jsonlite or MASS) is not installed. Nothing was installed; install it yourself, then re-run.' -ForegroundColor Red
    exit 2
}

$sav = (Resolve-Path -LiteralPath $SavPath).Path
$out = [System.IO.Path]::GetFullPath($OutDir)
$argList = @('--sav', $sav, '--out-dir', $out)
if ($Overwrite) { $argList += '--overwrite' }
if ($ExpectedSavSha256 -ne '') { $argList += @('--expected-sav-sha256', $ExpectedSavSha256) }
if ($ExpectedRows -gt 0) { $argList += @('--expected-rows', [string]$ExpectedRows) }

Write-Host 'Converting (aggregate messages only; no participant records are printed) ...'
& $rscript $script @argList
$rc = $LASTEXITCODE
Write-Host ''
if ($rc -eq 0) {
    Write-Host 'PASS - private canonical CSV created.' -ForegroundColor Green
    Write-Host ("Private output folder: {0}" -f $out)
    Write-Host 'Read CONVERSION_SUMMARY.md and CONVERSION_AUDIT.csv there before running run_paper.ps1.'
    exit 0
} elseif ($rc -eq 2) {
    Write-Host 'FAIL - invalid arguments, missing software, unsafe output folder, or refusal to overwrite (nothing was converted).' -ForegroundColor Red
    exit 2
} else {
    Write-Host 'FAIL - the conversion or its validation failed. No canonical CSV was written; see CONVERSION_AUDIT.csv in the output folder.' -ForegroundColor Red
    exit 1
}
