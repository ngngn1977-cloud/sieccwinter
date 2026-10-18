# Private study-data run guide (Windows PowerShell)

Release 1.0.0. You run everything yourself. The confidential SPSS file is never uploaded, copied into this repository, or shared outside the study team.
R 4.4.3 at `C:\Program Files\R\R-4.4.3\bin\Rscript.exe` and the R packages `foreign`, `jsonlite`, `MASS` must already be installed (nothing is installed by these scripts).
If PowerShell blocks scripts: `Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass` (this window only).

## Step 0 - choose a private folder

Choose a folder **outside this repository** and outside the folder that holds the SAV file, preferably on a drive that is not synchronised to a cloud service (<private>, Dropbox ...). The converter refuses a folder inside the repository or inside the SAV's folder. It writes only inside the folder you name.

## Step 1 - convert the SAV to the canonical private CSV

```powershell
cd <package folder>
.\prepare_private_csv.ps1 -SavPath "<path to the .sav file>" -OutDir "<private folder>\conversion_01" -ExpectedSavSha256 <64-hex hash of the audited file> -ExpectedRows <expected row count>
```

* `-ExpectedSavSha256` and `-ExpectedRows` are optional safety stops. The audited file (Phase 1) has 55 rows; its SHA-256 is recorded in `WINTER_PHASE1\DATA_CODING_AUDIT.csv` (section A1).
* Runtime: well under a minute. Only aggregate messages are printed; no participant record is ever shown.
* Exit code 0 = converted; 1 = conversion/validation failed (no CSV is written; read `CONVERSION_AUDIT.csv`); 2 = invalid arguments, missing software, unsafe folder, or an existing conversion (add `-Overwrite` to replace it deliberately).

### What is written (all inside the private folder)

| File | Content | Contains participant-level values? |
|---|---|---|
| `siecc_winter_canonical.csv` | 574 columns (22 scalar, 472 daily L01-L59, 80 cumulative), rows in SAV order | **yes - keep private** |
| `canonical_data_dictionary.json` | canonical name, SAV source variable, type, role, allowed values, counts | no |
| `CONVERSION_AUDIT.csv` | aggregate checks (rows, missing counts, coding validation, round trip ...) | no |
| `CONVERSION_MANIFEST.csv` | basenames, sizes and hashes of the SAV (input) and the outputs | no |
| `SESSION_INFO.txt` | R and package versions | no |
| `CONVERSION_SUMMARY.md` | short PASS/FAIL summary | no |

### What the conversion does (legacy-compatible import)

`foreign::read.spss(use.value.labels = FALSE, to.data.frame = TRUE, use.missings = TRUE, trim.factor.names = FALSE, trim_values = FALSE, reencode = NA)`, exactly as the original analysis did:
value labels are not applied (numeric codes are kept); system-missing and user-defined missing values become empty cells; rows stay in file order; nothing is sorted, filtered or imputed;
`participant_code` is trimmed of blanks; `PedestLengthm` (metres) is kept in metres (the package converts to km); the aliases `Subjective_helth`, `Howwouldyourateyourmemory`, `Comparedwiththepreviousseasonsummerwinterisyourmemor`, `Subjective_wellbieng`, `outdoor_mobility`, `economic_vulnerabilityref.elsepoorfairavaerage1` are renamed to the canonical names.
`Jewish`, `Muslim`, `Christian` and `Memory_assess` are taken directly from the SAV (no fallback to the memory rating). Dates, addresses, coordinates and every unused variable are **not** exported.
The audit reports in aggregate how many cells were user-defined missing codes. Coding problems (out-of-range codes, unexpected levels, duplicate identifiers, `int_dummy` vs `integration_score`) fail the conversion; they are never corrected.

## Step 2 - review the conversion (before any analysis)

Open `CONVERSION_SUMMARY.md` and `CONVERSION_AUDIT.csv` in the private folder. Expected for the audited file: 55 rows, 574 columns, 0 FAIL. Any WARN (e.g. the rainfall anomaly check V14) is a reported property of the data, not corrected.
You may share the summary, audit, dictionary, manifest and session information for review (no participant values). **The canonical CSV and the SAV are never shared outside the study team.**

## Step 3 - later: paper and validation runs (only after you have reviewed Step 2)

```powershell
.\run_paper.ps1 -DataCsv "<private folder>\conversion_01\siecc_winter_canonical.csv" -OutRoot "<private folder>\runs"
.\run_validate.ps1 -DataCsv "<private folder>\conversion_01\siecc_winter_canonical.csv" -ReferenceDir "<folder of aggregate reference tables>" -OutRoot "<private folder>\runs"
```

Use `-OutRoot` so run outputs also stay in the private folder. See `MANUAL_RUN_GUIDE.md` and `MANUAL_ACCEPTANCE_CHECKLIST.md`. Nothing is accepted or locked until you have reviewed those runs.

## Rules

* Never commit the SAV, the canonical CSV, the private folder or any credential; `.gitignore` and `.Rbuildignore` exclude them, and the converter refuses to write inside the repository.
* The synthetic demo (`run_demo.ps1`) tests the workflow only and cannot reproduce article estimates.
* Numerical agreement with the manuscript is not part of the conversion and is not checked here.
