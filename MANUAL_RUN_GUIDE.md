# Manual run guide (Windows PowerShell)

Release 1.0.0. You run these commands yourself; nothing here needs RStudio or a particular working directory.
R 4.4.3 must be at `C:\Program Files\R\R-4.4.3\bin\Rscript.exe`. No package installation is needed.

If PowerShell refuses to run the scripts, allow them for this window only (changes nothing permanent):

```powershell
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
```

## 00. No-code run on a project folder (recommended)

**A. Author, once: the input-identification declaration.** The core, Batch A, Batch B, Batch C and DLM reference bundles and the historical accepted runs have no hashes in the protocols. So they are recognised only after a declaration is installed.

1. Double-click `DECLARE_SIECC_INPUTS.cmd`.
2. Pick, or paste the path of, each of these. Cancel skips an item.
   * the data CSV;
   * every reference bundle folder;
   * the failed ANOM run `anomhist_20261004_141846_28bd`;
   * the historical accepted paper run `paper_20260930_195007_26ca`;
   * the historical accepted Batch B run `batchb_20261001_134405_0f79`.
3. Add the folders that hold acceptance-time records: the earlier validation runs, for example `validate_…`, `batcha_…`, `batchb_…`, `batchc_…`, `dlmvalidate_…`.
4. Choose an output folder outside the package.

The helper records **only file names, byte sizes and SHA-256**. For runs it records the `RUN_MANIFEST.csv` hash after verifying every manifest-listed file. It never prints or copies file contents. Each bundle file is labelled:
* `CONFIRMED_AT_ACCEPTANCE`: the hash equals an acceptance-time record, and the run and record file are cited. A bundle folder never confirms itself.
* `UNCONFIRMED`: no record, or a different recorded hash. Both hashes are shown.

The four protocol-declared bundles are labelled `CONFIRMED_BY_PROTOCOL` or `UNCONFIRMED`.

Review `DECLARATION_REPORT.txt`, then install the declaration (single line):
```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\import_project_declaration.ps1 -File "<folder>\project_inputs_declaration_v1.json"
```
This copies it byte-identically into `config\` and `inst\extdata\`. The record is marked "post hoc input-identification record, 7 Oct 2026; does not alter any protocol, plan, tolerance or accepted result". The configuration hash stays `5A8C6054…`; the source fingerprint changes because a new file enters its scope.

UNCONFIRMED, undeclared or absent roles make their steps **NOT_TESTED** with the reason, never PASS.

**B. Every user: the run.**
1. Double-click `RUN_SIECC.cmd`, type `2`, and create a project folder with a short path (at most 110 characters, for example `C:\SIECC_project`).
2. Copy the data CSV into `data\`, and optionally the bundle and run folders into `references\`.
3. Double-click `RUN_SIECC.cmd` again, type `1`, and choose the folder.
4. Read the plan and type `Y`.

What happens:
* **Identification.** It is by content only. Files are skipped by size before hashing, and a name never identifies a file. Two candidates for one role stop the run with both paths named. Unknown files are listed and ignored.
* **Mode.** Data only gives ANALYSIS: the paper analysis and the coded record, with validations NOT_TESTED. Data plus references gives FULL VALIDATION: every step whose inputs were found.
* **Non-accepted data.** A data file with the canonical columns but not the accepted SHA-256 runs as ANALYSIS. INPUT_HASH is FAIL with `NOT_THE_ACCEPTED_INPUT`, every output folder gets a `NOT_THE_ACCEPTED_INPUT.txt` label, and every validation is NOT_TESTED.
* **Run folder.** Everything goes to `<project>\outputs\run_<date>_<time>_<id>\`. Identified reference files are staged there as byte-identical copies under their declared names (`staged_references\`). Run folders are used in place. Then `run_all.ps1` runs in project mode with a progress display. At the end the folder opens; read `RESULTS_SUMMARY.html` first.

Non-interactive form (single line): `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\siecc_launcher.ps1 -ProjectDir "<project folder>" -Yes`. Add `-PlanOnly` to see only the plan. From R: `siw_run_project()`.

## 0. The controlled full run (one command)

`run_all.ps1` runs everything in order, each step in a fresh PowerShell and R process:
1. environment check (R 4.4.3, MASS, jsonlite, configuration SHA-256, source fingerprint);
2. input SHA-256 check (accepted canonical CSV only);
3. paper analysis;
4. core validation;
5. Batch A;
6. Batch B;
7. refined ordinal;
8. Batch C;
9. DLM V2;
10. DLM V2.1;
11. archived DLM;
12. cold burden;
13. daily layer;
14. ANOM/ALT;
15. ANOM sample-SD diagnostic;
16. coded model record v2 (new file `SIECC_coded_model_record_v2.csv`; the legacy 37,348-row `SIECC_All_Models_with_Codes.csv` is never read or overwritten);
17. summary;
18. manifest, written last.

It calls the scripts in sections 1–3 and the batch scripts unchanged.

1. Copy `manual_run_paths.template.json` into your private folder as `manual_run_paths.json`. Fill in every path. `out_root` must be outside this package and outside every data or reference folder. Keep it short: Windows long paths are disabled on this machine.
   * **Recommended:** point `dlm_accepted_run`, `batch_b_legacy_run` and `batch_c_accepted_run` at the historical accepted run folders named in `DLM_VALIDATION_V2_GUIDE.md`, `ORDINAL_REFINEMENT_RUN_GUIDE.md` and `BATCH_C_RUN_GUIDE.md`.
     * If `dlm_accepted_run` is empty, DLM V2 compares with this run's own paper output. That self-reference proves only that the steps are wired together; it is not a comparison with the accepted results.
     * If either of the other two keys is empty, that optional comparison is not made.
     * The console and `FULL_RUN_SUMMARY.md` flag every empty key.
   * A value still containing a `<...>` marker is refused before anything runs.
2. Run (single line):

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\run_all.ps1 -PathsFile "..\private_validation\manual_run_paths.json"
```

* Runtime: about 30–60 minutes (sum of the per-batch guide estimates; the real data have not been timed end to end). Disk: well under 1 GB of aggregate outputs. The run refuses to start below 2 GB free (`-MinFreeGB`), and it stops cleanly before any step if space falls below that.
* **Expected outcome.**
  * Since the post hoc ANOM protocol v2 (author decision, 12 Oct 2026), ANOM_ALT is governed by `ANOM_VERIFICATION_STATUS_V2`. Each gaussian row's implied SD is compared with the directly measured SD of its own complete-case sample, at tolerance 1e-9.
  * `ANOM_VERIFICATION_STATUS` (the v1 composition) is still shown, superseded, for the record. The earlier runs anomhist_20261004_141846_28bd and run_20261012_114559_4737 keep their FAIL records.
  * Several steps are expected to be **REVIEW** (wrapper exit 3: documented review items), and the diagnostic step is **DIAGNOSTIC**. So exit code 3 is expected when no step FAILs; any FAIL gives 4.
  * Open `FULL_RUN_SUMMARY.md` first.
* Interrupted or a step failed to execute (exit 1)? Fix the cause and add `-ResumeRunDir "<out_root>\fullrun_..."`. Completed steps are reused after their manifests are re-verified. Failed executions and dependency-NOT_TESTED steps are rerun; earlier attempts are kept. A completed run (one that has `FULL_RUN_MANIFEST.csv`) is never modified.
* **Monitoring.** The console and `<out_root>\fullrun_...\run_progress.txt` show one line per event. Each line gives the time, the step number out of 16 and its name, START / END / RESTORED / NOT_TESTED, the class and exit code, the seconds taken, and the free disk space. Watch it from a second PowerShell window with `Get-Content "<out_root>\fullrun_...\run_progress.txt" -Wait`. The file holds aggregate information only.
* `-RainfallSensitivity` adds the separately labelled 0 mm rainfall sensitivity to the paper step (opt-in only).
* Synthetic rehearsal of the same command: `powershell -NoProfile -ExecutionPolicy Bypass -File .\run_all.ps1 -Demo` (about 2 minutes, exit 0).

## 1. Synthetic demo (start here)

```powershell
cd <package folder>
.\run_demo.ps1
```

* No arguments; uses only the shipped fully synthetic CSV.
* Runtime: about 15 to 30 seconds (13 s on the development machine).
* Output appears in a new folder `sieccwinter\runs\demo_<date>_<time>_<hex>`. An earlier run is never overwritten.
* Success: the last lines are `PASS - demo run completed in ... s` and `Output folder: ...`; the exit code is 0 (`$LASTEXITCODE`).
  Open `RUN_SUMMARY.md` first. The demo is expected to show **1 WARN** in input validation (the deliberately planted synthetic rainfall edge case). See `EXPECTED_OUTPUTS.md`.

## 2. Paper workflow on the study CSV (later, authorised access only)

```powershell
cd <package folder>
.\run_paper.ps1 -DataCsv "C:\path\to\study_data.csv"
```

* The CSV must be the canonical study CSV (see `README.md`); `.sav` files are refused. Convert privately first.
* Runtime: expect roughly 1 to 10 minutes; the real sample size has not been timed.
* Optional: `-Archived` (heavy legacy L01-L59 branch), `-RainfallSensitivity`, `-OutRoot "D:\somewhere"` to place the run folder outside the repository.
* Use `-OutRoot` to keep outputs of confidential runs outside any folder that is version-controlled or synchronised. `runs\` is git-ignored regardless.

## 3. Validation against reference outputs

```powershell
cd <package folder>
.\run_validate.ps1 -DataCsv "C:\path\to\study_data.csv" -ReferenceDir "C:\path\to\reference_outputs"
```

* `reference_outputs` is a folder of frozen **aggregate** tables. File names looked up (see `config\sieccwinter_config.json`, `validation.references`):
  cumulative: `cumulative_regression_grid.csv` or `MASTER_CUMULATIVE_ALL_MODELS_1200.csv` or `MASTER_CUMULATIVE_PRIMARY_90.csv`;
  final DLM: `DLM_L01_L56_nine_representative_summary.csv`;
  archived DLM: `fixed_DLM_summary.csv` or `MASTER_DLM_ALL_MODELS_120.csv` or `MASTER_DLM_ALL_M2_24.csv`.
* A reference with no matching file is listed as `NOT_TESTED` (a warning), not silently skipped. The run FAILS if any comparison differs beyond tolerance or nothing could be compared.
* Every discrepancy is reported in `validation_detail.csv`; results are never changed to fit a reference.

## What a run folder contains

`RUN_SUMMARY.md`, `RUN_MANIFEST.csv` (SHA-256 of every file), `SESSION_INFO.txt`, `INPUT_VALIDATION.csv`, `VALIDATION_STATUS.csv`, `RUNTIME.csv`,
`run_metadata.json` (input hash, configuration hash, seed, versions), `run_console.log`, plus the result tables (aggregate only). Details: `EXPECTED_OUTPUTS.md`.

## How failures are reported

* The window ends with a red `FAIL - ...` line, the first `ERROR` lines of the R log, and the run folder; the exit code is non-zero (1 = the run or its checks failed, 2 = bad arguments or R not found).
* The same run folder still receives `RUN_SUMMARY.md` (headed `FAIL`), `VALIDATION_STATUS.csv` and `run_console.log`, so a failed run can be reviewed.
* Input problems that stop a run are listed in `INPUT_VALIDATION.csv` (status `FAIL`, with counts and column names, never row values).

## What to share for review

Aggregate files only, from the run folder: `RUN_SUMMARY.md`, `VALIDATION_STATUS.csv`, `INPUT_VALIDATION.csv`, `RUNTIME.csv`, `RUN_MANIFEST.csv`, `SESSION_INFO.txt`, `run_metadata.json`,
and for validation runs `validation_summary.csv` and `validation_detail.csv`. **Do not share** the study CSV, the SPSS file, any `checkpoints\` folder, or anything with participant-level values or credentials.

## Limits of the synthetic demo

The synthetic data test that the workflow executes and that outputs have the expected structure. They have no true weather effects, so they cannot reproduce article estimates, and the synthetic numbers must not be compared with the article.
Exact article estimates require authorised access to the locked analytical data.

## Validator rules (Phase 2D)

`run_validate.ps1` now follows the declared plan in `config/validation_plan.json`. The run FAILS (never passes) if a required reference table is absent or unreadable, required columns or expected records are missing, keys are duplicated or blank, a required column is not actually compared, values that are missing or non-finite disagree, or any cell is outside tolerance or unclassified.
Only the archived L01-L59 reference is declared out of scope; it is reported NOT_TESTED (with a coverage warning) and never counts as PASS. `validation_summary.csv` now also lists duplicates, missing required columns, unclassified cells, record counts and a reason for every reference.

## Post hoc steps 17-30 (13-14 Oct 2026)

`run_all.ps1` runs fourteen labelled post hoc steps after CODED_MODEL_RECORD (`config/posthoc_sensitivity_protocol_v2.json`; README section 8). They cannot change q90, q9, q9-nested or any accepted status. Step 27 needs the role `gis_ims_v8_1_reference`: the eleven GIS files of the analysed IMS run declared in the protocol (participant-level daily / audit / plan files; station-level period coverage, raw 10-minute files and the stage A / B session records; identified by SHA-256; passed in place, never staged or copied; in a paths file a folder, a JSON array or a |-separated list). Step 28 needs no reference. Step 29 uses `archive_engine_reference`.

* **Project mode (RUN_SIECC.cmd).** Copy into the project folder (any subfolder; files are identified by SHA-256 declared in the post hoc protocol):
  * role `posthoc_v14_reference`: the frozen V1.4 files `selected_leave_out.csv` and `primary_resampling_summary.csv` (steps 18, 19);
  * role `archive_engine_reference`: the five archive-engine files `OUTCOME_DISTRIBUTION_SUMMARY.csv`, `SEASONAL_GEOGRAPHIC_CUMULATIVE_SENSITIVITY.csv`, `WEATHER_SOURCE_AGREEMENT.csv`, `EXPOSURE_DATE_LOCALITY_OVERLAP.csv`, `INT_DUMMY_FIRTH_PROFILE_INFERENCE.csv` (steps 17, 20-24);
  * the existing `core_reference` (step 25);
  * role `nearest_station_histories` (step 26; participant-level, local only, never staged or copied): the raw IMS nearest-station exports declared in the post hoc protocol as variants `jul19` and `jul25` are identified by SHA-256 and EACH is run; the step names the variant that reproduces SI Tables S9/S10 (REVIEW with the differences when none does). FEATURE_ID is read as participant_code and JERUSALEM_LOCAL_DATE / LOCAL_DATE as the date (declared aliases; author decision 13 Oct 2026). Without a declared variant, one CSV with the nearest-station header is used (identified by its header only; provenance REVIEW).
  A step whose role is absent is NOT_TESTED with the reason.
* **Classic mode (`-PathsFile`).** The optional keys `posthoc_v14_reference`, `archive_engine_reference` (folders) and `nearest_station_histories` (one CSV file, or a JSON array of CSV files); empty keys make the steps NOT_TESTED.
* **One step on its own.** `.\run_posthoc.ps1 -Item <item> -DataCsv <csv> [-V14ReferenceDir DIR] [-ArchiveReferenceDir DIR] [-CoreReferenceDir DIR] [-NearestHistoriesCsv FILE[|FILE]]`. H3 (`h3_nested`) checks the core reference files (`-CoreReferenceDir`) against the SHA-256 and size of the core_reference bundle of the input-identification declaration and the record counts of `validation_plan.json`.
