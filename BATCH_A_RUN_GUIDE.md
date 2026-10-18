# Batch A manual run guide (Windows PowerShell)

Release 1.0.0. You run this yourself; it reads the accepted canonical CSV and does not need RStudio.
R 4.4.3 at `C:\Program Files\R\R-4.4.3\bin\Rscript.exe`; no package installation. If PowerShell blocks scripts: `Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass`.

## What one run does

1. Runs the **core analyses** unchanged (1,680 cumulative specifications, nine final L01-L56 DLMs) and compares them with the accepted core bundle using the corrected strict validator.
2. Runs **Batch A**:
   * assessment-day checks (complete, integer, non-negative, number of distinct groups; **33 expected for the study data**; the canonical schema is unchanged);
   * influence and design diagnostics for all 1,680 specifications (the 16 columns of the original grid) in `cumulative_diagnostics.csv`, with dropped terms and any undefined diagnostic listed per model;
   * the 240 cumulative bivariate tests (Pearson / point-biserial, Spearman, descriptive q families) in `bivariate_cumulative.csv`;
   * descriptive analyses: original 17-row cohort table, SI Table S2 rows, group summaries and tests, participant-level exposure summaries (7/21/56 days) **separately from** participant-day weather distributions (3,080 participant-days; UTCI classes; rainfall classes, with the supplied missing rainfall total and its indicator reported as supplied), and coding notes (age-group rule, boundary ages, assessment-period rule, label rules) that record rather than reconcile discrepancies.
3. Compares the Batch A tables with the Batch A reference bundle (separate plan `config/validation_plan_batchA.json`; every reference is required).
4. Creates a **new** run folder (never overwrites) with versions, configuration and input hashes, plan hashes, reference-file hashes and an output manifest.

## The command

```powershell
cd <package folder>
.\run_batch_a.ps1 -DataCsv "<private folder>\conversion_01\siecc_winter_canonical.csv" `
                  -CoreReferenceDir "<private folder>\reference_bundle_01" `
                  -BatchAReferenceDir "<private folder>\reference_bundle_batchA_01" `
                  -OutRoot "<private folder>\runs"
```

`-ExpectedAssessmentDays` defaults to 33 (study data). Runtime: about 40-60 s for 55 participants (synthetic 60-participant dry run: about 36 s).

## Four separate statuses (printed and written to `BATCH_A_STATUS.txt`)

| Status | Meaning |
|---|---|
| EXECUTION | the workflow ran without error and all internal self-checks passed |
| INPUT CHECKS | core input validation and the assessment-day checks have no FAIL (WARN, e.g. the known rainfall cell, is reported) |
| CORE NUMERICAL COMPARISON | all required core references PASS (exact or within tolerance); the archived L59 reference is declared out of scope |
| BATCH A NUMERICAL COMPARISON | all five Batch A references PASS |

Exit codes: 0 everything passed; 1 execution failure; 2 invalid arguments or R missing; 4 executed, but an input check or a comparison FAILED.
A comparison FAIL never changes a result; read `validation_detail.csv` / `batch_a_validation_detail.csv`.

## Expected on the study data (from the frozen references; nothing is forced)

* 55 rows; distinct `assessment_day` = 33; input checks: 0 FAIL, 1 WARN (V14, the known rainfall cell).
* `cumulative_diagnostics.csv` 1,680 rows; `bivariate_cumulative.csv` 240 rows; `descriptive_table1.csv` 17; `descriptive_group_summaries.csv` 30; `descriptive_group_tests.csv` 12.
* References: diagnostics 1,680; bivariate 240; table 17; group summaries 30; group tests 12.
* NOT TESTED (no authoritative frozen reference): SI Table S2 rows beyond the original 17-row table, participant-day weather distributions, Kendall (legacy extra, not produced), M0-regression columns of the XY table (equal to the validated core M0 rows).

## What to send for review (aggregate only)

`RUN_SUMMARY.md`, `VALIDATION_STATUS.csv`, `BATCH_A_STATUS.txt`, `INPUT_VALIDATION.csv`, `validation_summary.csv`, `validation_detail.csv`, `batch_a_validation_summary.csv`, `batch_a_validation_detail.csv`,
`RUNTIME.csv`, `RUN_MANIFEST.csv`, `SESSION_INFO.txt`, `batch_a_metadata.json`, `run_metadata.json`, and the Batch A result tables (`cumulative_diagnostics.csv`, `bivariate_cumulative.csv`, `descriptive_*.csv`, `exposure_participant_level.csv`, `weather_participant_day.csv`, `batch_a_input_checks.csv`). Never send the study CSV or the SAV.
