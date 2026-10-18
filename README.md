# sieccwinter

## Run it in 3 steps (no programming needed)

**Before the first run, once:** install R 4.4.3 for Windows in its default folder (`C:\Program Files\R\R-4.4.3`). If R is missing, the program says so and stops.

1. **Make a project folder and copy your files into it.** Double-click `RUN_SIECC.cmd` in this package folder. A black window opens and asks *"Type 1 or 2"*. Type `2` and press Enter. A Windows *"Browse For Folder"* window appears: choose or create an empty folder, preferably with a short path such as `C:\SIECC_project`, and click **OK**. The program creates `data\`, `references\` and `outputs\` inside it, plus a `README_FIRST.txt`. Copy the data CSV into `data\`. Optionally copy the reference bundle folders and the historical run folders into `references\`. You may rename files or nest them: every file is recognised by its **content** (SHA-256), never by its name.
2. **Start.** Double-click `RUN_SIECC.cmd` again, type `1`, and choose the project folder in the *"Browse For Folder"* window. The window then shows a plan:
   * which file was found for each role;
   * the mode: ANALYSIS (data only) or FULL VALIDATION (data plus references);
   * which steps will be NOT_TESTED, and why;
   * free disk space and an estimated time.

   Type `Y` and press Enter. The window shows progress lines such as *"Step 05 of 26: BATCH_A - finished: PASS (exit 0), 41.2 s (elapsed 03:10; free 51.2 GB)"*.
3. **Read the results.** At the end, Windows Explorer opens the new run folder `outputs\run_<date>_<time>_<id>`. Double-click **`RESULTS_SUMMARY.html`**. It opens in your browser and needs no internet. It shows the outcome in plain words, every status with its meaning, the ANOM note, and links to the main result tables.

Nothing is written outside the project folder, except R's own temporary files. The program stops with a plain message if:
* two copies of the same file are found;
* the data file sits directly in the project folder, not in a subfolder;
* the project folder path is longer than 110 characters;
* less than 2 GB of disk space is free.

If the data file is not the accepted study CSV, the analysis still runs. Every output is then labelled `NOT_THE_ACCEPTED_INPUT`, and no validation step is run.

From R, the same thing is `siw_run_project()` (it opens a folder chooser) and `siw_create_project("C:/SIECC_project")`. Run them after `source("scripts/load_package.R"); siw_dev_load(".")` in this folder.

**One-time author step (identification of the reference bundles and historical runs).** The core, Batch A–C and DLM bundles and the historical accepted runs have no hashes in the protocols. The author records them once:
1. Double-click `DECLARE_SIECC_INPUTS.cmd` and pick each folder. It records only names, sizes and SHA-256. Each bundle file is cross-checked against acceptance-time records and labelled CONFIRMED_AT_ACCEPTANCE or UNCONFIRMED.
2. Review the report.
3. Import it with `scripts\import_project_declaration.ps1`.

Until then those roles are NOT_TESTED ("no input-identification declaration is installed"), as are UNCONFIRMED ones. Never PASS. See `MANUAL_RUN_GUIDE.md` §00.

---

An R implementation of the SIECC Winter analysis. It covers winter weather exposures and three social outcomes in older adults: UCLA loneliness, objective isolation and high social integration. It also validates its results against frozen reference outputs of the original analysis.

**This repository contains no study data.** The only data shipped are fully synthetic (`inst/extdata/synthetic_siecc_winter.csv`, artificial codes `SYN001`…). Synthetic numbers have no scientific meaning.

Version 1.0.0. Author and maintainer: Nahum M. Gabinet (ORCID [0000-0001-5861-1507](https://orcid.org/0000-0001-5861-1507)), Louis and Gabi Weisfeld School of Social Work, Bar-Ilan University, Ramat Gan, Israel; and School of Environmental Sciences, Faculty of Social Sciences, University of Haifa, Haifa, Israel (ngabinet@campus.haifa.ac.il). Licence: MIT (`LICENSE`, `LICENSE.md`). Repository: https://github.com/ngngn1977-cloud/sieccwinter. `DESCRIPTION` declares R (>= 4.4.0); the package was validated on R 4.4.3, and the full-run launcher still requires R 4.4.3.

## 1. Install

* Windows, R 4.4.3 at `C:\Program Files\R\R-4.4.3\bin\Rscript.exe`, Windows PowerShell 5.1.
* Required R packages: MASS (≥ 7.3-64), jsonlite (≥ 2.0.0), plus splines, stats, tools and utils, which come with R. Optional packages (testthat, numDeriv, dlnm) are not needed by any run. See `DEPENDENCY_GAPS.md`.
* No installation step is needed. The scripts load the package from this folder.
* If PowerShell blocks scripts, use `-ExecutionPolicy Bypass` on the command line (process scope only), as shown below.

## 2. Input

One canonical CSV, one row per participant: 22 scalar columns, daily exposure columns `<PREFIX>_L01…_L56` (`_L59` for the archived programme), and optionally the 80 cumulative columns. The full specification is in **`DATA_DICTIONARY.md`**. Convert an SPSS file privately with `prepare_private_csv.ps1` (`PRIVATE_DATA_RUN_GUIDE.md`). The SHA-256 of the accepted study CSV is declared in the private validation kit (section 8); the full study run refuses any other file.

## 3. How to run

| What | Command (from this folder) | Time |
|---|---|---|
| No-code run on a project folder | double-click `RUN_SIECC.cmd` (see the top of this page) | see the plan |
| Same, non-interactive | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\siecc_launcher.ps1 -ProjectDir "<project folder>" -Yes` (add `-PlanOnly` to see the plan only) | see the plan |
| Synthetic demo through the launcher | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\siecc_launcher.ps1 -ProjectDir "<project with the shipped synthetic CSV in data\>" -Demo -Yes` | about 1 min |
| Synthetic demo of the whole run | `powershell -NoProfile -ExecutionPolicy Bypass -File .\run_all.ps1 -Demo` | about 10 min |
| Synthetic demo from the PUBLIC tarball alone | install the tarball, then in R: `sieccwinter::siw_run_public_demo("<new folder>")` (paper workflow plus the ten post hoc steps; `DEMO_STATUS.txt`) | about 5 min |
| One post hoc step | `powershell -NoProfile -ExecutionPolicy Bypass -File .\run_posthoc.ps1 -Item <item> -DataCsv "<csv>" [-V14ReferenceDir / -ArchiveReferenceDir / -CoreReferenceDir / -NearestHistoriesCsv ...]` | 1-3 min |
| Full controlled study run | `powershell -NoProfile -ExecutionPolicy Bypass -File .\run_all.ps1 -PathsFile "<private folder>\manual_run_paths.json"` | see `MANUAL_RUN_GUIDE.md` |
| Resume an interrupted run | add `-ResumeRunDir "<the fullrun_... folder>"` | — |
| Synthetic tests | `& "C:\Program Files\R\R-4.4.3\bin\Rscript.exe" tests\testthat.R` | about 40–45 min (34 files) |
| (note) | Under `R CMD check` (an installed package has no `R/`, `config/`, `scripts/` or `validation/` sources), 22 source-folder tests (source audits, the launcher plan test, and two ANOM v2 tests that need `validation/` files) are reported `SKIPPED (source checkout only)` and never counted as passed. Run `tests\testthat.R` from this folder for full coverage. | — |
| Tests of `run_all.ps1` itself | `powershell -NoProfile -ExecutionPolicy Bypass -File .\validation\test_run_all_wrapper.ps1` | about 30 min |
| Tests of `run_posthoc.ps1` | `powershell -NoProfile -ExecutionPolicy Bypass -File .\validation\test_posthoc_wrapper.ps1` | about 5 min |
| Tests of the no-code interface | `powershell -NoProfile -ExecutionPolicy Bypass -File .\validation\test_project_launcher.ps1` | about 3 min |

`run_all.ps1` runs these steps, each in a fresh process:
1. environment and dependency check;
2. input SHA-256 check;
3. the paper analysis;
4. every strict validation against its frozen reference bundle: core, Batch A, Batch B, refined ordinal, Batch C, DLM V2 and V2.1, archived DLM, cold burden, daily layer, ANOM/ALT;
5. the ANOM sample-SD diagnostic;
6. the versioned coded model record (`SIECC_coded_model_record_v2.csv`, a new file beside the legacy record, never over it);
7. the post hoc steps 17-26 (`config/posthoc_sensitivity_protocol_v1.json`; section 6);
8. a summary.

It calls the existing per-batch scripts (`run_paper.ps1`, `run_validate.ps1`, `run_batch_*.ps1`, …) unchanged. Those scripts can still be run on their own; their guides are the `*_GUIDE.md` files. `manual_run_paths.template.json` shows the paths file. Keep the filled copy in your private folder and never commit it.

## 4. Outputs

A new folder `<out_root>\fullrun_<date>_<time>_<id>` is created. Earlier runs are never overwritten.

| File | Content |
|---|---|
| `FULL_RUN_SUMMARY.md` | Read this first: outcome, preflight, one row per step, carried statuses, every step's statuses verbatim |
| `FULL_RUN_STATUS.csv` / `FULL_RUN_STATUS_LINES.csv` | the same, machine-readable |
| `run_progress.txt` | Live progress, one line per event: step number and name, start or end time, class and exit code, seconds, free disk space. Aggregate only. |
| `RUN_PARAMETERS.txt`, `preflight\ENVIRONMENT_*.txt` | input hash, configuration hash, source fingerprint, R and package versions |
| `paper_*`, `validate_*`, `batcha_*`, … `anomdiag_*` | each wrapper's own run folder with its own `RUN_MANIFEST.csv` and status file (`EXPECTED_OUTPUTS.md`) |
| `steps\NN_<STEP>\` | `STEP_RESULT.txt` and the console log of each step |
| `FULL_RUN_MANIFEST.csv` | SHA-256 of every file. Written last, and only when the run completed. |

Every output is aggregate. No participant code or row value is written to any output or log.

## 5. What the statuses mean

| Status | Meaning |
|---|---|
| **PASS** | The check was made and met its declared criterion. Only PASS counts as passed. |
| **FAIL** | The check was made and did not meet its criterion, or required evidence was missing. Results are never altered to fit a reference. |
| **REVIEW** | Required statuses passed, but a documented item needs human judgement, for example manuscript concordance, the MAXT permutation assumptions, or a documented legacy limitation. A REVIEW is not a pass. |
| **NOT_TESTED** | Not compared, for example because no reference exists, a dependency did not run, or the item is declared out of scope. A NOT_TESTED is not a pass. |
| **DIAGNOSTIC** | An informational post hoc result (the ANOM sample-SD diagnostic). It changes no status. |
| EXECUTION_FAILURE / ARGUMENT_ERROR | A step did not run to completion. The run is incomplete and can be resumed. |

Exit codes of `run_all.ps1`:
* 0: every step PASS. In the demo, the study-reference steps are NOT_TESTED by design and are listed.
* 3: no FAIL, but REVIEW, NOT_TESTED or DIAGNOSTIC present.
* 4: at least one FAIL.
* 1: incomplete.
* 2: refused before any analysis.

Since the post hoc ANOM protocol v2 (12 Oct 2026), step 14 ANOM_ALT is governed by `ANOM_VERIFICATION_STATUS_V2`. The v1 status is still shown, superseded, kept for the record. A study run therefore ends with 3 if no step FAILs; several steps are documented REVIEW. Any FAIL still gives 4.

## 6. Scope and limits

* **Primary inference.** The M2-L cumulative models with the Benjamini-Hochberg families q10, q80 and q90, and the nine final L01–L56 distributed-lag models (q9). The final L01–L56 numerical validation is **accepted under the post hoc protocol V2.1**. The historical V1 FAIL records are preserved.
* **Sensitivity analyses.** Batch B (Firth, ordinal, exposure transformations, nonlinear forms), the refined ordinal protocol (whether its results are used for reporting is an open author decision), and the rainfall zero-mm treatment (opt-in only).
* **Validation.** DLM validation V2.1 is the accepted final L01–L56 validation (author decision 11 Oct 2026); 'pending' wording in generated reports is historical.
* **Archived, validation only.** The L01–L59 DLM grid (168 models; q24 and an archived q9) and the lag-form sensitivity (216; q216). They are never mixed with the final models. Since 2026-10-06, archived binary fits use glm epsilon 1e-10, as in the archived specification (J02); the final models keep 1e-12. The daily-layer and ANOM/ALT status files still print `J02_GLM_EPSILON_ENTRY_POINTS=OPEN (no change made)`. Those source files are traced by the ANOM diagnostic and were deliberately left unchanged, so the label is historical; the run summary says so. Archived simultaneous bands are not validated; the 18 L59 bootstrap rows are post hoc step 19.
* **Exploratory families.** The cold-burden scan (22,302 models), the daily-lag layer (XYD, DAY, MAXT) and ANOM (7,080) / ALT (312). These are reproduced and verified numerically only, with no scientific interpretation. **ANOM identity, protocol v2 (post hoc, author decision 12 Oct 2026; `config/anomaly_history_validation_protocol_v2.json`).**
  * **Why.** The v1 identity "implied SD(outcome) constant within each outcome", evaluated within groups of equal n, assumed that equal n means the same complete-case sample. The declared diagnostic disproved this: run_20261012_114559_4737, step 15, decision `PREMISE_DISPROVEN_EQUAL_N_DIFFERENT_SAMPLES`.
  * **The v2 rule.** Each gaussian row's implied SD is compared with the directly measured SD of that row's own complete-case sample, using the traced mask of `validation/anom_sample_sd_diag.R`, at the unchanged 1e-9 relative tolerance. This rule decides `ANOM_VERIFICATION_STATUS_V2`, which governs step 14.
  * **V1 kept.** The v1 by-n row and `ANOM_VERIFICATION_STATUS` are still computed and reported as "V1 identity (superseded, kept for the record)".
  * **Earlier runs.** anomhist_20261004_141846_28bd and run_20261012_114559_4737 keep `ANOM_VERIFICATION_STATUS=FAIL` in their own records.
  * **Unchanged.** Protocol v1 and every other check and tolerance. Protocol v2 also records the two existing near-zero scalings `|a-b|/max(1,|b|)` (anomaly values 1e-9, window features 1e-12), unchanged.
* **Not implemented.** The selected-model bootstrap and grouped cross-validation, maxT outside the daily layer, and figure drawing. Figure provenance and dlnm equality are NOT_TESTED. (The selected deletion refits and the 18 L59 DLM bootstrap rows are now post hoc steps 18 and 19.)
* **Age.** Analytic ages are reported as recorded; no participant is filtered.

## 7. Reproducibility and data handling

See `REPRODUCIBILITY.md`. Confidential data, the SPSS file and any credentials must never be committed. `.gitignore` and `.Rbuildignore` exclude `private_validation`, `runs`, `output` and data files. The study CSV is never shared outside the study team. Share only the aggregate summary files.

## 8. Post hoc steps 17-30 (13-14 Oct 2026)

Declared in `config/posthoc_sensitivity_protocol_v2.json` (steps 17-26 on 13 Oct 2026, unchanged from version 1, which is kept beside it; steps 27-30 on 14 Oct 2026), after the 30 July 2026 family lock. q9-nested is also read-only. Each step is labelled; none changes an estimator, rule, seed, tolerance, family or accepted result, and **none can change q90, q9 or any accepted status**. Statuses are PASS / REVIEW / NOT_TESTED / FAIL, reported verbatim; REVIEW and NOT_TESTED are never counted as PASS.

| Step | Label | What | Compared with |
|---|---|---|---|
| 17 EXPOSURE_EXPORT | POST_HOC | aggregate exposure export: participant-days in L01-L56, UTCI categories, rainfall days (>=1 mm / 0 mm / 0-1 mm), exposure date span, winter / spring UTCI, participants at the minimum UCLA score | manuscript values; archive stage 13 |
| 18 PH_DELETION | SENSITIVITY | 6 selected models x (55 participant + 33 date deletions) = 528 refits | frozen V1.4 output; SI Table S9 |
| 19 PH_L59_BOOTSTRAP | SENSITIVITY | 18 archived L01-L59 DLM bootstrap rows (participant 2,000 / date 1,000) | frozen V1.4 output; SI Table S9 |
| 20 PH_NO_APRIL | SENSITIVITY | no-April restriction (n=38) of the 90 primary cumulative models | archive engine; SI Table S10 |
| 21 PH_SOURCE_AGREEMENT | SENSITIVITY | ERA5-IMS agreement (80 rows) | archive engine; SI text |
| 22 PH_EXPOSURE_RESIDUALS | SENSITIVITY | exposure residual diagnostics (27 regressions) | archive engine; SI text |
| 23 PH_PROFILE_LIKELIHOOD | SENSITIVITY | penalized profile likelihood, 30 integration models (base R; logistf is not used) | archive engine; SI text |
| 24 PH_EARLY_LATE | RECONSTRUCTION | early/late analyses (SI Table S11): interaction rows reproduced; subgroup fits and q values reconstructed | archive engine; SI Table S11 |
| 25 PH_H3_NESTED | POST_HOC | H3 nested test, constant versus varying lag weights (nine pairs; q9-nested, distinct from q9); `H3_NESTED_TEST.csv` | archived W56 coefficients and global p; Table 3 |
| 26 PH_NEAREST_STATION | POST_HOC | nearest-station check (120 IMS cumulative fits, six L01-L56 DLMs), run once per declared export variant present (raw exports read directly: FEATURE_ID = participant_code) | SI Tables S9 / S10; the reproducing variant is named |
| 27 PH_EXPOSURE_QA | RECONSTRUCTION | exposure QA of the analysed IMS run (toolkit V8.1, 25 Jul 2026): required source identity with the canonical CSV (TD, RH, rainfall; 1e-9); daily coverage of the three assigned stations per participant-day (>= 80 % of 144 values, 138 on the DST day 27 Mar 2026; 1,440 / 1,380 for the five 1-minute rain stations); interpolated-value support; distances; station pool | manuscript / SI values (`exposure_qa_si_concordance.csv`) |
| 28 PH_VIF_SUMMARY | RECONSTRUCTION | maximum and per-column VIFs of the 240 M2-L cumulative models (90 representative), the term attaining the maximum | manuscript / SI values (`vif_si_concordance.csv`) |
| 29 PH_CALENDAR_SPLINE | SENSITIVITY | the 90 representative M2-L models with ns(assessment day, df 2 and 3) for the linear assessment-day term; t-based p for continuous outcomes; BH within each df (q90-calspline-df2 / -df3) | archive engine (1e-10); SI and Limitations (`calendar_spline_si_concordance.csv`) |
| 30 PH_AGE65 | SENSITIVITY | the 90 representative M2-L models refitted without participants aged under 65 (Age = 2026 minus birth year); same estimators and inference (HC3 t for continuous outcomes, model-based Wald for high integration); BH across the 90 tests (q90-age65); declared as amendment v2_4 of the protocol on 14 Oct 2026 | SI and Limitations (`age65_si_concordance.csv`); no reference file |

**Calendar anchor.** assessment_day 0 = 2026-01-29 (the first data-collection day) is declared as one constant; no date file is read. A participant's interview (L00) date is 2026-01-29 + assessment_day, and lag k falls on 2026-01-29 + assessment_day - k. This rule gives the no-April restriction, the exposure date span, the winter / spring class of each lag day and the date-cluster labels of the archived resampling. Early / Late groups are assessment_day <= 54 / >= 55.

**Exposure provenance.** Primary IMS exposures were assigned upstream from each participant's geocoded home address by three-nearest-station inverse-distance-squared weighting, with a minimum of two valid stations; ERA5 / ERA5-HEAT exposures were assigned by bilinear interpolation. Retrieval (IMS API, Copernicus CDS), geocoding and interpolation were all done upstream. The package consumes only the resulting daily histories; it never reads, stores or outputs addresses or coordinates, and it does not retrieve or interpolate. The nearest-station check (step 26) substitutes the single nearest eligible station's daily history for the IDW history of the same address; UTCI stays ERA5.

## Running with the study data (available on request)

The study data are not public. The SIECC winter data set and its declarations (the SHA-256 of the accepted canonical CSV and of every frozen reference) are available from the author on request (ngabinet@campus.haifa.ac.il), subject to the study's ethics approval and a data-use agreement.

* **Without the study data** everything runs on the shipped fully synthetic CSV: `run_demo.ps1`, `siw_run_public_demo()`, the full run in demo mode (`run_all.ps1 -Demo`) and the no-code launcher (`RUN_SIECC.cmd`, demo project). Numbers have no scientific meaning; comparisons with study values are reported as NOT_APPLICABLE_SYNTHETIC_INPUT or NOT_TESTED, never PASS.
* **With the study data** copy `config/private_local.template.json` to `config/private_local.json` (git-ignored, never committed) and fill in the values supplied with the data; place the declarations supplied with the data in `config/` and `inst/extdata/` (the public copies show `REDACTED_PUBLIC_BUILD`); then run `RUN_SIECC.cmd` on a project folder holding the data CSV and the reference files. Inputs are recognised by SHA-256, never by name. Without `config/private_local.json`, a study CSV is labelled NOT_THE_ACCEPTED_INPUT and every validation against the study references is NOT_TESTED.
* The full run requires Windows, Windows PowerShell 5.1 and R 4.4.3 (the package itself declares R >= 4.4.0).
## 9. Public package and private validation kit

| Part | Contents | Where |
|---|---|---|
| **PUBLIC** R package (source tarball `sieccwinter_<version>.tar.gz`) | R code, help pages, the fully synthetic CSV, the synthetic tests, this README, and **redacted** declarations (every SHA-256, size and location of the accepted input and of the frozen references replaced by `REDACTED_PUBLIC_BUILD`; marked by `inst/extdata/PUBLIC_BUILD.json`). `siw_run_public_demo()` runs end to end from the tarball alone. Tests that check a private declared value are reported SKIPPED. | may be shared |
| **PUBLIC** source tree (`sieccwinter_source\sieccwinter`) | everything in the tarball plus the no-code launcher (`RUN_SIECC.cmd`), the run wrappers (`run_*.ps1`, `scripts\`), the validation scripts and the guides, with every private value redacted (`REDACTED_PUBLIC_BUILD` or `<private>`); the local configuration is shipped only as `config/private_local.template.json`. The demo project and every synthetic step run from it. | may be shared |
| **PRIVATE** validation kit (`private_kit\`) | every file that is absent from or differs in the public source tree: `config/private_local.json` (accepted-input SHA-256, privacy deny list), the full declarations, the input-identification declaration, the unredacted guides and validation files, the test constants and the development log; `install_private_kit.ps1` restores the development tree on top of a copy of the public source tree and checks the source fingerprint | local only; never published |

The public tree contains no participant data, credentials or local paths; file-name examples in the guides are illustrative. The privacy scan (`scripts/release_public.R scan`) fails on user paths, account and machine folder names, private data-file names, deny-listed hashes, participant-code patterns, any data file other than the synthetic CSV and non-synthetic participant codes in that CSV; its deny list is read from `config/private_local.json`. The maintainer address is allowed only in `DESCRIPTION` and `README.md`.

`scripts/install_private_kit.ps1` is for the maintainer only. It requires the private validation kit, which is not distributed. Without it the package runs on the public template (`config/private_local.template.json`).

## Documents cited but not distributed

The guides, protocols and help pages name the following files. They are not part of this repository.

* Internal development reviews and reports: `reports/PHASE3D2_MANUAL_DLM_VALIDATION_REVIEW.md`, `reports/PHASE3D3_SIMULATION_DIAGNOSTIC_BUILD_REPORT.md`, `reports/PHASE3D4_MANUAL_SIMULATION_DIAGNOSTIC_REVIEW.md`, `reports/PHASE3D6_MANUAL_VALIDATION_V2_REVIEW.md`, and the development log (`DEVELOPMENT_LOG_2026-10-06.md`).
* Specifications: `SPEC_ExposureQA_CalendarSpline_2026-10-14.md`.
* Python sources of the independent calculations: `independent_calculations/winter_independent_python.py`, `stncov.py`, `nscheck3.py` and `Exposure_QA_reconciliation_2026-10-14/age65.py`.
* The original analysis programme (`WINTER_SOURCE` / `WINTER_CODE`), from which specifications were traced: `000_utils.R`, `020_import_validate.R`, `030_model_engine.R`, `040_transformations_core.R`, `050_descriptive.R`, `060_xy.R`, `070_cumulative_models.R`, `080_dlm.R`, `090_sensitivity_models.R`, `100_resampling.R`, `110_cold_burden.R`, `120_daily_anomaly.R`, `130_alternative_methods.R` and `170_pipeline.R`.
* Original manuscript drafts cited by the validation protocols (the manuscript and SI versions whose values the protocols were declared against; superseded by the submitted manuscript): `SIECC_Manuscript_Methods_Results_2809.docx`, `SIECC_Focused_Supplementary_Methods_Results_Q_FORMULAS_REVISED_FINAL.docx`.
* Original analysis configuration and scripts cited by the protocols (the historical production code whose outputs are the frozen references; reimplemented in this package): `config/analysis_config.R`, `src/r/13_descriptive_data_quality.R`, `src/r/17_post_dlm_sensitivity_contrasts.R`, `src/r/25_binary_small_sample_completion.R`, `src/r/28_dlm_scenario_weather_assignment.R`.
* Frozen reference outputs of the original programme (private reference bundles; compared against, not distributed): `batchA_bivariate_reference.csv`, `batchA_cumulative_diagnostics_reference.csv`, `batchA_descriptive_table1_reference.csv`, `batchA_group_summaries_reference.csv`, `batchA_group_tests_reference.csv`, `BATCHB_REFERENCE_PROVENANCE.csv`, `batchC_dlm_bootstrap_L59_reference.csv`, `batchC_primary_cumulative_bootstrap_reference.csv`, `DLM_CORRECTED_L56_9.csv`, `EXPOSURE_DATE_LOCALITY_OVERLAP.csv`, `INT_DUMMY_FIRTH_PROFILE_INFERENCE.csv`, `OUTCOME_DISTRIBUTION_SUMMARY.csv`, `primary_resampling_summary.csv`, `SEASONAL_GEOGRAPHIC_CUMULATIVE_SENSITIVITY.csv`, `selected_leave_out.csv`, `WEATHER_SOURCE_AGREEMENT.csv`, `WINTER_PHASE1/DATA_CODING_AUDIT.csv`, `cumulative_regression_grid.csv`, `MASTER_CUMULATIVE_ALL_MODELS_1200.csv`, `MASTER_CUMULATIVE_PRIMARY_90.csv`, `DLM_L01_L56_nine_representative_summary.csv`, `fixed_DLM_summary.csv`, `MASTER_DLM_ALL_MODELS_120.csv`, `MASTER_DLM_ALL_M2_24.csv`, `batchB_firth_reference.csv`, `batchB_ordinal_reference.csv`, `batchB_transformations_reference.csv`, `batchB_nonlinear_reference.csv`, `dlmL56_audited_estimates.csv`, `dlmL56_audited_discrete.csv`, `dlmL56_audited_critical_10000.csv`, `SIECC_All_Models_with_Codes.csv`.
* Private local files created from the shipped templates (git-ignored, never committed): `config/private_local.json` (template `config/private_local.template.json`), `manual_run_paths.json` and `private_validation/manual_run_paths.json` (template `manual_run_paths.template.json`), `project_inputs_declaration_v1.json` (no template; written by `scripts/declare_project_inputs.ps1`).

These are internal development records; aggregate summaries are available from the author on request.