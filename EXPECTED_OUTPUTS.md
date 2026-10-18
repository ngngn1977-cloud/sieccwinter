# Expected outputs

## No-code run (`RUN_SIECC.cmd` / `siw_run_project()`)

Folder `<project>\outputs\run_<date>_<time>_<id>\`:
* everything a full run writes (below), with `RUN_PARAMETERS.txt` recording `project_mode`, `project_mode_label` (`ANALYSIS` or `FULL_VALIDATION`) and `project_input_status` (`ACCEPTED`, `DEMO_SYNTHETIC_INPUT` or `NOT_THE_ACCEPTED_INPUT`);
* **`RESULTS_SUMMARY.html`**: self-contained, no scripts, no internet; outcome in plain words, status meanings, every status verbatim, the ANOM note, links to the main aggregate tables, fingerprints;
* `staged_references\<role>\`: byte-identical copies of the identified reference files, under their declared names;
* `launcher\`:
  * `PROJECT_PLAN.txt`: the plan shown before the run;
  * `identification.csv`: role, status, declared file, project path, bytes, SHA-256, reason;
  * `unknown_files.csv`;
  * `paths.json`: the project-mode paths file;
  * the engine console logs;
  * `LAUNCHER_MANIFEST.csv`: hashes of the files written after `FULL_RUN_MANIFEST.csv`;
* `NOT_THE_ACCEPTED_INPUT.txt` in the run folder and in each step folder, when the data file is not the accepted input.

| Project content | Mode | Expected classes | Exit |
|---|---|---|---|
| shipped synthetic CSV only, `-Demo` (synthetic test, about 35 s) | ANALYSIS | PASS: ENVIRONMENT, INPUT_HASH, PAPER, CODED_MODEL_RECORD and nine post hoc steps (synthetic demo: comparisons NOT_APPLICABLE_SYNTHETIC_INPUT); REVIEW: PH_PROFILE_LIKELIHOOD (the reproduced archived optimiser does not solve the profile equation for 3 of the 30 synthetic models); the other 11 validation steps NOT_TESTED ("input absent") | 3 |
| synthetic CSV, synthetic core bundle and synthetic accepted paper run, renamed and nested (`-Demo`, synthetic declaration) | FULL_VALIDATION | CORE_VALIDATION PASS against the staged references; steps without identified inputs NOT_TESTED with the reason (for example "UNCONFIRMED") | 3 |
| a data file that is not the accepted CSV | ANALYSIS | INPUT_HASH FAIL (`NOT_THE_ACCEPTED_INPUT`), PAPER and CODED_MODEL_RECORD run, every validation and every post hoc step NOT_TESTED | 4 |
| two identical candidates for one role, data directly in the project folder, project path over 110 characters, under 2 GB free, R missing | — | refused before anything is written | 2 |

## Full run (`run_all.ps1`)

| Mode | Expected step classes | Exit |
|---|---|---|
| `-Demo` (development machine: about 10 min) | PASS: ENVIRONMENT, INPUT_HASH, PAPER, CORE_VALIDATION (self-reference, PASS_EXACT), the diagnostic's synthetic test (33/33), CODED_MODEL_RECORD (2,073 rows; 1,689 matched to the demo paper run) and nine post hoc steps run on the synthetic CSV (`-SyntheticDemo`). REVIEW: PH_PROFILE_LIKELIHOOD (see above). The 10 study-reference steps are NOT_TESTED by design. | 3 (`COMPLETED_WITH_REVIEW_NOT_TESTED_OR_DIAGNOSTIC`) |
| study (`-PathsFile`) | ANOM_ALT is governed by `ANOM_VERIFICATION_STATUS_V2` (post hoc protocol v2, 12 Oct 2026). Its status file still lists `ANOM_VERIFICATION_STATUS` (v1 composition, superseded, kept for the record), `ANOM_VERIFICATION_GOVERNED_BY`, `ANOM_IDENTITY_V1_BY_N` and `ANOM_IDENTITY_V2_ROWS`. Its folder adds `anom_identity_v2_rows.csv` (per gaussian row: keys, recorded n, traced mask n, implied and direct SD, relative difference, status) and `anom_scale_and_inference_identities_v2.csv`, which lists every v1 row, the by-n row as "V1 identity (superseded, kept for the record)", plus the v2 rows. `anom_scale_and_inference_identities.csv` is unchanged. ANOM_SAMPLE_SD_DIAGNOSTIC is DIAGNOSTIC. CODED_MODEL_RECORD is PASS when its 16 internal checks pass. For BATCH_B, DLM_V2, DLM_V21, ARCHIVED_DLM, COLD_BURDEN and DAILY_LAYER, the documented good outcomes are PASS or REVIEW (exit 3). Every other class is whatever its wrapper reports; nothing is assumed. A `NOTE:` line (console, `run_progress.txt`, and the summary's "Accepted-run references" section) flags each empty `dlm_accepted_run`, `batch_b_legacy_run` or `batch_c_accepted_run`. | 3 if no step FAILs (REVIEW steps present); 4 if any FAIL |

Folder `fullrun_<date>_<time>_<id>`: `run_progress.txt` (live progress lines), `FULL_RUN_SUMMARY.md`, `FULL_RUN_STATUS.csv` (one row per step), `FULL_RUN_STATUS_LINES.csv` (every status line of every step, verbatim), `RUN_PARAMETERS.txt`, `preflight\ENVIRONMENT_*.txt`, `steps\NN_<STEP>\STEP_RESULT.txt` and `step_console.log`, one run folder per wrapper (`paper_*`, `validate_*`, …, `anomdiag_*`), and `FULL_RUN_MANIFEST.csv` (written last; absent when the run is incomplete).

## Synthetic demo (`.\run_demo.ps1`), development machine values

Fixed properties of the shipped synthetic input and configuration:

| Item | Expected |
|---|---|
| Participants (rows) | 60 |
| Input file SHA-256 | `C2462ED6286C4CCCDBE21913F164AB086B1C0D2DC19CC6FC18337301CF99C4D8` |
| Configuration SHA-256 | `5A8C605469AE0AA8133215E8E1666182BEAE17F92F01A00CD0BDE0612E3DC5DE` (changes if the configuration file is edited) |
| Seed / draws / MVN method | 20260729 / 5000 / `MASS_mvrnorm` |
| Cumulative fields | `supplied` |
| Input validation | 27 checks: 26 PASS, 1 WARN (V14, rainfall total missing with indicator 0 at lag 35, 1 cell), 0 FAIL |
| Self-checks | 20 PASS, 0 FAIL |
| Runtime | about 8 s pipeline, 13 s wall clock; 15-30 s on a slower machine |

Files and row counts (columns in parentheses):

| File | Rows | Content |
|---|---|---|
| `cumulative_results.csv` | 1,680 (56) | 3 outcomes x 8 metrics x 10 windows x 7 tiers; q10/q80/q90 only in M2-L rows |
| `complete_case_accounting.csv` | 1,680 (8) | n input, n complete and reasons for exclusion per specification |
| `cumulative_design_audit.csv` | 7 (6) | design columns and reference levels per tier |
| `dlm_final_summary.csv` | 9 (38) | nine L01-L56 models: global test, q9, cumulative effect, simultaneous critical value, seed |
| `dlm_final_lag_curves.csv` | 504 (8) | 9 models x 56 lags, pointwise and simultaneous bands |
| `dlm_final_cumulative_curves.csv` | 504 (6) | cumulative effect by lag |
| `rainfall_anomaly_report.csv` | 5 (3) | 1 anomalous cell, 1 participant, lag 35 |
| `rainfall_sensitivity_summary.csv` | 69 (15) | optional named sensitivity (demo only) |
| `input_validation_report.csv` / `INPUT_VALIDATION.csv` | 27 (5) | check id, status, count, detail |
| `self_checks.csv` | 20 (3) | internal consistency checks |
| `timings.csv` / `RUNTIME.csv` | 6 / 7 | stage times |
| `RUN_MANIFEST.csv` | one row per file | SHA-256, bytes |
| `VALIDATION_STATUS.csv` | 3 | run exit code, input validation, self-checks |
| `RUN_SUMMARY.md`, `SESSION_INFO.txt`, `run_metadata.json`, `run.log`, `run_console.log`, `MANIFEST.csv`, `dependency_report.csv` | - | documentation |

Numeric values in the synthetic results are not scientifically meaningful. Expected reproducibility: two demo runs on the same machine give byte-identical result tables. On another machine, tables agree to numerical tolerance; simultaneous critical values (Monte Carlo based) may differ in the last digits.
Column schemas: `output_schema\*.schema.json`.

## Paper run

Same tables as the demo without `rainfall_sensitivity_summary.csv` (unless `-RainfallSensitivity`), plus archived tables with `-Archived`
(`archived_dlm_summary.csv`: 168 rows; `archived_dlm_sensitivity_summary.csv`: 216 rows). `input_validation_report.csv` must have 0 FAIL. Row counts of the two main tables are unchanged (1,680 and 9).

## Validate run

Adds `validation_summary.csv` (one row per reference: status PASS_EXACT, PASS_WITHIN_TOLERANCE, FAIL or NOT_TESTED; counts of exact/within-tolerance/different/unmatched/non-finite comparisons),
`validation_detail.csv` (every non-exact comparison, listed by result key and statistic) and `VALIDATION_STATUS.txt`.

## Failed run

`RUN_SUMMARY.md` headed `FAIL`, `VALIDATION_STATUS.csv` naming the failing component, `run_console.log`; result tables may be absent.

## Post hoc steps (`run_posthoc.ps1`, run_all steps 17-30)

Each run folder `ph_<item>_<date>_<time>_<id>` holds `POSTHOC_STATUS.txt` (every status verbatim, `POSTHOC_OVERALL`, `POSTHOC_LABEL`, `PRIMARY_INFERENCE_EFFECT=NONE ...`), `RUN_SUMMARY.md`, `posthoc_metadata.json`, `posthoc_reference_provenance.csv`, `input_validation_report.csv`, `SOURCE_FINGERPRINT.txt`, `SESSION_INFO.txt` and `RUN_MANIFEST.csv`, plus:

| Item | Main tables |
|---|---|
| exposure_export | `exposure_aggregate_export.csv` (one row per aggregate quantity), `exposure_export_acceptance.csv`, `exposure_export_independent_verification.csv`, `outcome_distribution_summary.csv` |
| deletion | `selected_models.csv` (6), `selected_deletion_refits_with_intervals.csv` (528; keyed P<row> / D<assessment day>, never a participant code), `deletion_comparison_*.csv`, `deletion_si_concordance.csv` |
| l59_bootstrap | `l59_dlm_bootstrap_summary.csv` (18), `l59_bootstrap_full_sample_verification.csv`, `l59_bootstrap_comparison_*.csv`, `l59_bootstrap_si_concordance.csv` |
| no_april | `no_april_cumulative_90.csv`, `no_april_independent_verification.csv`, `no_april_comparison_*.csv`, `no_april_si_concordance.csv` |
| source_agreement | `weather_source_agreement_80.csv`, verification, comparison and SI files |
| exposure_residuals | `exposure_residual_diagnostics_27.csv`, verification, comparison (numeric and categorical) and SI files |
| profile_likelihood | `profile_likelihood_integration_30.csv`, `profile_likelihood_independent_verification.csv` (profile equation at both ends; LRT-interval duality), comparison and SI files |
| early_late | `early_late_interaction_90.csv` (governing q90; q9 informational), `early_late_subgroups_180.csv` (governing q180; q18 informational), `early_late_panel_a.csv`, `early_late_q_family_comparison.csv`, comparison and SI files |
| h3_nested | `H3_NESTED_TEST.csv` (9 rows; the declared columns), `H3_RUN_RECORD.json`, `h3_nesting_checks.csv`, `h3_acceptance_values.csv`, `h3_table3_concordance.csv`, comparison files |
| nearest_station | `nearest_station_variant_summary.csv` (one row per input: declared variant or not, participants matched, lag coverage, fits, SI values agreeing, whether it reproduces SI S9/S10), `nearest_station_variant_differences.csv`; per input `nearest_station_cumulative_120.csv`, `nearest_station_dlm_6.csv`, `nearest_history_coverage.csv`, `nearest_station_si_concordance.csv` (suffix `_<variant>` when more than one input is given); `NEAREST_REPRODUCING_VARIANT` in POSTHOC_STATUS.txt |
| exposure_qa | `exposure_source_identity.csv` (3 rows: TD, RH, rainfall), `exposure_qa_station_coverage.csv` (participant-days with 0-3 covering stations; all and by locality group), `exposure_qa_interpolation_support.csv`, `exposure_qa_distances.csv`, `exposure_qa_station_pool.csv`, `exposure_qa_variable_labels.csv`, `exposure_qa_si_concordance.csv` |
| vif_summary | `vif_per_model_m2l.csv` (240 rows), `vif_per_column_m2l.csv`, `vif_computation_checks.csv`, `vif_si_concordance.csv` |
| calendar_spline | `calendar_spline_cumulative_180.csv` (DATE_NS_DF2 / DF3 x 90; p, p_t, p_primary, q90_calspline), `calendar_spline_independent_verification.csv`, `calendar_spline_reference_comparison_summary.csv` / `_detail.csv`, `calendar_spline_si_concordance.csv` |
| age65 | `age65_cumulative_90.csv` (AGE65 x 90; p, p_t, p_primary, q90_age65), `age65_independent_verification.csv`, `age65_sample.csv`, `age65_si_concordance.csv` |

Expected classes: with the shipped synthetic CSV and `-SyntheticDemo`, every step PASS except PH_PROFILE_LIKELIHOOD (REVIEW, see above); without its reference role a step is NOT_TESTED in run_all. PH_EARLY_LATE uses the SI-declared q families (180 subgroup, 90 interaction tests; author decision 13 Oct 2026).
