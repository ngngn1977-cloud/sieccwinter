# ANOM equal-n analytic-sample diagnostic: declaration (Phase 3I.2)

Declared on 6 October 2026, before any study-data execution. The SHA-256 of this file, the script and the synthetic test is printed by the synthetic test run and recorded in the study output metadata.

**Status.** This is a post hoc diagnostic. It fits no model. It changes no status, tolerance, protocol, reference or result. The run `anomhist_20261004_141846_28bd` keeps `ANOM_VERIFICATION_STATUS=FAIL`. Its files are verified against its `RUN_MANIFEST.csv` before and after, and they are never written.

## 1. The failure being diagnosed

The identity row "gaussian standardised_effect: implied SD(outcome) constant within each outcome" failed in two rows (package and frozen). The ANOM grid is first split by `n` (`R/daily_layer.R .siw_dl_identities`). Within each `n` group, `R/cold_burden.R siw_cold_identities` requires `max |z/z[1] - 1| <= 1e-9`, where `z = coefficient x exposure_sd / standardised_effect`. Among the 1,080 gaussian n = 54 rows (all in the locality reference), windows 1-4 and 37-59 carry two implied SDs. The relative difference is 0.0020863. The leading explanation is two different complete-case samples of size 54. So far this is an inference, not a measurement.

## 2. Traced specification (code, not assumption)

- **Anomaly values.** `R/anomaly_history.R siw_anomaly_values(reference = "locality")` groups calendar day x `Locality_3group`. Calendar day is `assessment_day - lag`. The reference is the leave-one-out mean of the group's finite values. It is NA for a non-finite value or when the group has fewer than two finite values (a singleton). V1.4 `R/120_daily_anomaly.R siecc_daily_anomaly_values` does the same, using `assessment_date - lag`. The `ANOMDIAG_64` comparison reproduced exactly.
- **Methods.** `.siw_ah_methods`: stable anomaly for every metric. The zero-safe log anomaly is added for `IMS_rainfall_total` and `IMS_rainy_day_count_ge_1_mm`.
- **Features.** `siw_anomaly_features`: NA if fewer than `ceiling(0.8 x W)` of lags 1..W are finite. Otherwise it is the signed or absolute mean (V1.4 `siecc_build_anomaly_feature`).
- **Complete cases.** `R/cold_burden.R .siw_cold_one`: `mask = tier_ok & is.finite(y) & is.finite(x)`, with `tier_ok = complete.cases(siw_tier_matrix(dat, M2_L))`. V1.4 `030_model_engine.R` uses `complete.cases(outcome, exposure, covariates)`.
- **Scale fields.** `R/engine.R siw_fit_gaussian` sets `standardised_effect = b x sd(x) / sd(y)` and `exposure_sd = sd(x)`, both over that model's complete cases (V1.4 `030_model_engine.R`, lines 557-559). The implied SD is therefore algebraically SD(y) of each model's own complete-case sample. This diagnostic measures that SD directly instead of assuming it.

## 3. Inputs (read only; the diagnostic fails closed before writing anything)

| Input | Requirement |
|---|---|
| canonical CSV | SHA-256 equals the failed run's `input_hash` and the accepted `REDACTED_PUBLIC_BUILD` |
| failed run folder | the five files read (`anom_package_results.csv`, `anom_scale_and_inference_identities.csv`, `anomhist_metadata.json`, `source_manifest_anomhist.csv`, `ANOMHIST_STATUS.txt`) match `RUN_MANIFEST.csv`, before and after |
| frozen reference | `daily_anomaly_results.csv` SHA-256 equals the protocol declaration (`4311E613...ADB2`) |
| protocol and configuration | hashes equal the failed run's (`8974AB58...275F`, `5A8C6054...DC5DE`) |
| traced source files | the 13 files listed in `ASD_DEPENDENCIES` are byte-identical to the failed run's source manifest. Equality of the full source fingerprint, and any changed paths, are reported. |
| output folder | new or empty, and outside the run, reference, data and package folders |

## 4. Computation

The diagnostic covers the locality reference only, windows 1, 4, 5, 36, 37 and 59. Windows 5 and 36 are n = 55 controls. It runs over all 10 metric-method combinations x 2 burdens x 2 gaussian outcomes, giving 240 rows. It does not pick a representative metric, because the full set costs almost nothing. For each row it computes:

- the traced mask and `n`;
- the literal per-participant feature route as a check of that mask;
- the direct `sd(y[mask])`;
- the implied SD from the failed run's row and from the frozen row;
- the relative differences, using the identity's own scaling `|a - b| / |b|`;
- a label for the distinct complete-case set (in memory only).

The unchanged constancy rule is re-evaluated on these rows grouped by `n`, which reproduces the failure. It is then evaluated grouped by the actual set. That second grouping is diagnostic only and is not a revised check.

## 5. Output (aggregate only)

| File | Content |
|---|---|
| `ANOMDIAG_STATUS.txt` | the decision, the checks D1-D9, input/run/source statements |
| `anomdiag_rows.csv` | outcome, metric, method, burden, window, set label, n, excluded count, recorded n (run, frozen), statuses, mask-route agreement, direct SD, implied SDs, relative differences |
| `anomdiag_sets.csv` | for each set and outcome: n, excluded count, windows, rows, direct SD, implied SD ranges, maximum differences |
| `anomdiag_set_pairs.csv` | for each pair of sets: sizes, intersection and symmetric-difference **counts** |
| `anomdiag_window_summary.csv` | for each outcome and window: n values, set labels, SD ranges |
| `anomdiag_identity_check.csv` | the constancy rule by n (these rows), by actual set (diagnostic only), and as recorded in the failed run |
| `anomdiag_checks.csv`, `anomdiag_failed_run_integrity.csv`, `anomdiag_traced_source_files.csv`, `anomdiag_metadata.json`, `MANIFEST.csv` | checks, file hashes, provenance |

The output never contains a participant code, value, date or locality, a membership list or index, or a hash of membership. Set labels are `FULL`, `S1`, `S2`, ... in order of first appearance. A privacy guard fails closed on identifying column names and on any cell equal to a participant code.

## 6. Checks and decision (declared)

D1-D5 are required.

- **D1.** Every row is a COMPLETED gaussian model in the run and in the frozen file.
- **D2.** Mask n equals the recorded n in both.
- **D3.** The vectorised and literal feature routes give the same mask.
- **D4/D5.** Direct SD equals the implied SD of the run / of the frozen file, with relative difference ≤ 1e-9. This is the identity's unchanged tolerance.

D6-D9 are descriptive.

- **D6.** Two or more different sets share an n.
- **D7.** The by-n rule fails on these rows.
- **D8.** The by-set rule passes.
- **D9.** The by-n value equals the recorded failing value.

| Decision | Condition | Meaning / next step |
|---|---|---|
| `BLOCKING_MASK_NOT_REPRODUCED` | D1, D2 or D3 fails | The tracing or the inputs differ. Nothing is concluded. |
| `BLOCKING_IMPLIED_SD_NOT_EXPLAINED` | D4 or D5 fails | Blocking. Investigate without widening any tolerance. |
| `PREMISE_DISPROVEN_EQUAL_N_DIFFERENT_SAMPLES` | D1-D5 pass, D7 fails, D6 = yes, D8 passes | The equal-n = same-sample premise is false. Revising the check requires a new declared post hoc protocol, adversarial equal-n regression tests and an author decision. The failed run stays FAIL and is preserved. |
| `BLOCKING_INCONSISTENT` | D1-D5 pass, D7 fails, but D6 or D8 does not support the explanation | Blocking. |
| `NO_EQUAL_N_CONFLICT_IN_SELECTED_ROWS` | D1-D5 pass and D7 passes | No conflict in these rows. Not expected for the study data. |

## 7. Synthetic tests (`validation/test_anom_sample_sd_diag_synthetic.R`)

The synthetic data create equal-n different samples by locality-day singletons, the suspected mechanism itself:

- one participant alone on its locality's last day, so no feature for windows 1-4;
- one participant who is first in its locality 30 days before the next, so no feature for windows 37-59.

The tests are:

- **T1.** The expected decision, hand-computed SDs per set, the symmetric difference of 2, and the hand-computed failing value. No participant code appears in the output, and the run folder is unchanged.
- **T2.** An identical-sample control.
- **T3/T4.** Wrong-SD negatives in the run and in the frozen file.
- **T5.** A wrong-n negative.
- **T6.** Fail-closed guards: another data file, a non-accepted CSV, an overlapping or non-empty output folder, another protocol, a tampered run file, a tampered frozen file.
- **T7.** The command-line entry point: the usage error, and the accepted-CSV guard refusing synthetic data while writing nothing.

The tests use no study data.
