# Phase 3I: the exploratory anomaly (ANOM) and alternative-history (ALT) families

Exploratory, separate from primary inference, the nine final L01-L56 models and every earlier family. Numerical reproduction against the frozen V1.4 output and independent mathematical verification only; no scientific interpretation. Specification traced from `WINTER_CODE` (`R/120_daily_anomaly.R`, `R/130_alternative_methods.R`) and declared in `config/anomaly_history_validation_protocol.json` before any run.

## What is compared
* **ANOM, 7,080 models** (`daily_anomaly_results.csv`): 2 references (all participants / same locality) x 8 metrics x methods (stable anomaly for every metric; zero-safe log anomaly additionally for the two rainfall metrics) x 2 burdens (signed / absolute mean) x windows 1-59 days x 3 outcomes. Reference of a participant-day = mean of the other participants on the same calendar day (leave-one-out; calendar day = `assessment_day` - lag); stable anomaly = deviation / pooled within-day SD; window feature = mean over the window, missing if fewer than 80% of the days are available; adjusted M2_L single-exposure model. q_global / q_outcome / q_curve (59 windows per curve, 120 curves). Supporting table: `daily_anomaly_diagnostics.csv` (64 rows).
* **ALT, 312 tests** (`alternative_method_results.csv`): per metric and outcome 13 tests on the L01-L56 history: a joint test of three disjoint periods (1-7, 8-28, 29-56 d), five weighted cumulative features, four trajectory contrasts, three trajectory-PCA components; q_global / q_outcome. Supporting table: `alternative_PCA_variance.csv` (439 rows).
* Existing deterministic tolerance class (exact 1e-12; within 1e-7 abs + 1e-6 rel). A missing reference, duplicate key, unmatched cell or unclassified comparison fails. The sign of a principal component is compared as it is; a sign difference fails and is shown separately in `alt_pca_sign_diagnostic.csv`.

## Statuses (exit 0 only when all eight are PASS)
EXECUTION, INPUT_PROVENANCE, ANOM_REPRODUCTION, ANOM_VERIFICATION, ALT_REPRODUCTION, ALT_VERIFICATION, COVERAGE, SOURCE_INTEGRITY. There is no REVIEW exception in this batch. Verification coverage: ANOM anomaly values, features, q values and identities on the whole grid and an independent lm/glm refit of **every** model (parameter `independent_refit_every_anom` = 1); ALT features, independent refits of all 288 single-feature models and all 24 joint tests, PCA from an independent eigen decomposition.

## Command (single line)
```powershell
Set-Location '<package folder>'; .\run_anomaly_history_validation.ps1 -DataCsv '<private folder>\conversion_01\siecc_winter_canonical.csv' -ReferenceDir '<private folder>\reference_bundle_anomhist_01' -OutRoot '<private folder>\runs'
```
Exit codes: 0 all PASS; 4 a status failed or evidence is missing; 1 execution failure or inconsistent outcome; 2 invalid arguments or R missing. The R and wrapper outcomes are saved in `HANDOFF_OUTCOME.txt` in the run folder, written after `RUN_MANIFEST.csv` and deliberately not listed in it (the manifest stays valid). Tests: `validation/test_anomaly_history_wrapper.ps1` (synthetic only).

## Protocol v2: the ANOM identity check (post hoc, 12 Oct 2026)

Declared in `config/anomaly_history_validation_protocol_v2.json` before any study run under it, by author decision. It revises **only** the gaussian implied-SD identity. Protocol v1 is unchanged and governs everything else; v2 refuses to load if v1 changes.

* **Reason.** The v1 row "gaussian standardised_effect: implied SD(outcome) constant within each outcome" is evaluated within groups of equal n, and so assumes that equal n means the same complete-case sample. The declared diagnostic disproved this:
  * run_20261012_114559_4737, step 15, decision `PREMISE_DISPROVEN_EQUAL_N_DIFFERENT_SAMPLES`;
  * checks D1-D5 PASS;
  * the direct SDs equal the implied SDs to <= 7.5e-15;
  * the two n = 54 sets differ by one participant each.
* **v2 rule.** For every COMPLETED gaussian ANOM row (package and frozen), the implied SD (coefficient x exposure_sd / standardised_effect) is compared with the directly measured outcome SD of that row's own complete-case sample.
  * The sample is the traced mask of `validation/anom_sample_sd_diag.R`: tier covariates complete, outcome finite, feature finite.
  * Relative difference: |a - b| / |b|. Tolerance: the unchanged 1e-9.
  * A row with no traced mask, a mask size different from the recorded n, or a non-finite SD FAILS.
* **Statuses.**
  * `ANOM_VERIFICATION_STATUS_V2` is the v1 components with the v1 by-n row replaced by the v2 rule. It governs the wrapper's exit code.
  * `ANOM_VERIFICATION_STATUS` (the v1 composition) and the v1 by-n row are still computed and shown, marked "V1 identity (superseded, kept for the record)".
  * Earlier runs keep their own FAIL records.
* **New files.** `anom_identity_v2_rows.csv` and `anom_scale_and_inference_identities_v2.csv`. The v1 file `anom_scale_and_inference_identities.csv` is unchanged.
* **Recorded explicitly, unchanged.** The near-zero scaling |a - b| / max(1, |b|) of the anomaly-value verification (threshold 1e-9) and of the window-feature verification (threshold 1e-12).