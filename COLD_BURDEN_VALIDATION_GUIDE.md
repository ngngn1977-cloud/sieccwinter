# Phase 3F: exploratory cold-burden scan (22,302 models): reproduction and mathematical verification

Separate from the nine final L01-L56 models (V2.1) and the archived DLM validation (Phase 3E). The scientific interpretation of the scan is **not assessed**; no cold-burden model is promoted.

## What the run does
* Computes the 15 thresholds (pooled type-7 percentiles P10 / P05 / P025 of UTCI, MRT, ERA5 air temperature and IMS temperature over L01-L59, plus UTCI < 9, < 0, < -13) and fits the 14 thresholds with at least 20 participant-days below: 14 x 59 windows x 9 burden/transformation variants x 3 outcomes = 22,302 models (M2_L) with the validated engine (`siw_fit_gaussian`, `siw_fit_logistic`, `siw_model_diagnostics_one`). Burden features of all windows come from one pass over the lags; the design and reduced models are built once.
* **Numerical reproduction**: compares 892,943 numeric cells (54 fields) plus 57 threshold cells and nine categorical fields with the frozen V1.4 `cold_burden_results.csv` and `cold_thresholds.csv` (SHA-256 declared in `config/cold_burden_validation_protocol.json`; the bundle `reference_bundle_coldburden_01` holds hash-verified byte copies). Tolerance class: the cumulative class (exact 1e-12; within 1e-7 abs + 1e-6 rel). 3,450 infinite-equal pairs (condition number and VIF of the 1,725 non-estimable models) are declared.
* **Mathematical verification** (separate statuses): independent per-participant burden features for every threshold, window and participant; independent type-7 thresholds; scale and inference identities (standardised and IQR effects, odds ratios, p values, confidence intervals) for the package and the frozen file; BH q_global / q_outcome / q_curve recomputed with `p.adjust`; an independent formula-based refit of a deterministic sample (every 40th model, the SI-reported model, the first non-estimable model); the SI statement (exactly one globally supported row: UCLA / MRT / P025 / mean cold intensity / 29 d, q about 0.00015, 8 participants with positive burden, leverage about 0.92, 6 influential cases).
* Descriptive support, leverage, influence and separation flags are written for review (`coldburden_descriptive_flags.csv`). Numerical agreement does not show that sparse, influence-sensitive or separated fits give reliable inference.

## Command (single line)

```powershell
Set-Location '<package folder>'; .\run_cold_burden_validation.ps1 -DataCsv '<private folder>\conversion_01\siecc_winter_canonical.csv' -ReferenceDir '<private folder>\reference_bundle_coldburden_01' -OutRoot '<private folder>\runs'
```

Expected: a new `coldburden_<date>_<time>_<id>` folder in about 2-4 minutes. Exit 3 = all statuses PASS with the scientific interpretation not assessed (expected best case); 4 = a status failed or evidence is missing; 1 execution failure; 2 invalid arguments.

Outputs (aggregate only): `COLDBURDEN_STATUS.txt`, `RUN_SUMMARY.md`, `coldburden_comparison_summary.csv` / `_coverage.csv` / `_detail.csv`, `coldburden_categorical_comparison.csv`, `coldburden_package_results.csv` and `coldburden_package_thresholds.csv` (model-level statistics, no participant values), `coldburden_feature_verification.csv`, `coldburden_threshold_verification.csv`, `coldburden_scale_and_inference_identities.csv`, `coldburden_multiplicity_verification.csv`, `coldburden_independent_refit.csv`, `coldburden_si_statement_check.csv`, `coldburden_descriptive_flags.csv`, `coldburden_not_validated.csv`, `coldburden_reference_provenance.csv`, `coldburden_metadata.json` (runtimes), `coldburden_environment.json`, manifest and fingerprints.
