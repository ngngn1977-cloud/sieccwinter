# Phase 3E: archived L01-L59 grid (168) and lag-form sensitivity (216): validation against the frozen V1.4 outputs

Separate from the nine final L01-L56 models (accepted under V2.1, PHASE3D8). Nothing here extends that acceptance, and no archived result is ever combined with a final model.

## What the run does
* Fits the 168 archived grid models (3 outcomes x 8 metrics x 7 tiers, L01-L59, df 3) and the 216 lag-form sensitivity models (24 pairs x lag ends 35/45/59 x df 2/3/4) with the **unchanged** package functions (`siw_archived_dlm_grid`, `siw_archived_dlm_sensitivity`, `siw_fit_dlm`), with the archived glm epsilon (1e-10, maxit 200, traced to the V1.4 source) set on a **copy** of the configuration. No simulated draws are made.
* Compares deterministic quantities with the five frozen V1.4 files (SHA-256 declared in `config/archived_dlm_validation_protocol.json`): n, event count, lag range, global statistic / df / p, cumulative effect, SE and CI, peak effect, rank, parameters, residual df, q24, q9, q216, lag-specific and cumulative curves (estimate, SE, pointwise 95% limits), plus status, convergence and separation flags and the peak lag (exact, or a numerical tie).
* Verifies the nine lag bases (L = 35, 45, 59; df = 2, 3, 4) against the independent construction; dlnm equality is NOT_TESTED (dlnm is not installed).
* Recomputes the three multiplicity families with `stats::p.adjust` BH from the reference and from the package p values and checks the 24 `L59_DF3` specifications against the 24 `M2_L` grid models.
* Preserves the supplied rainfall missingness (strict complete history; nothing imputed; the data passed to the fits are verified unchanged).

## What it does not do (reported as NOT_VALIDATED, never PASS)
Simultaneous critical values (168), simultaneous bands (9,912 curve rows), `any_simultaneous_band_excludes_null` and the derived `evidence_status` of the archived grid: Monte Carlo quantities that need a separate protocol. No independent-route verification of the archived fits. Condition numbers and exponentiated estimates are not produced by the package and are NOT_TESTED. Manuscript concordance stays REVIEW; figure provenance and dlnm equality stay NOT_TESTED.

## Command (single line)

```powershell
Set-Location '<package folder>'; .\run_archived_dlm_validation.ps1 -DataCsv '<private folder>\conversion_01\siecc_winter_canonical.csv' -ReferenceDir '<WINTER_SOURCE>\outputs\SIECC_FULL_20260801T125700\models' -OutRoot '<private folder>\runs'
```

Exit codes: 0 all statuses PASS; 3 all compared statuses PASS with the separate-validation items open (the expected best outcome); 4 a status FAILED or evidence is missing (a missing or non-matching reference file fails the input-provenance status and nothing is compared); 1 execution failure; 2 invalid arguments.

Outputs (aggregate only): `ARCHIVED_DLM_STATUS.txt`, `RUN_SUMMARY.md`, `archived_comparison_summary.csv` / `_coverage.csv` / `_detail.csv`, `archived_model_level_comparison.csv`, `archived_stratum_summary.csv`, `archived_peak_lag_comparison.csv`, `archived_flag_comparison.csv`, `archived_internal_consistency_and_multiplicity.csv`, `archived_lag_basis_verification.csv`, `archived_reference_provenance.csv`, `archived_not_validated.csv`, `archived_metadata.json`, `archived_environment.json`, manifest and fingerprints.
