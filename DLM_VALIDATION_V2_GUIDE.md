# Phase 3D validation protocol V2 (candidate)

V2 is a revision introduced after the orientation investigation (PHASE3D2 to PHASE3D4). It does not replace or re-grade V1: the V1 protocol, plan, reports and the recorded FAIL statuses of `dlmvalidate_20261002_163715_0c0b` stay as they are. No tolerance or Monte Carlo criterion is changed; V2 reads them from V1.

It runs the existing V1 validation and the existing simulation diagnostic (nested, unchanged) and reports five separate required checks plus items reported separately:

| Required status | Question |
|---|---|
| PRODUCTION_REPRODUCTION | unchanged code, data, configuration and RNG settings reproduce the accepted fits and the production draws (bitwise) |
| DETERMINISTIC_AGREEMENT | coefficients, SEs, covariance (SD scale), curves, cumulative effects, global tests, q9, peaks, band formula and exclusion counts agree at the existing tolerances |
| ORIENTATION_ALIGNED_SIMULATION | identical standard normals and a documented lag-space sign convention give the same critical values within the covariance allowance; every orientation difference is explained |
| NATIVE_ORIENTATION_MC | the V1 Monte Carlo stability criteria, unchanged; exact differences stay visible but are not the acceptance requirement |
| REFERENCE_DETERMINISTIC / REFERENCE_SIMULATION_MC | the reconstructed reference: deterministic comparisons at the existing tolerances; the simulated critical value by the Monte Carlo criterion (never by a matching sign pattern) |

Reported separately: MANUSCRIPT_CONCORDANCE (3.50 versus 3.55 stays REVIEW), FIGURE_PROVENANCE (NOT_TESTED), DLNM_EQUALITY (NOT_TESTED). The V1 criteria are also computed for the record (`V1_*` lines); they do not decide V2 acceptance. Genuine fitting, covariance, sampling, unresolved-orientation and stability failures are FAIL.

## Command

```powershell
cd <package folder>
.\run_dlm_validation_v2.ps1 `
  -DataCsv "<private folder>\conversion_01\siecc_winter_canonical.csv" `
  -DlmReferenceDir "<private folder>\reference_bundle_dlm_01" `
  -AcceptedRunDir "<package folder>\runs\paper_20260930_195007_26ca" `
  -OutRoot "<private folder>\runs"
```

A new `dlmvalidatev2_<date>_<time>_<id>` folder is created (a few minutes). Exit codes: 0 every status PASS; 3 every required status PASS while a separately reported status is REVIEW or NOT_TESTED (the expected outcome); 4 a required status FAILED; 1 execution failure; 2 invalid arguments.

Main outputs (aggregate only): `DLM_V2_STATUS.txt`, `RUN_SUMMARY.md`, `dlm_v2_checks.csv`, `dlm_v2_check_crosswalk.csv` (old/new checks with rationale), `dlm_v2_exact_differences_visible.csv`, `dlm_v2_covariance_matrices.csv`, `dlm_v2_eigen_orientation.csv`, `dlm_v2_rng_metadata.csv`, `dlm_v2_environment.json`, the nested `v1_component/` and `simulation_diagnostic/` folders, manifest and fingerprints.
