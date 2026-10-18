# Phase 3D validation V2.1 candidate: corrected covariance rule

**Status:** DLM validation V2.1 is the accepted final L01–L56 validation (author decision 11 Oct 2026); 'pending' wording in generated reports is historical.

V2.1 corrects only the covariance validation rule of V2, after the V2 review showed that its proxy ("half of the largest relative covariance difference", normalised by the largest matrix element) does not measure the relative standard-error difference. It is a **post hoc correction**: not prespecified, and not an unchanged criterion (the covariance metric changes). V1, V2, their protocols, outputs and recorded statuses are preserved. Adoption is pending review.

New rule: for the 3 x 3 cross-basis covariance matrices of the production (candidate) and independent (reference) routes in identical coordinates, `delta_SE = max_j |sqrt(lambda_j) - 1|` over the generalized eigenvalues of `V_candidate v = lambda V_reference v`, which equals the largest relative difference of the standard error of any linear combination of the coefficients. Tolerances are the existing V1 relative SE tolerances (read at run time and recorded with their field names): continuous 1e-7 (`criteria.independent_route.fitted_quantities_rel`), binary at the accepted epsilon 1e-6 (`criteria.independent_route.binomial_fitted_quantities_rel_accepted_epsilon`). Invalid matrices fail; no jitter, clipping or repair; asymmetry is recorded and the symmetric part is used (a standard error depends only on it).

The run reads only aggregate files of the V2 run folder (no data, no refit) and reports exactly which files are missing if any. Direct lag-SE and cumulative-SE checks, the Monte Carlo rule and all earlier statuses are carried forward as labelled previously reviewed evidence, not recomputed.

## Command (single line)

```powershell
Set-Location '<package folder>'; .\run_dlm_validation_v21_covariance.ps1 -V2RunDir '<private folder>\runs\dlmvalidatev2_20261003_095400_4e62' -OutRoot '<private folder>\runs'
```

Outputs (aggregate only): `DLM_V21_STATUS.txt`, `RUN_SUMMARY.md`, `dlm_v21_covariance_checks.csv`, `dlm_v21_provenance.csv`, `dlm_v21_tolerance_source.csv`, `dlm_v21_carried_forward_*.csv`, `dlm_v21_monte_carlo_rule_actuals.csv`, `dlm_v21_cross_implementation_actuals.csv`, `dlm_v21_environment.json`, `dlm_v21_sessionInfo.txt`, manifest and fingerprints. Exit codes: 0 / 3 (new statuses PASS, carried-forward items open: expected) / 4 (a new status failed) / 1 / 2.
