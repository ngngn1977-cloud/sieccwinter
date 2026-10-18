# Phase 3D.3: simulation diagnostic for the DLM simultaneous critical values

Question: do the 0.1% to 0.5% critical-value differences between the package, the independent route and the audited reconstruction come from the orientation (eigenvector signs) of the covariance factor that turns identical standard-normal draws into multivariate-normal draws, rather than from fitting differences?

The diagnostic is read-only with respect to production: it never changes a fit, glm epsilon, simulation method, seed, draw count, reference, criterion or earlier run. It needs the study CSV because the covariance matrices of the lag-basis coefficients were never saved; the production covariance and draws are captured from the unchanged `siw_fit_dlm`. Declared before execution in `config/dlm_simulation_diagnostic_protocol.json`.

## Command

```powershell
cd <package folder>
.\run_dlm_simulation_diagnostic.ps1 `
  -DataCsv "<private folder>\conversion_01\siecc_winter_canonical.csv" `
  -ValidationRunDir "<private folder>\runs\dlmvalidate_20261002_163715_0c0b" `
  -DlmReferenceDir "<private folder>\reference_bundle_dlm_01" `
  -OutRoot "<private folder>\runs"
```

A new `dlmsimdiag_<date>_<time>_<id>` folder is created. Exit codes: 0 all self-checks PASS and the mechanism explained; 3 self-checks PASS but a status is PARTIAL or NOT_TESTED (explicit); 4 a self-check failed; 1 execution failure; 2 invalid arguments.

## Main outputs (aggregate only)

`RUN_SUMMARY.md`, `DLM_SIMDIAG_STATUS.txt`, `dlm_sd_covariance_trace.csv` (seeds, eigenvalues, near-repeated / ill-conditioned flags, asymmetry, decomposition), `dlm_sd_covariance_agreement.csv`, `dlm_sd_factor_reconstruction.csv`, `dlm_sd_sign_alignment.csv` (lag-space cosines: SAME_SIGN / SIGN_FLIPPED / ORIENTATION_DIFFERS), `dlm_sd_critical_comparison.csv` (production, aligned, symmetric square root; flip reproduction), `dlm_sd_sign_pattern_search.csv`, `dlm_sd_mechanism_summary.csv`, `dlm_sd_unexplained_differences.csv`, `dlm_sd_production_replica.csv`, `dlm_sd_metrics_clarification.csv` (relative critical change, relative band half-width change, endpoint change in SE units, absolute endpoint change), `dlm_sd_exactness_accounting.csv` and `dlm_sd_exactness_counts.csv` ("5_OF_9 exact" versus failed checks), plus manifest, fingerprints and session information. No participant-level values.

The symmetric square root is a diagnostic factor only; it never replaces the production draws. PARTIAL and NOT_TESTED are never PASS.
