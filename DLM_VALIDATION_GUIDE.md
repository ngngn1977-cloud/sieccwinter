# Phase 3D: validation of the nine final L01-L56 DLM outputs (manual run)

What it checks (declared in `config/dlm_validation_protocol.json` and `config/validation_plan_dlm.json` before any study run): lag basis, lag-specific estimates and SEs, cumulative curves and the L56 cumulative effect, pointwise and simultaneous 95% bands, simultaneous critical values and descriptive peak lags of the nine M2-L models (3 outcomes x UTCI, IMS relative humidity, IMS rainy-day count). It reuses the accepted fitting code and settings (5,000 MVN draws, `MASS::mvrnorm`, seed `20260729 + 100 * metric_index + outcome_index`, type-8 quantile, glm epsilon 1e-12) and adds exact reproduction of the accepted run, a strict comparison with the **audited L56 reconstruction** (Phase 1; not an original deposited result), an independent route (own B-spline basis, own least squares / Newton fit, own HC3, own cumulative sums, own MVN draws) and Monte Carlo stability.

What it does not do: redraw any figure, touch the archived L01-L59 analyses, modify the accepted run or the reference bundle, edit the manuscript/SI, or read anything but the canonical CSV, the accepted run's DLM tables and the reference bundle. Figure 6 provenance and (if dlnm is not installed) equality with dlnm are reported NOT_TESTED, never PASS.

## Command

```powershell
cd <package folder>
.\run_dlm_validation.ps1 `
  -DataCsv "<private folder>\conversion_01\siecc_winter_canonical.csv" `
  -DlmReferenceDir "<private folder>\reference_bundle_dlm_01" `
  -AcceptedRunDir "<package folder>\runs\paper_20260930_195007_26ca" `
  -OutRoot "<private folder>\runs"
```

A new folder `dlmvalidate_<date>_<time>_<id>` is created under `-OutRoot`. Add `-NoMonteCarlo` only for a quick check (the Monte Carlo status then stays NOT_TESTED). Expected run time: a few minutes.

## Statuses (separate)

EXECUTION, INPUT CHECKS, ACCEPTED FIT REPRODUCTION, REFERENCE COMPARISON, LAG BASIS, CURVES, CUMULATIVE, PEAK LAG, SIMULTANEOUS EXACT SEEDED, SIMULTANEOUS MC STABILITY, CONVERGENCE SENSITIVITY, MANUSCRIPT CONCORDANCE, FIGURE PROVENANCE, DLNM EQUALITY, SOURCE INTEGRITY.

Exit codes: 0 every status PASS; **3 every tested status PASS but some components are NOT_TESTED (the expected outcome while Figure 6 has no provenance)**; 1 execution failure; 2 invalid arguments or R missing; 4 a status FAILED or needs REVIEW.

Exact seeded reproduction (same seed, draw count, MVN method; a tiny covariance difference can flip an eigenvector sign and change the realised draws) is reported separately from Monte Carlo stability; a cross-implementation critical value that is not exact is labelled `MC_CONSISTENT_NOT_EXACT`, never `EXACT`.

Send the output folder (aggregate tables only; no participant-level values) for review.
