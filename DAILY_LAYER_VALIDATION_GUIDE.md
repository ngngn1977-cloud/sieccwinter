# Phase 3H / Batch D: the archived daily-lag layer (XYD, DAY, MAXT)

Horizon L01-L59 (archived); separate from the nine final L01-L56 models, the archived DLMs (Phase 3E) and the cold-burden scan (Phase 3F). Specification traced from `WINTER_SOURCE` (`R/060_xy.R`, `R/070_cumulative_models.R`, `R/100_resampling.R`) and declared in `config/daily_layer_validation_protocol.json` before any run.

## Components
* **XYD (1,416)**: 3 outcomes x 8 metrics x lags 1-59, crude: Pearson (point-biserial for the binary outcome), Spearman and Kendall (`cor.test`, `exact = FALSE`), the M0 regression (exposure only), BH q global (1,416) and within outcome (3 x 472).
* **DAY (1,416)**: the same grid, adjusted M2_L single-day regression on the raw daily exposure, validated engine and diagnostics, q59 (24 families of 59), q472 (3 families of 472), q_global.
* **MAXT (240)**: for each outcome separately, the single-step maximum of the 80 |Pearson r| of the cumulative exposures (8 metrics x 10 windows); the OUTCOME is permuted across participants (`sample(y)`, unrestricted); 10,000 replicates; seed = 20260729 + 1000 x outcome index (20261729 / 20262729 / 20263729); p = (1 + #{max >= observed}) / (1 + #finite maxima). The optimised engine keeps the original sampling sequence; every replicate within 1e-12 of an observed value is recomputed with the original `cor` route; the whole original route is also run for all 3 x 10,000 replicates and compared.

## Statuses
Reproduction (against the frozen V1.4 tables; XYD and DAY at the cumulative tolerance class, MAXT **exact**), independent verification (XYD all rows: Pearson, Spearman, own Kendall, own regression; DAY: identities, BH families, sampled refit; MAXT: engine vs original route), **MAXT assumptions = REVIEW** (an assessment, never a pass), provenance, coverage, source integrity. J02 (glm epsilon entry points) stays OPEN; nothing is changed.

## Command (single line)

```powershell
Set-Location '<package folder>'; .\run_daily_layer_validation.ps1 -DataCsv '<private folder>\conversion_01\siecc_winter_canonical.csv' -ReferenceDir '<private folder>\reference_bundle_dailylag_01' -OutRoot '<private folder>\runs'
```

Exit 3 = all reproduction and verification statuses PASS with the MAXT assumptions under REVIEW (expected best case); 4 = a status failed or evidence is missing; 1 execution failure; 2 invalid arguments. Expected run time about 1.5-3 minutes. Outputs (aggregate only): `DAILYLAYER_STATUS.txt`, `RUN_SUMMARY.md`, comparison summary / detail / coverage per component, categorical comparisons, package results, independent verification tables, `maxt_replicate_accounting.csv`, `maxt_engine_verification.csv`, `maxt_assumption_assessment.csv`, provenance, RNG note, metadata, manifest and fingerprints.

## Wrapper status handling (Phase 3H.2)
`MAXT_ASSUMPTIONS_STATUS = REVIEW` is the only permitted non-PASS value and only for that status: it is shown in yellow and retained, and gives exit 3 when every other required status is PASS and the R exit code is 3. Any other non-PASS, missing, NOT_TESTED or unexpected status gives exit 4; an R failure, a missing `EXECUTION_STATUS`/status file or an R exit code other than 0, 3, 4 gives exit 1; a status/exit-code inconsistency is never accepted. The actual R exit code is recorded as `R_EXIT_CODE=` in `run_console.log`. Test: `validation/test_daily_layer_wrapper.ps1` (synthetic only).
