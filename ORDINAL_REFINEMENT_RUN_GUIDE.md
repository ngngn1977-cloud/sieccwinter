# Ordinal refinement manual run guide (Windows PowerShell)

Release 1.0.0. You run this yourself on the accepted canonical CSV. It does not rerun the core, Batch A or Batch B analyses, never writes to an accepted run folder or reference bundle, and creates a new private run folder.
R 4.4.3 at `C:\Program Files\R\R-4.4.3\bin\Rscript.exe`; no package installation.

## The command

```powershell
cd <package folder>
.\run_ordinal_refinement.ps1 `
  -DataCsv "<private folder>\conversion_01\siecc_winter_canonical.csv" `
  -BatchBReferenceDir "<private folder>\reference_bundle_batchB_01" `
  -LegacyRunDir "<private folder>\runs\batchb_20261001_134405_0f79" `
  -OutRoot "<private folder>\runs"
```

New folder `ordinalrefine_<date>_<time>_<hex>` under `-OutRoot`. `-LegacyRunDir` is optional and only **read** (its `ordinal_integration.csv` must be reproduced exactly).

## Inputs
| Input | Role |
|---|---|
| `-DataCsv` | accepted canonical CSV (SHA-256 `57E43F26…984F87`, 55 rows) |
| `-BatchBReferenceDir` | `reference_bundle_batchB_01`; only `batchB_ordinal_reference.csv` (80 frozen polr rows) is used, for the legacy-reproduction status |
| `-LegacyRunDir` | the accepted Batch B run (optional, read-only) |
| `config\ordinal_refinement_plan.json` | the fitting protocol and the acceptance criteria, declared before any study-data run; its hash is recorded |

## What the step does
1. Recomputes the **legacy** `MASS::polr` table (default optimiser) and compares it with the frozen reference (strict validator, unchanged tolerance) and, if supplied, with the accepted run's table (exact). This is **LEGACY REPRODUCTION**.
2. Refits the **same 80 specifications** (same participants, M2-L covariates, `ordered(integration_score, 1:4)`, logistic link, Wald z, OR per SD) with **one protocol** (`POLR_SCALED_TIGHT_V1`): predictors centred and scaled internally; `MASS::polr` with `reltol` 1e-14, `maxit` 5000, finite-difference Hessian step 1e-5; up to 3 restarts from its own solution while the Newton decrement exceeds 1e-6. Coefficients, thresholds and the covariance matrix are transformed back exactly (beta = gamma/s; zeta = zeta_s + sum(beta·mu); V = T Vs T').
3. Evaluates, for every refined model, the pre-declared **acceptance criteria** against an independent maximum-likelihood fit (own analytic derivatives): optimiser code 0 (necessary, not sufficient); Newton decrement <= 1e-5; |log-likelihood difference| <= 1e-9; thresholds finite and strictly increasing; polr's Hessian and the analytic observed information positive definite, condition <= 1e12, all-parameter SEs within 1e-5 relative of the analytic ones; all coefficients and thresholds within 1e-6; exposure coefficient within 1e-6 and exposure SE within 1e-6 relative; independent fit converged (gradient <= 1e-8, Newton decrement <= 1e-8); all estimates finite. A model is ACCEPTED only if every criterion holds. The existing Batch B criteria (1e-6 / 1e-4 / 1e-4) are not loosened and are still reported (`inherited_batch_b_criteria_met`).
4. Computes a **separate BH q80 over all 80 refined tests** (`q80_ordinal_refined`; the legacy `q80_ordinal` and the primary cumulative q80 are untouched) and a descriptive comparison with the legacy table.

## Separate statuses (printed; `ORDINAL_REFINEMENT_STATUS.txt`, `VALIDATION_STATUS.csv`)
| Status | Meaning |
|---|---|
| EXECUTION | ran without error |
| INPUT CHECKS | input validation has no FAIL |
| LEGACY REPRODUCTION | legacy table equals the frozen reference and (if supplied) the accepted run's table |
| LEGACY NUMERICAL VERIFICATION | informational: `PASS` or `DOCUMENTED_LIMITATION` (legacy fits not at the maximum by the Batch B rule); never part of pass/fail |
| REFINED NUMERICAL VERIFICATION | `PASS` only if all 80 refined models are ACCEPTED; otherwise `FAIL` (NOT_TESTED is never PASS) |

Refined-vs-legacy is a descriptive comparison, not a status. Exit codes: 0 passed; 1 execution failure; 2 invalid arguments or R missing; 4 executed, but legacy reproduction or refined verification FAILED.

## Outputs (all aggregate; 80 rows unless stated)
`ordinal_refined.csv` (refined results incl. `q80_ordinal_refined`), `ordinal_refined_verification.csv` (one row per model: every measurement, e.g. `newton_decrement_value`; every criterion as an explicit logical field `criterion_<name>_pass` (e.g. `criterion_newton_decrement_pass`); `all_criteria_pass`; unique column names guaranteed), `ordinal_refined_vs_legacy.csv`, `ordinal_legacy_reproduction.csv`, `ordinal_legacy_verification.csv`, `ordinal_refinement_summary.csv`, `ordinal_refinement_validation_summary.csv`/`_detail.csv` (legacy vs frozen), `input_validation_report.csv`, `ORDINAL_REFINEMENT_STATUS.txt`, `ordinal_refinement_metadata.json` (protocol, plan hash, criteria, input hash, config hash, reference hash, MASS version), `SESSION_INFO.txt`, `RUN_SUMMARY.md`, `RUNTIME.csv`, `RUN_MANIFEST.csv` (SHA-256, verified by the finaliser), `MANIFEST.csv`, logs. Runtime: about 25 s (synthetic dry run, 23 s wall clock).

## What to send for review
All the aggregate files above. Never send the study CSV.
