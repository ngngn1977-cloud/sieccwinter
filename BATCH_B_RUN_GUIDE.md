# Batch B manual run guide (Windows PowerShell)

Release 1.0.0. You run this yourself; it reads the accepted canonical CSV and does not need RStudio.
R 4.4.3 at `C:\Program Files\R\R-4.4.3\bin\Rscript.exe`; no package installation (`MASS` and `numDeriv` are already installed; `logistf`, `brglm2`, `sandwich` are not and are not needed). If PowerShell blocks scripts: `Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass`.

## The command

```powershell
cd <package folder>
.\run_batch_b.ps1 `
  -DataCsv "<private folder>\conversion_01\siecc_winter_canonical.csv" `
  -CoreReferenceDir "<private folder>\reference_bundle_01" `
  -BatchAReferenceDir "<private folder>\reference_bundle_batchA_01" `
  -BatchBReferenceDir "<private folder>\reference_bundle_batchB_01" `
  -OutRoot "<private folder>\runs"
```

`-ExpectedAssessmentDays` defaults to 33 (study data). A **new** run folder `batchb_<date>_<time>_<hex>` is created under `-OutRoot`; nothing existing is overwritten.

## Required inputs

| Input | What | Status |
|---|---|---|
| `-DataCsv` | accepted canonical CSV (SHA-256 `REDACTED_PUBLIC_BUILD`, 55 participants) | accepted (Phase 2B) |
| `-CoreReferenceDir` | `reference_bundle_01` (cumulative grid, final L01-L56 DLMs) | accepted core bundle, unchanged |
| `-BatchAReferenceDir` | `reference_bundle_batchA_01` (diagnostics, bivariate, descriptives) | accepted Batch A bundle, unchanged |
| `-BatchBReferenceDir` | `reference_bundle_batchB_01` (Firth 560, ordinal 80, transformations 1,080, nonlinear 360) | **new**, built from the frozen V1.4 aggregate outputs; provenance and hashes in `BATCHB_REFERENCE_PROVENANCE.csv` |

Never send the study CSV or the SAV for review.

## What one run does

1. Runs the **core analyses** and **Batch A** unchanged (1,680 cumulative specifications, nine final L01-L56 DLMs; diagnostics, 240 bivariate tests, descriptives) and compares them with the accepted core and Batch A bundles using the corrected strict validator (same comparisons as the accepted Batch A run).
2. Runs **Batch B** (separate stage, separate tables; the primary results are not touched):
   * **Firth logistic sensitivity** `firth_sensitivity.csv`: 560 = 8 metrics x 10 windows x 7 tiers for `Int_dummy`; Jeffreys-penalised likelihood l(b) + 0.5 log|I(b)|; exposure coefficient, SE from the penalised expected information, Wald interval b +/- 1.96 SE. No p-value and no multiplicity family (none is documented).
   * **Ordinal integration** `ordinal_integration.csv`: 80 = 8 metrics x 10 windows; `MASS::polr` (logistic, `Hess = TRUE`) on `ordered(integration_score, 1:4)`, M2-L; Wald p, OR per SD; BH over the 80 stored as `q80_ordinal` (**separate** from the primary cumulative q80).
   * **Exposure transformations** `exposure_transformations.csv`: 1,080 = 3 outcomes x (6 mean-type metrics x 4 + 2 sum-type metrics x 6) x 10 windows; raw, z, signed-log z, exp z (+ log1p, sqrt for rainfall total and rainy-day count); BH per outcome (360) and global (1,080).
   * **Nonlinear forms** `nonlinear_forms.csv`: 360 = 3 outcomes x 6 mean-type metrics x 10 windows x 2 forms (centred quadratic; natural spline df 2); joint Wald; BH per outcome x form (60) and global (360).
3. **Independent mathematical verification** of every Batch B specification (`firth_verification.csv`, `ordinal_verification.csv`, `transformation_verification.csv`, `nonlinear_verification.csv`, summary `batch_b_math_verification.csv`).
4. Compares the Batch B tables with the Batch B bundle (separate plan `config/validation_plan_batchB.json`; every reference is required).
5. Writes versions, configuration/input/plan/reference hashes, session information and an output manifest into the same run folder.

## Separate statuses (printed, and written to `BATCH_B_STATUS.txt` and `VALIDATION_STATUS.csv`)

| Status | Meaning |
|---|---|
| EXECUTION | the workflow ran without error and all internal self-checks passed |
| INPUT CHECKS | input validation and assessment-day checks have no FAIL (the known rainfall cell is a WARN) |
| CORE NUMERICAL COMPARISON | all required core references PASS (archived L59 reference is declared out of scope) |
| BATCH A NUMERICAL COMPARISON | all five Batch A references PASS |
| BATCH B NUMERICAL COMPARISON | all four Batch B references PASS. `PASS_WITH_DOCUMENTED_LEGACY_LIMITATION` if Firth coefficient/interval cells beyond the cumulative tolerance are explained by the frozen fits' own recorded convergence (see below) |
| BATCH B MATHEMATICAL VERIFICATION | independent checks of the new implementations (separate from agreement with the frozen estimates). `PASS`, `PASS_WITH_DOCUMENTED_LIMITATION` (ordinal fits at which `MASS::polr` stopped short of the likelihood maximum), `FAIL`, or `NOT_TESTED` (never counted as PASS) |

Exit codes: 0 everything passed; 1 execution failure; 2 invalid arguments or R missing; **3 executed, every comparison and verification passed, with at least one documented legacy limitation**; 4 executed, but an input check, a comparison or a verification FAILED.
A comparison FAIL never changes a result; read `batch_b_validation_detail.csv` / `validation_detail.csv` / `batch_a_validation_detail.csv`.

## Expected on the study data (from the frozen references; nothing is forced)

* Counts: firth 560; ordinal 80; transformations 1,080; nonlinear 360; verification tables 560 / 80 / 1,080 / 360.
* Reference records: 560 / 80 / 1,080 / 360. Tolerances: the existing `cumulative` set (exact 1e-12; within 1e-7 abs + 1e-6 rel); none is changed or added.
* Ordinal, transformations, nonlinear: expected EXACT or WITHIN_TOLERANCE (the same software and data; core cumulative differences were 202 within-tolerance cells of 43,690). `MASS::polr` uses a numerical optimiser, so a few "within tolerance" cells are normal.
* **Firth: read this.** The frozen Firth iteration stops when the parameter change is <= 1e-8 after step-halving, but its recorded adjusted score is up to about 8e-5 (median 1.5e-6; 551 of 560 fits above 1e-8). The new implementation converges to a scaled adjusted score <= 1e-10, so the frozen estimates are the penalised maximum only to about I^-1 U (1e-7 to 1e-6 in the synthetic comparison; **all 2,240 synthetic cells were within the cumulative tolerance**). If any coefficient or interval-limit cell on the real data exceeds the cumulative tolerance, it is classified DIFFERENT and then tested **only for those cells** against the first-order bound implied by the frozen fit's own recorded score; cells inside it are labelled `DIFFERENT_EXPLAINED_BY_LEGACY_CONVERGENCE` (listed in `firth_attribution.csv`) and the reference status is `PASS_EXPLAINED_BY_LEGACY_CONVERGENCE` (reported as `PASS_WITH_DOCUMENTED_LEGACY_LIMITATION`, exit code 3). Cells outside the bound and every standard-error cell FAIL. The number of cells strictly beyond the unchanged cumulative tolerance is `n_different + n_explained_by_legacy_bound` in `batch_b_validation_summary.csv`; if you regard the bound as a change of tolerance, treat `PASS_WITH_DOCUMENTED_LEGACY_LIMITATION` as a FAIL.
* **Ordinal: read this.** `MASS::polr` (default optimiser tolerance) can stop short of the maximum: in the synthetic data 8 of 80 fits (long windows) had a log-likelihood up to 1.6e-4 below the maximum and standard errors that differ from the maximum-likelihood values. The package reports the polr result as returned (the documented specification), records the Newton decrement of every fit, and lists the independent maximum-likelihood values in `ordinal_verification.csv` with the status `POLR_NOT_AT_MAXIMUM`. This does not change the comparison with the frozen polr output, but it is a property of the frozen results too.
* NOT TESTED (no authoritative reference or software): agreement of the Firth estimates with a Firth package (`logistf`, `brglm2` are not installed); Firth profile-likelihood intervals and p-values (not documented, not produced).

## What to send for review (aggregate only)

`RUN_SUMMARY.md`, `VALIDATION_STATUS.csv`, `BATCH_B_STATUS.txt`, `INPUT_VALIDATION.csv`, `validation_summary.csv`, `validation_detail.csv`, `batch_a_validation_summary.csv`, `batch_a_validation_detail.csv`,
`batch_b_validation_summary.csv`, `batch_b_validation_detail.csv`, `firth_attribution.csv` (if written), `batch_b_math_verification.csv`, `RUNTIME.csv`, `RUN_MANIFEST.csv`, `SESSION_INFO.txt`, `batch_b_metadata.json`, `batch_a_metadata.json`, `run_metadata.json`,
the Batch B result tables (`firth_sensitivity.csv`, `firth_verification.csv`, `ordinal_integration.csv`, `ordinal_verification.csv`, `exposure_transformations.csv`, `transformation_verification.csv`, `nonlinear_forms.csv`, `nonlinear_verification.csv`) and the Batch A tables.
Runtime: about 2 minutes (synthetic 60-participant dry run, 110 s wall clock: core 7 s, Batch A 11 s, Firth 23 s including verification, ordinal 3 s, transformations 8 s, nonlinear 2 s; the rest is R start-up, file hashing and finalisation).
