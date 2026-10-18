# Batch C manual run guide (Windows PowerShell): primary cumulative bootstrap

Release 1.0.0. You run this yourself on the accepted canonical CSV. It does **not** rerun the core, Batch A, Batch B or ordinal steps; accepted run folders and reference bundles are only read. It creates a new private run folder.
R 4.4.3 at `C:\Program Files\R\R-4.4.3\bin\Rscript.exe`; no package installation. Expected duration: about **2–4 minutes** (synthetic benchmark: 110 s for 360,000 refits including verification).

## The command

```powershell
cd <package folder>
.\run_batch_c.ps1 `
  -DataCsv "<private folder>\conversion_01\siecc_winter_canonical.csv" `
  -BatchCReferenceDir "<private folder>\reference_bundle_batchC_01" `
  -AcceptedRunDir "<private folder>\runs\batchb_20261001_134405_0f79" `
  -OutRoot "<private folder>\runs"
```

A **new** folder `batchc_<date>_<time>_<hex>` is created. `-AcceptedRunDir` is optional and read only: the 90 M2-L point estimates recomputed here must equal its `cumulative_results.csv`.
If the run is interrupted, repeat the command with `-ResumeRunDir "<that run folder>"`: finished targets are reused only if the input, configuration, protocol and **package source** are unchanged (checkpoints are checked by signature); anything else is recomputed.

## Inputs
| Input | Role |
|---|---|
| `-DataCsv` | accepted canonical CSV (SHA-256 `57E43F26…984F87`, 55 participants, 33 assessment dates) |
| `-BatchCReferenceDir` | new `reference_bundle_batchC_01`: the 180 frozen bootstrap summaries (provenance, hashes, plan and not-tested list inside) |
| `-AcceptedRunDir` | optional accepted run (read only) for the point-estimate identity check |
| `config\bootstrap_protocol.json`, `config\validation_plan_batchC.json` | the declared protocol and the strict comparison plan; their hashes are recorded |

## What it does
1. Builds the **180 target–scheme combinations**: the 90 primary M2-L cumulative models (3 outcomes × UTCI, IMS relative humidity, IMS rainy-day count × 10 windows) × **participant bootstrap** and **assessment-date-cluster bootstrap** (33 dates from `assessment_day`; a sampled date keeps all its participants, a repeated date repeats them), **2,000 resamples** each.
2. Reproduces the original procedure: Mersenne-Twister RNG, per-target seed = 20260729 + 100 × target index (+1 for the date scheme; one index is skipped after each outcome–metric block, as in the original), sampling order, M2-L refit (OLS / ML logistic, default controls), failure rules (a refit is successful iff the exposure coefficient is finite; **failed samples are counted and classified, never redrawn**), the **raw** exposure coefficient, median, **type-8 percentile 95 % interval** (at least 20 successes), sign fractions.
3. Verifies independently (every target): sampling invariants on every replicate; lm()/glm() fits of the original form on identical resamples (6 per target); independent type-8 quantile and median; the seed rule; conservation of attempted = successful + failed; the full-sample fit equals the validated M2-L estimate; two targets recomputed from scratch.
4. Assesses scientific validity **separately** (descriptive flags with declared thresholds): success fraction ≥ 0.95, glm non-convergence ≤ 1 %, bootstrap SD within 0.5–2× the primary SE, interval contains the primary estimate. A `REVIEW` flag never changes a result.
5. Compares the 180 summaries with the Batch C bundle with the strict validator (existing tolerance; counts must agree exactly) and writes the **package-source manifest and SHA-256 fingerprint** (start and end of the run; scope and exclusions in `SOURCE_FINGERPRINT.txt`; relative paths in `source_manifest.csv`).

## Separate statuses (printed; `BATCH_C_STATUS.txt`, `VALIDATION_STATUS.csv`)
| Status | Meaning |
|---|---|
| EXECUTION | ran without error, full scope (180 × 2,000) |
| INPUT CHECKS | input validation has no FAIL |
| NUMERICAL REPRODUCTION | the 180 summaries match the frozen references (required comparison missing, duplicated, unmatched or unclassified = FAIL) |
| MATHEMATICAL VERIFICATION | all independent checks pass (NOT_TESTED is never PASS) |
| SCIENTIFIC VALIDITY (descriptive) | `PASS` or `REVIEW` (number of flagged targets listed); not part of pass/fail |
| SOURCE INTEGRITY | no package source file changed during the run |

Exit codes: 0 executed, verified and reproduced; 1 execution failure; 2 invalid arguments or R missing; 4 executed, but reproduction, verification, an input check or source integrity FAILED.

## Expected outputs
`bootstrap_summary.csv` (180 rows), `bootstrap_failures.csv` (failure reasons and non-failure diagnostics per target), `bootstrap_diagnostics.csv` (180), `bootstrap_validity.csv` (180), `bootstrap_verification.csv` (180), `bootstrap_math_verification.csv` (7 check families), `bootstrap_point_estimates.csv` (90), `batch_c_validation_summary.csv`/`_detail.csv`, `source_manifest.csv`, `SOURCE_FINGERPRINT.txt`, `batch_c_metadata.json`, `BATCH_C_STATUS.txt`, `VALIDATION_STATUS.csv`, `RUN_SUMMARY.md`, `RUNTIME.csv`, `RUN_MANIFEST.csv`, `SESSION_INFO.txt`, `input_validation_report.csv`, `run.log` (per-target progress with elapsed time and ETA), `checkpoints\` (resumption only; not in the manifest).
**Expected on the study data (from the frozen references; nothing is forced):** 180 summaries with 2,000 requested; the frozen run recorded 2,000 successful refits in all 180 rows (no failures) and no interval excluding zero. A difference of even one successful refit is reported as DIFFERENT.
Not in this batch (NOT_TESTED, declared in the plan): the 18 DLM cumulative-effect bootstrap rows of the 198-row frozen file (window 59, archived L01–L59 horizon) and the final L01–L56 simultaneous bands.

## What to send for review
All aggregate files above (not `checkpoints\`). Never send the study CSV.
