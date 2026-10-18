# Reproducibility statement

## What can be reproduced, and by whom

1. **Workflow reproduction (anyone).** `run_all.ps1 -Demo` and `tests/testthat.R` run the whole pipeline on the shipped, fully synthetic CSV. They show that the code executes and that the outputs have the declared structure and pass their internal checks. Synthetic numbers carry no scientific meaning. Do not compare them with the article.
2. **Numerical reproduction of the article (authorised data holders only).** This needs the locked analytical CSV (SHA-256 `REDACTED_PUBLIC_BUILD`) and the frozen aggregate reference bundles. `run_all.ps1 -PathsFile …` refuses any other input file, then compares every result with its reference. Every discrepancy is reported and none is corrected.

## What is fixed, and where it is recorded

| Item | Value | Recorded in |
|---|---|---|
| R | 4.4.3 (checked; the run refuses another version) | `preflight\ENVIRONMENT_*.txt`, each `SESSION_INFO.txt` |
| R packages | MASS ≥ 7.3-64, jsonlite ≥ 2.0.0, base packages | same |
| Configuration | `config/sieccwinter_config.json`, SHA-256 `5A8C6054…DC5DE` (checked; the run refuses another) | `RUN_PARAMETERS.txt`, `run_metadata.json` |
| Package source | SHA-256 fingerprint over `R/`, `config/`, `inst/extdata/`, `scripts/`, `run.R`, `run_*.ps1`, `DESCRIPTION`, `NAMESPACE` | `RUN_PARAMETERS.txt`; each batch's `SOURCE_FINGERPRINT.txt` and its `SOURCE_INTEGRITY_STATUS` (unchanged during the run) |
| Random numbers | Mersenne-Twister / Inversion / Rejection; seed 20260729; per-model seed = seed + 100 × metric index + outcome index | configuration; per model in the result tables |
| Simultaneous bands | 5,000 multivariate-normal draws, `MASS::mvrnorm`, quantile type 8 | configuration; `dlm_final_summary.csv` |
| Logistic convergence | cumulative 1e-8 / 25; final DLM 1e-12 / 200; archived DLM 1e-10 / 200 | configuration; `R/archived.R` (`siw_archived_glm_control`) |
| Protocols and tolerances | `config/*_protocol.json`, `config/validation_plan*.json`; each SHA-256 is recorded by the run that uses it | each batch's metadata |
| Every output file | SHA-256 | each `RUN_MANIFEST.csv`; `FULL_RUN_MANIFEST.csv` (written last) |

## Known sources of non-identity

* **Simultaneous critical values** are Monte Carlo quantities. `MASS::mvrnorm` eigenvector signs can flip when a covariance matrix changes at machine precision, which changes the realised draws but not their distribution. Values agree exactly on the same platform with the same seed and draw count; elsewhere they may differ in the last digits. This is why the final L01–L56 acceptance uses the post hoc V2.1 covariance rule. The V1 FAIL records are preserved.
* **Iterative fits** (`glm`, `MASS::polr`, Firth) agree to their convergence tolerance, not bit for bit, across platforms. The documented legacy limitations (the Firth frozen bound, and `polr` stopping short of the maximum) are reported, not hidden.
* **Archived binary DLMs**: `run.R paper|demo --archived` before 2026-10-06 used epsilon 1e-12. They now use the archived 1e-10. Gaussian rows are bit-identical; binary rows differ at convergence level only (see `DEVELOPMENT_LOG_2026-10-06.md`). The daily-layer and ANOM/ALT status texts still carry the historical label `J02_GLM_EPSILON_ENTRY_POINTS=OPEN`, because their source files are traced by the ANOM diagnostic and were left unchanged.

## Open items (not hidden; see the run summary)

* ANOM: the identity check was revised post hoc (protocol v2, `config/anomaly_history_validation_protocol_v2.json`, author decision 12 Oct 2026, based on the step-15 diagnostic decision `PREMISE_DISPROVEN_EQUAL_N_DIFFERENT_SAMPLES`).
  * `ANOM_VERIFICATION_STATUS_V2` governs new runs. The v1 status is still computed and shown as superseded.
  * `anomhist_20261004_141846_28bd` and run_20261012_114559_4737 keep `ANOM_VERIFICATION_STATUS = FAIL` in their own records.
* REVIEW: manuscript concordance; the MAXT permutation assumptions.
* NOT_TESTED: figure provenance; dlnm equality (dlnm is not installed); archived simultaneous bands. The 18 L59 bootstrap rows are now post hoc step 19.
* Post hoc steps 17-30 (`config/posthoc_sensitivity_protocol_v2.json`: 17-26 of 13 Oct 2026, unchanged from v1; 27-30 of 14 Oct 2026): labelled SENSITIVITY / POST_HOC / RECONSTRUCTION; they cannot change q90, q9 or any accepted status. The early/late q values use the SI-declared families (180 / 90 tests; author decision 13 Oct 2026; 18 / 9 kept as informational columns). The nearest-station step reads the raw exports directly (FEATURE_ID is participant_code: 55/55 ID-only match; author decision 13 Oct 2026) and runs each declared export variant (19 Jul, 25 Jul 2026) separately. H3 reads its archived W56 coefficients and global p from the identified core reference (fix of 13 Oct 2026).
* Public package versus private validation kit: the public tarball ships redacted declarations; the full declarations, the input declaration, the private-run wrappers and the deny list are in the local-only kit (`scripts/release_public.R`, `scripts/install_private_kit.ps1`).
