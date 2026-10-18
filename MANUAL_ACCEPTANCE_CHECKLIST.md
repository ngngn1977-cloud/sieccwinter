# Manual acceptance checklist

Nothing is locked until the three runs below all pass and you record the result. Tick each box yourself.
Run folder = the folder printed as `Output folder:` at the end of each run.

## A. Synthetic demo (`.\run_demo.ps1`)

- [ ] The window ends with green `PASS`; `$LASTEXITCODE` is 0.
- [ ] A new folder exists under `runs\`; earlier run folders (if any) are untouched.
- [ ] `RUN_SUMMARY.md` starts with `# Run summary: PASS`.
- [ ] `VALIDATION_STATUS.csv`: `run_exit_code` PASS, `self_checks` PASS (20 PASS, 0 FAIL); `input_validation` WARN with `26 PASS, 1 WARN, 0 FAIL` (the planted rainfall edge case).
- [ ] `cumulative_results.csv` has 1,680 rows; `dlm_final_summary.csv` has 9 rows; `dlm_final_lag_curves.csv` has 504 rows (9 x 56).
- [ ] `run_metadata.json` shows seed 20260729, draws 5000, mvn_method `MASS_mvrnorm`, and an input hash equal to the one in `EXPECTED_OUTPUTS.md`.
- [ ] `RUN_MANIFEST.csv` lists every file with a hash; `SESSION_INFO.txt` shows R 4.4.3.
- [ ] No file in the run folder contains a participant row (the synthetic codes `SYN001...` do not appear).
- [ ] Running `.\run_demo.ps1` a second time creates a second folder, and the result tables are identical to the first (compare the hashes of `cumulative_results.csv` in both `RUN_MANIFEST.csv` files).

## B. Paper run (`.\run_paper.ps1 -DataCsv "..."`), authorised access only

- [ ] The path you supplied is not stored anywhere in the repository; outputs went to a folder you chose (`-OutRoot`) or to git-ignored `runs\`.
- [ ] `RUN_SUMMARY.md` starts with `PASS`; `INPUT_VALIDATION.csv` has 0 FAIL. Every WARN is understood (non-Jewish coding, `Memory_assess`, cumulative-field mismatch, rainfall anomaly); none was corrected by the package.
- [ ] Participant count in the summary equals the expected study N; `complete_case_accounting.csv` sample sizes are as expected for each tier.
- [ ] `cumulative_results.csv` 1,680 rows; `dlm_final_summary.csv` 9 rows (L01-L56); self-checks 0 FAIL.
- [ ] Only aggregate files are shared for review.

## C. Validation run (`.\run_validate.ps1 -DataCsv "..." -ReferenceDir "..."`)

- [ ] `RUN_SUMMARY.md` starts with `PASS`, or every discrepancy in `validation_detail.csv` is understood.
- [ ] Each reference row in `VALIDATION_STATUS.csv` is PASS (exact or within the tolerance in the configuration) or NOT_TESTED with a stated reason (no reference file); none is FAIL.
- [ ] Simultaneous critical values may differ slightly from legacy because they are Monte Carlo based (see `README.md`, analytical defaults); any such difference is listed, not hidden.

## Sign-off

| Run | Date | Folder | PASS / FAIL | Notes |
|---|---|---|---|---|
| Demo | | | | |
| Paper | | | | |
| Validate | | | | |
