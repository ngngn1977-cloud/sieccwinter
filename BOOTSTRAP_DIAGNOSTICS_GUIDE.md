# Bootstrap diagnostics (Phase 3C.2): diagnostic-only audit of saved Batch C estimates

What it does: reads the saved target checkpoints (`checkpoints/CKPT__<target>.rds`) and the aggregate tables of a finished Batch C run, and reports (1) counts and distribution of extreme replicate estimates, (2) an exact reconstruction of the original type-8 percentile limits, (3) how flagged estimates relate to the quantile ranks, (4) diagnostic-only limits after omitting flagged estimates, (5) checkpoint hashes and provenance.

What it does not do: read the SAV or the canonical CSV, load participant data, reconstruct a resample, refit a model, change any checkpoint, summary, flag, interval or criterion. Coefficient-based flags are **extremeness diagnostics, not confirmed separation**. Filtered limits are **diagnostic only** and never replace the original intervals. Outputs are aggregate tables only.

Definitions and thresholds are declared in `config/bootstrap_diagnostics_protocol.json` before any study run (X1 |e| > 1e6; X2 |e - primary| > 100 x reference SE (primary diagnostic); X3 > 10 SE; X4 > 10 scaled MAD (not evaluated when MAD = 0); X5 > 1000 SE).

## Command

```powershell
.\run_bootstrap_diagnostics.ps1 -BatchCRunDir "<private folder>\runs\batchc_20261001_161758_c9e5" -OutRoot "<private folder>\runs"
```

A new folder `bootdiag_<date>_<time>_<id>` is created under `-OutRoot` (it must not be inside the audited run). The audited run folder is only read.

## Statuses (separate)

EXECUTION, CHECKPOINT INTEGRITY, CHECKPOINTS UNCHANGED, ACCOUNTING, QUANTILE RECONSTRUCTION, ORIGINAL RUN PROVENANCE, SOURCE INTEGRITY, OUTPUT COMPLETENESS (a missing value in the distribution or reconstruction tables is accepted only where the quantity is undefined by an explicit rule, listed in `audit_field_completeness.csv`; any other missing value fails). Extremeness diagnostics are descriptive and never a pass/fail status. Exit codes: 0 all checks passed; 1 execution failure; 2 invalid arguments or R missing; 4 executed but a check failed.

## Main outputs (aggregate only)

`RUN_SUMMARY.md`, `BOOTSTRAP_DIAGNOSTICS_STATUS.txt`, `audit_scope.csv` (180 targets, binary and previously flagged), `audit_target_distribution.csv`, `audit_extremeness_by_target.csv`, `audit_extremeness_by_definition.csv`, `audit_quantile_reconstruction.csv`, `audit_tail_ranks.csv`, `audit_omission_sensitivity_DIAGNOSTIC_ONLY.csv`, `audit_field_completeness.csv`, `audit_checkpoint_checks.csv`, `audit_checkpoint_hashes.csv`, `audit_definitions.csv`, `audit_provenance.json`, `audit_tool_source_manifest.csv`, `RUN_MANIFEST.csv`, `SESSION_INFO.txt`.

Send me the output folder (or its `RUN_SUMMARY.md` and CSVs); nothing in it contains participant-level values or replicate-level estimates.
