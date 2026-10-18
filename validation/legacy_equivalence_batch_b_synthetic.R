# Batch B equivalence with the original (legacy) SIECC scripts on FULLY SYNTHETIC data only (no real data).
# The legacy code is sourced read-only; its own functions produce the comparators:
#   siecc_cumulative_grid()$firth (560 Firth fits), siecc_ordinal_integration_grid, siecc_transformation_grid, siecc_nonlinearity_grid.
# The legacy tables are mapped to the layout of the Batch B reference bundle with siw_batch_b_reference_tables() - the function that also builds the real bundle -
# written to a temporary reference folder, and the package is compared with them through the real Batch B validator (siw_validate_batch_b) and the real plan.
# The independent mathematical verification of the package results (batch_b_math_verification) is written as well. Nothing in the legacy tree is modified.
#   Rscript validation/legacy_equivalence_batch_b_synthetic.R <legacy_package_root> [<library_with_dlnm>] [--out DIR]
args <- commandArgs(trailingOnly = TRUE)
legacy_root <- args[1L]; lib <- if (length(args) > 1L && !startsWith(args[2L], "--")) args[2L] else NULL
oi <- match("--out", args); out <- if (!is.na(oi)) args[oi + 1L] else NULL
ri <- match("--ref-out", args); ref_out <- if (!is.na(ri)) args[ri + 1L] else NULL     # keeps the synthetic reference folder (legacy results in the bundle layout) for dry runs of run_batch_b.ps1
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); root <- normalizePath(file.path(dirname(script_arg[1L]), ".."), winslash = "/")
if (!is.null(lib)) .libPaths(c(lib, .libPaths()))
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
cfg <- siw_load_config()
lenv <- new.env(parent = globalenv())
for (f in sort(list.files(file.path(legacy_root, "R"), pattern = "[.]R$", full.names = TRUE))) sys.source(f, envir = lenv)
suppressMessages(library(dlnm)); lcfg <- lenv$siecc_default_config(legacy_root)
raw <- siw_make_synthetic(config = cfg); dat <- siw_attach_cumulative(siw_prepare_data(raw, cfg), cfg)$data
ldat <- dat; ldat$Int_dummy <- as.integer(ldat$Int_dummy); ldat$integration_ordinal <- ordered(ldat$integration_score, levels = 1:4)   # derived exactly as the legacy import does
tm <- list(); tick <- function(name, expr) { t0 <- proc.time()[["elapsed"]]; v <- force(expr); tm[[name]] <<- proc.time()[["elapsed"]] - t0; v }
legacy <- list()
cum <- tick("legacy_cumulative_with_firth", lenv$siecc_cumulative_grid(ldat, lcfg, tiers = cfg$tier_order)); legacy$firth <- cum$firth
legacy$ordinal <- tick("legacy_ordinal", lenv$siecc_ordinal_integration_grid(ldat, lcfg, tier = "M2_L"))
legacy$transformation <- tick("legacy_transformation", lenv$siecc_transformation_grid(ldat, lcfg, tiers = "M2_L"))
legacy$nonlinear <- tick("legacy_nonlinear", lenv$siecc_nonlinearity_grid(ldat, lcfg, tier = "M2_L"))
ref <- siw_batch_b_reference_tables(legacy)
refdir <- file.path(tempdir(), "batchB_synthetic_reference"); dir.create(refdir, showWarnings = FALSE)
nm <- c(firth = "batchB_firth_reference.csv", ordinal = "batchB_ordinal_reference.csv", transformation = "batchB_transformations_reference.csv", nonlinear = "batchB_nonlinear_reference.csv")
for (k in names(nm)) utils::write.csv(ref[[k]], file.path(refdir, nm[[k]]), row.names = FALSE, na = "")
if (!is.null(ref_out)) { dir.create(ref_out, recursive = TRUE, showWarnings = FALSE); invisible(file.copy(file.path(refdir, nm), ref_out, overwrite = TRUE)) }
core <- tick("package_core_grid", siw_cumulative_grid(dat, cfg)$results)
bt <- tick("package_batch_b", siw_batch_b_tables(dat, cfg, core, verify = TRUE))
bv <- siw_validate_batch_b(bt, cfg, refdir)
options(width = 220)
cat("\n== Batch B comparison with the legacy results on synthetic data ==\n")
print(bv$summary[, c("reference", "status", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_explained_by_legacy_bound", "n_unmatched", "n_nonfinite", "n_missing_required", "reason")], right = FALSE)
if (nrow(bv$detail)) { cat("\nLargest discrepancies:\n"); print(utils::head(bv$detail[order(-bv$detail$abs_diff), ], 12), right = FALSE) }
if (nrow(bv$attribution)) { cat("\nFirth attribution (cells beyond the cumulative tolerance):", nrow(bv$attribution), " explained:", sum(bv$attribution$explained), " max abs_diff/bound:", format(max(bv$attribution$abs_diff_over_bound, na.rm = TRUE), digits = 3), "\n") }
cat("\n== Independent mathematical verification of the package results ==\n"); print(bt$batch_b_math_verification[, c("component", "n_specifications", "n_verified_pass", "n_documented_limitation", "n_fail", "n_not_verified", "status")], right = FALSE)
sc <- siw_batch_b_self_checks(bt, cfg, core); cat("\nself-checks FAIL:", sum(sc$status == "FAIL"), "\n"); if (any(sc$status == "FAIL")) print(sc[sc$status == "FAIL", ])
cat("\ntimings (s):\n"); print(round(c(unlist(tm), attr(bt, "timings")), 2))
if (!is.null(out)) {
  dir.create(out, recursive = TRUE, showWarnings = FALSE); siw_write_csv(bv$summary, file.path(out, "legacy_equivalence_batch_b_summary.csv")); siw_write_csv(bv$detail, file.path(out, "legacy_equivalence_batch_b_detail.csv"))
  if (nrow(bv$attribution)) siw_write_csv(bv$attribution, file.path(out, "legacy_equivalence_batch_b_firth_attribution.csv"))
  siw_write_csv(bt$batch_b_math_verification, file.path(out, "batch_b_math_verification_synthetic.csv")); siw_write_csv(sc, file.path(out, "batch_b_self_checks_synthetic.csv"))
  siw_write_csv(data.frame(stage = names(c(unlist(tm), attr(bt, "timings"))), seconds = round(unname(c(unlist(tm), attr(bt, "timings"))), 3)), file.path(out, "batch_b_synthetic_timings.csv"))
  if (nrow(bt$ordinal_verification)) siw_write_csv(bt$ordinal_verification, file.path(out, "ordinal_verification_synthetic.csv"))
}
quit(status = as.integer(any(bv$summary$status == "FAIL")))
