# Batch A numerical equivalence with the original (legacy) SIECC scripts on FULLY SYNTHETIC data only (no real data).
# The legacy code is sourced read-only; its own functions produce the comparators (siecc_cumulative_grid diagnostics columns, siecc_xy_scan_cumulative,
# siecc_table1, siecc_demographic_analysis). Nothing in the legacy tree is modified.
#   Rscript validation/legacy_equivalence_batch_a_synthetic.R <legacy_package_root> [<library_with_dlnm>] [--out DIR]
args <- commandArgs(trailingOnly = TRUE)
legacy_root <- args[1L]; lib <- if (length(args) > 1L && !startsWith(args[2L], "--")) args[2L] else NULL
oi <- match("--out", args); out <- if (!is.na(oi)) args[oi + 1L] else NULL
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); root <- normalizePath(file.path(dirname(script_arg[1L]), ".."), winslash = "/")
if (!is.null(lib)) .libPaths(c(lib, .libPaths()))
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
cfg <- siw_load_config()
lenv <- new.env(parent = globalenv())
for (f in sort(list.files(file.path(legacy_root, "R"), pattern = "[.]R$", full.names = TRUE))) sys.source(f, envir = lenv)
suppressMessages(library(dlnm)); lcfg <- lenv$siecc_default_config(legacy_root)
raw <- siw_make_synthetic(config = cfg); dat <- siw_attach_cumulative(siw_prepare_data(raw, cfg), cfg)$data
ldat <- dat; fs <- siw_descriptive_factors(dat)       # legacy descriptive code expects these derived columns (same labels and rules as legacy siecc_prepare_analysis_data)
ldat$Sex_factor <- fs$Sex_factor; ldat$Ethnicity_factor <- fs$Ethnicity_factor; ldat$Econ_STS_factor <- fs$Econ_STS_factor
ldat$Age_group <- cut(ldat$Age, breaks = c(-Inf, 69, 79, Inf), labels = c("<70", "70-79", ">79"), right = TRUE); ldat$Int_dummy <- as.integer(ldat$Int_dummy)
tol <- cfg$tolerances; tol$name <- "cumulative"; res <- list()
# 1. diagnostics: the 16 columns of the legacy grid
old <- lenv$siecc_cumulative_grid(ldat, lcfg, tiers = cfg$tier_order)$results; old$window_or_lag <- as.integer(old$window_or_lag)
D <- siw_cumulative_diagnostics(dat, cfg)
res$diagnostics <- siw_compare_tables(D, old, c("outcome", "metric", "window_or_lag", "model_tier"), tol, name = "diagnostics_1680_vs_legacy", required_columns = .siw_diag_columns, expected_records = 1680L)
# 2. bivariate
xy <- lenv$siecc_xy_scan_cumulative(ldat, lcfg); xy$window_or_lag <- as.integer(xy$window_or_lag)
B <- siw_bivariate_grid(dat, cfg)
res$bivariate <- siw_compare_tables(B, xy, c("outcome", "metric", "window_or_lag"), tol, name = "bivariate_240_vs_legacy", expected_records = 240L,
  required_columns = c("n", "pearson_or_point_biserial_r", "pearson_p", "spearman_rho", "spearman_p", "model_p", "q_pearson_global", "q_spearman_global", "q_model_global", "q_pearson_outcome", "q_spearman_outcome", "q_model_outcome", "q_pearson_outcome_metric", "q_spearman_outcome_metric", "q_model_outcome_metric"))
# 3. descriptive table 1 and group analyses
t1 <- lenv$siecc_table1(ldat, lcfg); N1 <- siw_table1(dat)
res$table1 <- siw_compare_tables(N1, t1, "characteristic", tol, name = "table1_vs_legacy", expected_records = 17L, required_columns = c("n_or_count", "percent", "mean", "sd", "median", "iqr", "minimum", "maximum"))
dm <- lenv$siecc_demographic_analysis(ldat, lcfg); G <- siw_group_analysis(dat, cfg)
res$group_summaries <- siw_compare_tables(transform(G$summaries, key = paste(outcome, factor, level)), transform(dm$summary, key = paste(outcome, factor, level)), "key", tol, name = "group_summaries_vs_legacy", expected_records = 30L, required_columns = c("n", "mean_or_proportion", "se", "ci_lower", "ci_upper"))
res$group_tests <- siw_compare_tables(transform(G$tests, key = paste(outcome, factor)), transform(dm$tests, key = paste(outcome, factor)), "key", tol, name = "group_tests_vs_legacy", expected_records = 12L, required_columns = c("statistic", "p", "q_within_outcome"))
summ <- do.call(rbind, lapply(res, function(r) r$summary)); rownames(summ) <- NULL; detail <- siw_bind_rows(lapply(res, function(r) r$detail))
print(summ[, c("reference", "status", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_unmatched", "n_nonfinite", "n_missing_required", "reason")], right = FALSE)
if (nrow(detail)) { cat("Largest discrepancies:\n"); print(utils::head(detail[order(-detail$abs_diff), ], 12), right = FALSE) }
if (!is.null(out)) { dir.create(out, recursive = TRUE, showWarnings = FALSE); siw_write_csv(summ, file.path(out, "legacy_equivalence_batch_a_summary.csv")); siw_write_csv(detail, file.path(out, "legacy_equivalence_batch_a_detail.csv")) }
quit(status = as.integer(any(summ$status == "FAIL")))
