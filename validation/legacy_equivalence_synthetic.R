# Numerical equivalence of sieccwinter with the original (legacy) SIECC scripts on FULLY SYNTHETIC data only.
# The legacy code is sourced read-only from a path you supply; nothing in the legacy tree is modified and no real data are used.
#   Rscript validation/legacy_equivalence_synthetic.R <legacy_package_root> [<library_with_dlnm>] [--archived] [--out DIR]
args <- commandArgs(trailingOnly = TRUE)
legacy_root <- args[1L]; lib <- if (length(args) > 1L && !startsWith(args[2L], "--")) args[2L] else NULL
do_arch <- "--archived" %in% args; oi <- match("--out", args); out <- if (!is.na(oi)) args[oi + 1L] else NULL
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); root <- normalizePath(file.path(dirname(script_arg[1L]), ".."), winslash = "/")
if (!is.null(lib)) .libPaths(c(lib, .libPaths()))
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
cfg <- siw_load_config()
# --- legacy code (functions only)
lenv <- new.env(parent = globalenv())
for (f in sort(list.files(file.path(legacy_root, "R"), pattern = "\\.R$", full.names = TRUE))) sys.source(f, envir = lenv)
suppressMessages(library(dlnm))
lcfg <- lenv$siecc_default_config(legacy_root)
# --- synthetic data in both formats
raw <- siw_make_synthetic(config = cfg); dat <- siw_prepare_data(raw, cfg); dat <- siw_attach_cumulative(dat, cfg)$data
dat$assessment_date_iso <- "SYN"                       # legacy DLM code requires this column to exist (value unused)
res <- list()
# --- cumulative grid: all 1,680 specifications
t0 <- proc.time()[["elapsed"]]; new <- siw_cumulative_grid(dat, cfg)$results; t_new <- proc.time()[["elapsed"]] - t0
t0 <- proc.time()[["elapsed"]]; old <- lenv$siecc_cumulative_grid(dat, lcfg, tiers = cfg$tier_order)$results; t_old <- proc.time()[["elapsed"]] - t0
old$window_or_lag <- as.integer(old$window_or_lag)
tol <- cfg$tolerances; tol$name <- "cumulative"
cmp <- siw_compare_tables(new, old, c("outcome", "metric", "window_or_lag", "model_tier"), tol, name = "cumulative_1680_vs_legacy")
res$cumulative <- cmp
# --- final DLMs at 5,000 draws with the legacy engine (same seed rule)
S <- list(); for (oi_ in seq_len(nrow(cfg$outcomes))) for (m in cfg$representative_metrics) S[[length(S) + 1L]] <-
  lenv$siecc_fit_dlm(dat, lcfg, cfg$outcomes$outcome[oi_], cfg$outcomes$family[oi_], m, "M2_L", max_lag = 56L, lag_df = 3L, simultaneous_replicates = 5000L, seed = lcfg$seed)$summary
lg <- lenv$siecc_bind_rows(S); lg$q9 <- lenv$siecc_bh(lg$global_p)
nd <- siw_final_dlm(dat, cfg)$summary
tol$name <- "dlm"
res$dlm_final <- siw_compare_tables(nd, lg, c("outcome", "metric", "model_tier"), tol, aliases = cfg$validation$dlm_column_aliases, name = "dlm_final_9_vs_legacy_engine_at_5000_draws")
res$dlm_final_exact_tol <- siw_compare_tables(nd, lg, c("outcome", "metric", "model_tier"), modifyList(tol, list(dlm_abs = 1e-9, dlm_rel = 1e-9)), aliases = cfg$validation$dlm_column_aliases, name = "dlm_final_9_vs_legacy_engine_tolerance_1e-9")
if (do_arch) {
  t0 <- proc.time()[["elapsed"]]; na <- siw_archived_dlm_grid(dat, cfg); t_na <- proc.time()[["elapsed"]] - t0
  t0 <- proc.time()[["elapsed"]]; oa <- lenv$siecc_dlm_grid(dat, lcfg, tiers = cfg$tier_order, simultaneous_replicates = 10000L)$summary; t_oa <- proc.time()[["elapsed"]] - t0
  res$archived <- siw_compare_tables(na, oa, c("outcome", "metric", "model_tier"), tol, aliases = cfg$validation$dlm_column_aliases, name = "archived_168_vs_legacy_engine")
  ns <- siw_archived_dlm_sensitivity(dat, cfg); os <- lenv$siecc_dlm_sensitivity_grid(dat, lcfg, simultaneous_replicates = 0L)$summary
  ns$key2 <- paste(ns$outcome, ns$metric, ns$sensitivity_specification); os$key2 <- paste(os$outcome, os$metric, os$sensitivity_specification)
  res$archived_sens <- siw_compare_tables(ns, os, "key2", tol, aliases = cfg$validation$dlm_column_aliases, name = "archived_216_vs_legacy_engine", skip = c("seed", "simultaneous_draws"))
}
summ <- do.call(rbind, lapply(res, function(r) r$summary)); rownames(summ) <- NULL
detail <- siw_bind_rows(lapply(res, function(r) r$detail))
timing <- data.frame(item = c("cumulative_1680_new_seconds", "cumulative_1680_legacy_seconds"), value = round(c(t_new, t_old), 2))
if (do_arch) timing <- rbind(timing, data.frame(item = c("archived_168_new_seconds", "archived_168_legacy_seconds"), value = round(c(t_na, t_oa), 2)))
print(summ, right = FALSE); print(timing)
if (nrow(detail)) { cat("Largest discrepancies:\n"); print(utils::head(detail[order(-detail$abs_diff), ], 12), right = FALSE) }
if (!is.null(out)) { dir.create(out, recursive = TRUE, showWarnings = FALSE); siw_write_csv(summ, file.path(out, "legacy_equivalence_summary.csv")); siw_write_csv(detail, file.path(out, "legacy_equivalence_detail.csv")); siw_write_csv(timing, file.path(out, "legacy_equivalence_timing.csv")) }
