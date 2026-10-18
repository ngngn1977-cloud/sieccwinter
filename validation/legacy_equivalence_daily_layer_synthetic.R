# Daily-layer equivalence with the original (legacy) SIECC scripts on FULLY SYNTHETIC data only (no real data).
# Legacy functions are sourced read-only: siecc_xy_scan_daily (XYD), siecc_fit_model (DAY, the loop of siecc_single_day_adjusted_grid without its checkpoint writer) and siecc_xy_permutation_maxT (MAXT, original sampling sequence and seed rule).
#   Rscript validation/legacy_equivalence_daily_layer_synthetic.R <legacy_package_root> [--lags 1,35,59] [--replicates 2000] [--out DIR]
args <- commandArgs(trailingOnly = TRUE); legacy_root <- args[1L]; opt <- function(nm, d) { i <- match(nm, args); if (is.na(i)) d else args[i + 1L] }
lags <- as.integer(strsplit(opt("--lags", "1,35,59"), ",")[[1L]]); B <- as.integer(opt("--replicates", "2000")); out <- opt("--out", NULL)
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); root <- normalizePath(file.path(dirname(script_arg[1L]), ".."), winslash = "/")
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root); cfg <- siw_load_config(); P <- siw_daily_protocol()
lenv <- new.env(parent = globalenv()); for (f in sort(list.files(file.path(legacy_root, "R"), pattern = "[.]R$", full.names = TRUE))) sys.source(f, envir = lenv)
lcfg <- lenv$siecc_default_config(legacy_root); lcfg$lags <- lags; raw <- siw_make_synthetic(n = 55L, seed = 21L, config = cfg); dat <- siw_attach_cumulative(siw_prepare_data(raw, cfg), cfg)$data; ldat <- dat
tol <- cfg$tolerances; tol$name <- "cumulative"; tolE <- cfg$tolerances; tolE$name <- "exact"; res <- list(); timing <- list(); cmpf <- unlist(Filter(function(z) z$id == "XYD_1416", P$comparisons)[[1L]]$fields)
# 1. XYD
t0 <- proc.time()[["elapsed"]]; old <- lenv$siecc_xy_scan_daily(ldat, lcfg); timing$xyd_legacy <- proc.time()[["elapsed"]] - t0; t0 <- proc.time()[["elapsed"]]; new <- siw_xy_daily_grid(dat, cfg, P, lags = lags); timing$xyd_package <- proc.time()[["elapsed"]] - t0
res$xyd <- siw_compare_tables(new, old, c("outcome", "metric", "window_or_lag"), tol, name = "xyd_vs_legacy", expected_records = nrow(old), required_columns = cmpf)
# 2. DAY: the loop of siecc_single_day_adjusted_grid for the selected lags, then its q assignment
t0 <- proc.time()[["elapsed"]]; rows <- list()
for (oi in seq_len(nrow(lcfg$primary_outcomes))) { o <- lcfg$primary_outcomes$outcome[[oi]]; fam <- lcfg$primary_outcomes$family[[oi]]
  for (mi in seq_len(nrow(lcfg$metrics))) { sp <- lcfg$metrics[mi, ]; for (lag in lags) { ex <- paste0(sp$daily_prefix, "_L", sprintf("%02d", lag))
    fit <- lenv$siecc_fit_model(data = ldat, model_id = paste("SINGLE_DAY", o, sp$metric, lag, sep = "__"), outcome = o, exposure = ex, covariates = lcfg$model_tiers[["M2_L"]], outcome_family = fam, model_tier = "M2_L", metric = sp$metric, window_or_lag = lag, transformation = "raw_daily", exposure_unit = sp$unit, primary_role = "SUPPLEMENTARY_SINGLE_DAY_DIAGNOSTIC", run_firth = FALSE)
    rows[[length(rows) + 1L]] <- fit$result } } }
oldd <- do.call(rbind, rows); oldd$window_or_lag <- as.integer(oldd$window_or_lag); oldd$q59 <- oldd$q472 <- oldd$q_global <- NA_real_
for (o in unique(oldd$outcome)) { i <- which(oldd$outcome == o & is.finite(oldd$nominal_p)); oldd$q472[i] <- lenv$siecc_bh(oldd$nominal_p[i]); for (m in unique(oldd$metric)) { j <- which(oldd$outcome == o & oldd$metric == m & is.finite(oldd$nominal_p)); oldd$q59[j] <- lenv$siecc_bh(oldd$nominal_p[j]) } }; oldd$q_global <- lenv$siecc_bh(oldd$nominal_p); timing$day_legacy <- proc.time()[["elapsed"]] - t0
t0 <- proc.time()[["elapsed"]]; newd <- siw_single_day_grid(dat, cfg, P, lags = lags); timing$day_package <- proc.time()[["elapsed"]] - t0; fixinf <- function(d) { for (f in names(d)) if (is.numeric(d[[f]]) && any(is.infinite(d[[f]]))) d[[f]][is.infinite(d[[f]])] <- NA_real_; d }
dayf <- unlist(Filter(function(z) z$id == "DAY_1416", P$comparisons)[[1L]]$fields); res$day <- siw_compare_tables(fixinf(newd[, c("outcome", "metric", "window_or_lag", "model_tier", dayf)]), fixinf(oldd), c("outcome", "metric", "window_or_lag", "model_tier"), tol, name = "day_vs_legacy", expected_records = nrow(oldd), required_columns = dayf)
# 3. MAXT: original function, original seed rule (config seed + 1000 x outcome index)
xyc <- lenv$siecc_xy_scan_cumulative(ldat, lcfg); t0 <- proc.time()[["elapsed"]]; oldm <- do.call(rbind, lapply(seq_len(nrow(lcfg$primary_outcomes)), function(i) lenv$siecc_xy_permutation_maxT(ldat, xyc, lcfg, lcfg$primary_outcomes$outcome[[i]], replicates = B, seed = lcfg$seed + i * 1000L))); timing$maxt_legacy <- proc.time()[["elapsed"]] - t0
t0 <- proc.time()[["elapsed"]]; newm <- siw_xy_maxt_grid(dat, cfg, P, replicates = B)$table; timing$maxt_package <- proc.time()[["elapsed"]] - t0
res$maxt <- siw_compare_tables(newm, oldm, c("outcome", "exposure_field"), tolE, name = "maxt_vs_legacy", expected_records = nrow(oldm), required_columns = c("observed_abs_correlation", "p_outcome_maxT"))
summ <- do.call(rbind, lapply(res, function(r) r$summary)); rownames(summ) <- NULL; detail <- siw_bind_rows(lapply(res, function(r) r$detail))
print(summ[, c("reference", "status", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_unmatched", "n_nonfinite", "n_missing_required", "reason")], right = FALSE)
cat(sprintf("lags %s; MAXT replicates %d. Runtime (s): XYD legacy %.1f vs package %.1f; DAY legacy %.1f vs package %.1f; MAXT legacy %.1f vs package %.1f\n", paste(lags, collapse = ","), B, timing$xyd_legacy, timing$xyd_package, timing$day_legacy, timing$day_package, timing$maxt_legacy, timing$maxt_package))
if (nrow(detail)) { cat("Largest discrepancies:\n"); print(utils::head(detail[order(-detail$abs_diff), ], 10), right = FALSE) }
if (!is.null(out)) { dir.create(out, recursive = TRUE, showWarnings = FALSE); siw_write_csv(summ, file.path(out, "legacy_equivalence_daily_layer_summary.csv")); siw_write_csv(detail, file.path(out, "legacy_equivalence_daily_layer_detail.csv")); siw_write_csv(data.frame(timing), file.path(out, "legacy_equivalence_daily_layer_timing.csv")) }
quit(status = as.integer(any(summ$status == "FAIL")))
