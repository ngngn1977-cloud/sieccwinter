# Cold-burden equivalence with the original (legacy) SIECC scripts on FULLY SYNTHETIC data only (no real data).
# The legacy code is sourced read-only: its own functions (siecc_cold_thresholds, siecc_build_cold_feature, siecc_fit_model, siecc_bh) produce the comparators; nothing in the legacy tree is modified and no legacy checkpoint is written.
#   Rscript validation/legacy_equivalence_cold_burden_synthetic.R <legacy_package_root> [--windows 1,7,29,59] [--out DIR]
# 1. thresholds; 2. burden features for every threshold, window (1..59) and measure; 3. every model of the selected windows (all fitted thresholds, 9 variants, 3 outcomes), all compared columns of the package scan against siecc_fit_model;
# 4. q_global / q_outcome / q_curve of the subset (the legacy assignment code applied to the legacy subset); 5. runtimes of the two routes on the same models.
args <- commandArgs(trailingOnly = TRUE); legacy_root <- args[1L]
oi <- match("--out", args); out <- if (!is.na(oi)) args[oi + 1L] else NULL; wi <- match("--windows", args); wins <- if (!is.na(wi)) as.integer(strsplit(args[wi + 1L], ",")[[1L]]) else c(1L, 7L, 29L, 59L)
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); root <- normalizePath(file.path(dirname(script_arg[1L]), ".."), winslash = "/")
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root); cfg <- siw_load_config(); P <- siw_cold_burden_protocol()
lenv <- new.env(parent = globalenv()); for (f in sort(list.files(file.path(legacy_root, "R"), pattern = "[.]R$", full.names = TRUE))) sys.source(f, envir = lenv)
lcfg <- lenv$siecc_default_config(legacy_root); raw <- siw_make_synthetic(n = 55L, seed = 21L, config = cfg); dat <- siw_prepare_data(raw, cfg)
tol <- cfg$tolerances; tol$name <- "cumulative"; res <- list(); timing <- list()
# 1. thresholds
lt <- lenv$siecc_cold_thresholds(dat, lcfg); pt <- siw_cold_thresholds(dat, cfg, P)
res$thresholds <- siw_compare_tables(pt, lt, c("metric", "threshold_name"), tol, name = "thresholds_vs_legacy", expected_records = nrow(lt), required_columns = c("quantile", "threshold", "participant_days_below", "participant_days_total"))
# 2. features
fit_thr <- pt[pt$participant_days_below >= 20L, ]; md <- 0; cnt_bad <- 0L; ncomp <- 0L
for (i in seq_len(nrow(fit_thr))) { tr <- fit_thr[i, ]; sp <- cfg$metrics[cfg$metrics$metric == tr$metric, ]; H <- as.matrix(dat[, siw_daily_names(sp$daily_prefix, 59L)]); fc <- siw_cold_feature_curves(H, tr$threshold)
  for (w in 1:59) for (m in c("degree_burden", "cold_day_count", "persistent_cold_days_ge2", "longest_cold_spell", "mean_cold_intensity")) { old <- lenv$siecc_build_cold_feature(dat, lcfg, tr$metric, tr$threshold, w, m); d <- max(abs(fc[[m]][, w] - old)); ncomp <- ncomp + length(old); if (m %in% c("cold_day_count", "persistent_cold_days_ge2", "longest_cold_spell")) cnt_bad <- cnt_bad + sum(fc[[m]][, w] != old) else md <- max(md, d) } }
cat(sprintf("features: %d values compared; count-type mismatches %d; max abs difference of degree_burden / mean_cold_intensity %.3g\n", ncomp, cnt_bad, md))
# 3. models of the selected windows
t0 <- proc.time()[["elapsed"]]; sc <- siw_cold_burden_scan(dat, cfg, P, windows = wins); timing$package_seconds <- proc.time()[["elapsed"]] - t0; pk <- sc$results
t0 <- proc.time()[["elapsed"]]; rows <- list(); measures <- c("degree_burden", "cold_day_count", "persistent_cold_days_ge2", "longest_cold_spell", "mean_cold_intensity")
for (ti in seq_len(nrow(fit_thr))) { tr <- fit_thr[ti, ]
  for (window in wins) for (measure in measures) { raw_x <- lenv$siecc_build_cold_feature(dat, lcfg, tr$metric, tr$threshold, window, measure)
    for (tf in if (measure %in% c("degree_burden", "mean_cold_intensity")) c("raw", "log1p", "sqrt") else "raw") { xv <- switch(tf, raw = raw_x, log1p = log1p(raw_x), sqrt = sqrt(raw_x)); support <- sum(xv > 0, na.rm = TRUE)
      for (oi2 in seq_len(nrow(lcfg$primary_outcomes))) { o <- lcfg$primary_outcomes$outcome[[oi2]]; fam <- lcfg$primary_outcomes$family[[oi2]]; d <- dat; d$.__cold_burden <- xv
        fit <- lenv$siecc_fit_model(d, model_id = paste("COLD", o, tr$metric, tr$threshold_name, measure, window, tf, sep = "__"), outcome = o, exposure = ".__cold_burden", covariates = lcfg$model_tiers[["M2_L"]], outcome_family = fam, model_tier = "M2_L", metric = tr$metric, window_or_lag = window,
          transformation = tf, exposure_unit = "cold_burden", primary_role = "EXPLORATORY_COLD_BURDEN", run_firth = FALSE)
        r <- fit$result; r$threshold_name <- tr$threshold_name; r$threshold_value <- tr$threshold; r$burden_measure <- measure; r$participants_with_positive_burden <- support; rows[[length(rows) + 1L]] <- r } } } }
timing$legacy_seconds <- proc.time()[["elapsed"]] - t0; old <- do.call(rbind, rows); old$window_or_lag <- as.integer(old$window_or_lag)
old$q_global <- old$q_outcome <- old$q_curve <- NA_real_; ok <- is.finite(old$nominal_p); old$q_global[ok] <- lenv$siecc_bh(old$nominal_p[ok])
for (o in unique(old$outcome)) { i <- which(old$outcome == o & ok); old$q_outcome[i] <- lenv$siecc_bh(old$nominal_p[i]) }
fam <- paste(old$outcome, old$metric, old$threshold_name, old$burden_measure, old$transformation, sep = "|"); for (f in unique(fam[ok])) { i <- which(fam == f & ok); old$q_curve[i] <- lenv$siecc_bh(old$nominal_p[i]) }
fields <- siw_cold_burden_protocol()$comparisons[[1L]]$fields |> unlist(); fix_inf <- function(d) { for (f in fields) { v <- suppressWarnings(as.numeric(d[[f]])); if (any(is.infinite(v))) d[[f]][is.infinite(v)] <- NA_real_ }; d }
keys <- c("outcome", "metric", "threshold_name", "burden_measure", "transformation", "window_or_lag"); inf_equal <- sum(is.infinite(pk$condition_number) & is.infinite(old$condition_number[match(do.call(paste, c(pk[keys], sep = "|")), do.call(paste, c(old[keys], sep = "|")))]))
res$models <- siw_compare_tables(fix_inf(pk), fix_inf(old), keys, tol, name = "cold_models_vs_legacy", expected_records = nrow(old), required_columns = setdiff(fields, c("q_global", "q_outcome", "q_curve")))
res$q <- siw_compare_tables(pk[, c(keys, "q_global", "q_outcome", "q_curve")], old[, c(keys, "q_global", "q_outcome", "q_curve")], keys, tol, name = "cold_q_subset_vs_legacy", expected_records = nrow(old), required_columns = c("q_global", "q_outcome", "q_curve"))
summ <- do.call(rbind, lapply(res, function(r) r$summary)); rownames(summ) <- NULL; detail <- siw_bind_rows(lapply(res, function(r) r$detail))
print(summ[, c("reference", "status", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_unmatched", "n_nonfinite", "n_missing_required", "reason")], right = FALSE)
cat(sprintf("models compared: %d (windows %s); infinite-equal condition numbers: %d; runtime package %.1f s vs legacy %.1f s (%.1fx)\n", nrow(old), paste(wins, collapse = ","), inf_equal, timing$package_seconds, timing$legacy_seconds, timing$legacy_seconds / timing$package_seconds))
if (nrow(detail)) { cat("Largest discrepancies:\n"); print(utils::head(detail[order(-detail$abs_diff), ], 12), right = FALSE) }
if (!is.null(out)) { dir.create(out, recursive = TRUE, showWarnings = FALSE); siw_write_csv(summ, file.path(out, "legacy_equivalence_cold_burden_summary.csv")); siw_write_csv(detail, file.path(out, "legacy_equivalence_cold_burden_detail.csv")); siw_write_csv(data.frame(timing), file.path(out, "legacy_equivalence_cold_burden_timing.csv")) }
quit(status = as.integer(any(summ$status == "FAIL")))
