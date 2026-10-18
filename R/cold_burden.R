# Phase 3F: exploratory cold-burden scan (22,302 models in the frozen V1.4 run): reproduction and mathematical verification.
# Specification traced from WINTER_SOURCE R/110_cold_burden.R and R/030_model_engine.R (see config/cold_burden_validation_protocol.json, declared before any run).
# The fitting engine is the validated one (siw_fit_gaussian, siw_fit_logistic, siw_model_diagnostics_one); only redundant work is removed: the burden features of every window and measure are obtained once per threshold
# from cumulative sums over the lags (instead of one pass per window x measure), the covariate design and the reduced models are built once, results are collected in preallocated columns.
# The scientific interpretation of the scan is NOT part of this code: it reports descriptive support / influence / separation flags only.

#' Load the declared cold-burden validation protocol (config/cold_burden_validation_protocol.json).
#' @export
siw_cold_burden_protocol <- function(path = NULL) {
  if (is.null(path)) { root <- siw_root(); cand <- c(file.path(root, "config", "cold_burden_validation_protocol.json"), file.path(root, "extdata", "cold_burden_validation_protocol.json"), file.path(root, "inst", "extdata", "cold_burden_validation_protocol.json")); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_COLD_PROTOCOL_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value; p
}

.siw_cold_measures <- c("degree_burden", "cold_day_count", "persistent_cold_days_ge2", "longest_cold_spell", "mean_cold_intensity")
.siw_cold_transformations <- function(measure) if (measure %in% c("degree_burden", "mean_cold_intensity")) c("raw", "log1p", "sqrt") else "raw"
.siw_cold_rowcum <- function(M, fn) { out <- matrix(0, nrow(M), ncol(M)); for (i in seq_len(nrow(M))) out[i, ] <- fn(M[i, ]); out }

#' Burden features of every window 1..W for every participant, from one pass over the lags (cumulative sums).
#' A day is cold if the value is finite and strictly below the threshold; a missing / non-finite value is never cold (no deficit, ends a run).
#' degree_burden: sum of deficits; cold_day_count; persistent_cold_days_ge2: days in runs of >= 2 consecutive cold lags; longest_cold_spell; mean_cold_intensity: mean deficit over cold days (0 without a cold day).
#' @param H participants x lags matrix (lag 1 first); the window w uses columns 1..w
#' @return list of five participants x W numeric matrices
#' @export
siw_cold_feature_curves <- function(H, threshold) {
  H <- as.matrix(H); storage.mode(H) <- "double"; n <- nrow(H); W <- ncol(H)
  cold <- is.finite(H) & H < threshold; cold[is.na(cold)] <- FALSE
  deficit <- matrix(0, n, W); deficit[cold] <- threshold - H[cold]
  run <- matrix(0, n, W); r <- numeric(n); for (l in seq_len(W)) { r <- ifelse(cold[, l], r + 1, 0); run[, l] <- r }
  inc <- (run == 2) * 2 + (run > 2) * 1                       # a run adds its first two days when it reaches length 2, then one day per further cold lag
  burden <- .siw_cold_rowcum(deficit, cumsum); count <- .siw_cold_rowcum(cold * 1, cumsum)
  list(degree_burden = burden, cold_day_count = count, persistent_cold_days_ge2 = .siw_cold_rowcum(inc, cumsum), longest_cold_spell = .siw_cold_rowcum(run, cummax),
       mean_cold_intensity = ifelse(count > 0, burden / pmax(count, 1), 0))
}

#' Independent per-participant route for one window (the legacy semantics written out separately: run-length encoding of the first w lags). Used for verification only.
#' @export
siw_cold_features_reference <- function(x, threshold) {
  cold <- is.finite(x) & x < threshold; cold[is.na(cold)] <- FALSE; deficit <- ifelse(cold, threshold - x, 0); rl <- rle(as.logical(cold)); runs <- rl$lengths[rl$values]
  c(degree_burden = sum(deficit), cold_day_count = sum(cold), persistent_cold_days_ge2 = sum(runs[runs >= 2L]), longest_cold_spell = if (length(runs)) max(runs) else 0, mean_cold_intensity = if (any(cold)) mean(deficit[cold]) else 0)
}

#' Thresholds of the scan: pooled type-7 percentiles of all finite participant-day values over L01..L59 for each thermal metric, plus the fixed UTCI thresholds; with the participant-day counts below.
#' @export
siw_cold_thresholds <- function(dat, config, protocol = siw_cold_burden_protocol()) {
  P <- protocol$parameters; rows <- list()
  for (m in unlist(P$thermal_metrics)) {
    sp <- config$metrics[config$metrics$metric == m, ]; v <- as.numeric(as.matrix(dat[, siw_daily_names(sp$daily_prefix, P$lag_end), drop = FALSE])); v <- v[is.finite(v)]
    add <- function(nm, type, q, thr) rows[[length(rows) + 1L]] <<- data.frame(metric = m, threshold_name = nm, threshold_type = type, quantile = q, threshold = thr, participant_days_below = sum(v < thr), participant_days_total = length(v), stringsAsFactors = FALSE)
    for (nm in names(P$percentiles)) { q <- P$percentiles[[nm]]; add(nm, "pooled_percentile", q, as.numeric(stats::quantile(v, q, names = FALSE, type = P$quantile_type))) }
    if (identical(m, "UTCI")) for (thr in unlist(P$utci_standard_thresholds)) add(paste0("UTCI_LT_", thr), "standard_utci", NA_real_, as.numeric(thr))
  }
  do.call(rbind, rows)
}

.siw_cold_num_fields <- function(protocol) unlist(Filter(function(z) identical(z$id, "RESULTS_22302"), protocol$comparisons)[[1L]]$fields)
.siw_cold_id <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

# one model: complete cases, design, validated engine, diagnostics; returns a named numeric vector over the compared fields plus the categorical items
.siw_cold_one <- function(y, family, x, Xc, tier_ok, outcome, tier, cache, config, P, diagnostics = TRUE) {
  mask <- tier_ok & is.finite(y) & is.finite(x); Xs <- Xc[mask, , drop = FALSE]; drop <- grepl("^Locality_3group", colnames(Xs)) & (colSums(Xs != 0) == 0L); Xs <- Xs[, !drop, drop = FALSE]
  X <- cbind(Xs[, 1L, drop = FALSE], exposure = x[mask], Xs[, -1L, drop = FALSE]); colnames(X)[1L] <- "(Intercept)"; ys <- y[mask]; key <- paste(outcome, tier, paste(which(!mask), collapse = ","), sep = "|")
  st <- if (family == "gaussian") siw_fit_gaussian(X, ys, 2L, cache, key) else siw_fit_logistic(X, ys, 2L, cache, key, epsilon = P$glm_epsilon, maxit = P$glm_maxit)
  xs <- X[, 2L]; est <- isTRUE(st$estimable)
  if (!est) { st$exposure_sd <- stats::sd(xs); st$exposure_iqr <- stats::IQR(xs); st$residual_df <- st$n - st$rank; st$parameter_count <- st$p
    if (family == "binomial") { st$event_count <- sum(ys == 1); st$non_event_count <- sum(ys == 0) } }
  d <- if (diagnostics) siw_model_diagnostics_one(X, ys, family == "binomial") else NULL
  list(st = st, d = d, est = est, n = nrow(X))
}

#' The cold-burden scan with the validated engine. Order of the rows: thresholds (thresholds table order), window, measure, transformation, outcome (the legacy order).
#' @param thresholds optional precomputed threshold table; windows optional subset of windows (tests); diagnostics = FALSE skips the influence / calibration columns (tests of speed only)
#' @return list(thresholds, results, seconds)
#' @export
siw_cold_burden_scan <- function(dat, config, protocol = siw_cold_burden_protocol(), thresholds = NULL, windows = NULL, diagnostics = TRUE) {
  P <- protocol$parameters; t0 <- proc.time()[["elapsed"]]; thr <- thresholds %||% siw_cold_thresholds(dat, config, protocol); fit_thr <- thr[thr$participant_days_below >= P$min_participant_days_below, , drop = FALSE]
  Wv <- windows %||% seq_len(P$lag_end); tier <- P$tier; Xc <- siw_tier_matrix(dat, config$tiers[[tier]]); tier_ok <- stats::complete.cases(Xc); cache <- .siw_new_cache(); fields <- .siw_cold_num_fields(protocol)
  nvar <- sum(vapply(.siw_cold_measures, function(m) length(.siw_cold_transformations(m)), 0L)); N <- nrow(fit_thr) * length(Wv) * nvar * nrow(config$outcomes)
  num <- matrix(NA_real_, N, length(fields), dimnames = list(NULL, fields)); meta <- list(model_id = character(N), outcome = character(N), outcome_family = character(N), metric = character(N), threshold_name = character(N), threshold_type = character(N), burden_measure = character(N), transformation = character(N), window_or_lag = integer(N), status = character(N), separation_status = character(N), convergence = rep(NA, N), not_estimable_reason = character(N), support_eligible = rep(NA, N))
  secs <- c(features = 0, fits = 0); k <- 0L
  for (ti in seq_len(nrow(fit_thr))) {
    tr <- fit_thr[ti, ]; sp <- config$metrics[config$metrics$metric == tr$metric, ]; H <- as.matrix(dat[, siw_daily_names(sp$daily_prefix, P$lag_end), drop = FALSE]); storage.mode(H) <- "double"
    ta <- proc.time()[["elapsed"]]; fc <- siw_cold_feature_curves(H, tr$threshold); secs[["features"]] <- secs[["features"]] + proc.time()[["elapsed"]] - ta; ta <- proc.time()[["elapsed"]]
    for (w in Wv) for (measure in .siw_cold_measures) { raw <- fc[[measure]][, w]
      for (tf in .siw_cold_transformations(measure)) {
        x <- switch(tf, raw = raw, log1p = log1p(raw), sqrt = sqrt(raw)); support <- sum(x > 0, na.rm = TRUE)
        for (oi in seq_len(nrow(config$outcomes))) {
          o <- config$outcomes$outcome[oi]; fam <- config$outcomes$family[oi]; y <- as.numeric(dat[[o]]); r <- .siw_cold_one(y, fam, x, Xc, tier_ok, o, tier, cache, config, P, diagnostics); st <- r$st; d <- r$d; k <- k + 1L
          g <- function(nm) { v <- st[[nm]]; if (is.null(v)) NA_real_ else as.numeric(v) }; gd <- function(nm) { v <- if (is.null(d)) NULL else d[[nm]]; if (is.null(v)) NA_real_ else as.numeric(v) }
          for (f in fields) num[k, f] <- switch(f, parameter_count = if (r$est) g("p") else g("parameter_count"), n = r$n, threshold_value = tr$threshold, participants_with_positive_burden = support, q_global = NA_real_, q_outcome = NA_real_, q_curve = NA_real_,
            condition_number = gd("condition_number"), maximum_vif = gd("maximum_vif"), calibration_intercept = gd("calibration_intercept"), calibration_slope = gd("calibration_slope"), maximum_coefficient_magnitude = gd("maximum_coefficient_magnitude"), maximum_cooks_distance = gd("maximum_cooks_distance"),
            maximum_leverage = gd("maximum_leverage"), maximum_absolute_dfbeta = gd("maximum_absolute_dfbeta"), influential_case_count = gd("influential_case_count"), exposure_p10 = gd("exposure_p10"), exposure_p25 = gd("exposure_p25"), exposure_p75 = gd("exposure_p75"), exposure_p90 = gd("exposure_p90"), g(f))
          meta$model_id[k] <- paste("COLD", .siw_cold_id(o), .siw_cold_id(tr$metric), tr$threshold_name, measure, paste0("W", w), tf, sep = "__"); meta$outcome[k] <- o; meta$outcome_family[k] <- fam; meta$metric[k] <- tr$metric; meta$threshold_name[k] <- tr$threshold_name; meta$threshold_type[k] <- tr$threshold_type
          meta$burden_measure[k] <- measure; meta$transformation[k] <- tf; meta$window_or_lag[k] <- as.integer(w); meta$status[k] <- if (r$est) "COMPLETED" else "NOT_ESTIMABLE"; meta$separation_status[k] <- st$separation_status %||% ""; meta$convergence[k] <- if (is.null(st$convergence)) NA else st$convergence
          meta$not_estimable_reason[k] <- if (r$est) "" else st$reason; meta$support_eligible[k] <- support >= P$min_support_participants
        } } }
    secs[["fits"]] <- secs[["fits"]] + proc.time()[["elapsed"]] - ta
  }
  stopifnot(k == N); res <- cbind(as.data.frame(meta, stringsAsFactors = FALSE), model_tier = tier, exposure_unit = "cold_burden", primary_role = "EXPLORATORY_COLD_BURDEN", as.data.frame(num), stringsAsFactors = FALSE); res$q_global <- res$q_outcome <- res$q_curve <- NA_real_
  res <- siw_cold_assign_q(res); rownames(res) <- NULL
  list(thresholds = thr, results = res, seconds = c(secs, total = proc.time()[["elapsed"]] - t0))
}

#' Benjamini-Hochberg q_global (all finite exposure p values), q_outcome (within outcome) and q_curve (over the windows within outcome, metric, threshold, measure and transformation).
#' @export
siw_cold_assign_q <- function(res) {
  ok <- is.finite(res$nominal_p) & res$status == "COMPLETED"; res$q_global <- res$q_outcome <- res$q_curve <- NA_real_; res$q_global[ok] <- siw_bh(res$nominal_p[ok])
  for (o in unique(res$outcome)) { i <- which(ok & res$outcome == o); res$q_outcome[i] <- siw_bh(res$nominal_p[i]) }
  fam <- paste(res$outcome, res$metric, res$threshold_name, res$burden_measure, res$transformation, sep = "|"); for (f in unique(fam[ok])) { i <- which(ok & fam == f); res$q_curve[i] <- siw_bh(res$nominal_p[i]) }
  res
}

# ---- mathematical verification ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' Independent verification of the burden features (all fitted thresholds, all windows, all participants): the optimised cumulative route against the per-participant run-length route.
#' @export
siw_cold_verify_features <- function(dat, config, thresholds, protocol = siw_cold_burden_protocol(), windows = NULL) {
  P <- protocol$parameters; thr <- thresholds[thresholds$participant_days_below >= P$min_participant_days_below, , drop = FALSE]; Wv <- windows %||% seq_len(P$lag_end); rows <- list()
  for (ti in seq_len(nrow(thr))) { tr <- thr[ti, ]; sp <- config$metrics[config$metrics$metric == tr$metric, ]; H <- as.matrix(dat[, siw_daily_names(sp$daily_prefix, P$lag_end), drop = FALSE]); fc <- siw_cold_feature_curves(H, tr$threshold); md <- stats::setNames(numeric(5L), .siw_cold_measures)
    for (w in Wv) for (i in seq_len(nrow(H))) { ref <- siw_cold_features_reference(H[i, seq_len(w)], tr$threshold); for (m in .siw_cold_measures) md[[m]] <- max(md[[m]], abs(fc[[m]][i, w] - ref[[m]])) }
    rows[[ti]] <- data.frame(metric = tr$metric, threshold_name = tr$threshold_name, t(md), n_participants = nrow(H), n_windows = length(Wv), stringsAsFactors = FALSE) }
  r <- do.call(rbind, rows); r$status <- ifelse(r$degree_burden <= 1e-12 & r$mean_cold_intensity <= 1e-12 & r$cold_day_count == 0 & r$persistent_cold_days_ge2 == 0 & r$longest_cold_spell == 0, "PASS", "FAIL"); r
}

#' Independent verification of the thresholds (own type-7 quantile, recount of the participant-days below) against a threshold table (package or frozen).
#' @export
siw_cold_verify_thresholds <- function(dat, config, thr, protocol = siw_cold_burden_protocol()) {
  P <- protocol$parameters; out <- list()
  for (i in seq_len(nrow(thr))) { m <- thr$metric[i]; sp <- config$metrics[config$metrics$metric == m, ]; v <- as.numeric(as.matrix(dat[, siw_daily_names(sp$daily_prefix, P$lag_end), drop = FALSE])); v <- v[is.finite(v)]
    t <- if (identical(thr$threshold_type[i], "pooled_percentile")) .siw_dv_q7(v, thr$quantile[i]) else thr$threshold[i]
    out[[i]] <- data.frame(metric = m, threshold_name = thr$threshold_name[i], independent_threshold = t, table_threshold = thr$threshold[i], abs_diff = abs(t - thr$threshold[i]), independent_days_below = sum(v < t), table_days_below = thr$participant_days_below[i], independent_total = length(v), table_total = thr$participant_days_total[i], stringsAsFactors = FALSE) }
  r <- do.call(rbind, out); r$status <- ifelse(r$abs_diff <= 1e-12 & r$independent_days_below == r$table_days_below & r$independent_total == r$table_total, "PASS", "FAIL"); r
}

#' Scale and inference identities of a results table (package or frozen), recomputed from its own columns (relative tolerance 1e-9). sd_y_by_outcome: SD of each outcome in the analytic sample (NULL: only constancy of the implied SD is checked).
#' @export
siw_cold_identities <- function(res, sd_y = NULL, tol = 1e-9) {
  e <- res[res$status == "COMPLETED", ]; g <- e$outcome_family == "gaussian"; b <- !g; rows <- list(); rel <- function(a, bb) { d <- abs(a - bb) / pmax(abs(bb), 1e-300); d[abs(a - bb) <= 1e-300] <- 0; max(d, 0, na.rm = TRUE) }
  add <- function(check, n, v) rows[[length(rows) + 1L]] <<- data.frame(check = check, n_models = n, max_rel_diff = v, tolerance = tol, status = if (is.finite(v) && v <= tol) "PASS" else "FAIL", stringsAsFactors = FALSE)
  add("iqr_effect = coefficient x exposure IQR", nrow(e), rel(e$iqr_effect, e$coefficient * e$exposure_iqr)); add("binomial standardised_effect = coefficient x exposure SD", sum(b), rel(e$standardised_effect[b], (e$coefficient * e$exposure_sd)[b]))
  add("binomial odds_ratio_raw = exp(coefficient)", sum(b), rel(e$odds_ratio_raw[b], exp(e$coefficient[b]))); add("binomial odds_ratio_sd = exp(coefficient x SD)", sum(b), rel(e$odds_ratio_sd[b], exp((e$coefficient * e$exposure_sd)[b]))); add("binomial odds_ratio_iqr = exp(coefficient x IQR)", sum(b), rel(e$odds_ratio_iqr[b], exp((e$coefficient * e$exposure_iqr)[b])))
  imp <- (e$coefficient * e$exposure_sd / e$standardised_effect)[g]; sdy <- tapply(imp, e$outcome[g], function(z) max(abs(z / z[1L] - 1))); add("gaussian standardised_effect: implied SD(outcome) constant within each outcome", sum(g), max(sdy))
  if (!is.null(sd_y)) { imp_by <- tapply(imp, e$outcome[g], function(z) z[1L]); add("gaussian standardised_effect: implied SD(outcome) equals the SD of the analytic sample", sum(g), { nm <- intersect(names(imp_by), names(sd_y)); max(abs(imp_by[nm] / sd_y[nm] - 1)) }) }
  tg <- e$coefficient[g] / e$robust_hc3_se[g]; add("gaussian robust_statistic = coefficient / HC3 SE", sum(g), rel(e$robust_statistic[g], tg)); add("gaussian p = 2 pt(|t|, residual_df)", sum(g), rel(e$nominal_p[g], 2 * stats::pt(abs(tg), e$residual_df[g], lower.tail = FALSE)))
  cr <- stats::qt(0.975, e$residual_df[g]); add("gaussian CI = coefficient +/- qt(0.975, residual_df) x HC3 SE", sum(g), max(rel(e$ci_lower[g], e$coefficient[g] - cr * e$robust_hc3_se[g]), rel(e$ci_upper[g], e$coefficient[g] + cr * e$robust_hc3_se[g])))
  z <- e$coefficient[b] / e$model_based_se[b]; add("binomial statistic = coefficient / model SE", sum(b), rel(e$statistic[b], z)); add("binomial p = 2 pnorm(|z|)", sum(b), rel(e$nominal_p[b], 2 * stats::pnorm(abs(z), lower.tail = FALSE)))
  add("binomial CI = coefficient +/- 1.96 x model SE", sum(b), max(rel(e$ci_lower[b], e$coefficient[b] - 1.96 * e$model_based_se[b]), rel(e$ci_upper[b], e$coefficient[b] + 1.96 * e$model_based_se[b]))); add("binomial lr_p = chi-square tail of lr_statistic on lr_df", sum(b), rel(e$lr_p[b], stats::pchisq(e$lr_statistic[b], e$lr_df[b], lower.tail = FALSE)))
  do.call(rbind, rows)
}

#' Multiplicity verification of a results table: family counts and sizes, and q recomputed with stats::p.adjust(method = "BH") (max absolute difference).
#' @export
siw_cold_verify_multiplicity <- function(res) {
  ok <- is.finite(res$nominal_p) & res$status == "COMPLETED"; fam <- paste(res$outcome, res$metric, res$threshold_name, res$burden_measure, res$transformation, sep = "|"); rows <- list(); add <- function(check, value, criterion, pass) rows[[length(rows) + 1L]] <<- data.frame(check = check, value = as.character(value), criterion = criterion, status = if (isTRUE(pass)) "PASS" else "FAIL", stringsAsFactors = FALSE)
  add("q_global: finite q values = estimable models with a finite p", sum(is.finite(res$q_global)), "equals the number of estimable models", sum(is.finite(res$q_global)) == sum(ok)); d <- max(abs(res$q_global[ok] - stats::p.adjust(res$nominal_p[ok], "BH"))); add("q_global equals p.adjust BH over all estimable models (max abs diff)", d, "<= 1e-12", d <= 1e-12)
  d <- 0; for (o in unique(res$outcome)) { i <- which(ok & res$outcome == o); d <- max(d, abs(res$q_outcome[i] - stats::p.adjust(res$nominal_p[i], "BH"))) }; add("q_outcome equals p.adjust BH within each outcome (max abs diff)", d, "<= 1e-12", d <= 1e-12); add("q_outcome families", length(unique(res$outcome)), "3 (one per outcome)", length(unique(res$outcome)) == 3L)
  d <- 0; ff <- unique(fam[ok]); for (f in ff) { i <- which(ok & fam == f); d <- max(d, abs(res$q_curve[i] - stats::p.adjust(res$nominal_p[i], "BH"))) }; add("q_curve equals p.adjust BH over the windows within each family (max abs diff)", d, "<= 1e-12", d <= 1e-12); add("q_curve families (outcome x metric x threshold x measure x transformation)", length(ff), "378 for the full scan", NA)
  r <- do.call(rbind, rows); r$status[is.na(r$status) | r$check == "q_curve families (outcome x metric x threshold x measure x transformation)"] <- "INFO"; r
}

# separate formula-based refit of one model (stats::lm / stats::glm, own HC3): the legacy route written independently of the package engine
.siw_cold_refit_reference <- function(dat, config, outcome, family, x, tier_covs) {
  d <- dat; d$.x <- x; vars <- unique(c(outcome, ".x", tier_covs)); inc <- stats::complete.cases(d[, vars, drop = FALSE]); md <- droplevels(d[inc, vars, drop = FALSE]); f <- stats::reformulate(c(".x", tier_covs), response = outcome); X <- stats::model.matrix(f, md)
  if (family == "gaussian") { fit <- stats::lm(f, md); b <- stats::coef(fit)[[".x"]]; sm <- sqrt(diag(stats::vcov(fit)))[[".x"]]; e <- stats::residuals(fit); h <- stats::hatvalues(fit); XtXi <- solve(crossprod(X)); V <- XtXi %*% crossprod(X * (e / pmax(1e-6, 1 - h))) %*% XtXi; sr <- sqrt(V[".x", ".x"]); rdf <- stats::df.residual(fit); fs <- summary(fit)$fstatistic
    c(coefficient = b, model_based_se = sm, robust_hc3_se = sr, nominal_p = 2 * stats::pt(abs(b / sr), rdf, lower.tail = FALSE), ci_lower = b - stats::qt(0.975, rdf) * sr, ci_upper = b + stats::qt(0.975, rdf) * sr, aic = stats::AIC(fit), model_f = unname(fs[["value"]]), r_squared = summary(fit)$r.squared)
  } else { fit <- suppressWarnings(stats::glm(f, data = md, family = stats::binomial())); b <- stats::coef(fit)[[".x"]]; se <- sqrt(diag(stats::vcov(fit)))[[".x"]]; red <- suppressWarnings(stats::glm(stats::reformulate(tier_covs, response = outcome), data = md, family = stats::binomial())); lr <- 2 * (as.numeric(stats::logLik(fit)) - as.numeric(stats::logLik(red)))
    c(coefficient = b, model_based_se = se, nominal_p = 2 * stats::pnorm(abs(b / se), lower.tail = FALSE), ci_lower = b - 1.96 * se, ci_upper = b + 1.96 * se, aic = stats::AIC(fit), lr_statistic = lr, lr_p = stats::pchisq(lr, length(stats::coef(fit)) - length(stats::coef(red)), lower.tail = FALSE)) }
}

#' Independent refit of a deterministic sample (every `every`-th model in the result order, the named SI model, the first non-estimable model) against the package rows (cumulative tolerance class).
#' @export
siw_cold_independent_refit <- function(dat, config, res, thresholds, protocol = siw_cold_burden_protocol(), tol = config$tolerances) {
  P <- protocol$parameters; idx <- seq(1L, nrow(res), by = P$independent_refit_every); s <- P$si_row
  si <- which(res$outcome == s$outcome & res$metric == s$metric & res$threshold_name == s$threshold_name & res$burden_measure == s$burden_measure & res$transformation == s$transformation & res$window_or_lag == s$window_or_lag); ne <- which(res$status == "NOT_ESTIMABLE")[1L]
  idx <- sort(unique(c(idx, si, ne[!is.na(ne)]))); covs <- config$tiers[[P$tier]]; fc_cache <- list(); rows <- list()
  for (i in idx) { r <- res[i, ]; tr <- thresholds[thresholds$metric == r$metric & thresholds$threshold_name == r$threshold_name, ]; ck <- paste(r$metric, r$threshold_name)
    if (is.null(fc_cache[[ck]])) { sp <- config$metrics[config$metrics$metric == r$metric, ]; fc_cache[[ck]] <- siw_cold_feature_curves(as.matrix(dat[, siw_daily_names(sp$daily_prefix, P$lag_end), drop = FALSE]), tr$threshold) }
    x0 <- vapply(seq_len(nrow(dat)), function(j) siw_cold_features_reference(as.numeric(dat[j, siw_daily_names(config$metrics$daily_prefix[config$metrics$metric == r$metric], r$window_or_lag)]), tr$threshold)[[r$burden_measure]], 0)
    x <- switch(r$transformation, raw = x0, log1p = log1p(x0), sqrt = sqrt(x0))
    if (r$status == "NOT_ESTIMABLE") { rows[[length(rows) + 1L]] <- data.frame(model_id = r$model_id, field = "status", package = r$status, independent = if (all(x == 0)) "NOT_ESTIMABLE" else "ESTIMABLE", max_rel_diff = 0, status = if (all(x == 0)) "PASS" else "FAIL", stringsAsFactors = FALSE); next }
    ref <- .siw_cold_refit_reference(dat, config, r$outcome, r$outcome_family, x, covs)
    for (f in names(ref)) { a <- r[[f]]; b <- ref[[f]]; dd <- abs(a - b); ok <- is.finite(dd) && dd <= tol$cumulative_abs + tol$cumulative_rel * abs(b); rows[[length(rows) + 1L]] <- data.frame(model_id = r$model_id, field = f, package = a, independent = b, max_rel_diff = dd / max(abs(b), 1e-300), status = if (ok) "PASS" else "FAIL", stringsAsFactors = FALSE) } }
  do.call(rbind, rows)
}

# SI statement-level check of the one globally supported row (the number of supported models is declared as n_supported, 1 in the declared protocol)
.siw_cold_si_check <- function(res, protocol) {
  s <- protocol$parameters$si_row; n_sup <- s$n_supported %||% 1L; sup <- which(res$q_global <= 0.05); i <- which(res$outcome == s$outcome & res$metric == s$metric & res$threshold_name == s$threshold_name & res$burden_measure == s$burden_measure & res$transformation == s$transformation & res$window_or_lag == s$window_or_lag)
  row <- function(st, exp, got, ok) data.frame(statement = st, expected = exp, found = got, status = if (isTRUE(ok)) "PASS" else "FAIL", stringsAsFactors = FALSE); one <- length(i) == 1L
  out <- list(row("number of models with q_global <= 0.05", n_sup, length(sup), length(sup) == n_sup))
  if (n_sup == 1L) out[[2L]] <- row("that model is the named model (UCLA / MRT / P025 / mean_cold_intensity / raw / window 29)", "TRUE", one && length(sup) == 1L && sup == i, one && length(sup) == 1L && sup == i)
  out <- c(out, list(row("q_global of the named model", paste(s$q_global_range, collapse = " - "), if (one) signif(res$q_global[i], 4) else NA, one && is.finite(res$q_global[i]) && res$q_global[i] >= s$q_global_range[[1L]] && res$q_global[i] <= s$q_global_range[[2L]]),
    row("participants with positive burden", s$positive_burden, if (one) res$participants_with_positive_burden[i] else NA, one && res$participants_with_positive_burden[i] == s$positive_burden),
    row("maximum leverage", paste(s$leverage_range, collapse = " - "), if (one) round(res$maximum_leverage[i], 4) else NA, one && is.finite(res$maximum_leverage[i]) && res$maximum_leverage[i] >= s$leverage_range[[1L]] && res$maximum_leverage[i] <= s$leverage_range[[2L]]),
    row("influential cases", s$influential, if (one) res$influential_case_count[i] else NA, one && res$influential_case_count[i] == s$influential))); do.call(rbind, out)
}

#' Locate, hash and check the two frozen reference files named in the protocol (existence, SHA-256, bytes, record count, unique complete keys). Read only.
#' @export
siw_cold_read_references <- function(reference_dir, protocol = siw_cold_burden_protocol(), config = siw_load_config()) {
  P <- protocol$parameters; tabs <- list(); rows <- list(); add <- function(file, item, value, ok) rows[[length(rows) + 1L]] <<- data.frame(file = file, item = item, value = as.character(value), ok = isTRUE(ok), stringsAsFactors = FALSE)
  for (rf in protocol$reference_files) {
    f <- file.path(reference_dir, rf$file); ex <- file.exists(f); add(rf$file, "file exists", ex, ex); if (!ex) { add(rf$file, "REQUIRED REFERENCE ABSENT: nothing is compared", "absent", FALSE); next }
    h <- siw_file_hash(f)$value; add(rf$file, "SHA-256 equals the declared value", paste0("found ", h, "; declared ", rf$sha256), identical(toupper(h), toupper(rf$sha256))); add(rf$file, "bytes equal the declared value", paste0("found ", file.info(f)$size, "; declared ", rf$bytes), file.info(f)$size == rf$bytes)
    d <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")), error = function(e) NULL); add(rf$file, "file is readable", !is.null(d), !is.null(d)); if (is.null(d)) next; keys <- unlist(rf$keys); hk <- all(keys %in% names(d)); add(rf$file, "key columns present", paste(keys, collapse = ","), hk); tabs[[rf$name]] <- d; if (!hk) next
    kv <- do.call(paste, c(lapply(d[keys], as.character), sep = "|")); blank <- sum(Reduce(`|`, lapply(d[keys], function(z) is.na(z) | !nzchar(trimws(as.character(z)))))); add(rf$file, "records equal the declared count", paste0("found ", nrow(d), "; expected ", rf$expected_records), nrow(d) == rf$expected_records); add(rf$file, "no blank or missing keys", blank, blank == 0L); add(rf$file, "keys unique", sum(duplicated(kv)), !anyDuplicated(kv))
    cm <- Filter(function(z) identical(z$reference, rf$name), protocol$comparisons)[[1L]]; need <- c(unlist(cm$fields), if (identical(rf$name, "cold_burden_results")) c(unlist(protocol$categorical_comparisons$fields), "not_estimable_reason")); miss <- setdiff(need, names(d)); add(rf$file, paste0("required columns present for ", cm$id), if (length(miss)) paste("missing:", paste(miss, collapse = ",")) else "all present", !length(miss))
    if (identical(rf$name, "cold_burden_results")) { G <- P$expected_grid; add(rf$file, "the declared grid (windows x thresholds x variants x outcomes) is complete",paste0("windows ", length(unique(d$window_or_lag)), "; thresholds ", length(unique(paste(d$metric, d$threshold_name))), "; variants ", length(unique(paste(d$burden_measure, d$transformation))), "; outcomes ", length(unique(d$outcome))),
        length(unique(d$window_or_lag)) == G$windows && length(unique(paste(d$metric, d$threshold_name))) == G$thresholds && length(unique(paste(d$burden_measure, d$transformation))) == G$variants && length(unique(d$outcome)) == G$outcomes && nrow(d) == G$windows * G$thresholds * G$variants * G$outcomes) }
  }
  prov <- if (length(rows)) do.call(rbind, rows) else data.frame(file = character(), item = character(), value = character(), ok = logical()); list(tables = tabs, provenance = prov, ok = nrow(prov) > 0L && all(prov$ok) && length(tabs) == length(protocol$reference_files))
}

.siw_cold_cmp <- function(cc, new, ref, keys, tol, refname) {
  fields <- unlist(cc$fields); inf_pairs <- 0L; for (f in fields) { a <- suppressWarnings(as.numeric(new[[f]])); b <- suppressWarnings(as.numeric(ref[[f]])); both <- is.infinite(a) & is.infinite(b) & (a == b); inf_pairs <- inf_pairs + sum(both, na.rm = TRUE); if (any(is.infinite(a), na.rm = TRUE)) new[[f]][is.infinite(a)] <- NA_real_; if (any(is.infinite(b), na.rm = TRUE)) ref[[f]][is.infinite(b)] <- NA_real_
  }
  nn <- new[, intersect(c(keys, fields), names(new)), drop = FALSE]; r <- siw_compare_tables(nn, ref, keys, tol, name = cc$id, reference_file = refname, required_columns = fields, expected_records = as.integer(cc$expected_records), required = TRUE, skip = character()); r$inf_pairs <- inf_pairs; r
}

.siw_cold_status <- function(x) if (!length(x)) "NOT_TESTED" else if (any(x == "FAIL")) "FAIL" else if (any(x == "NOT_TESTED")) "NOT_TESTED" else "PASS"

#' Run the cold-burden numerical reproduction and mathematical verification on one canonical CSV against the frozen V1.4 reference files (read only). Never changes a result to fit a reference.
#' @param protocol_path optional alternative protocol (synthetic tests only)
#' @export
siw_run_cold_burden_validation <- function(data_csv, reference_dir, out_dir, config = siw_load_config(), protocol_path = NULL) {
  t0 <- proc.time()[["elapsed"]]; reference_dir <- normalizePath(reference_dir, winslash = "/", mustWork = TRUE); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); out_dir <- normalizePath(out_dir, winslash = "/")
  if (startsWith(out_dir, paste0(reference_dir, "/")) || startsWith(reference_dir, paste0(out_dir, "/"))) stop("SIW_COLD_OUTPUT_AND_REFERENCE_FOLDERS_OVERLAP", call. = FALSE)
  options(sieccwinter.log_file = file.path(out_dir, "run.log")); on.exit(options(sieccwinter.log_file = NULL), add = TRUE)
  protocol <- siw_cold_burden_protocol(protocol_path); P <- protocol$parameters; sm0 <- siw_source_manifest(); W <- function(x, f) siw_write_csv(x, file.path(out_dir, f)); siw_log("Phase 3F cold-burden validation; protocol ", protocol$protocol_hash, "; source fingerprint ", sm0$fingerprint, " (", sm0$n_files, " files)")
  env <- .siw_v21_env(); jsonlite::write_json(env, file.path(out_dir, "coldburden_environment.json"), auto_unbox = TRUE, pretty = TRUE, null = "null"); writeLines(utils::capture.output(print(utils::sessionInfo())), file.path(out_dir, "coldburden_sessionInfo.txt"))
  refs <- siw_cold_read_references(reference_dir, protocol, config); W(refs$provenance, "coldburden_reference_provenance.csv")
  carried <- c(FINAL_NINE_L56_NUMERICAL_VALIDATION = "ACCEPTED_UNDER_V2_1 (not recomputed)", ARCHIVED_DLM_DETERMINISTIC = "ACCEPTED_PHASE3E1 (not recomputed)", MANUSCRIPT_CONCORDANCE_STATUS = "REVIEW", FIGURE_PROVENANCE_STATUS = "NOT_TESTED", DLNM_EQUALITY_STATUS = "NOT_TESTED", SCIENTIFIC_INTERPRETATION = "NOT_ASSESSED (descriptive support / influence / separation flags only)")
  finish <- function(st, status, extra = character(), msg = NULL) { writeLines(c(paste0(names(st), "=", st), extra, paste0(names(carried), "=", carried), "SCOPE=exploratory cold-burden scan (22,302 models); numerical reproduction and mathematical verification only; does not promote or interpret any model", "Results were not altered, redrawn or replaced to fit any reference."), file.path(out_dir, "COLDBURDEN_STATUS.txt"))
    sm1 <- siw_source_manifest(); W(sm0$manifest, "source_manifest_coldburden.csv"); writeLines(c(paste0("SOURCE_FINGERPRINT_SHA256=", sm0$fingerprint), paste0("FILES=", sm0$n_files), paste0("SOURCE_CHANGED_DURING_RUN=", !isTRUE(siw_source_manifest_diff(sm0, sm1)$identical_fingerprint))), file.path(out_dir, "SOURCE_FINGERPRINT.txt")); if (!is.null(msg)) writeLines(msg, file.path(out_dir, "RUN_SUMMARY.md")); list(out_dir = out_dir, statuses = st, status = status) }
  if (!refs$ok) { st <- c(EXECUTION_STATUS = "PASS", INPUT_PROVENANCE_STATUS = "FAIL", THRESHOLDS_STATUS = "NOT_TESTED", NUMERICAL_REPRODUCTION_STATUS = "NOT_TESTED", FEATURE_VERIFICATION_STATUS = "NOT_TESTED", SCALE_AND_INFERENCE_STATUS = "NOT_TESTED", MULTIPLICITY_STATUS = "NOT_TESTED", INDEPENDENT_REFIT_STATUS = "NOT_TESTED", SI_STATEMENT_STATUS = "NOT_TESTED", COVERAGE_STATUS = "FAIL", SOURCE_INTEGRITY_STATUS = "PASS")
    bad <- refs$provenance[!refs$provenance$ok, ]; fr <- finish(st, "VERIFICATION_FAILURE", c(paste0("PROVENANCE_PROBLEMS=", nrow(bad)), "NOTHING WAS COMPARED: required reference evidence is missing or does not match the declaration; nothing was passed."), c("# Cold-burden validation: reference provenance FAILED", "", paste0("* Reference folder: `", reference_dir, "`"), paste0("* Problems: ", nrow(bad), " (see coldburden_reference_provenance.csv)"), "", "No comparison was made and nothing was passed.")); fr$provenance <- refs$provenance; return(fr) }
  ih <- siw_file_hash(data_csv); raw <- siw_read_study_csv(data_csv, config); rep <- siw_validate_input(raw, config, lag_end = P$lag_end); W(rep, "input_validation_report.csv"); if (any(rep$status == "FAIL")) stop("SIW_INPUT_VALIDATION_FAILED: see input_validation_report.csv", call. = FALSE)
  dat <- siw_prepare_data(raw, config); sec <- c(); tk <- function(nm, f) { a <- proc.time()[["elapsed"]]; v <- f(); sec[[nm]] <<- round(proc.time()[["elapsed"]] - a, 2); v }
  siw_log("thresholds, burden features and the scan (22,302 models expected)"); scan <- tk("scan_total", function() siw_cold_burden_scan(dat, config, protocol, windows = if (is.null(P$windows_subset)) NULL else as.integer(unlist(P$windows_subset)))); res <- scan$results; thr <- scan$thresholds; sec[["scan_features"]] <- round(scan$seconds[["features"]], 2); sec[["scan_fits_and_diagnostics"]] <- round(scan$seconds[["fits"]], 2)
  tol <- config$tolerances; tol$name <- protocol$tolerance$class; keysR <- unlist(protocol$reference_files[[1L]]$keys); keysT <- unlist(protocol$reference_files[[2L]]$keys); ref <- refs$tables$cold_burden_results; reft <- refs$tables$cold_thresholds
  cR <- Filter(function(z) identical(z$id, "RESULTS_22302"), protocol$comparisons)[[1L]]; cT <- Filter(function(z) identical(z$id, "THRESHOLDS_15"), protocol$comparisons)[[1L]]
  rr <- tk("compare_results", function() .siw_cold_cmp(cR, res, ref, keysR, tol, "cold_burden_results.csv")); rt <- .siw_cold_cmp(cT, thr, reft, keysT, tol, "cold_thresholds.csv"); summ <- rbind(rr$summary, rt$summary); summ$expected_compared_cells <- c(cR$expected_compared_cells, cT$expected_compared_cells); summ$cells_as_declared <- summ$n_compared == summ$expected_compared_cells; summ$infinite_equal_pairs <- c(rr$inf_pairs, rt$inf_pairs)
  summ$infinite_pairs_as_declared <- c(rr$inf_pairs == cR$expected_infinite_equal_pairs, TRUE); det <- siw_bind_rows(list(rr$detail, rt$detail)); W(summ, "coldburden_comparison_summary.csv"); W(det, "coldburden_comparison_detail.csv")
  jk <- do.call(paste, c(lapply(res[keysR], as.character), sep = "|")); jr <- do.call(paste, c(lapply(ref[keysR], as.character), sep = "|")); j <- match(jr, jk); cat_rows <- list()
  for (f in unlist(protocol$categorical_comparisons$fields)) { a <- as.character(res[[f]][j]); b <- as.character(ref[[f]]); a[is.na(a) | a == "NA"] <- ""; b[is.na(b) | b == "NA"] <- ""; if (f == "convergence") { a[a == "TRUE"] <- "TRUE"; }; cat_rows[[f]] <- data.frame(field = f, n_compared = length(b), n_equal = sum(a == b & !is.na(j)), n_different = sum(a != b | is.na(j)), stringsAsFactors = FALSE) }
  ra <- as.character(res$not_estimable_reason[j]); rb <- as.character(ref$not_estimable_reason); cls <- function(z) ifelse(is.na(z) | z == "", "", ifelse(grepl("^RANK_DEFICIENT", z), "RANK_DEFICIENT", "OTHER")); cat_rows$not_estimable_reason_class <- data.frame(field = "not_estimable_reason_class", n_compared = length(rb), n_equal = sum(cls(ra) == cls(rb) & !is.na(j)), n_different = sum(cls(ra) != cls(rb) | is.na(j)), stringsAsFactors = FALSE)
  catt <- do.call(rbind, cat_rows); W(catt, "coldburden_categorical_comparison.csv"); W(res, "coldburden_package_results.csv"); W(thr, "coldburden_package_thresholds.csv")
  fv <- tk("verify_features", function() siw_cold_verify_features(dat, config, thr, protocol)); W(fv, "coldburden_feature_verification.csv"); tv <- siw_cold_verify_thresholds(dat, config, thr, protocol); tvr <- siw_cold_verify_thresholds(dat, config, reft, protocol); tv$source <- "package"; tvr$source <- "frozen"; W(rbind(tv, tvr), "coldburden_threshold_verification.csv")
  sdy <- vapply(config$outcomes$outcome, function(o) stats::sd(as.numeric(dat[[o]])[stats::complete.cases(siw_tier_matrix(dat, config$tiers[[P$tier]])) & is.finite(as.numeric(dat[[o]]))]), 0); names(sdy) <- config$outcomes$outcome
  idp <- siw_cold_identities(res, sdy); idp$source <- "package"; idr <- siw_cold_identities(ref, NULL); idr$source <- "frozen"; W(rbind(idp, idr), "coldburden_scale_and_inference_identities.csv")
  mp <- siw_cold_verify_multiplicity(res); mp$source <- "package"; mr <- siw_cold_verify_multiplicity(ref); mr$source <- "frozen"; mult <- rbind(mp, mr); fam_ok <- data.frame(check = "family counts equal the declaration (q_global 1; q_outcome 3; q_curve 378)", value = paste(1L, length(unique(res$outcome)), length(unique(paste(res$outcome, res$metric, res$threshold_name, res$burden_measure, res$transformation)[res$status == "COMPLETED"]))), criterion = "1; 3; 378", status = "INFO", source = "package", stringsAsFactors = FALSE); W(rbind(mult, fam_ok), "coldburden_multiplicity_verification.csv")
  rf <- tk("independent_refit", function() siw_cold_independent_refit(dat, config, res, thr, protocol)); W(rf, "coldburden_independent_refit.csv"); sit <- .siw_cold_si_check(res, protocol); W(sit, "coldburden_si_statement_check.csv")
  desc <- stats::aggregate(cbind(models = 1, estimable = as.integer(res$status == "COMPLETED"), support_eligible = as.integer(res$support_eligible %in% TRUE), separation_suspected = as.integer(res$separation_status == "SUSPECTED"), leverage_ge_0_9 = as.integer(res$maximum_leverage >= 0.9 & is.finite(res$maximum_leverage)), q_global_le_0_05 = as.integer(res$q_global <= 0.05 & is.finite(res$q_global))) ~ metric + threshold_name, data = res, FUN = sum); W(desc, "coldburden_descriptive_flags.csv")
  nv <- data.frame(item = c("scientific interpretation of any cold-burden model", "bootstrap / resampling, nested cross-validation and candidate-selection analyses of the earlier archive", "Firth sensitivity of the legacy fitting function (not part of the frozen cold-burden output)", "figures, manuscript and SI text"), status = "NOT_VALIDATED_NOT_IN_SCOPE", stringsAsFactors = FALSE); W(nv, "coldburden_not_validated.csv")
  okc <- function(r, c) grepl("^PASS", r$status) && isTRUE(r$cells_as_declared); r1 <- summ[1L, ]; r2 <- summ[2L, ]; catok <- all(catt$n_different == 0L)
  st <- c(EXECUTION_STATUS = "PASS", INPUT_PROVENANCE_STATUS = "PASS", THRESHOLDS_STATUS = if (okc(r2) && all(tv$status == "PASS") && all(tvr$status == "PASS")) "PASS" else "FAIL",
          NUMERICAL_REPRODUCTION_STATUS = if (okc(r1) && isTRUE(r1$infinite_pairs_as_declared) && catok) "PASS" else "FAIL", FEATURE_VERIFICATION_STATUS = .siw_cold_status(fv$status), SCALE_AND_INFERENCE_STATUS = .siw_cold_status(c(idp$status, idr$status)),
          MULTIPLICITY_STATUS = .siw_cold_status(c(mp$status[mp$status != "INFO"], mr$status[mr$status != "INFO"])), INDEPENDENT_REFIT_STATUS = .siw_cold_status(rf$status), SI_STATEMENT_STATUS = .siw_cold_status(sit$status))
  st[["COVERAGE_STATUS"]] <- if (nrow(summ) == 2L && all(summ$n_unclassified == 0L) && all(summ$cells_as_declared) && all(summ$n_records_reference == summ$expected_records) && all(summ$n_records_results == summ$n_records_reference) && nrow(res) == cR$expected_records) "PASS" else "FAIL"
  sm1 <- siw_source_manifest(); st[["SOURCE_INTEGRITY_STATUS"]] <- if (!isTRUE(siw_source_manifest_diff(sm0, sm1)$identical_fingerprint)) "FAIL" else "PASS"; overall <- if (any(st == "FAIL")) "VERIFICATION_FAILURE" else "OK_WITH_INTERPRETATION_NOT_ASSESSED"
  info <- c(paste0("MODELS_FITTED=", nrow(res), " (estimable ", sum(res$status == "COMPLETED"), "; not estimable ", sum(res$status == "NOT_ESTIMABLE"), ")"), paste0("COMPARED_CELLS_RESULTS=", r1$n_compared, " (declared ", r1$expected_compared_cells, "); EXACT ", r1$n_exact, "; WITHIN_TOLERANCE ", r1$n_within_tolerance, "; DIFFERENT ", r1$n_different),
            paste0("INFINITE_EQUAL_PAIRS=", r1$infinite_equal_pairs, " (declared ", cR$expected_infinite_equal_pairs, ")"), paste0("COMPARED_CELLS_THRESHOLDS=", r2$n_compared, " (declared ", r2$expected_compared_cells, ")"), paste0("RUNTIME_SECONDS_SCAN=", sec[["scan_total"]], " (features ", sec[["scan_features"]], "; fits and diagnostics ", sec[["scan_fits_and_diagnostics"]], ")"), paste0("RUNTIME_SECONDS_TOTAL=", round(proc.time()[["elapsed"]] - t0, 1)))
  jsonlite::write_json(list(package_version = siw_pkg_version(), batch = "3F_cold_burden_validation", protocol_hash = protocol$protocol_hash, config_hash = config$config_hash, input_hash = ih$value, source_fingerprint_sha256 = sm0$fingerprint, source_files = sm0$n_files, reference_hashes = vapply(protocol$reference_files, function(z) z$sha256, ""), glm_epsilon_used = P$glm_epsilon, glm_maxit_used = P$glm_maxit,
                           statuses = as.list(st), seconds = as.list(sec), created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)), file.path(out_dir, "coldburden_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  W(summ[, c("reference", "expected_records", "n_records_reference", "n_records_results", "expected_compared_cells", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_nonfinite", "n_unmatched", "n_duplicate", "n_missing_required", "n_unclassified", "cells_as_declared", "infinite_equal_pairs", "status", "reason")], "coldburden_comparison_coverage.csv")
  fin <- finish(st, overall, info, c("# Exploratory cold-burden scan (22,302 models): numerical reproduction and mathematical verification", "", paste0("* Protocol `", protocol$protocol_hash, "`; source fingerprint `", sm0$fingerprint, "` (", sm0$n_files, " files)"), "* Scientific interpretation is NOT assessed; no model is promoted.", "", "## Statuses", "", paste0("* ", names(st), ": ", st), "", "## Counts and runtime", "", paste0("* ", info), "", "Nothing in this folder contains participant-level values."))
  fin$summary <- summ; fin$detail <- det; fin$results <- res; fin$thresholds <- thr; fin$categorical <- catt; fin$features <- fv; fin$identities <- rbind(idp, idr); fin$multiplicity <- rbind(mult, fam_ok); fin$refit <- rf; fin$si <- sit; fin$provenance <- refs$provenance; fin$seconds <- sec; fin
}
