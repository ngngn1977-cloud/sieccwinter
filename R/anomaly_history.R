# Phase 3I: the two EXPLORATORY families of the archived V1.4 programme that the deposited record lists as ANOM and ALT (kept separate from primary inference and from every earlier family).
#   ANOM : daily-reference anomaly scan, 7,080 models (WINTER_CODE R/120_daily_anomaly.R; horizon L01-L59): leave-one-out reference of each calendar day, stable / zero-safe anomalies, window features, adjusted M2_L single-exposure models.
#   ALT  : alternative exposure-history methods, 312 tests (R/130_alternative_methods.R; histories L01-L56): joint three-period test, weighted cumulative features, trajectory contrasts, trajectory PCA.
# Declared in config/anomaly_history_validation_protocol.json before any run. The validated engines are reused (siw_fit_gaussian, siw_fit_logistic, siw_model_diagnostics_one through .siw_cold_one); the group totals, window features and weighted
# features are computed once instead of repeatedly, with the original estimators and missing-data rules. The Firth fit that the original also ran for the binary outcome is not part of any frozen output and is not repeated.

#' Load the declared anomaly / alternative-history validation protocol (config/anomaly_history_validation_protocol.json).
#' @export
siw_ah_protocol <- function(path = NULL) {
  if (is.null(path)) { root <- siw_root(); cand <- c(file.path(root, "config", "anomaly_history_validation_protocol.json"), file.path(root, "extdata", "anomaly_history_validation_protocol.json"), file.path(root, "inst", "extdata", "anomaly_history_validation_protocol.json")); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_AH_PROTOCOL_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value; p
}

.siw_ah_id <- function(x) { x <- gsub("[^A-Za-z0-9._-]+", "_", as.character(x)); gsub("_+", "_", x) }                                  # the original siecc_sanitize_id
.siw_ah_fields <- function(protocol, id) unlist(Filter(function(z) identical(z$id, id), protocol$comparisons)[[1L]]$fields)
.siw_ah_matrix <- function(dat, prefix, n_lag) { m <- as.matrix(dat[, siw_daily_names(prefix, n_lag), drop = FALSE]); storage.mode(m) <- "double"; m }
.siw_ah_cum <- function(M) matrix(t(apply(M, 1L, cumsum)), nrow(M), ncol(M))                                                          # row-wise cumulative sums, also for one column

# ---- ANOM: reference anomalies -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' Leave-one-out reference anomalies of every participant-day. Group = metric x calendar day (reference 'all') or metric x calendar day x locality ('locality'); calendar day = assessment_day - lag (days since the first assessment, a fixed offset of the date).
#' reference_mean_loo = (total - value) / (n - 1) over the finite values of the group (NA for a non-finite value or n < 2); stable_anomaly divides by the pooled SD of the residuals of the metric; zero_safe_log_anomaly (rainfall metrics) = log1p(value) - log1p(loo).
#' Returns, per metric, the four anomaly matrices (participants x lags) and the diagnostics table of the original (one row per reference, metric and method).
#' @export
siw_anomaly_values <- function(dat, config, protocol = siw_ah_protocol(), reference = c("all", "locality")) {
  reference <- match.arg(reference); P <- protocol$parameters; L <- as.integer(P$lag_end); n <- nrow(dat); day <- as.numeric(dat$assessment_day)
  if (anyNA(day)) stop("SIW_ANOMALY_ASSESSMENT_DAY_MISSING: every participant needs assessment_day (calendar day = assessment_day - lag)", call. = FALSE)
  CD <- as.vector(outer(day, seq_len(L), "-")); loc <- rep(as.character(dat$Locality_3group), L); key <- if (reference == "all") as.character(CD) else paste(CD, loc, sep = "|"); g <- match(key, unique(key)); idx <- split(seq_along(g), g)
  rain <- unlist(P$rain_metrics); vals <- list(); diag <- list(); methods <- c("literal_ratio", "daily_z", "stable_anomaly", "zero_safe_log_anomaly")
  for (mi in seq_len(nrow(config$metrics))) { sp <- config$metrics[mi, ]; x <- as.vector(.siw_ah_matrix(dat, sp$daily_prefix, L)); fin <- is.finite(x); tot <- nn <- sdg <- numeric(length(idx))
    for (k in seq_along(idx)) { v <- x[idx[[k]]]; v <- v[is.finite(v)]; tot[k] <- sum(v); nn[k] <- length(v); sdg[k] <- if (length(v) >= 2L) stats::sd(v) else NA_real_ }
    loo <- ifelse(fin & nn[g] > 1L, (tot[g] - x) / (nn[g] - 1L), NA_real_); resid <- x - loo; pooled <- stats::sd(resid[is.finite(resid)])
    A <- list(literal_ratio = (x - loo) / loo, daily_z = (x - loo) / sdg[g], stable_anomaly = (x - loo) / pooled, zero_safe_log_anomaly = if (sp$metric %in% rain) suppressWarnings(log1p(x) - log1p(loo)) else rep(NA_real_, length(x)))
    vals[[sp$metric]] <- lapply(A, function(v) matrix(v, n, L))
    for (m in methods) { v <- A[[m]]; f <- v[is.finite(v)]; diag[[length(diag) + 1L]] <- data.frame(metric = sp$metric, method = m, n_total = length(v), n_finite = length(f), missing_fraction = mean(!is.finite(v)), minimum = if (length(f)) min(f) else NA_real_, maximum = if (length(f)) max(f) else NA_real_,
      p01 = if (length(f)) as.numeric(stats::quantile(f, 0.01, names = FALSE)) else NA_real_, p99 = if (length(f)) as.numeric(stats::quantile(f, 0.99, names = FALSE)) else NA_real_, stringsAsFactors = FALSE) } }
  d <- do.call(rbind, diag); d$reference <- reference; list(reference = reference, metrics = vals, diagnostics = d)
}

#' Window features of an anomaly matrix for ALL windows at once: column W holds the feature of window W. NA if fewer than ceiling(0.8 x W) of the W values are finite; otherwise mean(x, na.rm = TRUE) (signed) or mean(abs(x), na.rm = TRUE) (absolute), as in the original.
#' @export
siw_anomaly_features <- function(A, min_fraction = 0.8) {
  L <- ncol(A); need <- ceiling(min_fraction * seq_len(L))
  one <- function(M) { M0 <- M; M0[is.na(M0)] <- 0; S <- .siw_ah_cum(M0); cf <- .siw_ah_cum(is.finite(M) * 1); cn <- .siw_ah_cum(!is.na(M) * 1); o <- S / cn; o[sweep(cf, 2L, need, "<")] <- NA_real_; o }
  list(signed_mean = one(A), absolute_mean = one(abs(A)))
}

#' The original per-participant feature of ONE window (reference route of the verification).
#' @export
siw_anomaly_feature_reference <- function(x, window, burden, min_fraction = 0.8) {
  x <- x[seq_len(window)]; if (sum(is.finite(x)) < ceiling(min_fraction * window)) return(NA_real_); if (burden == "signed_mean") mean(x, na.rm = TRUE) else mean(abs(x), na.rm = TRUE)
}

.siw_ah_methods <- function(metric, P) if (metric %in% unlist(P$rain_metrics)) c("stable_anomaly", "zero_safe_log_anomaly") else "stable_anomaly"

#' The anomaly scan: 2 references x 8 metrics x methods x 2 burdens x 59 windows x 3 outcomes = 7,080 adjusted M2_L models in the original order; q_global (all finite p), q_outcome (within outcome), q_curve (59 windows within outcome, metric, reference, method, burden).
#' @param windows optional subset of windows (tests only); diagnostics = FALSE skips the influence / calibration columns (speed tests only)
#' @return list(results, diagnostics (the 64-row table), features (named list of the feature vectors; never written), seconds)
#' @export
siw_anomaly_scan <- function(dat, config, protocol = siw_ah_protocol(), windows = NULL, diagnostics = TRUE) {
  P <- protocol$parameters; t0 <- proc.time()[["elapsed"]]; Wv <- as.integer(windows %||% seq_len(P$lag_end)); tier <- P$tier; Xc <- siw_tier_matrix(dat, config$tiers[[tier]]); tier_ok <- stats::complete.cases(Xc); cache <- .siw_new_cache(); fields <- .siw_ah_fields(protocol, "ANOM_7080")
  vals <- list(); dg <- list(); for (rf in c("all", "locality")) { vals[[rf]] <- siw_anomaly_values(dat, config, protocol, rf); dg[[rf]] <- vals[[rf]]$diagnostics }
  N <- 2L * sum(vapply(config$metrics$metric, function(m) length(.siw_ah_methods(m, P)), 0L)) * 2L * length(Wv) * nrow(config$outcomes); num <- matrix(NA_real_, N, length(fields), dimnames = list(NULL, fields))
  meta <- list(model_id = character(N), outcome = character(N), outcome_family = character(N), metric = character(N), exposure = rep(".__anomaly", N), exposure_unit = rep("daily_reference_anomaly", N), window_or_lag = integer(N), transformation = character(N), model_tier = rep(tier, N), primary_role = rep("EXPLORATORY_DAILY_ANOMALY", N),
               status = character(N), convergence = rep(NA, N), separation_status = character(N), not_estimable_reason = character(N), reference_definition = character(N), anomaly_method = character(N), burden_direction = character(N)); k <- 0L; store <- list()
  diagn <- c("condition_number", "maximum_vif", "calibration_intercept", "calibration_slope", "maximum_coefficient_magnitude", "maximum_cooks_distance", "maximum_leverage", "maximum_absolute_dfbeta", "influential_case_count", "exposure_p10", "exposure_p25", "exposure_p75", "exposure_p90")
  for (rf in c("all", "locality")) for (mi in seq_len(nrow(config$metrics))) { sp <- config$metrics[mi, ]
    for (method in .siw_ah_methods(sp$metric, P)) { F <- siw_anomaly_features(vals[[rf]]$metrics[[sp$metric]][[method]], P$min_available_fraction)
      for (burden in c("signed_mean", "absolute_mean")) for (w in Wv) { x <- F[[burden]][, w]; store[[paste(rf, sp$metric, method, burden, w, sep = "|")]] <- x
        for (oi in seq_len(nrow(config$outcomes))) { o <- config$outcomes$outcome[oi]; fam <- config$outcomes$family[oi]; r <- .siw_cold_one(as.numeric(dat[[o]]), fam, x, Xc, tier_ok, o, tier, cache, config, P, diagnostics); st <- r$st; d <- r$d; k <- k + 1L
          g <- function(nm) { v <- st[[nm]]; if (is.null(v)) NA_real_ else as.numeric(v) }; gd <- function(nm) { v <- if (is.null(d)) NULL else d[[nm]]; if (is.null(v)) NA_real_ else as.numeric(v) }
          for (fl in fields) num[k, fl] <- if (fl == "n") r$n else if (fl == "parameter_count") (if (r$est) g("p") else g("parameter_count")) else if (fl %in% diagn) gd(fl) else if (fl %in% c("q_global", "q_curve", "q_outcome")) NA_real_ else g(fl)
          meta$model_id[k] <- paste("ANOMALY", .siw_ah_id(o), .siw_ah_id(sp$metric), rf, method, burden, paste0("W", w), sep = "__"); meta$outcome[k] <- o; meta$outcome_family[k] <- fam; meta$metric[k] <- sp$metric; meta$window_or_lag[k] <- w; meta$transformation[k] <- paste(rf, method, burden, sep = ":")
          meta$status[k] <- if (r$est) "COMPLETED" else "NOT_ESTIMABLE"; meta$separation_status[k] <- st$separation_status %||% ""; meta$convergence[k] <- if (is.null(st$convergence)) NA else st$convergence; meta$not_estimable_reason[k] <- if (r$est) "" else (st$reason %||% ""); meta$reference_definition[k] <- rf; meta$anomaly_method[k] <- method; meta$burden_direction[k] <- burden } } } }
  stopifnot(k == N); res <- cbind(as.data.frame(meta, stringsAsFactors = FALSE), as.data.frame(num)); res <- siw_anomaly_assign_q(res); rownames(res) <- NULL
  list(results = res, diagnostics = do.call(rbind, dg), features = store, seconds = proc.time()[["elapsed"]] - t0)
}

#' Benjamini-Hochberg q_global (all finite p), q_outcome (within outcome) and q_curve (over the windows within outcome, metric, reference, method and burden).
#' @export
siw_anomaly_assign_q <- function(res) {
  res$q_global <- siw_bh(res$nominal_p); res$q_outcome <- res$q_curve <- NA_real_; ok <- is.finite(res$nominal_p)
  for (o in unique(res$outcome)) { i <- which(res$outcome == o & ok); res$q_outcome[i] <- siw_bh(res$nominal_p[i]) }
  fam <- paste(res$outcome, res$metric, res$reference_definition, res$anomaly_method, res$burden_direction, sep = "|"); for (f in unique(fam[ok])) { i <- which(ok & fam == f); res$q_curve[i] <- siw_bh(res$nominal_p[i]) }
  res
}

# ---- ANOM: independent verification --------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' Independent recomputation of the reference anomalies: every participant-day directly from the other participant-days of the same calendar day (no group totals), the pooled scale from its own formula, the zero-safe anomaly and the diagnostics (own type-7 quantiles).
#' Compares with siw_anomaly_values for one reference and returns the maximum relative differences and the agreement of the missing-value pattern (whole grid: all metrics, participants and lags).
#' @export
siw_anomaly_values_verify <- function(dat, config, protocol, vals) {
  P <- protocol$parameters; L <- as.integer(P$lag_end); n <- nrow(dat); day <- as.numeric(dat$assessment_day); CD <- as.vector(outer(day, seq_len(L), "-")); loc <- rep(as.character(dat$Locality_3group), L); rain <- unlist(P$rain_metrics); rows <- list()
  rel <- function(a, b) { k <- is.finite(a) & is.finite(b); if (!any(k)) 0 else max(abs(a[k] - b[k]) / pmax(abs(b[k]), 1)) }                  # relative to max(1, |b|): anomalies are on a unit scale and a value that is zero up to rounding must not give an unbounded ratio
  for (mi in seq_len(nrow(config$metrics))) { sp <- config$metrics[mi, ]; x <- as.vector(.siw_ah_matrix(dat, sp$daily_prefix, L)); m <- length(x); loo <- rep(NA_real_, m); fx <- is.finite(x)
    for (j in which(fx)) { same <- if (vals$reference == "all") CD == CD[j] else CD == CD[j] & loc == loc[j]; same[j] <- FALSE; o <- x[same & fx]; if (length(o) >= 1L) loo[j] <- sum(o) / length(o) }
    r <- x - loo; rf <- r[is.finite(r)]; pooled <- sqrt(sum((rf - mean(rf))^2) / (length(rf) - 1L)); st <- (x - loo) / pooled; zs <- if (sp$metric %in% rain) suppressWarnings(log1p(x) - log1p(loo)) else rep(NA_real_, m)
    pk <- as.vector(vals$metrics[[sp$metric]]$stable_anomaly); pz <- as.vector(vals$metrics[[sp$metric]]$zero_safe_log_anomaly)
    qf <- function(v, p) if (length(v)) .siw_dv_q7(v, p) else NA_real_; d <- vals$diagnostics[vals$diagnostics$metric == sp$metric, ]
    fs <- st[is.finite(st)]; fz <- zs[is.finite(zs)]; dst <- d[d$method == "stable_anomaly", ]; dzs <- d[d$method == "zero_safe_log_anomaly", ]
    rows[[length(rows) + 1L]] <- data.frame(reference = vals$reference, metric = sp$metric, cells = m, missing_pattern_equal_stable = identical(is.finite(st), is.finite(pk)), missing_pattern_equal_zero_safe = identical(is.finite(zs), is.finite(pz)), max_rel_diff_stable = rel(pk, st), max_rel_diff_zero_safe = rel(pz, zs),
      max_rel_diff_diagnostics = max(rel(dst$minimum, min(fs)), rel(dst$maximum, max(fs)), rel(dst$p01, qf(fs, 0.01)), rel(dst$p99, qf(fs, 0.99)), if (length(fz)) max(rel(dzs$minimum, min(fz)), rel(dzs$maximum, max(fz)), rel(dzs$p01, qf(fz, 0.01)), rel(dzs$p99, qf(fz, 0.99))) else 0), n_finite_equal = dst$n_finite == length(fs) && dzs$n_finite == length(fz), stringsAsFactors = FALSE) }
  r <- do.call(rbind, rows); r$status <- ifelse(r$missing_pattern_equal_stable & r$missing_pattern_equal_zero_safe & r$n_finite_equal & pmax(r$max_rel_diff_stable, r$max_rel_diff_zero_safe, r$max_rel_diff_diagnostics) <= 1e-9, "PASS", "FAIL"); r
}

#' Every window feature (2 references x metric-method combinations x 2 burdens x windows x participants) recomputed with the literal per-participant function and compared with the vectorised features.
#' @export
siw_anomaly_features_verify <- function(config, protocol, vals_all, vals_loc, windows = NULL) {
  P <- protocol$parameters; Wv <- as.integer(windows %||% seq_len(P$lag_end)); rows <- list()
  for (vals in list(vals_all, vals_loc)) for (mi in seq_len(nrow(config$metrics))) { sp <- config$metrics[mi, ]; for (method in .siw_ah_methods(sp$metric, P)) { A <- vals$metrics[[sp$metric]][[method]]; F <- siw_anomaly_features(A, P$min_available_fraction)
    for (burden in c("signed_mean", "absolute_mean")) { md <- 0; bad <- 0L; cells <- 0L; for (w in Wv) for (i in seq_len(nrow(A))) { a <- F[[burden]][i, w]; b <- siw_anomaly_feature_reference(A[i, ], w, burden, P$min_available_fraction); cells <- cells + 1L; if (is.finite(a) != is.finite(b)) bad <- bad + 1L else if (is.finite(a)) md <- max(md, abs(a - b) / max(abs(b), 1)) }                  # relative to max(1, |b|): a mean of anomalies that cancels to zero up to rounding must not give an unbounded ratio
      rows[[length(rows) + 1L]] <- data.frame(reference = vals$reference, metric = sp$metric, method = method, burden = burden, cells = cells, availability_mismatches = bad, max_rel_diff = md, stringsAsFactors = FALSE) } } }
  r <- do.call(rbind, rows); r$status <- ifelse(r$availability_mismatches == 0L & r$max_rel_diff <= 1e-12, "PASS", "FAIL"); r
}

#' Independent lm / glm refit of the models of a results table (every `every`-th model), compared at the cumulative tolerance class. Returns the per-field summary and the failing comparisons.
#' @export
siw_anomaly_refit_verify <- function(dat, config, res, features, protocol, every = 1L, tol = config$tolerances) {
  P <- protocol$parameters; covs <- config$tiers[[P$tier]]; sub <- seq(1L, nrow(res), by = as.integer(every)); rows <- list(); nref <- 0L
  for (i in sub) { r <- res[i, ]; if (r$status != "COMPLETED") next; x <- features[[paste(r$reference_definition, r$metric, r$anomaly_method, r$burden_direction, r$window_or_lag, sep = "|")]]; ref1 <- tryCatch(.siw_cold_refit_reference(dat, config, r$outcome, r$outcome_family, x, covs), error = function(e) NULL); nref <- nref + 1L
    if (is.null(ref1)) { rows[[length(rows) + 1L]] <- data.frame(model_id = r$model_id, field = "REFIT_ERROR", package = NA_real_, independent = NA_real_, ok = FALSE, stringsAsFactors = FALSE); next }
    for (f in names(ref1)) { a <- r[[f]]; b <- ref1[[f]]; dd <- abs(a - b); ok <- is.finite(dd) && dd <= tol$cumulative_abs + tol$cumulative_rel * abs(b); rows[[length(rows) + 1L]] <- data.frame(model_id = r$model_id, field = f, package = a, independent = b, ok = ok, stringsAsFactors = FALSE) } }
  d <- do.call(rbind, rows); s <- do.call(rbind, lapply(split(d, d$field), function(z) data.frame(field = z$field[1L], models = nrow(z), failures = sum(!z$ok), max_rel_diff = suppressWarnings(max(abs(z$package - z$independent) / pmax(abs(z$independent), 1e-12), na.rm = TRUE)), stringsAsFactors = FALSE)))
  s$status <- ifelse(s$failures == 0L, "PASS", "FAIL"); list(summary = s, failures = d[!d$ok, , drop = FALSE], models_refitted = nref, models_total = nrow(res), every = as.integer(every))
}

#' BH multiplicity of the anomaly results (q_global, q_outcome, q_curve) recomputed with stats::p.adjust over the declared families.
#' @export
siw_anomaly_multiplicity_verify <- function(res) {
  ok <- is.finite(res$nominal_p); g <- max(abs(res$q_global[ok] - stats::p.adjust(res$nominal_p[ok], "BH"))); o <- max(vapply(unique(res$outcome), function(u) { i <- which(ok & res$outcome == u); max(abs(res$q_outcome[i] - stats::p.adjust(res$nominal_p[i], "BH"))) }, 0))
  fam <- paste(res$outcome, res$metric, res$reference_definition, res$anomaly_method, res$burden_direction, sep = "|"); cs <- vapply(unique(fam[ok]), function(f) { i <- which(ok & fam == f); max(abs(res$q_curve[i] - stats::p.adjust(res$nominal_p[i], "BH"))) }, 0)
  data.frame(check = c("q_global vs p.adjust BH", "q_outcome vs p.adjust BH", "q_curve vs p.adjust BH", "q_outcome families", "q_curve families", "q_curve family size (windows)"), value = c(g, o, max(cs), length(unique(res$outcome)), length(cs), as.numeric(max(table(fam[ok])))), status = c(ifelse(c(g, o, max(cs)) <= 1e-12, "PASS", "FAIL"), "INFO", "INFO", "INFO"), stringsAsFactors = FALSE)
}

# ---- ALT -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' Histories of one metric (participants x lags 1..max_lag), the original siecc_history_matrix.
#' @export
siw_alt_history <- function(dat, config, metric, max_lag = 59L) .siw_ah_matrix(dat, config$metrics$daily_prefix[config$metrics$metric == metric], max_lag)

.siw_alt_agg <- function(x, aggregation) { available <- rowSums(is.finite(x)); out <- if (aggregation == "mean") rowMeans(x, na.rm = TRUE) else rowSums(x, na.rm = TRUE); out[available == 0] <- NA_real_; out }
.siw_alt_z <- function(x) { x <- as.numeric(x); s <- stats::sd(x, na.rm = TRUE); if (!is.finite(s) || s <= 0) return(rep(NA_real_, length(x))); (x - mean(x, na.rm = TRUE)) / s }

#' The three disjoint periods (1-7, 8-28, 29-56 days) aggregated by the metric's rule (mean or sum).
#' @export
siw_alt_disjoint <- function(H, aggregation) data.frame(recent_1_7 = .siw_alt_agg(H[, 1:7, drop = FALSE], aggregation), intermediate_8_28 = .siw_alt_agg(H[, 8:28, drop = FALSE], aggregation), remote_29_56 = .siw_alt_agg(H[, 29:56, drop = FALSE], aggregation), stringsAsFactors = FALSE)

#' Weighted cumulative feature: weights 0.5^((lag - 1) / half_life) (equal weights for an infinite half-life) over lags 1..max_lag, NA if fewer than ceiling(0.8 x max_lag) finite lags, otherwise the weighted mean of the finite lags.
#' @export
siw_alt_weighted <- function(H, half_life = Inf, max_lag = 56L, min_fraction = 0.8) {
  h <- H[, seq_len(max_lag), drop = FALSE]; w <- if (is.infinite(half_life)) rep(1, max_lag) else 0.5^((seq_len(max_lag) - 1) / half_life)
  vapply(seq_len(nrow(h)), function(i) { keep <- is.finite(h[i, ]); if (sum(keep) < ceiling(min_fraction * max_lag)) return(NA_real_); sum(h[i, keep] * w[keep]) / sum(w[keep]) }, numeric(1L))
}

#' Trajectory contrasts over lags 1..max_lag: overall mean, mean(1-14) - mean(43-56), linear and curvature contrasts normalised by the sum of |weights| over the finite lags.
#' @export
siw_alt_contrasts <- function(H, max_lag = 56L) {
  h <- H[, seq_len(max_lag), drop = FALSE]; lag <- seq_len(max_lag); linear <- as.numeric(scale(lag, center = TRUE, scale = FALSE)); quadratic <- linear^2 - mean(linear^2)
  data.frame(overall_mean = rowMeans(h, na.rm = TRUE), recent_minus_remote = rowMeans(h[, 1:14, drop = FALSE], na.rm = TRUE) - rowMeans(h[, 43:56, drop = FALSE], na.rm = TRUE),
             linear_lag_contrast = apply(h, 1L, function(x) sum(x * linear, na.rm = TRUE) / sum(abs(linear[is.finite(x)]))), curvature_contrast = apply(h, 1L, function(x) sum(x * quadratic, na.rm = TRUE) / sum(abs(quadratic[is.finite(x)]))), stringsAsFactors = FALSE)
}

#' Joint test of several exposures against the covariate-only model on complete cases: gaussian F test from the residual sums of squares; binary likelihood-ratio chi-square (ordinary glm).
#' @export
siw_alt_joint <- function(data, outcome, family, exposures, covariates) {
  variables <- unique(c(outcome, exposures, covariates)); complete <- stats::complete.cases(data[, variables, drop = FALSE]); d <- droplevels(data[complete, variables, drop = FALSE]); ff <- stats::reformulate(c(exposures, covariates), outcome); fr <- if (length(covariates)) stats::reformulate(covariates, outcome) else stats::as.formula(paste(outcome, "~ 1"))
  if (family == "gaussian") { full <- stats::lm(ff, data = d); red <- stats::lm(fr, data = d); rf <- sum(stats::residuals(full)^2); rr <- sum(stats::residuals(red)^2); df1 <- length(exposures); df2 <- stats::df.residual(full); stat <- ((rr - rf) / df1) / (rf / df2); p <- stats::pf(stat, df1, df2, lower.tail = FALSE)
  } else { full <- suppressWarnings(stats::glm(ff, data = d, family = stats::binomial())); red <- suppressWarnings(stats::glm(fr, data = d, family = stats::binomial())); stat <- 2 * (as.numeric(stats::logLik(full)) - as.numeric(stats::logLik(red))); df1 <- length(exposures); df2 <- NA_real_; p <- stats::pchisq(stat, df1, lower.tail = FALSE) }
  data.frame(n = nrow(d), statistic = stat, df1 = df1, df2 = df2, p = p, stringsAsFactors = FALSE)
}

#' The alternative exposure-history scan: 8 metrics x 3 outcomes x 13 tests = 312 in the original order; q_global (all finite p) and q_outcome (within outcome). Features are built once per metric.
#' @param metrics optional subset of metrics (tests only)
#' @return list(results, pca_variance, features (per metric: history, disjoint, weighted, contrasts, scores, complete; never written), seconds)
#' @export
siw_alt_scan <- function(dat, config, protocol = siw_ah_protocol(), metrics = NULL) {
  P <- protocol$parameters; t0 <- proc.time()[["elapsed"]]; tier <- P$tier; covs <- config$tiers[[tier]]; Xc <- siw_tier_matrix(dat, covs); tier_ok <- stats::complete.cases(Xc); cache <- .siw_new_cache(); rows <- list(); pcar <- list(); store <- list(); hl <- unlist(P$half_lives); Hend <- as.integer(P$alt_history_end)
  for (mi in seq_len(nrow(config$metrics))) { sp <- config$metrics[mi, ]; if (!is.null(metrics) && !(sp$metric %in% metrics)) next
    H <- siw_alt_history(dat, config, sp$metric, P$lag_end); dj <- siw_alt_disjoint(H, sp$aggregation); ct <- siw_alt_contrasts(H, Hend)
    wt <- data.frame(half_life_3 = siw_alt_weighted(H, hl[1L], Hend, P$min_available_fraction), half_life_7 = siw_alt_weighted(H, hl[2L], Hend, P$min_available_fraction), half_life_14 = siw_alt_weighted(H, hl[3L], Hend, P$min_available_fraction), half_life_28 = siw_alt_weighted(H, hl[4L], Hend, P$min_available_fraction), uniform_56 = siw_alt_weighted(H, Inf, Hend, P$min_available_fraction), stringsAsFactors = FALSE)
    cc <- stats::complete.cases(H[, seq_len(Hend), drop = FALSE]); pca <- NULL; scores <- NULL
    if (sum(cc) >= P$pca_min_complete_cases) { pca <- stats::prcomp(H[cc, seq_len(Hend), drop = FALSE], center = TRUE, scale. = TRUE); vr <- pca$sdev^2 / sum(pca$sdev^2); pcar[[length(pcar) + 1L]] <- data.frame(metric = sp$metric, component = seq_along(vr), variance_fraction = vr, cumulative_variance = cumsum(vr), stringsAsFactors = FALSE)
      scores <- matrix(NA_real_, nrow(dat), min(as.integer(P$pca_components_tested), ncol(pca$x))); scores[cc, ] <- pca$x[, seq_len(ncol(scores)), drop = FALSE]; colnames(scores) <- paste0("PC", seq_len(ncol(scores))) }
    store[[sp$metric]] <- list(history = H, disjoint = dj, weighted = wt, contrasts = ct, scores = scores, complete = cc)
    for (oi in seq_len(nrow(config$outcomes))) { o <- config$outcomes$outcome[oi]; fam <- config$outcomes$family[oi]; y <- as.numeric(dat[[o]]); d <- dat; d$recent_1_7 <- .siw_alt_z(dj$recent_1_7); d$intermediate_8_28 <- .siw_alt_z(dj$intermediate_8_28); d$remote_29_56 <- .siw_alt_z(dj$remote_29_56)
      jt <- siw_alt_joint(d, o, fam, c("recent_1_7", "intermediate_8_28", "remote_29_56"), covs); rows[[length(rows) + 1L]] <- data.frame(outcome = o, metric = sp$metric, method = "three_disjoint_periods_joint", feature = "1-7|8-28|29-56", n = jt$n, effect = NA_real_, p = jt$p, stringsAsFactors = FALSE)
      one <- function(method, feature, x) { r <- .siw_cold_one(y, fam, x, Xc, tier_ok, o, tier, cache, config, list(glm_epsilon = P$glm_epsilon, glm_maxit = P$glm_maxit), diagnostics = FALSE); e <- r$st$standardised_effect; pv <- r$st$nominal_p
        data.frame(outcome = o, metric = sp$metric, method = method, feature = feature, n = r$n, effect = if (is.null(e)) NA_real_ else as.numeric(e), p = if (is.null(pv)) NA_real_ else as.numeric(pv), stringsAsFactors = FALSE) }
      for (f in names(wt)) rows[[length(rows) + 1L]] <- one("weighted_cumulative", f, wt[[f]]); for (f in names(ct)) rows[[length(rows) + 1L]] <- one("trajectory_contrast", f, ct[[f]]); if (!is.null(scores)) for (cn in colnames(scores)) rows[[length(rows) + 1L]] <- one("trajectory_PCA", cn, scores[, cn]) } }
  res <- do.call(rbind, rows); res$q_global <- siw_bh(res$p); res$q_outcome <- NA_real_; for (o in unique(res$outcome)) { i <- which(res$outcome == o & is.finite(res$p)); res$q_outcome[i] <- siw_bh(res$p[i]) }; rownames(res) <- NULL
  list(results = res, pca_variance = do.call(rbind, pcar), features = store, seconds = proc.time()[["elapsed"]] - t0)
}

# ---- ALT: independent verification ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' Independent recomputation of the features of every metric and participant with explicit loops over the lags (own means, own weights, own contrasts); returns the maximum differences and the agreement of the missing-value pattern.
#' @export
siw_alt_features_verify <- function(config, protocol, features) {
  P <- protocol$parameters; Hend <- as.integer(P$alt_history_end); hl <- unlist(P$half_lives); rows <- list(); lag <- seq_len(Hend); lin <- lag - mean(lag); quad <- lin^2 - mean(lin^2)
  for (m in names(features)) { S <- features[[m]]; H <- S$history; ag <- config$metrics$aggregation[config$metrics$metric == m]; n <- nrow(H); own <- list(); fin <- function(v) v[is.finite(v)]
    per <- list(recent_1_7 = 1:7, intermediate_8_28 = 8:28, remote_29_56 = 29:56); for (nm in names(per)) own[[nm]] <- vapply(seq_len(n), function(i) { v <- fin(H[i, per[[nm]]]); if (!length(v)) NA_real_ else if (ag == "mean") sum(v) / length(v) else sum(v) }, 0)
    for (j in seq_along(hl)) own[[paste0("half_life_", hl[j])]] <- vapply(seq_len(n), function(i) { w <- 0.5^((lag - 1) / hl[j]); k <- is.finite(H[i, lag]); if (sum(k) < ceiling(0.8 * Hend)) NA_real_ else sum(H[i, lag][k] * w[k]) / sum(w[k]) }, 0)
    own$uniform_56 <- vapply(seq_len(n), function(i) { k <- is.finite(H[i, lag]); if (sum(k) < ceiling(0.8 * Hend)) NA_real_ else sum(H[i, lag][k]) / sum(k) }, 0)
    mn <- function(v) { v <- fin(v); if (length(v)) sum(v) / length(v) else NA_real_ }; own$overall_mean <- vapply(seq_len(n), function(i) mn(H[i, lag]), 0); own$recent_minus_remote <- vapply(seq_len(n), function(i) mn(H[i, 1:14]) - mn(H[i, 43:56]), 0)
    own$linear_lag_contrast <- vapply(seq_len(n), function(i) { k <- is.finite(H[i, lag]); sum(H[i, lag][k] * lin[k]) / sum(abs(lin[k])) }, 0); own$curvature_contrast <- vapply(seq_len(n), function(i) { k <- is.finite(H[i, lag]); sum(H[i, lag][k] * quad[k]) / sum(abs(quad[k])) }, 0)
    pk <- cbind(S$disjoint, S$weighted, S$contrasts)
    for (nm in names(own)) { a <- pk[[nm]]; b <- own[[nm]]; k <- is.finite(a) & is.finite(b); rows[[length(rows) + 1L]] <- data.frame(metric = m, feature = nm, participants = n, finite_pattern_mismatches = sum(is.finite(a) != is.finite(b)), max_rel_diff = if (any(k)) max(abs(a[k] - b[k]) / pmax(abs(b[k]), 1e-12)) else 0, stringsAsFactors = FALSE) } }
  r <- do.call(rbind, rows); r$status <- ifelse(r$finite_pattern_mismatches == 0L & r$max_rel_diff <= 1e-9, "PASS", "FAIL"); r
}

#' Independent refit of every single-feature ALT model (lm / glm with the declared covariates; effect = b x SD(x) / SD(y) for the gaussian outcomes and b x SD(x) for the binary outcome over the complete cases) and of every joint test (own normal equations / own Newton iterations).
#' @export
siw_alt_refit_verify <- function(dat, config, res, features, protocol, tol = config$tolerances) {
  P <- protocol$parameters; covs <- config$tiers[[P$tier]]; rows <- list(); tl <- function(a, b) is.finite(a) && is.finite(b) && abs(a - b) <= tol$cumulative_abs + tol$cumulative_rel * abs(b)
  for (i in seq_len(nrow(res))) { r <- res[i, ]; fam <- config$outcomes$family[config$outcomes$outcome == r$outcome]; S <- features[[r$metric]]
    if (r$method == "three_disjoint_periods_joint") { d <- dat; for (nm in names(S$disjoint)) d[[nm]] <- .siw_alt_z(S$disjoint[[nm]]); vars <- unique(c(r$outcome, names(S$disjoint), covs)); cc <- stats::complete.cases(d[, vars, drop = FALSE]); md <- droplevels(d[cc, vars, drop = FALSE]); y <- md[[r$outcome]]
      Xf <- stats::model.matrix(stats::reformulate(c(names(S$disjoint), covs)), md); Xr <- stats::model.matrix(stats::reformulate(covs), md)
      if (fam == "gaussian") { rs <- function(X) { b <- solve(crossprod(X), crossprod(X, y)); sum((y - X %*% b)^2) }; rf <- rs(Xf); rr <- rs(Xr); df2 <- nrow(Xf) - ncol(Xf); st <- ((rr - rf) / 3) / (rf / df2); pv <- stats::pf(st, 3, df2, lower.tail = FALSE)
      } else { dev <- function(X) { mu <- (y + 0.5) / 2; eta <- log(mu / (1 - mu)); dold <- -2 * sum(y * log(mu) + (1 - y) * log(1 - mu)); for (it in 1:50) { w <- mu * (1 - mu); z <- eta + (y - mu) / w; b <- drop(solve(crossprod(X * w, X), crossprod(X * w, z))); eta <- drop(X %*% b); mu <- 1 / (1 + exp(-eta)); dn <- -2 * sum(y * log(mu) + (1 - y) * log(1 - mu)); cv <- abs(dn - dold) / (abs(dn) + 0.1) < 1e-12; dold <- dn; if (cv) break }; dn }
        st <- dev(Xr) - dev(Xf); pv <- stats::pchisq(st, 3, lower.tail = FALSE) }
      rows[[length(rows) + 1L]] <- data.frame(outcome = r$outcome, metric = r$metric, method = r$method, feature = r$feature, n_package = r$n, n_independent = nrow(md), p_package = r$p, p_independent = pv, effect_package = NA_real_, effect_independent = NA_real_, ok = (r$n == nrow(md)) && tl(r$p, pv), stringsAsFactors = FALSE)
    } else { x <- switch(r$method, weighted_cumulative = S$weighted[[r$feature]], trajectory_contrast = S$contrasts[[r$feature]], trajectory_PCA = S$scores[, r$feature]); ref1 <- tryCatch(.siw_cold_refit_reference(dat, config, r$outcome, fam, x, covs), error = function(e) c(coefficient = NA_real_, nominal_p = NA_real_)); cc <- stats::complete.cases(cbind(dat[, c(r$outcome, covs), drop = FALSE], .x = x)) & is.finite(x); xs <- x[cc]; ys <- as.numeric(dat[[r$outcome]])[cc]
      eff <- if (fam == "gaussian") ref1[["coefficient"]] * stats::sd(xs) / stats::sd(ys) else ref1[["coefficient"]] * stats::sd(xs)
      rows[[length(rows) + 1L]] <- data.frame(outcome = r$outcome, metric = r$metric, method = r$method, feature = r$feature, n_package = r$n, n_independent = sum(cc), p_package = r$p, p_independent = ref1[["nominal_p"]], effect_package = r$effect, effect_independent = eff, ok = (r$n == sum(cc)) && tl(r$p, ref1[["nominal_p"]]) && tl(r$effect, eff), stringsAsFactors = FALSE) } }
  d <- do.call(rbind, rows); d$status <- ifelse(d$ok, "PASS", "FAIL"); d$ok <- NULL; d
}

#' Independent check of the trajectory PCA: variance fractions from the eigen decomposition of the correlation matrix and the absolute component scores from the eigenvectors (sign free), compared with the package table and scores.
#' @export
siw_alt_pca_verify <- function(config, protocol, features, pca_variance, tol = config$tolerances) {
  Hend <- as.integer(protocol$parameters$alt_history_end); rows <- list()
  for (m in names(features)) { S <- features[[m]]; v <- pca_variance[pca_variance$metric == m, ]; if (is.null(S$scores) || !nrow(v)) { rows[[length(rows) + 1L]] <- data.frame(metric = m, components = 0L, max_abs_diff_variance = NA_real_, max_abs_diff_scores = NA_real_, status = "NOT_TESTED", stringsAsFactors = FALSE); next }
    Hc <- S$history[S$complete, seq_len(Hend), drop = FALSE]; Z <- sweep(sweep(Hc, 2L, colMeans(Hc), "-"), 2L, apply(Hc, 2L, stats::sd), "/"); e <- eigen(crossprod(Z) / (nrow(Z) - 1L), symmetric = TRUE); fr <- pmax(e$values, 0) / sum(pmax(e$values, 0)); k <- nrow(v)
    dv <- max(abs(fr[seq_len(k)] - v$variance_fraction)); ds <- max(vapply(seq_len(ncol(S$scores)), function(j) max(abs(abs(drop(Z %*% e$vectors[, j])) - abs(S$scores[S$complete, j]))), 0))
    rows[[length(rows) + 1L]] <- data.frame(metric = m, components = k, max_abs_diff_variance = dv, max_abs_diff_scores = ds, status = if (dv <= tol$cumulative_abs && ds <= 1e-6) "PASS" else "FAIL", stringsAsFactors = FALSE) }
  do.call(rbind, rows)
}

#' BH multiplicity of the ALT results (q_global, q_outcome) recomputed with stats::p.adjust.
#' @export
siw_alt_multiplicity_verify <- function(res) {
  ok <- is.finite(res$p); g <- max(abs(res$q_global[ok] - stats::p.adjust(res$p[ok], "BH"))); o <- max(vapply(unique(res$outcome), function(u) { i <- which(ok & res$outcome == u); max(abs(res$q_outcome[i] - stats::p.adjust(res$p[i], "BH"))) }, 0))
  data.frame(check = c("q_global vs p.adjust BH", "q_outcome vs p.adjust BH", "q_global family size", "q_outcome family sizes"), value = c(g, o, sum(ok), paste(unique(as.vector(table(res$outcome[ok]))), collapse = ",")), status = c(ifelse(c(g, o) <= 1e-12, "PASS", "FAIL"), "INFO", "INFO"), stringsAsFactors = FALSE)
}

#' Diagnostic of the sign of the principal-component rows: for each PC test the sign agreement, the agreement of |effect| and of p with the frozen table. A sign difference alone is never accepted silently (it is a difference in the comparison); the table lets a reviewer see whether it is the only difference.
#' @export
siw_alt_pca_sign_diagnostic <- function(new, ref, tol) {
  k <- c("outcome", "metric", "method", "feature"); a <- new[new$method == "trajectory_PCA", ]; j <- match(do.call(paste, c(a[k], sep = "|")), do.call(paste, c(ref[k], sep = "|"))); b <- ref[j, ]; eq <- function(x, y) is.finite(x) & is.finite(y) & abs(x - y) <= tol$cumulative_abs + tol$cumulative_rel * abs(y)
  data.frame(a[k], effect_package = a$effect, effect_frozen = b$effect, p_package = a$p, p_frozen = b$p, sign_equal = sign(a$effect) == sign(b$effect), abs_effect_equal = eq(abs(a$effect), abs(b$effect)), p_equal = eq(a$p, b$p), stringsAsFactors = FALSE)
}
