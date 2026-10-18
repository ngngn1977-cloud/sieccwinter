# Phase 3H / Batch D: the documented daily-lag layer of the V1.4 archived programme (horizon L01-L59; separate from the nine final L01-L56 DLMs).
#   XYD  : crude daily-lag associations, 3 outcomes x 8 metrics x 59 lags = 1,416 (WINTER_SOURCE R/060_xy.R siecc_xy_scan_daily)
#   DAY  : adjusted (M2_L) single-day regressions, 1,416 (R/070_cumulative_models.R siecc_single_day_adjusted_grid)
#   MAXT : outcome-wide single-step max|r| permutation test over the 80 cumulative exposures of each outcome, 3 x 80 = 240 (R/100_resampling.R siecc_xy_permutation_maxT)
# Declared in config/daily_layer_validation_protocol.json before any run. The validated engines are reused (siw_fit_gaussian, siw_fit_logistic, siw_model_diagnostics_one); only redundant work is removed.
# The original sampling sequence of MAXT is preserved: set.seed(seed); one sample(y) per replicate.

#' Load the declared daily-layer validation protocol (config/daily_layer_validation_protocol.json).
#' @export
siw_daily_protocol <- function(path = NULL) {
  if (is.null(path)) { root <- siw_root(); cand <- c(file.path(root, "config", "daily_layer_validation_protocol.json"), file.path(root, "extdata", "daily_layer_validation_protocol.json"), file.path(root, "inst", "extdata", "daily_layer_validation_protocol.json")); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_DAILY_PROTOCOL_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value; p
}

# ---- XYD ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' Pearson (point-biserial for a 0/1 outcome), Spearman (exact = FALSE) and Kendall (exact = FALSE) of one pair under the original rules: finite pairs only; fewer than 4 pairs or a constant variable gives NA.
#' @export
siw_correlation_set_full <- function(x, y) {
  keep <- is.finite(x) & is.finite(y); x <- as.numeric(x[keep]); y <- as.numeric(y[keep])
  if (length(x) < 4L || length(unique(x)) < 2L || length(unique(y)) < 2L) return(c(n = length(x), pearson_r = NA_real_, pearson_p = NA_real_, spearman_rho = NA_real_, spearman_p = NA_real_, kendall_tau = NA_real_, kendall_p = NA_real_))
  p <- suppressWarnings(stats::cor.test(x, y, method = "pearson")); s <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE)); k <- suppressWarnings(stats::cor.test(x, y, method = "kendall", exact = FALSE))
  c(n = length(x), pearson_r = unname(p$estimate), pearson_p = p$p.value, spearman_rho = unname(s$estimate), spearman_p = s$p.value, kendall_tau = unname(k$estimate), kendall_p = k$p.value)
}

.siw_dl_xy_one <- function(x, y, family, cache, outcome, P) {
  cs <- siw_correlation_set_full(x, y); mask <- is.finite(x) & is.finite(y); X <- cbind(`(Intercept)` = 1, exposure = x[mask]); ys <- y[mask]; key <- paste(outcome, "M0", paste(which(!mask), collapse = ","), sep = "|")
  st <- if (family == "gaussian") siw_fit_gaussian(X, ys, 2L, cache, key) else siw_fit_logistic(X, ys, 2L, cache, key, epsilon = P$glm_epsilon, maxit = P$glm_maxit); g <- function(nm) { v <- st[[nm]]; if (is.null(v)) NA_real_ else as.numeric(v) }
  list(cs = cs, vals = c(coefficient = g("coefficient"), ci_lower = g("ci_lower"), ci_upper = g("ci_upper"), model_p = g("nominal_p"), r_squared = g("r_squared"), adjusted_r_squared = g("adjusted_r_squared"), nagelkerke_r_squared = g("nagelkerke_r_squared"), odds_ratio_sd = g("odds_ratio_sd"), partial_r_squared = g("partial_r_squared_weather")),
       status = if (isTRUE(st$estimable)) "COMPLETED" else "NOT_ESTIMABLE")
}

#' The crude daily-lag scan: 3 outcomes x 8 metrics x lags 1..59 (outcome, metric, lag order as in the original); q_*_global over all tests and q_*_outcome within outcome, for Pearson, Spearman and the M0 model p.
#' @param lags optional subset of lags (tests only)
#' @export
siw_xy_daily_grid <- function(dat, config, protocol = siw_daily_protocol(), lags = NULL) {
  P <- protocol$parameters; Lv <- lags %||% seq_len(P$lag_end); cache <- .siw_new_cache(); rows <- list()
  for (oi in seq_len(nrow(config$outcomes))) { o <- config$outcomes[oi, ]; y <- as.numeric(dat[[o$outcome]])
    for (mi in seq_len(nrow(config$metrics))) { sp <- config$metrics[mi, ]; for (l in Lv) { f <- paste0(sp$daily_prefix, "_L", sprintf("%02d", l)); r <- .siw_dl_xy_one(as.numeric(dat[[f]]), y, o$family, cache, o$outcome, P)
      rows[[length(rows) + 1L]] <- data.frame(outcome = o$outcome, outcome_family = o$family, metric = sp$metric, exposure_field = f, exposure_type = "daily_lag", window_or_lag = as.integer(l), n = as.integer(r$cs[["n"]]), pearson_or_point_biserial_r = r$cs[["pearson_r"]], pearson_p = r$cs[["pearson_p"]],
        spearman_rho = r$cs[["spearman_rho"]], spearman_p = r$cs[["spearman_p"]], kendall_tau = r$cs[["kendall_tau"]], kendall_p = r$cs[["kendall_p"]], coefficient = r$vals[["coefficient"]], ci_lower = r$vals[["ci_lower"]], ci_upper = r$vals[["ci_upper"]], model_p = r$vals[["model_p"]], r_squared = r$vals[["r_squared"]],
        adjusted_r_squared = r$vals[["adjusted_r_squared"]], nagelkerke_r_squared = r$vals[["nagelkerke_r_squared"]], odds_ratio_sd = r$vals[["odds_ratio_sd"]], partial_r_squared = r$vals[["partial_r_squared"]], status = r$status, stringsAsFactors = FALSE) } } }
  out <- do.call(rbind, rows); out$q_pearson_global <- siw_bh(out$pearson_p); out$q_spearman_global <- siw_bh(out$spearman_p); out$q_model_global <- siw_bh(out$model_p); out$q_pearson_outcome <- out$q_spearman_outcome <- out$q_model_outcome <- NA_real_
  for (o in unique(out$outcome)) { i <- which(out$outcome == o); out$q_pearson_outcome[i] <- siw_bh(out$pearson_p[i]); out$q_spearman_outcome[i] <- siw_bh(out$spearman_p[i]); out$q_model_outcome[i] <- siw_bh(out$model_p[i]) }
  rownames(out) <- NULL; out
}

#' Independent Kendall tau-b and its normal-approximation p (tie-corrected variance, no continuity correction) for verification of the XYD scan.
#' @export
siw_kendall_independent <- function(x, y) {
  keep <- is.finite(x) & is.finite(y); x <- x[keep]; y <- y[keep]; n <- length(x); S <- 0; for (i in seq_len(n - 1L)) for (j in (i + 1L):n) S <- S + sign(x[i] - x[j]) * sign(y[i] - y[j])
  tx <- as.numeric(table(x)); ty <- as.numeric(table(y)); n0 <- n * (n - 1) / 2; n1 <- sum(tx * (tx - 1) / 2); n2 <- sum(ty * (ty - 1) / 2); tau <- S / sqrt((n0 - n1) * (n0 - n2))
  v0 <- n * (n - 1) * (2 * n + 5); vt <- sum(tx * (tx - 1) * (2 * tx + 5)); vu <- sum(ty * (ty - 1) * (2 * ty + 5)); v1 <- sum(tx * (tx - 1)) * sum(ty * (ty - 1)) / (2 * n * (n - 1)); v2 <- sum(tx * (tx - 1) * (tx - 2)) * sum(ty * (ty - 1) * (ty - 2)) / (9 * n * (n - 1) * (n - 2))
  v <- (v0 - vt - vu) / 18 + v1 + v2; z <- S / sqrt(v); c(tau = tau, p = 2 * min(stats::pnorm(z), stats::pnorm(z, lower.tail = FALSE)))
}

#' Independent recomputation of every XYD row: Pearson (formula and t), Spearman (Pearson on average ranks, t approximation), Kendall (own tau-b), M0 regression (own formulas / own Newton logistic). Returns max differences per quantity.
#' @export
siw_xy_daily_verify <- function(dat, config, res) {
  mx <- c(pearson_r = 0, pearson_p = 0, spearman_rho = 0, spearman_p = 0, kendall_tau = 0, kendall_p = 0, coefficient = 0, ci_lower = 0, ci_upper = 0, model_p = 0, r_squared = 0); nrow_bad <- 0L
  for (i in seq_len(nrow(res))) { r <- res[i, ]; x0 <- as.numeric(dat[[r$exposure_field]]); y0 <- as.numeric(dat[[r$outcome]]); k <- is.finite(x0) & is.finite(y0); x <- x0[k]; y <- y0[k]; n <- length(x)
    tp <- function(rr) 2 * stats::pt(abs(rr * sqrt((n - 2) / (1 - rr^2))), n - 2, lower.tail = FALSE); rp <- sum((x - mean(x)) * (y - mean(y))) / sqrt(sum((x - mean(x))^2) * sum((y - mean(y))^2)); rs <- { a <- rank(x); b <- rank(y); sum((a - mean(a)) * (b - mean(b))) / sqrt(sum((a - mean(a))^2) * sum((b - mean(b))^2)) }
    kd <- siw_kendall_independent(x, y); up <- function(nm, a, b) mx[[nm]] <<- max(mx[[nm]], abs(a - b) / max(abs(b), 1e-12))
    up("pearson_r", rp, r$pearson_or_point_biserial_r); up("pearson_p", tp(rp), r$pearson_p); up("spearman_rho", rs, r$spearman_rho); up("spearman_p", tp(rs), r$spearman_p); up("kendall_tau", kd[["tau"]], r$kendall_tau); up("kendall_p", kd[["p"]], r$kendall_p)
    if (r$outcome_family == "gaussian") { X <- cbind(1, x); XtXi <- solve(crossprod(X)); b <- drop(XtXi %*% crossprod(X, y)); e <- drop(y - X %*% b); h <- rowSums((X %*% XtXi) * X); V <- XtXi %*% crossprod(X * (e / pmax(1e-6, 1 - h))) %*% XtXi; se <- sqrt(V[2, 2]); rdf <- n - 2; tcr <- stats::qt(0.975, rdf)
      up("coefficient", b[2], r$coefficient); up("ci_lower", b[2] - tcr * se, r$ci_lower); up("ci_upper", b[2] + tcr * se, r$ci_upper); up("model_p", 2 * stats::pt(abs(b[2] / se), rdf, lower.tail = FALSE), r$model_p); up("r_squared", 1 - sum(e^2) / sum((y - mean(y))^2), r$r_squared)
    } else { X <- cbind(1, x); mu <- (y + 0.5) / 2; eta <- log(mu / (1 - mu)); b <- c(0, 0); dv <- function(m) -2 * sum(y * log(m) + (1 - y) * log(1 - m)); dold <- dv(mu)                         # own IRLS with the documented glm stopping rule (relative deviance change < 1e-8, start mu = (y + 0.5) / 2)
      for (it in 1:25) { w <- mu * (1 - mu); z <- eta + (y - mu) / w; bn <- drop(solve(crossprod(X * w, X), crossprod(X * w, z))); eta <- drop(X %*% bn); mu <- 1 / (1 + exp(-eta)); dn <- dv(mu); cv <- abs(dn - dold) / (abs(dn) + 0.1) < 1e-8; b <- bn; dold <- dn; if (cv) break }
      se <- sqrt(solve(crossprod(X * w, X))[2, 2]);                                                       # glm convention: the information at the weights of the last iteration (before the final update)
 up("coefficient", b[2], r$coefficient); up("ci_lower", b[2] - 1.96 * se, r$ci_lower); up("ci_upper", b[2] + 1.96 * se, r$ci_upper); up("model_p", 2 * stats::pnorm(abs(b[2] / se), lower.tail = FALSE), r$model_p) } }
  data.frame(quantity = names(mx), max_rel_diff = unname(mx), tolerance = 1e-6, status = ifelse(mx <= 1e-6, "PASS", "FAIL"), stringsAsFactors = FALSE)
}

# ---- DAY ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

.siw_dl_day_fields <- function(protocol) unlist(Filter(function(z) identical(z$id, "DAY_1416"), protocol$comparisons)[[1L]]$fields)

#' The adjusted single-day grid: 3 outcomes x 8 metrics x lags 1..59, tier M2_L (raw daily exposure on its natural scale; complete cases with the covariates). q59 (within outcome and metric), q472 (within outcome), q_global: BH over finite p values.
#' @export
siw_single_day_grid <- function(dat, config, protocol = siw_daily_protocol(), lags = NULL, diagnostics = TRUE) {
  P <- protocol$parameters; Lv <- lags %||% seq_len(P$lag_end); tier <- P$tier; Xc <- siw_tier_matrix(dat, config$tiers[[tier]]); tier_ok <- stats::complete.cases(Xc); cache <- .siw_new_cache(); fields <- .siw_dl_day_fields(protocol); N <- nrow(config$outcomes) * nrow(config$metrics) * length(Lv)
  num <- matrix(NA_real_, N, length(fields), dimnames = list(NULL, fields)); meta <- list(model_id = character(N), outcome = character(N), outcome_family = character(N), metric = character(N), exposure = character(N), exposure_unit = character(N), window_or_lag = integer(N), transformation = rep("raw_daily", N), model_tier = rep(tier, N), primary_role = rep("SUPPLEMENTARY_SINGLE_DAY_DIAGNOSTIC", N), status = character(N), convergence = rep(NA, N), separation_status = character(N)); k <- 0L
  for (oi in seq_len(nrow(config$outcomes))) { o <- config$outcomes$outcome[oi]; fam <- config$outcomes$family[oi]; y <- as.numeric(dat[[o]])
    for (mi in seq_len(nrow(config$metrics))) { sp <- config$metrics[mi, ]; for (l in Lv) { f <- paste0(sp$daily_prefix, "_L", sprintf("%02d", l)); r <- .siw_cold_one(y, fam, as.numeric(dat[[f]]), Xc, tier_ok, o, tier, cache, config, P, diagnostics); st <- r$st; d <- r$d; k <- k + 1L
      g <- function(nm) { v <- st[[nm]]; if (is.null(v)) NA_real_ else as.numeric(v) }; gd <- function(nm) { v <- if (is.null(d)) NULL else d[[nm]]; if (is.null(v)) NA_real_ else as.numeric(v) }
      diagn <- c("condition_number", "maximum_vif", "calibration_intercept", "calibration_slope", "maximum_coefficient_magnitude", "maximum_cooks_distance", "maximum_leverage", "maximum_absolute_dfbeta", "influential_case_count", "exposure_p10", "exposure_p25", "exposure_p75", "exposure_p90")
      for (fl in fields) num[k, fl] <- if (fl == "n") r$n else if (fl == "parameter_count") (if (r$est) g("p") else g("parameter_count")) else if (fl %in% diagn) gd(fl) else if (fl %in% c("q59", "q472", "q_global")) NA_real_ else g(fl)
      meta$model_id[k] <- paste("SINGLE_DAY", .siw_cold_id(o), .siw_cold_id(sp$metric), paste0("L", sprintf("%02d", l)), tier, sep = "__"); meta$outcome[k] <- o; meta$outcome_family[k] <- fam; meta$metric[k] <- sp$metric; meta$exposure[k] <- f; meta$exposure_unit[k] <- sp$unit; meta$window_or_lag[k] <- as.integer(l)
      meta$status[k] <- if (r$est) "COMPLETED" else "NOT_ESTIMABLE"; meta$separation_status[k] <- st$separation_status %||% ""; meta$convergence[k] <- if (is.null(st$convergence)) NA else st$convergence } } }
  out <- cbind(as.data.frame(meta, stringsAsFactors = FALSE), as.data.frame(num)); ok <- is.finite(out$nominal_p); out$q59 <- out$q472 <- out$q_global <- NA_real_
  for (o in unique(out$outcome)) { i <- which(out$outcome == o & ok); out$q472[i] <- siw_bh(out$nominal_p[i]); for (m in unique(out$metric)) { j <- which(out$outcome == o & out$metric == m & ok); out$q59[j] <- siw_bh(out$nominal_p[j]) } }
  out$q_global <- siw_bh(out$nominal_p); rownames(out) <- NULL; out
}

# ---- MAXT ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' The original single-step max|r| permutation test for ONE outcome, written exactly as in the original code (reference route): set.seed(seed); for each replicate yp <- sample(y), the maximum over the exposure columns of |cor(x_j, yp)| with pairwise-complete observations;
#' p_j = (1 + #{max >= observed_j}) / (1 + #finite maxima). Used for verification and for the near-tie recomputation.
#' @export
siw_maxt_legacy <- function(xmat, y, replicates, seed) {
  observed <- abs(vapply(seq_len(ncol(xmat)), function(j) suppressWarnings(stats::cor(xmat[, j], y, use = "pairwise.complete.obs")), numeric(1L)))
  mx <- siw_with_seed(seed, { m <- rep(NA_real_, replicates); for (b in seq_len(replicates)) { yp <- sample(y); vals <- abs(vapply(seq_len(ncol(xmat)), function(j) suppressWarnings(stats::cor(xmat[, j], yp, use = "pairwise.complete.obs")), numeric(1L))); m[[b]] <- suppressWarnings(max(vals, na.rm = TRUE)) }; m })
  list(observed = observed, max_stats = mx, p = vapply(observed, function(o) (1 + sum(mx >= o, na.rm = TRUE)) / (1 + sum(is.finite(mx))), numeric(1L)))
}

# columns grouped by their set of complete rows; centred columns and sums of squares are computed once
.siw_maxt_prepare <- function(xmat) {
  pat <- apply(!is.na(xmat), 2L, function(z) paste(as.integer(z), collapse = "")); groups <- list()
  for (u in unique(pat)) { cols <- which(pat == u); rows <- which(!is.na(xmat[, cols[1L]])); Xc <- sweep(xmat[rows, cols, drop = FALSE], 2L, colMeans(xmat[rows, cols, drop = FALSE])); groups[[length(groups) + 1L]] <- list(cols = cols, rows = rows, Xc = Xc, ssx = colSums(Xc^2)) }
  groups
}

# max |r| for every row of the permutation matrix Y (replicates x participants): vectorised over replicates; NA where no correlation is defined
.siw_maxt_max <- function(groups, Y) {
  best <- rep(NA_real_, nrow(Y))
  for (g in groups) { Yg <- Y[, g$rows, drop = FALSE]; Yc <- Yg - rowMeans(Yg); ssy <- rowSums(Yc^2); R <- abs(Yc %*% g$Xc) / sqrt(outer(ssy, g$ssx)); R[!is.finite(R)] <- NA_real_; R[, g$ssx == 0] <- NA_real_; R[ssy == 0, ] <- NA_real_
    mg <- suppressWarnings(apply(R, 1L, function(z) if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE))); best <- ifelse(is.na(best), mg, ifelse(is.na(mg), best, pmax(best, mg))) }
  ifelse(is.na(best), -Inf, best)
}

#' Optimised MAXT for one outcome with the ORIGINAL sampling sequence (set.seed(seed); one sample(y) per replicate). Observed correlations are computed with stats::cor exactly as in the original; the maxima per replicate come from precomputed centred columns;
#' every replicate whose maximum lies within `tie_tol` of an observed value is recomputed with the original cor-based route, so rounding cannot change a count. Missing outcome values fall back to the original route.
#' @return list(table, max_stats, replicates_requested, replicates_successful, near_tie_recomputed, p_changed_by_recompute, engine)
#' @export
siw_maxt_outcome <- function(xmat, y, replicates, seed, fields = colnames(xmat), outcome = NA_character_, tie_tol = 1e-12) {
  if (anyNA(y)) { l <- siw_maxt_legacy(xmat, y, replicates, seed); mx <- l$max_stats; obs <- l$observed; p <- l$p; eng <- "original (outcome has missing values)"; nt <- 0L; ch <- 0L }
  else {
    obs <- abs(vapply(seq_len(ncol(xmat)), function(j) suppressWarnings(stats::cor(xmat[, j], y, use = "pairwise.complete.obs")), numeric(1L))); groups <- .siw_maxt_prepare(xmat)
    Y <- siw_with_seed(seed, { M <- matrix(0, replicates, length(y)); for (b in seq_len(replicates)) M[b, ] <- sample(y); M }); mx <- .siw_maxt_max(groups, Y); pf <- function(m) vapply(obs, function(o) (1 + sum(m >= o, na.rm = TRUE)) / (1 + sum(is.finite(m))), numeric(1L)); p0 <- pf(mx)
    near <- which(vapply(seq_len(replicates), function(b) any(is.finite(obs) & abs(mx[b] - obs) <= tie_tol * pmax(1, obs)), NA)); nt <- length(near)
    for (b in near) { vals <- abs(vapply(seq_len(ncol(xmat)), function(j) suppressWarnings(stats::cor(xmat[, j], Y[b, ], use = "pairwise.complete.obs")), numeric(1L))); mx[b] <- suppressWarnings(max(vals, na.rm = TRUE)) }
    p <- pf(mx); ch <- sum(abs(p - p0) > 0); eng <- "optimised (original sampling sequence)" }
  list(table = data.frame(outcome = outcome, exposure_field = fields, observed_abs_correlation = obs, p_outcome_maxT = p, stringsAsFactors = FALSE), max_stats = mx, replicates_requested = as.integer(replicates), replicates_successful = sum(is.finite(mx)), near_tie_recomputed = nt, p_changed_by_recompute = ch, engine = eng)
}

#' MAXT for all outcomes: per outcome the 80 cumulative exposure fields (config order: metrics x windows), seed = config seed + 1000 x outcome index, replicates as declared (10,000 in the V1.4 full profile).
#' @export
siw_xy_maxt_grid <- function(dat, config, protocol = siw_daily_protocol(), replicates = NULL, engine = "optimised") {
  P <- protocol$parameters; B <- replicates %||% as.integer(P$maxt_replicates); fields <- as.vector(t(outer(seq_len(nrow(config$metrics)), config$windows, function(i, w) paste0(config$metrics$cumulative_prefix[i], w, "D")))); tabs <- list(); diag <- list(); mxs <- list()
  for (i in seq_len(nrow(config$outcomes))) { o <- config$outcomes$outcome[i]; xmat <- as.matrix(dat[, fields, drop = FALSE]); storage.mode(xmat) <- "double"; y <- as.numeric(dat[[o]]); seed <- as.integer(config$seed + i * P$maxt_seed_step)
    r <- if (identical(engine, "original")) { l <- siw_maxt_legacy(xmat, y, B, seed); list(table = data.frame(outcome = o, exposure_field = fields, observed_abs_correlation = l$observed, p_outcome_maxT = l$p, stringsAsFactors = FALSE), max_stats = l$max_stats, replicates_requested = B, replicates_successful = sum(is.finite(l$max_stats)), near_tie_recomputed = NA_integer_, p_changed_by_recompute = NA_integer_, engine = "original") } else siw_maxt_outcome(xmat, y, B, seed, fields, o)
    tabs[[i]] <- r$table; mxs[[o]] <- r$max_stats; diag[[i]] <- data.frame(outcome = o, seed = seed, replicates_requested = r$replicates_requested, replicates_successful = r$replicates_successful, replicates_failed = r$replicates_requested - r$replicates_successful, near_tie_recomputed = r$near_tie_recomputed, p_changed_by_recompute = r$p_changed_by_recompute, engine = r$engine, minimum_attainable_p = 1 / (1 + r$replicates_successful), stringsAsFactors = FALSE) }
  list(table = do.call(rbind, tabs), diagnostics = do.call(rbind, diag), max_stats = mxs)
}

#' Assessment of the permutation assumptions (separate from numerical reproduction; the original method is NOT replaced): exchangeability (clustering of the outcome by assessment date), missing exposures, Monte Carlo error, attainable minimum, share of p = 1, unequal n across the 80 statistics, observed-NA handling.
#' @export
siw_maxt_assumptions <- function(dat, config, maxt) {
  rows <- list(); tb <- maxt$table; dg <- maxt$diagnostics; add <- function(outcome, item, value, note) rows[[length(rows) + 1L]] <<- data.frame(outcome = outcome, item = item, value = as.character(value), note = note, status = "REVIEW", stringsAsFactors = FALSE)
  day <- if ("assessment_day" %in% names(dat)) dat$assessment_day else NULL
  for (o in unique(tb$outcome)) { t <- tb[tb$outcome == o, ]; d <- dg[dg$outcome == o, ]; y <- as.numeric(dat[[o]])
    if (!is.null(day)) { g <- factor(day); k <- nlevels(g); n <- length(y); ssb <- sum(tapply(y, g, function(z) length(z) * (mean(z) - mean(y))^2)); sst <- sum((y - mean(y))^2); add(o, "outcome variance share between assessment dates (eta squared; 33 groups expected)", signif(ssb / sst, 4), paste0(k, " dates, n = ", n, "; the permutation shuffles the outcome across all participants (unit = participant, unrestricted) and ignores date clustering; a large between-date share means participants are not exchangeable and the null distribution can be anti-conservative")) }
    add(o, "exposure statistics with fewer than all participants (n < N)", sum(vapply(t$exposure_field, function(f) sum(is.finite(as.numeric(dat[[f]]))) < nrow(dat), NA)), "|r| of columns with different n are compared on one scale (non-pivotal); the original uses pairwise-complete correlations")
    add(o, "observed |r| not finite", sum(!is.finite(t$observed_abs_correlation)), "the original gives such a statistic the smallest attainable p (comparison dropped); flagged, not changed")
    add(o, "Monte Carlo standard error of p at p = 0.05 and 0.5", paste0(signif(sqrt(0.05 * 0.95 / d$replicates_successful), 3), "; ", signif(sqrt(0.25 / d$replicates_successful), 3)), "finite-simulation error of the adjusted p")
    add(o, "minimum attainable p / share of p = 1", paste0(signif(d$minimum_attainable_p, 4), " / ", signif(mean(t$p_outcome_maxT >= 1), 3)), "p = (1 + count) / (1 + successful replicates): the observed arrangement counts as one permutation; ties count (>=)")
    add(o, "statistic and family", "two-sided |Pearson r| (point-biserial for 0/1), single-step maximum over 80 nested cumulative exposures of ONE outcome", "no covariate adjustment (crude); H0: the outcome is independent of all 80 exposures; no adjustment across the three outcomes; the 80 statistics are strongly dependent (nested windows, eight metrics)") }
  do.call(rbind, rows)
}

# ---- reference handling and run -----------------------------------------------------------------------------------------------------------------------------------------------------------------

#' Locate, hash and check the three frozen reference files named in the protocol (existence, SHA-256, bytes, record counts, unique keys, required columns). Read only.
#' @export
siw_daily_read_references <- function(reference_dir, protocol = siw_daily_protocol(), components = c("XYD", "DAY", "MAXT")) {
  tabs <- list(); rows <- list(); add <- function(file, item, value, ok) rows[[length(rows) + 1L]] <<- data.frame(file = file, item = item, value = as.character(value), ok = isTRUE(ok), stringsAsFactors = FALSE)
  for (rf in Filter(function(z) z$component %in% components, protocol$reference_files)) {
    f <- file.path(reference_dir, rf$file); ex <- file.exists(f); add(rf$file, "file exists", ex, ex); if (!ex) { add(rf$file, "REQUIRED REFERENCE ABSENT: nothing is compared", "absent", FALSE); next }
    h <- siw_file_hash(f)$value; add(rf$file, "SHA-256 equals the declared value", paste0("found ", h, "; declared ", rf$sha256), identical(toupper(h), toupper(rf$sha256))); add(rf$file, "bytes equal the declared value", paste0("found ", file.info(f)$size, "; declared ", rf$bytes), file.info(f)$size == rf$bytes)
    d <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")), error = function(e) NULL); add(rf$file, "file is readable", !is.null(d), !is.null(d)); if (is.null(d)) next; keys <- unlist(rf$keys); hk <- all(keys %in% names(d)); add(rf$file, "key columns present", paste(keys, collapse = ","), hk); tabs[[rf$component]] <- d; if (!hk) next
    kv <- do.call(paste, c(lapply(d[keys], as.character), sep = "|")); blank <- sum(Reduce(`|`, lapply(d[keys], function(z) is.na(z) | !nzchar(trimws(as.character(z)))))); add(rf$file, "records equal the declared count", paste0("found ", nrow(d), "; expected ", rf$expected_records), nrow(d) == rf$expected_records); add(rf$file, "no blank or missing keys", blank, blank == 0L); add(rf$file, "keys unique", sum(duplicated(kv)), !anyDuplicated(kv))
    cm <- Filter(function(z) identical(z$reference, rf$component), protocol$comparisons)[[1L]]; need <- c(unlist(cm$fields), unlist(cm$categorical)); miss <- setdiff(need, names(d)); add(rf$file, paste0("required columns present for ", cm$id), if (length(miss)) paste("missing:", paste(miss, collapse = ",")) else "all present", !length(miss))
    G <- rf$expected_grid; if (!is.null(G)) add(rf$file, "the declared grid is complete", paste0("outcomes ", length(unique(d$outcome)), "; per-outcome records ", paste(unique(table(d$outcome)), collapse = ",")), length(unique(d$outcome)) == G$outcomes && all(table(d$outcome) == G$per_outcome)) }
  prov <- if (length(rows)) do.call(rbind, rows) else data.frame(file = character(), item = character(), value = character(), ok = logical()); nreq <- length(Filter(function(z) z$component %in% components, protocol$reference_files)); list(tables = tabs, provenance = prov, ok = nrow(prov) > 0L && all(prov$ok) && length(tabs) == nreq)
}

.siw_dl_cmp <- function(cc, new, ref, keys, tol, refname) {
  fields <- unlist(cc$fields); nn <- new[, intersect(c(keys, fields), names(new)), drop = FALSE]
  siw_compare_tables(nn, ref, keys, tol, name = cc$id, reference_file = refname, required_columns = fields, expected_records = as.integer(cc$expected_records), required = TRUE, skip = character())
}

.siw_dl_cat <- function(new, ref, keys, fields) {
  jk <- do.call(paste, c(lapply(new[keys], as.character), sep = "|")); jr <- do.call(paste, c(lapply(ref[keys], as.character), sep = "|")); j <- match(jr, jk); out <- list()
  for (f in fields) { a <- as.character(new[[f]][j]); b <- as.character(ref[[f]]); a[is.na(a) | a == "NA"] <- ""; b[is.na(b) | b == "NA"] <- ""; out[[f]] <- data.frame(field = f, n_compared = length(b), n_equal = sum(toupper(a) == toupper(b) & !is.na(j)), n_different = sum(toupper(a) != toupper(b) | is.na(j)), stringsAsFactors = FALSE) }
  do.call(rbind, out)
}

# scale and inference identities of the cold-burden checker, applied separately to each sample size (the implied SD of the outcome differs between the n = 55 and the n = 54 samples)
.siw_dl_identities <- function(d) do.call(rbind, lapply(split(d, d$n), function(z) siw_cold_identities(z, NULL)))
.siw_dl_status <- function(x) if (!length(x)) "NOT_TESTED" else if (any(x == "FAIL")) "FAIL" else if (any(x == "NOT_TESTED")) "NOT_TESTED" else "PASS"

#' Run the daily-layer reproduction and verification on one canonical CSV against the frozen V1.4 reference files (read only). Components XYD, DAY, MAXT are separate; a component whose reference is absent is not run and is NOT_TESTED.
#' @param protocol_path optional alternative protocol (synthetic tests only); replicates / lags overrides come from the protocol parameters (maxt_replicates, lags_subset)
#' @export
siw_run_daily_layer_validation <- function(data_csv, reference_dir, out_dir, config = siw_load_config(), protocol_path = NULL) {
  t0 <- proc.time()[["elapsed"]]; reference_dir <- normalizePath(reference_dir, winslash = "/", mustWork = TRUE); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); out_dir <- normalizePath(out_dir, winslash = "/")
  if (startsWith(out_dir, paste0(reference_dir, "/")) || startsWith(reference_dir, paste0(out_dir, "/"))) stop("SIW_DAILY_OUTPUT_AND_REFERENCE_FOLDERS_OVERLAP", call. = FALSE)
  options(sieccwinter.log_file = file.path(out_dir, "run.log")); on.exit(options(sieccwinter.log_file = NULL), add = TRUE)
  protocol <- siw_daily_protocol(protocol_path); P <- protocol$parameters; sm0 <- siw_source_manifest(); W <- function(x, f) siw_write_csv(x, file.path(out_dir, f)); siw_log("Phase 3H daily-layer validation; protocol ", protocol$protocol_hash, "; source fingerprint ", sm0$fingerprint, " (", sm0$n_files, " files)")
  env <- .siw_v21_env(); jsonlite::write_json(env, file.path(out_dir, "dailylayer_environment.json"), auto_unbox = TRUE, pretty = TRUE, null = "null"); writeLines(utils::capture.output(print(utils::sessionInfo())), file.path(out_dir, "dailylayer_sessionInfo.txt")); rng <- RNGkind(); writeLines(c(paste0("RNGkind (session) = ", paste(rng, collapse = ", ")), "the MAXT runs set RNGkind(Mersenne-Twister, Inversion, Rejection) explicitly and restore the caller's state"), file.path(out_dir, "dailylayer_rng.txt"))
  refs <- siw_daily_read_references(reference_dir, protocol); W(refs$provenance, "dailylayer_reference_provenance.csv")
  carried <- c(FINAL_NINE_L56_NUMERICAL_VALIDATION = "ACCEPTED_UNDER_V2_1 (not recomputed)", ARCHIVED_DLM_DETERMINISTIC = "ACCEPTED_PHASE3E1 (not recomputed)", COLD_BURDEN_NUMERICAL = "ACCEPTED_PHASE3F1 (not recomputed)", MANUSCRIPT_CONCORDANCE_STATUS = "REVIEW", FIGURE_PROVENANCE_STATUS = "NOT_TESTED", DLNM_EQUALITY_STATUS = "NOT_TESTED", J02_GLM_EPSILON_ENTRY_POINTS = "OPEN (no change made)")
  finish <- function(st, status, extra = character(), msg = NULL) { writeLines(c(paste0(names(st), "=", st), extra, paste0(names(carried), "=", carried), "SCOPE=archived daily-lag layer (horizon L01-L59): XYD 1,416, DAY 1,416, MAXT 240; separate from the nine final L01-L56 models", "Results were not altered, redrawn or replaced to fit any reference."), file.path(out_dir, "DAILYLAYER_STATUS.txt"))
    sm1 <- siw_source_manifest(); W(sm0$manifest, "source_manifest_dailylayer.csv"); writeLines(c(paste0("SOURCE_FINGERPRINT_SHA256=", sm0$fingerprint), paste0("FILES=", sm0$n_files), paste0("SOURCE_CHANGED_DURING_RUN=", !isTRUE(siw_source_manifest_diff(sm0, sm1)$identical_fingerprint))), file.path(out_dir, "SOURCE_FINGERPRINT.txt")); if (!is.null(msg)) writeLines(msg, file.path(out_dir, "RUN_SUMMARY.md")); list(out_dir = out_dir, statuses = st, status = status) }
  if (!refs$ok) { st <- c(EXECUTION_STATUS = "PASS", INPUT_PROVENANCE_STATUS = "FAIL", XYD_REPRODUCTION_STATUS = "NOT_TESTED", XYD_VERIFICATION_STATUS = "NOT_TESTED", DAY_REPRODUCTION_STATUS = "NOT_TESTED", DAY_VERIFICATION_STATUS = "NOT_TESTED", MAXT_REPRODUCTION_STATUS = "NOT_TESTED", MAXT_VERIFICATION_STATUS = "NOT_TESTED", MAXT_ASSUMPTIONS_STATUS = "NOT_TESTED", COVERAGE_STATUS = "FAIL", SOURCE_INTEGRITY_STATUS = "PASS")
    bad <- refs$provenance[!refs$provenance$ok, ]; fr <- finish(st, "VERIFICATION_FAILURE", c(paste0("PROVENANCE_PROBLEMS=", nrow(bad)), "NOTHING WAS COMPARED: required reference evidence is missing or does not match the declaration; nothing was passed."), c("# Daily-layer validation: reference provenance FAILED", "", paste0("* Reference folder: `", reference_dir, "`"), paste0("* Problems: ", nrow(bad), " (see dailylayer_reference_provenance.csv)"), "", "No comparison was made and nothing was passed.")); fr$provenance <- refs$provenance; return(fr) }
  ih <- siw_file_hash(data_csv); raw <- siw_read_study_csv(data_csv, config); rep <- siw_validate_input(raw, config, lag_end = P$lag_end); W(rep, "input_validation_report.csv"); if (any(rep$status == "FAIL")) stop("SIW_INPUT_VALIDATION_FAILED: see input_validation_report.csv", call. = FALSE)
  dat <- siw_attach_cumulative(siw_prepare_data(raw, config), config)$data; sec <- c(); tk <- function(nm, f) { a <- proc.time()[["elapsed"]]; v <- f(); sec[[nm]] <<- round(proc.time()[["elapsed"]] - a, 2); v }
  tolC <- config$tolerances; tolC$name <- "cumulative"; tolE <- config$tolerances; tolE$name <- "exact"; cmpC <- function(id) Filter(function(z) identical(z$id, id), protocol$comparisons)[[1L]]; lagsub <- if (is.null(P$lags_subset)) NULL else as.integer(unlist(P$lags_subset))
  siw_log("XYD: crude daily-lag scan"); xyd <- tk("xyd_scan", function() siw_xy_daily_grid(dat, config, protocol, lags = lagsub)); cX <- cmpC("XYD_1416"); rX <- tk("xyd_compare", function() .siw_dl_cmp(cX, xyd, refs$tables$XYD, unlist(cX$keys), tolC, "XY_daily_all_1416.csv")); catX <- .siw_dl_cat(xyd, refs$tables$XYD, unlist(cX$keys), unlist(cX$categorical)); W(rX$summary, "xyd_comparison_summary.csv"); W(rX$detail, "xyd_comparison_detail.csv"); W(catX, "xyd_categorical_comparison.csv"); W(xyd, "xyd_package_results.csv")
  vX <- tk("xyd_verify", function() siw_xy_daily_verify(dat, config, xyd)); W(vX, "xyd_independent_verification.csv"); fam <- list(pearson = "pearson_p", spearman = "spearman_p", model = "model_p"); mrows <- list(); for (nm in names(fam)) { p <- xyd[[fam[[nm]]]]; g <- max(abs(xyd[[paste0("q_", nm, "_global")]] - stats::p.adjust(p, "BH")), na.rm = TRUE); o <- max(vapply(unique(xyd$outcome), function(u) { i <- xyd$outcome == u; max(abs(xyd[[paste0("q_", nm, "_outcome")]][i] - stats::p.adjust(p[i], "BH")), na.rm = TRUE) }, 0)); mrows[[nm]] <- data.frame(family = nm, global_max_abs_diff = g, within_outcome_max_abs_diff = o, status = if (g <= 1e-12 && o <= 1e-12) "PASS" else "FAIL", stringsAsFactors = FALSE) }; mX <- do.call(rbind, mrows); W(mX, "xyd_multiplicity_verification.csv")
  siw_log("DAY: adjusted single-day grid"); day <- tk("day_scan", function() siw_single_day_grid(dat, config, protocol, lags = lagsub)); cD <- cmpC("DAY_1416"); rD <- tk("day_compare", function() .siw_dl_cmp(cD, day, refs$tables$DAY, unlist(cD$keys), tolC, "single_day_adjusted_regression_grid.csv")); catD <- .siw_dl_cat(day, refs$tables$DAY, unlist(cD$keys), unlist(cD$categorical)); W(rD$summary, "day_comparison_summary.csv"); W(rD$detail, "day_comparison_detail.csv"); W(catD, "day_categorical_comparison.csv"); W(day, "day_package_results.csv")
  idD <- .siw_dl_identities(day); idD$source <- "package"; idR <- .siw_dl_identities(refs$tables$DAY[refs$tables$DAY$status == "COMPLETED", ]); idR$source <- "frozen"; W(rbind(idD, idR), "day_scale_and_inference_identities.csv")
  mD <- { ok <- is.finite(day$nominal_p); f59 <- paste(day$outcome, day$metric); a <- max(vapply(unique(f59[ok]), function(u) { i <- which(ok & f59 == u); max(abs(day$q59[i] - stats::p.adjust(day$nominal_p[i], "BH"))) }, 0)); b <- max(vapply(unique(day$outcome), function(u) { i <- which(ok & day$outcome == u); max(abs(day$q472[i] - stats::p.adjust(day$nominal_p[i], "BH"))) }, 0)); gg <- max(abs(day$q_global[ok] - stats::p.adjust(day$nominal_p[ok], "BH")))
    data.frame(check = c("q59 (within outcome and metric) vs p.adjust BH", "q472 (within outcome) vs p.adjust BH", "q_global vs p.adjust BH", "q59 families", "q472 families"), value = c(a, b, gg, length(unique(f59[ok])), length(unique(day$outcome))), status = c(ifelse(c(a, b, gg) <= 1e-12, "PASS", "FAIL"), "INFO", "INFO"), stringsAsFactors = FALSE) }; W(mD, "day_multiplicity_verification.csv")
  sub <- seq(1L, nrow(day), by = as.integer(P$independent_refit_every)); covs <- config$tiers[[P$tier]]; rr <- list()
  for (i in sub) { r <- day[i, ]; if (r$status != "COMPLETED") next; ref1 <- .siw_cold_refit_reference(dat, config, r$outcome, r$outcome_family, as.numeric(dat[[r$exposure]]), covs); for (f in names(ref1)) { a <- r[[f]]; b <- ref1[[f]]; dd <- abs(a - b); ok <- is.finite(dd) && dd <= tolC$cumulative_abs + tolC$cumulative_rel * abs(b); rr[[length(rr) + 1L]] <- data.frame(model_id = r$model_id, field = f, package = a, independent = b, status = if (ok) "PASS" else "FAIL", stringsAsFactors = FALSE) } }; rfD <- do.call(rbind, rr); W(rfD, "day_independent_refit.csv")
  siw_log("MAXT: 3 outcomes x 80 exposures; optimised engine with the original sampling sequence"); cM <- cmpC("MAXT_240"); B <- as.integer(P$maxt_replicates); mx <- tk("maxt_optimised", function() siw_xy_maxt_grid(dat, config, protocol, replicates = B, engine = "optimised")); rM <- .siw_dl_cmp(cM, mx$table, refs$tables$MAXT, unlist(cM$keys), tolE, "selected_xy_maxT.csv"); W(rM$summary, "maxt_comparison_summary.csv"); W(rM$detail, "maxt_comparison_detail.csv"); W(mx$table, "maxt_package_results.csv"); W(mx$diagnostics, "maxt_replicate_accounting.csv")
  ol <- tk("maxt_original_route", function() siw_xy_maxt_grid(dat, config, protocol, replicates = B, engine = "original")); vM <- do.call(rbind, lapply(names(mx$max_stats), function(o) { a <- mx$max_stats[[o]]; b <- ol$max_stats[[o]]; data.frame(outcome = o, replicates = length(a), max_abs_diff_of_maxima = max(abs(a - b)[is.finite(a) & is.finite(b)], 0), non_finite_pattern_equal = identical(is.finite(a), is.finite(b)), p_identical_to_original_route = identical(mx$table$p_outcome_maxT[mx$table$outcome == o], ol$table$p_outcome_maxT[ol$table$outcome == o]), stringsAsFactors = FALSE) })); vM$status <- ifelse(vM$max_abs_diff_of_maxima <= 1e-12 & vM$non_finite_pattern_equal & vM$p_identical_to_original_route, "PASS", "FAIL"); W(vM, "maxt_engine_verification.csv")
  asm <- siw_maxt_assumptions(dat, config, mx); W(asm, "maxt_assumption_assessment.csv"); statements <- data.frame(item = c("minimum p over the 240 statistics", "number of p below 0.05"), value = c(signif(min(mx$table$p_outcome_maxT), 5), sum(mx$table$p_outcome_maxT < 0.05)), stringsAsFactors = FALSE); W(statements, "maxt_descriptive.csv")
  okc <- function(r, cc) grepl("^PASS", r$status) && isTRUE(r$n_compared == cc$expected_compared_cells); rs <- function(r) r$summary[1L, ]
  st <- c(EXECUTION_STATUS = "PASS", INPUT_PROVENANCE_STATUS = "PASS", XYD_REPRODUCTION_STATUS = if (okc(rs(rX), cX) && all(catX$n_different == 0L)) "PASS" else "FAIL", XYD_VERIFICATION_STATUS = .siw_dl_status(c(vX$status, mX$status)),
          DAY_REPRODUCTION_STATUS = if (okc(rs(rD), cD) && all(catD$n_different == 0L)) "PASS" else "FAIL", DAY_VERIFICATION_STATUS = .siw_dl_status(c(idD$status, idR$status, mD$status[mD$status != "INFO"], rfD$status)),
          MAXT_REPRODUCTION_STATUS = if (okc(rs(rM), cM)) "PASS" else "FAIL", MAXT_VERIFICATION_STATUS = .siw_dl_status(vM$status), MAXT_ASSUMPTIONS_STATUS = "REVIEW")
  cov <- rbind(rs(rX), rs(rD), rs(rM)); cov$expected_compared_cells <- c(cX$expected_compared_cells, cD$expected_compared_cells, cM$expected_compared_cells); cov$cells_as_declared <- cov$n_compared == cov$expected_compared_cells; W(cov[, c("reference", "expected_records", "n_records_reference", "n_records_results", "expected_compared_cells", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_nonfinite", "n_unmatched", "n_duplicate", "n_missing_required", "n_unclassified", "cells_as_declared", "status", "reason")], "dailylayer_comparison_coverage.csv")
  st[["COVERAGE_STATUS"]] <- if (all(cov$n_unclassified == 0L) && all(cov$cells_as_declared) && all(cov$n_records_reference == cov$expected_records) && all(cov$n_records_results == cov$n_records_reference)) "PASS" else "FAIL"
  sm1 <- siw_source_manifest(); st[["SOURCE_INTEGRITY_STATUS"]] <- if (!isTRUE(siw_source_manifest_diff(sm0, sm1)$identical_fingerprint)) "FAIL" else "PASS"; overall <- if (any(st == "FAIL")) "VERIFICATION_FAILURE" else "OK_WITH_ASSUMPTIONS_UNDER_REVIEW"
  info <- c(paste0("XYD_CELLS=", cov$n_compared[1], " (declared ", cov$expected_compared_cells[1], "); EXACT ", cov$n_exact[1], "; WITHIN_TOLERANCE ", cov$n_within_tolerance[1], "; DIFFERENT ", cov$n_different[1]), paste0("DAY_CELLS=", cov$n_compared[2], " (declared ", cov$expected_compared_cells[2], "); EXACT ", cov$n_exact[2], "; WITHIN_TOLERANCE ", cov$n_within_tolerance[2], "; DIFFERENT ", cov$n_different[2]),
            paste0("MAXT_CELLS=", cov$n_compared[3], " (declared ", cov$expected_compared_cells[3], "); EXACT ", cov$n_exact[3], "; WITHIN_TOLERANCE ", cov$n_within_tolerance[3], "; DIFFERENT ", cov$n_different[3]), paste0("MAXT_REPLICATES_REQUESTED=", paste(mx$diagnostics$replicates_requested, collapse = ","), "; SUCCESSFUL=", paste(mx$diagnostics$replicates_successful, collapse = ","), "; NEAR_TIE_RECOMPUTED=", paste(mx$diagnostics$near_tie_recomputed, collapse = ","), "; P_CHANGED_BY_RECOMPUTE=", paste(mx$diagnostics$p_changed_by_recompute, collapse = ",")),
            paste0("RUNTIME_SECONDS=", paste(names(sec), sec, sep = ":", collapse = " "), "; TOTAL ", round(proc.time()[["elapsed"]] - t0, 1)))
  jsonlite::write_json(list(package_version = siw_pkg_version(), batch = "3H_batch_D_daily_layer", protocol_hash = protocol$protocol_hash, config_hash = config$config_hash, input_hash = ih$value, source_fingerprint_sha256 = sm0$fingerprint, source_files = sm0$n_files, reference_hashes = vapply(protocol$reference_files, function(z) z$sha256, ""), maxt_replicates = B, maxt_seeds = as.list(mx$diagnostics$seed), statuses = as.list(st), seconds = as.list(sec), created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)), file.path(out_dir, "dailylayer_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  fin <- finish(st, overall, info, c("# Archived daily-lag layer (XYD 1,416; DAY 1,416; MAXT 240): reproduction and verification", "", paste0("* Protocol `", protocol$protocol_hash, "`; source fingerprint `", sm0$fingerprint, "` (", sm0$n_files, " files)"), "* MAXT permutation assumptions are assessed separately and stay under REVIEW; the original method is not replaced.", "", "## Statuses", "", paste0("* ", names(st), ": ", st), "", "## Counts and runtime", "", paste0("* ", info), "", "Nothing in this folder contains participant-level values."))
  fin$xyd <- xyd; fin$day <- day; fin$maxt <- mx; fin$coverage <- cov; fin$assumptions <- asm; fin$engine_verification <- vM; fin$provenance <- refs$provenance; fin$seconds <- sec; fin$xyd_verify <- vX; fin$day_refit <- rfD; fin$summ <- list(xyd = rX$summary, day = rD$summary, maxt = rM$summary); fin$cat <- list(xyd = catX, day = catD); fin
}
