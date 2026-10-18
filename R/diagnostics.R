# Batch A: model-level influence and design diagnostics for the cumulative grid (aggregate per model; no participant identifiers or row values).
# Definitions are those of the original implementation (WINTER_SOURCE R/030_model_engine.R siecc_fit_model, siecc_max_vif, siecc_calibration;
# R/070_cumulative_models.R siecc_logistic_probability_contrast). They are computed here in a separate stage and table: the validated core
# results (siw_cumulative_grid) are not touched.
#
#   condition_number      base::kappa(X)                         X = design matrix (intercept, exposure, covariates), complete cases
#   maximum_vif           max diag(solve(cor(X without intercept))); 1 if one predictor; Inf if a predictor is constant or cor singular
#   maximum_coefficient_magnitude  max |coefficient| (all coefficients incl. intercept)
#   maximum_cooks_distance  max stats::cooks.distance(fit)       lm (continuous) or glm binomial (binary)
#   maximum_leverage      max stats::hatvalues(fit)
#   maximum_absolute_dfbeta  max |stats::dfbetas(fit)| (standardised, all coefficients)
#   influential_case_count  # cases with Cook > 4/n or leverage > 2p/n  (p = number of design columns incl. intercept)
#   calibration_intercept / calibration_slope  (binary) glm(y ~ 1 + offset(logit p)) intercept; glm(y ~ logit p) slope; p clamped to [1e-8, 1-1e-8]
#   exposure_p10/p25/p75/p90  stats::quantile(exposure, type 7) in the model's complete-case sample
#   marginal_probability_difference_per_sd, probability_at_mean_minus_half_sd, probability_at_mean_plus_half_sd  (binary) average predicted probability
#                         with the exposure set to mean -/+ 0.5 SD for every complete-case participant, and the mean difference; NA if not converged

.siw_diag_columns <- c("condition_number", "maximum_vif", "calibration_intercept", "calibration_slope", "maximum_coefficient_magnitude", "maximum_cooks_distance",
                       "maximum_leverage", "maximum_absolute_dfbeta", "influential_case_count", "exposure_p10", "exposure_p25", "exposure_p75", "exposure_p90",
                       "marginal_probability_difference_per_sd", "probability_at_mean_minus_half_sd", "probability_at_mean_plus_half_sd")

.siw_max_vif <- function(X) {
  P <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  if (ncol(P) <= 1L) return(1)
  s <- apply(P, 2L, stats::sd)
  if (any(!is.finite(s) | s == 0)) return(Inf)
  inv <- tryCatch(solve(stats::cor(P)), error = function(e) NULL)
  if (is.null(inv)) return(Inf)
  max(diag(inv))
}

.siw_calibration <- function(y, p) {
  p <- pmin(pmax(p, 1e-8), 1 - 1e-8); lp <- stats::qlogis(p)
  a <- tryCatch(suppressWarnings(stats::glm(y ~ 1 + offset(lp), family = stats::binomial())), error = function(e) NULL)
  b <- tryCatch(suppressWarnings(stats::glm(y ~ lp, family = stats::binomial())), error = function(e) NULL)
  c(intercept = if (is.null(a)) NA_real_ else unname(stats::coef(a)[["(Intercept)"]]),
    slope = if (is.null(b) || !("lp" %in% names(stats::coef(b)))) NA_real_ else unname(stats::coef(b)[["lp"]]))
}

.siw_safe_max <- function(x) { x <- x[is.finite(x)]; if (length(x)) max(x) else NA_real_ }

#' Diagnostics of one model on a prepared design matrix (exposure in column 2).
#' @param X complete-case design matrix with column names; y outcome; binary logical.
#' @return named list of the 16 diagnostics plus diagnostic_status and undefined_diagnostics.
#' @export
siw_model_diagnostics_one <- function(X, y, binary) {
  n <- nrow(X); p <- ncol(X); expo <- X[, 2L]
  out <- stats::setNames(as.list(rep(NA_real_, length(.siw_diag_columns))), .siw_diag_columns)
  out$exposure_p10 <- unname(stats::quantile(expo, 0.10)); out$exposure_p25 <- unname(stats::quantile(expo, 0.25))
  out$exposure_p75 <- unname(stats::quantile(expo, 0.75)); out$exposure_p90 <- unname(stats::quantile(expo, 0.90))
  rank_x <- qr(X)$rank
  zero_var <- vapply(seq_len(p), function(j) colnames(X)[j] != "(Intercept)" && stats::sd(X[, j]) == 0, logical(1))
  ev <- if (binary) c(sum(y == 1), sum(y == 0)) else c(1, 1)
  reason <- if (n <= p) "INSUFFICIENT_RESIDUAL_DF" else if (rank_x < p) "RANK_DEFICIENT" else if (any(zero_var)) "ZERO_VARIANCE_COLUMN" else if (stats::sd(expo) == 0) "ZERO_VARIANCE_EXPOSURE" else if (any(ev == 0)) "NO_BINARY_OUTCOME_VARIATION" else ""
  if (nzchar(reason)) {                                        # legacy: condition number and VIF are Inf for a non-estimable model, everything else undefined
    out$condition_number <- Inf; out$maximum_vif <- Inf
    return(c(out, list(diagnostic_status = paste0("NOT_ESTIMABLE:", reason), undefined_diagnostics = paste(setdiff(.siw_diag_columns, c("condition_number", "maximum_vif", "exposure_p10", "exposure_p25", "exposure_p75", "exposure_p90")), collapse = "|"))))
  }
  out$condition_number <- kappa(X); out$maximum_vif <- .siw_max_vif(X)
  colnames(X)[colnames(X) == "(Intercept)"] <- "Intercept"
  if (!binary) {
    fit <- stats::lm(y ~ 0 + X); beta <- stats::coef(fit)
    ck <- stats::cooks.distance(fit); hv <- stats::hatvalues(fit); db <- stats::dfbetas(fit)
  } else {
    fit <- suppressWarnings(stats::glm(y ~ 0 + X, family = stats::binomial())); beta <- stats::coef(fit)
    ck <- suppressWarnings(stats::cooks.distance(fit)); hv <- suppressWarnings(stats::hatvalues(fit)); db <- suppressWarnings(stats::dfbetas(fit))
    pr <- pmin(pmax(stats::fitted(fit), 1e-12), 1 - 1e-12)
    cal <- .siw_calibration(y, pr); out$calibration_intercept <- cal[["intercept"]]; out$calibration_slope <- cal[["slope"]]
    s <- stats::sd(expo); ok <- isTRUE(fit$converged) && n >= 10L && is.finite(s) && s > 0
    if (ok) {
      ctr <- mean(expo); Xl <- X; Xh <- X; Xl[, 2L] <- ctr - 0.5 * s; Xh[, 2L] <- ctr + 0.5 * s
      pl <- stats::plogis(drop(Xl %*% beta)); ph <- stats::plogis(drop(Xh %*% beta))
      if (all(is.finite(pl)) && all(is.finite(ph))) { out$marginal_probability_difference_per_sd <- mean(ph - pl); out$probability_at_mean_minus_half_sd <- mean(pl); out$probability_at_mean_plus_half_sd <- mean(ph) }
    }
  }
  out$maximum_coefficient_magnitude <- .siw_safe_max(abs(beta))
  out$maximum_cooks_distance <- .siw_safe_max(ck); out$maximum_leverage <- .siw_safe_max(hv); out$maximum_absolute_dfbeta <- .siw_safe_max(abs(db))
  out$influential_case_count <- sum(ck > (4 / n) | hv > (2 * p / n), na.rm = TRUE)
  undefined <- names(out)[vapply(out, function(z) !is.finite(z), logical(1))]
  if (!binary) undefined <- setdiff(undefined, c("calibration_intercept", "calibration_slope", "marginal_probability_difference_per_sd", "probability_at_mean_minus_half_sd", "probability_at_mean_plus_half_sd"))
  nonfinite_influence <- sum(!is.finite(ck)) + sum(!is.finite(hv)) + sum(!is.finite(db))
  c(out, list(diagnostic_status = if (!length(undefined) && nonfinite_influence == 0L) "OK" else "UNDEFINED_VALUES", undefined_diagnostics = paste(undefined, collapse = "|")))
}

#' Diagnostics for every model of the cumulative grid (same samples, tiers and ordering as siw_cumulative_grid).
#' One row per specification: result_id, keys, the 16 diagnostics, sample size, rank, dropped terms (locality levels absent from the complete-case sample), status.
#' @export
siw_cumulative_diagnostics <- function(dat, config, tiers = config$tier_order) {
  tier_x <- lapply(stats::setNames(tiers, tiers), function(t) siw_tier_matrix(dat, config$tiers[[t]])); tier_ok <- lapply(tier_x, stats::complete.cases)
  rows <- list()
  for (oi in seq_len(nrow(config$outcomes))) {
    o <- config$outcomes[oi, ]; y <- as.numeric(dat[[o$outcome]]); y_ok <- is.finite(y)
    for (mi in seq_len(nrow(config$metrics))) {
      sp <- config$metrics[mi, ]
      for (w in config$windows) {
        x <- as.numeric(dat[[siw_cum_name(sp$cumulative_prefix, w)]]); x_ok <- is.finite(x)
        for (t in tiers) {
          mask <- tier_ok[[t]] & y_ok & x_ok; Xs <- tier_x[[t]][mask, , drop = FALSE]
          drop <- grepl("^Locality_3group", colnames(Xs)) & (colSums(Xs != 0) == 0L)
          dropped <- colnames(Xs)[drop]; Xs <- Xs[, !drop, drop = FALSE]
          X <- cbind(Xs[, 1L, drop = FALSE], exposure = x[mask], Xs[, -1L, drop = FALSE]); colnames(X)[1L] <- "(Intercept)"
          d <- siw_model_diagnostics_one(X, y[mask], o$family == "binomial")
          rows[[length(rows) + 1L]] <- data.frame(result_id = siw_cum_id(o$code, sp$code, w, t), outcome = o$outcome, metric = sp$metric, window_or_lag = as.integer(w), model_tier = t,
            n = nrow(X), parameter_count = ncol(X), rank = qr(X)$rank, as.data.frame(d[.siw_diag_columns]), dropped_terms = paste(dropped, collapse = "|"),
            diagnostic_status = d$diagnostic_status, undefined_diagnostics = d$undefined_diagnostics, stringsAsFactors = FALSE)
        }
      }
    }
  }
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}
