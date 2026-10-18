# Batch B: exposure-transformation sensitivity (documented 1,080 specifications).
# Recovered from the original code (R/040_transformations_core.R, R/090_sensitivity_models.R siecc_transformation_grid / siecc_fit_transformed_one),
# not from the model count:
#   transformations  raw, z, signed_log_z, exp_z for every metric; log1p and sqrt in addition for the two sum-type metrics (IMS rainfall total, IMS rainy-day count);
#   specifications   3 outcomes x (6 mean-type metrics x 4 + 2 sum-type metrics x 6) x 10 windows = 1,080; M2-L only;
#   source field     the cumulative exposure of the window (raw);
#   z                (x - mean) / sd computed over ALL non-missing values of the cumulative column in the data (all participants), not over the model's
#                    complete-case sample; missing values stay missing. Fewer than 2 non-missing values or sd <= 0: the transformation is undefined;
#   signed_log_z     sign(z) * log1p(|z|);        exp_z   exp(z);
#   log1p            log1p(x);  defined only if no non-missing value is negative (undefined otherwise);
#   sqrt             sqrt(x);   defined only if every non-missing value is >= 0 (undefined otherwise);
#   model            the M2-L model of the cumulative grid (OLS + HC3 with t reference for continuous outcomes; ML logistic with Wald z for the binary outcome)
#                    fitted to the transformed exposure on the complete cases of outcome, transformed exposure and M2-L covariates;
#   multiplicity     BH over the finite nominal p-values within outcome (360) and over all 1,080 (q_transformation_outcome, q_transformation_global).
# Difference from the original, by design: the original stops the whole grid with an error if a domain rule is violated; here the specification is reported
# NOT_ESTIMABLE with the reason and the remaining specifications continue. For data that satisfy the domain rules the results are identical.

.siw_trn_all <- c("raw", "z", "signed_log_z", "exp_z", "log1p", "sqrt")

#' The transformations that apply to a metric (aggregation "sum" adds log1p and sqrt).
#' @export
siw_transformations_for <- function(aggregation) if (identical(aggregation, "sum")) .siw_trn_all else .siw_trn_all[1:4]

#' Apply one exposure transformation under the original domain and missing-value rules.
#' @return list(value = numeric vector (NA where undefined), status = "OK" or "UNDEFINED", reason)
#' @export
siw_transform_exposure <- function(x, kind) {
  x <- as.numeric(x); obs <- !is.na(x)
  bad <- function(r) list(value = rep(NA_real_, length(x)), status = "UNDEFINED", reason = r)
  zs <- function() {
    if (sum(obs) < 2L) return(NULL)
    s <- stats::sd(x[obs]); if (!is.finite(s) || s <= 0) return(NULL)
    z <- rep(NA_real_, length(x)); z[obs] <- (x[obs] - mean(x[obs])) / s; z
  }
  if (kind == "raw") return(list(value = x, status = "OK", reason = ""))
  if (kind %in% c("z", "signed_log_z", "exp_z")) {
    z <- zs(); if (is.null(z)) return(bad(if (sum(obs) < 2L) "TRANSFORMATION_Z_INSUFFICIENT_VALUES" else "TRANSFORMATION_Z_ZERO_OR_INVALID_SD"))
    v <- switch(kind, z = z, signed_log_z = sign(z) * log1p(abs(z)), exp_z = exp(z)); return(list(value = v, status = "OK", reason = ""))
  }
  if (kind == "log1p") { if (any(x < 0, na.rm = TRUE)) return(bad("LOG1P_REQUIRES_NONNEGATIVE_VALUES")); return(list(value = log1p(x), status = "OK", reason = "")) }
  if (kind == "sqrt") { if (!all(x >= 0, na.rm = TRUE)) return(bad("SQRT_REQUIRES_NONNEGATIVE_VALUES")); return(list(value = sqrt(x), status = "OK", reason = "")) }
  stop("SIW_UNKNOWN_TRANSFORMATION: ", kind, call. = FALSE)
}

#' Design matrix of one specification: intercept, exposure (column 2), covariates; locality levels absent from the complete-case sample are dropped
#' (the dropped terms are returned). Rows with a missing outcome, exposure or covariate are excluded.
#' @return list(X, y, mask, dropped)
.siw_spec_design <- function(Xc, covariate_ok, y, x) {
  mask <- covariate_ok & is.finite(y) & is.finite(x); Xs <- Xc[mask, , drop = FALSE]
  drop <- grepl("^Locality_3group", colnames(Xs)) & (colSums(Xs != 0) == 0L)
  Xs <- Xs[, !drop, drop = FALSE]
  X <- cbind(Xs[, 1L, drop = FALSE], exposure = x[mask], Xs[, -1L, drop = FALSE]); colnames(X)[1L] <- "(Intercept)"
  list(X = X, y = y[mask], mask = mask, dropped = setdiff(colnames(Xc)[grepl("^Locality_3group", colnames(Xc))], colnames(Xs)))
}

#' The 1,080 transformation specifications (M2-L).
#' @param tier model tier (the documented analysis uses the primary tier M2-L only)
#' @export
siw_transformation_grid <- function(dat, config, tier = config$primary_tier) {
  Xc <- siw_tier_matrix(dat, config$tiers[[tier]]); cov_ok <- stats::complete.cases(Xc); cache <- .siw_new_cache(); rows <- list(); k <- 0L
  for (oi in seq_len(nrow(config$outcomes))) {
    o <- config$outcomes[oi, ]; y <- as.numeric(dat[[o$outcome]])
    for (mi in seq_len(nrow(config$metrics))) {
      sp <- config$metrics[mi, ]
      for (w in config$windows) {
        src <- siw_cum_name(sp$cumulative_prefix, w); x0 <- as.numeric(dat[[src]])
        for (tr in siw_transformations_for(sp$aggregation)) {
          k <- k + 1L; tx <- siw_transform_exposure(x0, tr); st <- NULL; dropped <- ""
          if (tx$status == "OK") {
            sd_ <- .siw_spec_design(Xc, cov_ok, y, tx$value); dropped <- paste(sd_$dropped, collapse = "|")
            key <- paste(o$outcome, tier, paste(which(!sd_$mask), collapse = ","), sep = "|")   # the reduced model (no exposure) depends only on the sample
            st <- if (o$family == "gaussian") siw_fit_gaussian(sd_$X, sd_$y, 2L, cache, key)
                  else siw_fit_logistic(sd_$X, sd_$y, 2L, cache, key, epsilon = config$glm$cumulative_epsilon, maxit = config$glm$cumulative_maxit)
          } else st <- list(estimable = FALSE, reason = paste0("TRANSFORMATION_UNDEFINED: ", tx$reason))
          ok <- isTRUE(st$estimable)
          meta <- data.frame(result_id = paste("TRN", o$code, sp$code, paste0("W", w), tier, tr, sep = "__"), outcome = o$outcome, outcome_family = o$family, metric = sp$metric, source_exposure = src,
                             window_or_lag = as.integer(w), transformation = tr, model_tier = tier, status = if (ok) "COMPLETED" else "NOT_ESTIMABLE", not_estimable_reason = st$reason %||% "",
                             separation_status = st$separation_status %||% "", convergence = if (is.null(st$convergence)) NA else st$convergence, dropped_terms = dropped, stringsAsFactors = FALSE)
          st$parameter_count <- st$p
          vals <- vapply(.siw_cum_stats, function(s) { v <- st[[s]]; if (is.null(v)) NA_real_ else as.numeric(v) }, numeric(1))
          rows[[k]] <- cbind(meta, as.data.frame(as.list(vals), stringsAsFactors = FALSE))
        }
      }
    }
  }
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out$q_transformation_outcome <- NA_real_
  for (oc in unique(out$outcome)) { i <- which(out$outcome == oc & is.finite(out$nominal_p)); out$q_transformation_outcome[i] <- siw_bh(out$nominal_p[i]) }
  out$q_transformation_global <- siw_bh(out$nominal_p)
  out$multiplicity_family_q_transformation_outcome <- ifelse(is.finite(out$q_transformation_outcome), paste0("TRANSFORMATION_q_outcome_", out$outcome), "")
  out$multiplicity_family_q_transformation_global <- ifelse(is.finite(out$q_transformation_global), "TRANSFORMATION_q_global_1080", "")
  out
}

#' Independent recalculation of every completed transformation specification with stats::lm / stats::glm and a hand-written HC3 / information
#' (no use of the package engine). Compares coefficient, SE, p-value and n.
#' @return data frame, one row per specification
#' @export
siw_transformation_verify <- function(dat, config, grid, tier = config$primary_tier, tol = 1e-8) {
  covs <- config$tiers[[tier]]; rows <- list()
  for (i in which(grid$status == "COMPLETED")) {
    g <- grid[i, ]; x0 <- as.numeric(dat[[g$source_exposure]]); tx <- siw_transform_exposure(x0, g$transformation)$value
    d <- dat; d$.x <- tx; vars <- unique(c(g$outcome, ".x", covs)); d <- droplevels(d[stats::complete.cases(d[, vars, drop = FALSE]), vars, drop = FALSE])
    f <- stats::reformulate(c(".x", covs), g$outcome)
    if (g$outcome_family == "gaussian") {
      fit <- stats::lm(f, d); X <- stats::model.matrix(fit); e <- stats::residuals(fit); xtxi <- solve(crossprod(X)); h <- rowSums((X %*% xtxi) * X)
      meat <- crossprod(X, X * (e^2 / pmax(1e-12, (1 - h)^2))); V <- xtxi %*% meat %*% xtxi; b <- stats::coef(fit)[[".x"]]; se <- sqrt(V[".x", ".x"])
      p <- 2 * stats::pt(abs(b / se), stats::df.residual(fit), lower.tail = FALSE)
    } else {
      fit <- suppressWarnings(stats::glm(f, d, family = stats::binomial())); b <- stats::coef(fit)[[".x"]]; se <- sqrt(diag(stats::vcov(fit)))[[".x"]]; p <- 2 * stats::pnorm(abs(b / se), lower.tail = FALSE)
    }
    rel <- function(a, r) abs(a - r) / pmax(1, abs(r))
    dd <- c(coefficient = rel(g$coefficient, b), se = rel(if (g$outcome_family == "gaussian") g$robust_hc3_se else g$model_based_se, se), p = rel(g$nominal_p, p), n = abs(g$n - nrow(d)))
    rows[[length(rows) + 1L]] <- data.frame(result_id = g$result_id, coefficient_difference = dd[["coefficient"]], se_difference = dd[["se"]], p_difference = dd[["p"]], n_difference = dd[["n"]],
                                            status = if (all(dd <= tol)) "PASS" else "FAIL", stringsAsFactors = FALSE)
  }
  siw_bind_rows(rows)
}
