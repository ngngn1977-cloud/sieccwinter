# Batch B: nonlinear exposure forms (documented 360 specifications).
# Recovered from the original code (R/090_sensitivity_models.R siecc_nonlinear_one / siecc_nonlinearity_grid, siecc_joint_wald):
#   metrics        the six mean-type metrics (UTCI, MRT, ERA5 air temperature, ERA5 RH, IMS temperature, IMS RH); M2-L only;
#   specifications 3 outcomes x 6 metrics x 10 windows x 2 forms = 360;
#   sample         complete cases of outcome, cumulative exposure and M2-L covariates; fewer than 15 rows or a constant exposure: NOT_ESTIMABLE;
#   quadratic      exposure_c = x - mean(x), exposure_c^2 (centred on the complete-case sample); joint test of the squared term only (1 df);
#   spline_df2     splines::ns(x, df = 2) of the complete-case exposure: one interior knot at the median, boundary knots at the sample range, no intercept;
#                  two basis columns, joint 2-df test of both columns;
#   joint test     Wald chi-square: b' V^-1 b over the tested coefficients, p = pchisq(upper tail, df);
#                  V = HC3 covariance for the continuous outcomes, model-based covariance (inverse expected information) for the binary outcome;
#   fit statistics AIC and small-sample AICc (k = coefficients + 1 for OLS, k = coefficients for logistic);
#   multiplicity   BH over finite joint p-values within outcome x form (60) -> q_outcome_form; over all 360 -> q_global.
# Differences from the original, by design: a rank-deficient or singular specification is reported NOT_ESTIMABLE with the reason (the original stops the grid
# for HC3 rank deficiency and returns a completed row with a missing statistic for a singular covariance); convergence of the logistic fit is recorded.

.siw_nl_forms <- c("quadratic", "spline_df2")

.siw_joint_wald <- function(b, V, idx) {
  v <- V[idx, idx, drop = FALSE]; inv <- tryCatch(solve(v), error = function(e) NULL)
  if (is.null(inv) || any(!is.finite(inv))) return(c(statistic = NA_real_, df = length(idx), p = NA_real_))
  s <- as.numeric(t(b[idx]) %*% inv %*% b[idx]); c(statistic = s, df = length(idx), p = stats::pchisq(s, df = length(idx), lower.tail = FALSE))
}

#' One nonlinear specification on a complete-case sample.
#' @param Xc covariate matrix of the tier (intercept first); covariate_ok logical complete-case indicator of the covariates; y, x outcome and exposure
#' @return one-row data frame
siw_nonlinear_one <- function(Xc, covariate_ok, y, x, binary, form) {
  mask <- covariate_ok & is.finite(y) & is.finite(x); n <- sum(mask); xs <- x[mask]
  na_row <- function(status, reason, nn = n, cv = NA) data.frame(n = nn, joint_statistic = NA_real_, joint_df = NA_real_, joint_p = NA_real_, aic = NA_real_, aicc = NA_real_, status = status, not_estimable_reason = reason,
                                                               convergence = cv, parameter_count = NA_integer_, dropped_terms = "", stringsAsFactors = FALSE)
  if (n < 15L || !(stats::sd(xs) > 0)) return(na_row("NOT_ESTIMABLE", if (n < 15L) "FEWER_THAN_15_COMPLETE_CASES" else "ZERO_VARIANCE_EXPOSURE"))
  Xs <- Xc[mask, , drop = FALSE]; keepc <- !(grepl("^Locality_3group", colnames(Xs)) & colSums(Xs != 0) == 0L); dropped <- paste(colnames(Xs)[!keepc], collapse = "|"); Xs <- Xs[, keepc, drop = FALSE]
  cen <- xs - mean(xs)
  basis <- if (form == "quadratic") cbind(exposure_c = cen, exposure_c2 = cen^2) else { b <- splines::ns(xs, df = 2); cbind(exposure_s1 = b[, 1L], exposure_s2 = b[, 2L]) }
  X <- cbind(Xs[, 1L, drop = FALSE], basis, Xs[, -1L, drop = FALSE]); colnames(X)[1L] <- "(Intercept)"; p <- ncol(X); ys <- y[mask]
  idx <- if (form == "quadratic") 3L else 2:3
  q <- qr(X); if (n <= p || q$rank < p) return(utils::modifyList(na_row("NOT_ESTIMABLE", if (n <= p) "INSUFFICIENT_RESIDUAL_DF" else "RANK_DEFICIENT"), list(dropped_terms = dropped)))
  if (!binary) {
    beta <- as.numeric(qr.coef(q, ys)); e <- ys - drop(X %*% beta); hc <- .siw_hc3(X, e, qr.Q(q), chol2inv(qr.R(q))); w <- .siw_joint_wald(beta, hc$cov, idx)
    rss <- sum(e^2); aic <- n * (log(2 * pi) + 1 - log(n) + log(rss)) + 2 * (p + 1); k <- p + 1L; conv <- TRUE
  } else {
    if (sum(ys == 1) == 0L || sum(ys == 0) == 0L) return(utils::modifyList(na_row("NOT_ESTIMABLE", "NO_BINARY_OUTCOME_VARIATION"), list(dropped_terms = dropped)))
    fit <- suppressWarnings(stats::glm.fit(X, ys, family = stats::binomial())); beta <- unname(fit$coefficients)
    V <- tryCatch(solve(crossprod(X * sqrt(fit$weights))), error = function(e) matrix(NA_real_, p, p)); w <- .siw_joint_wald(beta, V, idx); aic <- fit$aic; k <- p; conv <- isTRUE(fit$converged)
  }
  data.frame(n = n, joint_statistic = w[["statistic"]], joint_df = w[["df"]], joint_p = w[["p"]], aic = aic, aicc = if (n - k - 1 > 0) aic + 2 * k * (k + 1) / (n - k - 1) else NA_real_,
             status = if (is.finite(w[["p"]])) "COMPLETED" else "NOT_ESTIMABLE", not_estimable_reason = if (is.finite(w[["p"]])) "" else "SINGULAR_JOINT_COVARIANCE", convergence = conv, parameter_count = p, dropped_terms = dropped, stringsAsFactors = FALSE)
}

#' The 360 nonlinear specifications (M2-L) with q_outcome_form and q_global.
#' @export
siw_nonlinear_grid <- function(dat, config, tier = config$primary_tier) {
  Xc <- siw_tier_matrix(dat, config$tiers[[tier]]); cov_ok <- stats::complete.cases(Xc); rows <- list(); k <- 0L
  for (oi in seq_len(nrow(config$outcomes))) {
    o <- config$outcomes[oi, ]; y <- as.numeric(dat[[o$outcome]])
    for (mi in which(config$metrics$aggregation == "mean")) {
      sp <- config$metrics[mi, ]
      for (w in config$windows) {
        src <- siw_cum_name(sp$cumulative_prefix, w); x <- as.numeric(dat[[src]])
        for (form in .siw_nl_forms) {
          k <- k + 1L; r <- siw_nonlinear_one(Xc, cov_ok, y, x, o$family == "binomial", form)
          rows[[k]] <- cbind(data.frame(result_id = paste("NL", o$code, sp$code, paste0("W", w), tier, form, sep = "__"), outcome = o$outcome, outcome_family = o$family, metric = sp$metric, exposure = src,
                                        window_or_lag = as.integer(w), model_tier = tier, form = form, stringsAsFactors = FALSE), r)
        }
      }
    }
  }
  out <- do.call(rbind, rows); rownames(out) <- NULL; out$q_outcome_form <- NA_real_
  for (oc in unique(out$outcome)) for (f in unique(out$form)) { i <- which(out$outcome == oc & out$form == f & is.finite(out$joint_p)); out$q_outcome_form[i] <- siw_bh(out$joint_p[i]) }
  out$q_global <- siw_bh(out$joint_p)
  out$multiplicity_family_q_outcome_form <- ifelse(is.finite(out$q_outcome_form), paste0("NONLINEAR_q_", out$outcome, "_", out$form), "")
  out$multiplicity_family_q_global <- ifelse(is.finite(out$q_global), "NONLINEAR_q_global_360", "")
  out
}

#' Independent recalculation of every completed nonlinear specification with stats::lm / stats::glm, splines::ns, a hand-written HC3 and
#' a direct quadratic form (no use of the package's QR / engine helpers).
#' @export
siw_nonlinear_verify <- function(dat, config, grid, tier = config$primary_tier, tol = 1e-8) {
  covs <- config$tiers[[tier]]; rows <- list()
  for (i in which(grid$status == "COMPLETED")) {
    g <- grid[i, ]; vars <- unique(c(g$outcome, g$exposure, covs)); d <- droplevels(dat[stats::complete.cases(dat[, vars, drop = FALSE]), vars, drop = FALSE]); x <- as.numeric(d[[g$exposure]])
    if (g$form == "quadratic") { d$ec <- x - mean(x); d$ec2 <- d$ec^2; terms <- c("ec", "ec2", covs); tested <- "ec2" } else { b <- splines::ns(x, df = 2); d$es1 <- b[, 1]; d$es2 <- b[, 2]; terms <- c("es1", "es2", covs); tested <- c("es1", "es2") }
    f <- stats::reformulate(terms, g$outcome)
    if (g$outcome_family == "gaussian") {
      fit <- stats::lm(f, d); X <- stats::model.matrix(fit); e <- stats::residuals(fit); xtxi <- solve(crossprod(X)); h <- rowSums((X %*% xtxi) * X)
      V <- xtxi %*% crossprod(X, X * (e^2 / pmax(1e-12, (1 - h)^2))) %*% xtxi; kk <- length(stats::coef(fit)) + 1L
    } else { fit <- suppressWarnings(stats::glm(f, d, family = stats::binomial())); V <- stats::vcov(fit); kk <- length(stats::coef(fit)) }
    b <- stats::coef(fit)[tested]; Vt <- V[tested, tested, drop = FALSE]; stat <- sum(b * solve(Vt, b)); p <- stats::pchisq(stat, length(tested), lower.tail = FALSE)
    aic <- stats::AIC(fit); aicc <- aic + 2 * kk * (kk + 1) / (nrow(d) - kk - 1)
    rel <- function(a, r) abs(a - r) / pmax(1, abs(r))
    dd <- c(statistic = rel(g$joint_statistic, stat), p = rel(g$joint_p, p), aic = rel(g$aic, aic), aicc = rel(g$aicc, aicc), n = abs(g$n - nrow(d)))
    rows[[length(rows) + 1L]] <- data.frame(result_id = g$result_id, statistic_difference = dd[["statistic"]], p_difference = dd[["p"]], aic_difference = dd[["aic"]], aicc_difference = dd[["aicc"]], n_difference = dd[["n"]],
                                            status = if (all(dd <= tol)) "PASS" else "FAIL", stringsAsFactors = FALSE)
  }
  siw_bind_rows(rows)
}
