# Fast fitting engine: OLS with HC3 and ordinary maximum-likelihood logistic regression on pre-built design matrices.
# Estimators are the standard ones (QR least squares; glm.fit IRLS). Only redundant work is removed; see the optimisation report.

.siw_hc3 <- function(X, e, Q1 = NULL, XtXi = NULL) {
  # HC3 = (X'X)^-1 X' diag(e^2 / (1-h)^2) X (X'X)^-1 with h the leverages. h = row sums of the squared thin-Q factor.
  if (is.null(XtXi)) XtXi <- chol2inv(qr.R(qr(X)))
  h <- if (is.null(Q1)) rowSums((X %*% XtXi) * X) else rowSums(Q1^2)
  w <- e / pmax(1e-6, 1 - h)              # pmax(1e-12, (1-h)^2) == (pmax(1e-6, 1-h))^2 for 1-h >= 0
  meat <- crossprod(X * w)
  list(cov = XtXi %*% meat %*% XtXi, leverage = h)
}

# Reduced-model caches (keyed by outcome, tier and the set of excluded rows) so that models sharing a sample reuse them.
.siw_new_cache <- function() new.env(parent = emptyenv())

.siw_estimable <- function(X, n, exposure_col, y = NULL, binary = FALSE) {
  p <- ncol(X); q <- qr(X)
  zero_var <- vapply(seq_len(p), function(j) { if (colnames(X)[j] == "(Intercept)") FALSE else diff(range(X[, j])) == 0 }, logical(1))
  reason <- if (n <= p) "INSUFFICIENT_RESIDUAL_DF" else if (q$rank < p) "RANK_DEFICIENT" else if (any(zero_var)) paste0("ZERO_VARIANCE_", paste(colnames(X)[zero_var], collapse = "|")) else ""
  if (!nzchar(reason) && binary && (sum(y == 1) == 0L || sum(y == 0) == 0L)) reason <- "NO_BINARY_OUTCOME_VARIATION"
  list(ok = !nzchar(reason), reason = reason, qr = q)
}

#' Fit one Gaussian model (X includes the intercept in column 1 and the exposure in column `exposure_col`).
#' @return named list of statistics for the exposure coefficient.
#' @export
siw_fit_gaussian <- function(X, y, exposure_col = 2L, cache = NULL, cache_key = NULL, dfc = NULL) {
  n <- nrow(X); p <- ncol(X)
  est <- .siw_estimable(X, n, exposure_col)
  if (!est$ok) return(list(estimable = FALSE, reason = est$reason, n = n, p = p, rank = est$qr$rank))
  q <- est$qr; Q1 <- qr.Q(q); R <- qr.R(q); XtXi <- chol2inv(R)
  beta <- as.numeric(qr.coef(q, y)); fit <- as.numeric(X %*% beta); e <- y - fit
  rss <- sum(e^2); rdf <- n - p; sigma2 <- rss / rdf
  b <- beta[exposure_col]; se_m <- sqrt(sigma2 * XtXi[exposure_col, exposure_col])
  hc <- .siw_hc3(X, e, Q1, XtXi); se_r <- sqrt(max(0, hc$cov[exposure_col, exposure_col]))
  tr <- b / se_r; crit <- stats::qt(0.975, rdf)
  x <- X[, exposure_col]
  tss <- sum((y - mean(y))^2); r2 <- 1 - rss / tss; adj <- 1 - (1 - r2) * (n - 1) / rdf
  fstat <- ((tss - rss) / (p - 1)) / (rss / rdf)
  # reduced model without the exposure (cached by sample)
  red <- NULL
  if (!is.null(cache) && !is.null(cache_key)) red <- cache[[cache_key]]
  if (is.null(red)) {
    Xr <- X[, -exposure_col, drop = FALSE]; er <- y - Xr %*% qr.coef(qr(Xr), y)
    red <- list(rss = sum(er^2))
    if (!is.null(cache) && !is.null(cache_key)) cache[[cache_key]] <- red
  }
  aic <- n * (log(2 * pi) + 1 - log(n) + log(rss)) + 2 * (p + 1)
  k <- p + 1L
  list(estimable = TRUE, reason = "", n = n, p = p, rank = q$rank, residual_df = rdf, coefficient = b, model_based_se = se_m, robust_hc3_se = se_r,
       statistic = b / se_m, robust_statistic = tr, ci_lower = b - crit * se_r, ci_upper = b + crit * se_r,
       nominal_p = 2 * stats::pt(abs(tr), rdf, lower.tail = FALSE),
       standardised_effect = b * stats::sd(x) / stats::sd(y), iqr_effect = b * stats::IQR(x),
       model_f = fstat, model_f_p = stats::pf(fstat, p - 1, rdf, lower.tail = FALSE),
       r_squared = r2, adjusted_r_squared = adj, partial_r_squared_weather = if (red$rss > 0) (red$rss - rss) / red$rss else NA_real_,
       aic = aic, aicc = if (n - k - 1 > 0) aic + 2 * k * (k + 1) / (n - k - 1) else NA_real_, residual_rmse = sqrt(rss / n),
       convergence = TRUE, exposure_sd = stats::sd(x), exposure_iqr = stats::IQR(x))
}

#' Fit one ordinary maximum-likelihood logistic model (glm.fit with the configured control).
#' @export
siw_fit_logistic <- function(X, y, exposure_col = 2L, cache = NULL, cache_key = NULL, epsilon = 1e-8, maxit = 25L) {
  n <- nrow(X); p <- ncol(X)
  est <- .siw_estimable(X, n, exposure_col, y, binary = TRUE)
  if (!est$ok) return(list(estimable = FALSE, reason = est$reason, n = n, p = p, rank = est$qr$rank))
  ctl <- stats::glm.control(epsilon = epsilon, maxit = maxit)
  fit <- suppressWarnings(stats::glm.fit(X, y, family = stats::binomial(), control = ctl))
  b <- unname(fit$coefficients); w <- fit$weights
  V <- tryCatch(solve(crossprod(X * sqrt(w))), error = function(e) matrix(NA_real_, p, p))
  se <- sqrt(diag(V))[exposure_col]; z <- b[exposure_col] / se
  red <- NULL
  if (!is.null(cache) && !is.null(cache_key)) red <- cache[[cache_key]]
  if (is.null(red)) {
    Xr <- X[, -exposure_col, drop = FALSE]
    fr <- suppressWarnings(stats::glm.fit(Xr, y, family = stats::binomial(), control = ctl))
    n1 <- sum(y == 1); n0 <- n - n1
    red <- list(deviance = fr$deviance, p = ncol(Xr), ll0 = n1 * log(n1 / n) + n0 * log(n0 / n))
    if (!is.null(cache) && !is.null(cache_key)) cache[[cache_key]] <- red
  }
  ll <- -fit$deviance / 2; lr <- red$deviance - fit$deviance
  pr <- pmin(pmax(fit$fitted.values, 1e-12), 1 - 1e-12); x <- X[, exposure_col]
  r <- rank(pr, ties.method = "average"); n1 <- sum(y == 1); n0 <- n - n1
  sep <- !isTRUE(fit$converged) || any(!is.finite(b)) || max(abs(b)) > 10 || any(pr < 1e-8 | pr > 1 - 1e-8)
  k <- p
  list(estimable = TRUE, reason = "", n = n, p = p, rank = est$qr$rank, residual_df = n - p, event_count = n1, non_event_count = n0,
       coefficient = b[exposure_col], model_based_se = se, statistic = z, robust_statistic = z,
       ci_lower = b[exposure_col] - 1.96 * se, ci_upper = b[exposure_col] + 1.96 * se, nominal_p = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
       standardised_effect = b[exposure_col] * stats::sd(x), iqr_effect = b[exposure_col] * stats::IQR(x),
       odds_ratio_raw = exp(b[exposure_col]), odds_ratio_sd = exp(b[exposure_col] * stats::sd(x)), odds_ratio_iqr = exp(b[exposure_col] * stats::IQR(x)),
       lr_statistic = lr, lr_df = p - red$p, lr_p = stats::pchisq(lr, p - red$p, lower.tail = FALSE),
       nagelkerke_r_squared = (1 - exp((2 / n) * (red$ll0 - ll))) / (1 - exp((2 / n) * red$ll0)),
       aic = fit$aic, aicc = if (n - k - 1 > 0) fit$aic + 2 * k * (k + 1) / (n - k - 1) else NA_real_,
       brier_score = mean((y - pr)^2), log_loss = -mean(y * log(pr) + (1 - y) * log1p(-pr)),
       auc = if (n1 && n0) (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) else NA_real_,
       convergence = isTRUE(fit$converged), separation_status = if (sep) "SUSPECTED" else "NOT_DETECTED",
       exposure_sd = stats::sd(x), exposure_iqr = stats::IQR(x))
}
