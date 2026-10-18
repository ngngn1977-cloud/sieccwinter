# Distributed-lag models: linear exposure-response, natural cubic spline across lag (df = 3), L01..L<lag_end>.
# Final publication branch: nine representative M2-L models at L01-L56. The same fitting function serves the archived L01-L59 branch.

#' Multivariate-normal draws (n x p) with mean zero and covariance Sigma.
#' method "MASS_mvrnorm" is MASS::mvrnorm (as used by the original analysis). Its eigenvector signs can flip when Sigma changes at
#' machine precision, which changes the realised draws (not their distribution). method "eigen_sign_normalised" builds the draws exactly
#' as mvrnorm does but fixes each eigenvector's sign (largest-magnitude component positive), so the draws are stable to tiny changes in Sigma.
#' @export
siw_mvn_draws <- function(n, Sigma, method = "MASS_mvrnorm") {
  p <- ncol(Sigma)
  if (identical(method, "MASS_mvrnorm")) return(MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = Sigma))
  if (!identical(method, "eigen_sign_normalised")) stop("SIW_UNKNOWN_MVN_METHOD: ", method, call. = FALSE)
  eS <- eigen(Sigma, symmetric = TRUE); V <- eS$vectors
  for (j in seq_len(p)) if (V[which.max(abs(V[, j])), j] < 0) V[, j] <- -V[, j]
  X <- matrix(stats::rnorm(p * n), n)
  t(V %*% diag(sqrt(pmax(eS$values, 0)), p) %*% t(X))
}

#' Fit one DLM.
#' @param dat Prepared analysis data (see siw_prepare_data) with daily exposure columns.
#' @param draws Number of multivariate-normal draws for the simultaneous max-|t| critical value (0 = none).
#' @param cache Environment used to reuse reduced (covariate-only) models across metrics that share a sample.
#' @return list(summary, lag_curve, cumulative_curve); curves are empty data frames when the model is not estimable.
#' @export
siw_fit_dlm <- function(dat, config, outcome, metric, lag_end = 56L, lag_df = 3L, tier = config$primary_tier, draws = 5000L, seed = NULL, cache = NULL, return_curves = TRUE, x_cov = NULL) {
  o <- config$outcomes[config$outcomes$outcome == outcome, ]; sp <- config$metrics[config$metrics$metric == metric, ]
  family <- o$family; covs <- config$tiers[[tier]]
  dn <- siw_daily_names(sp$daily_prefix, lag_end)
  Xc <- x_cov %||% siw_tier_matrix(dat, covs)
  y <- as.numeric(dat[[outcome]])
  H <- as.matrix(dat[, dn, drop = FALSE]); storage.mode(H) <- "double"
  mask <- stats::complete.cases(Xc) & is.finite(y) & (rowSums(!is.finite(H)) == 0L)          # strict complete history: every lag required
  Xs <- Xc[mask, , drop = FALSE]; drop <- grepl("^Locality_3group", colnames(Xs)) & (colSums(Xs != 0) == 0L); Xs <- Xs[, !drop, drop = FALSE]
  B <- siw_lag_basis(lag_end, lag_df); cb <- siw_crossbasis(H[mask, , drop = FALSE], B)
  ys <- y[mask]; n <- nrow(Xs); pc <- ncol(Xs); X <- cbind(Xs, cb); p <- ncol(X); bi <- (pc + 1L):p
  seed <- seed %||% siw_model_seed(config, metric, outcome)
  id <- siw_dlm_id(o$code, sp$code, lag_end, lag_df, tier)
  base <- data.frame(result_id = id, outcome = outcome, outcome_family = family, metric = metric, model_tier = tier, n = n, lag_start = 1L, lag_end = as.integer(lag_end), lag_df = as.integer(lag_df), stringsAsFactors = FALSE)
  fail <- function(reason) list(summary = cbind(base, status = reason, global_p = NA_real_), lag_curve = data.frame(), cumulative_curve = data.frame())
  q <- qr(X)
  if (n <= p + 1L || q$rank < p || stats::sd(as.numeric(H[mask, ])) == 0) return(fail("NOT_ESTIMABLE_RANK"))
  if (family == "binomial" && length(unique(ys)) < 2L) return(fail("NOT_ESTIMABLE_RANK"))
  key <- paste(outcome, tier, lag_end, paste(which(!mask), collapse = ","), sep = "|")
  if (family == "gaussian") {
    beta <- as.numeric(qr.coef(q, ys)); e <- ys - as.numeric(X %*% beta); rss <- sum(e^2)
    red <- if (!is.null(cache)) cache[[key]] else NULL
    if (is.null(red)) { qr0 <- qr(Xs); e0 <- ys - as.numeric(Xs %*% qr.coef(qr0, ys)); red <- list(rss = sum(e0^2)); if (!is.null(cache)) cache[[key]] <- red }
    dfa <- p - pc; gstat <- ((red$rss - rss) / dfa) / (rss / (n - p)); gp <- stats::pf(gstat, dfa, n - p, lower.tail = FALSE); df2 <- n - p
    V <- .siw_hc3(X, e, qr.Q(q), chol2inv(qr.R(q)))$cov
    conv <- TRUE; sep <- FALSE; method <- "nested_F (homoskedastic residual sums of squares)"; cov_method <- "HC3"
  } else {
    ctl <- stats::glm.control(epsilon = config$dlm$final$glm_epsilon, maxit = config$dlm$final$glm_maxit)
    fit <- suppressWarnings(stats::glm.fit(X, ys, family = stats::binomial(), control = ctl)); beta <- unname(fit$coefficients)
    red <- if (!is.null(cache)) cache[[key]] else NULL
    if (is.null(red)) { fr <- suppressWarnings(stats::glm.fit(Xs, ys, family = stats::binomial(), control = ctl)); red <- list(deviance = fr$deviance); if (!is.null(cache)) cache[[key]] <- red }
    dfa <- p - pc; gstat <- red$deviance - fit$deviance; gp <- stats::pchisq(gstat, dfa, lower.tail = FALSE); df2 <- NA_real_
    V <- tryCatch(solve(crossprod(X * sqrt(fit$weights))), error = function(e) matrix(NA_real_, p, p))
    pr <- pmin(pmax(fit$fitted.values, 1e-12), 1 - 1e-12)
    conv <- isTRUE(fit$converged) && all(is.finite(beta)); sep <- !conv || max(abs(beta), na.rm = TRUE) > 10 || any(pr < 1e-8 | pr > 1 - 1e-8)
    method <- "likelihood_ratio_chi2 (ordinary ML)"; cov_method <- "model-based inverse information"
  }
  bc <- beta[bi]; Vb <- V[bi, bi, drop = FALSE]
  summ <- cbind(base, event_count = if (family == "binomial") sum(ys == 1) else NA_integer_, global_test_method = method, global_statistic = gstat, global_df1 = dfa, global_df2 = df2, global_p = gp,
                parameter_count = p, residual_df = n - qr(X)$rank, rank = q$rank, covariance_method = cov_method, convergence = conv, separation = sep, status = "COMPLETED", stringsAsFactors = FALSE)
  if (any(!is.finite(bc)) || any(!is.finite(Vb))) { summ$status <- "NOT_ESTIMABLE_NUMERIC_COVARIANCE"; return(list(summary = summ, lag_curve = data.frame(), cumulative_curve = data.frame())) }
  le <- drop(B %*% bc); C <- B %*% Vb %*% t(B); lse <- sqrt(pmax(0, diag(C)))
  ce <- cumsum(le); cvar <- cumsum(diag(C) + 2 * rowSums(C * lower.tri(C))); cse <- sqrt(pmax(0, cvar))
  crit <- NA_real_; sim_lo <- sim_hi <- rep(NA_real_, lag_end)
  if (draws >= 10L && all(is.finite(lse)) && all(lse > 0)) {
    crit <- siw_with_seed(seed, {
      Z <- siw_mvn_draws(draws, Vb, config$dlm$final$mvn_method %||% "MASS_mvrnorm")
      mt <- apply(abs(sweep(Z %*% t(B), 2L, lse, "/")), 1L, max)
      as.numeric(stats::quantile(mt, config$dlm$final$confidence, names = FALSE, type = config$dlm$final$quantile_type))
    })
    sim_lo <- le - crit * lse; sim_hi <- le + crit * lse
  }
  z <- 1.96; plo <- le - z * lse; phi <- le + z * lse                 # 1.96 as in the authoritative analysis (normal approximation)
  pk <- which.max(abs(le))
  summ$cumulative_effect <- ce[lag_end]; summ$cumulative_se <- cse[lag_end]; summ$cumulative_ci_lower <- ce[lag_end] - z * cse[lag_end]; summ$cumulative_ci_upper <- ce[lag_end] + z * cse[lag_end]
  summ$peak_lag <- pk; summ$peak_effect <- le[pk]; summ$simultaneous_critical <- crit; summ$simultaneous_draws <- as.integer(draws); summ$seed <- as.integer(seed)
  summ$band_method <- config$dlm$final$band_method
  summ$n_lags_pointwise_excl_null <- sum(plo > 0 | phi < 0); summ$any_pointwise_band_excludes_null <- summ$n_lags_pointwise_excl_null > 0
  summ$n_lags_simultaneous_excl_null <- if (is.finite(crit)) sum(sim_lo > 0 | sim_hi < 0) else NA_integer_
  summ$any_simultaneous_band_excludes_null <- if (is.finite(crit)) summ$n_lags_simultaneous_excl_null > 0 else NA
  if (!return_curves) return(list(summary = summ, lag_curve = data.frame(), cumulative_curve = data.frame()))
  lag <- seq_len(lag_end)
  list(summary = summ,
       lag_curve = data.frame(result_id = id, lag = lag, estimate = le, standard_error = lse, lower_95 = plo, upper_95 = phi, simultaneous_lower_95 = sim_lo, simultaneous_upper_95 = sim_hi, stringsAsFactors = FALSE),
       cumulative_curve = data.frame(result_id = id, lag = lag, cumulative_effect = ce, cumulative_se = cse, cumulative_lower_95 = ce - z * cse, cumulative_upper_95 = ce + z * cse, stringsAsFactors = FALSE))
}

#' Final publication DLMs: 3 outcomes x 3 representative metrics, M2-L, L01-L56, with q9 across the nine global tests.
#' @export
siw_final_dlm <- function(dat, config, draws = config$dlm$final$draws) {
  f <- config$dlm$final; cache <- .siw_new_cache(); xc <- siw_tier_matrix(dat, config$tiers[[f$tier]])
  S <- list(); LC <- list(); CC <- list()
  for (oi in seq_len(nrow(config$outcomes))) for (m in config$representative_metrics) {
    r <- siw_fit_dlm(dat, config, config$outcomes$outcome[oi], m, f$lag_end, f$lag_df, f$tier, draws = draws, cache = cache, x_cov = xc)
    S[[length(S) + 1L]] <- r$summary; LC[[length(LC) + 1L]] <- r$lag_curve; CC[[length(CC) + 1L]] <- r$cumulative_curve
  }
  s <- siw_bind_rows(S); s$q9 <- siw_bh(s$global_p); s$multiplicity_family_q9 <- "DLM_FINAL_L01_L56_M2_L_q9"
  list(summary = s, lag_curves = siw_bind_rows(LC), cumulative_curves = siw_bind_rows(CC))
}
