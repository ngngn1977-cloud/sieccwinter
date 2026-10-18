# Phase 3D: independent verification route for the final L01-L56 distributed-lag outputs (declared in config/dlm_validation_protocol.json).
# Independent of the package fit: own B-spline basis (Cox-de Boor) with the natural-spline boundary constraint, own least squares / Newton logistic fit, own HC3, own cumulative sums from explicit contrasts, own MVN draws.
# Shared with the package by design (and declared): the M2-L covariate matrix (siw_tier_matrix, validated in the core run), the daily column names, the RNG-state wrapper siw_with_seed and the independent type-8 quantile siw_quantile8.
# Nothing here reads a figure, a manuscript value or a reference; all functions work on synthetic data in the tests.

.siw_dv_q7 <- function(x, p) { x <- sort(x); n <- length(x); h <- (n - 1) * p + 1; lo <- floor(h); hi <- ceiling(h); x[lo] + (h - lo) * (x[hi] - x[lo]) }

# Cox-de Boor: B-spline i (1-based), order k, d-th derivative, at x, knot vector t. The right boundary belongs to the last non-empty interval (as in splines::splineDesign).
.siw_dv_bs <- function(i, k, d, x, t) {
  if (d > 0L) {
    if (k == 1L) return(rep(0, length(x)))
    a <- t[i + k - 1L] - t[i]; b <- t[i + k] - t[i + 1L]
    r1 <- if (a > 0) .siw_dv_bs(i, k - 1L, d - 1L, x, t) / a else 0; r2 <- if (b > 0) .siw_dv_bs(i + 1L, k - 1L, d - 1L, x, t) / b else 0
    return((k - 1L) * (r1 - r2))
  }
  if (k == 1L) { last <- max(t); return(as.numeric((x >= t[i] & x < t[i + 1L]) | (x == last & t[i] < t[i + 1L] & t[i + 1L] == last))) }
  a <- t[i + k - 1L] - t[i]; b <- t[i + k] - t[i + 1L]
  (if (a > 0) (x - t[i]) / a * .siw_dv_bs(i, k - 1L, 0L, x, t) else 0) + (if (b > 0) (t[i + k] - x) / b * .siw_dv_bs(i + 1L, k - 1L, 0L, x, t) else 0)
}

#' Independent natural cubic spline lag basis over lags 1..L with intercept: cubic B-splines with the interior knots at the quantiles of 1..L and boundary knots 1 and L, restricted to the null space of the second derivatives at the two boundaries (natural constraint).
#' The result spans the same space as splines::ns(1:L, df, intercept = TRUE); an orthonormal basis of that space is not unique, so equality is tested through the projector (lag effects and their covariance do not depend on the choice).
#' @return list(B = L x df basis, N = unconstrained B-spline design, knots = full knot vector, D2 = boundary second-derivative rows)
#' @export
siw_dlm_indep_basis <- function(L, df = 3L) {
  x <- seq_len(L); nik <- df - 2L; inner <- if (nik > 0L) vapply(seq_len(nik) / (nik + 1L), function(p) .siw_dv_q7(x, p), 0) else numeric(0)
  t <- c(rep(1, 4L), inner, rep(L, 4L)); K <- length(t) - 4L
  N <- vapply(seq_len(K), function(i) .siw_dv_bs(i, 4L, 0L, x, t), numeric(L)); D2 <- vapply(seq_len(K), function(i) .siw_dv_bs(i, 4L, 2L, c(1, L), t), numeric(2L))
  Z <- svd(D2, nu = 0L, nv = K)$v[, 3:K, drop = FALSE]; B <- N %*% Z; colnames(B) <- paste0("indep_lagbasis", seq_len(ncol(B)))
  list(B = B, N = N, knots = t, D2 = D2, Z = Z)
}

#' Verify the lag basis of the package against the independent construction (and splines::splineDesign, and dlnm where installed).
#' @return data frame of checks: check, value, criterion, status (NOT_TESTED where a comparator is unavailable)
#' @export
siw_dlm_verify_basis <- function(L = 56L, df = 3L) {
  ib <- siw_dlm_indep_basis(L, df); Bp <- siw_lag_basis(L, df); x <- seq_len(L); row <- function(check, value, crit, ok) data.frame(check = check, value = value, criterion = crit, status = if (is.na(ok)) "NOT_TESTED" else if (ok) "PASS" else "FAIL", stringsAsFactors = FALSE)
  proj <- function(M) M %*% solve(crossprod(M), t(M)); dp <- max(abs(proj(ib$B) - proj(Bp)))
  sd <- tryCatch(splines::splineDesign(ib$knots, x, ord = 4L, outer.ok = TRUE), error = function(e) NULL); d_sd <- if (is.null(sd)) NA_real_ else max(abs(unname(sd) - unname(ib$N)))
  C <- solve(crossprod(ib$N), crossprod(ib$N, Bp)); inspan <- max(abs(ib$N %*% C - Bp)); d2 <- max(abs(ib$D2 %*% C)); one <- max(abs(stats::lm.fit(Bp, rep(1, L))$residuals))
  dl <- if (requireNamespace("dlnm", quietly = TRUE)) max(abs(unname(as.matrix(dlnm::onebasis(x, fun = "ns", df = df, intercept = TRUE))) - unname(Bp))) else NA_real_
  rbind(row("own_bspline_values_vs_splines_splineDesign_max_abs", d_sd, "<= 1e-12", if (is.na(d_sd)) NA else d_sd <= 1e-12),
        row("package_basis_inside_independent_bspline_space_max_abs", inspan, "<= 1e-10", inspan <= 1e-10),
        row("natural_constraint_second_derivative_at_boundaries_max_abs", d2, "<= 1e-10", d2 <= 1e-10),
        row("independent_vs_package_basis_projector_max_abs", dp, "<= 1e-12", dp <= 1e-12),
        row("constant_function_in_span_max_abs_residual (intercept = TRUE)", one, "<= 1e-12", one <= 1e-12),
        row("package_basis_vs_dlnm_onebasis_max_abs", dl, "<= 1e-12 (NOT_TESTED if dlnm is not installed)", if (is.na(dl)) NA else dl <= 1e-12))
}

#' Independent fit of one final DLM: own basis, own least squares (HC3) or Newton logistic regression (inverse information), own cumulative sums and bands. Shares only the covariate matrix with the package.
#' @return list with n, status, global test, lag curve data frame, cumulative curve data frame, summary values and the lag-basis covariance (for the critical value)
#' @export
siw_dlm_indep_fit <- function(dat, config, outcome, metric, lag_end = 56L, lag_df = 3L, tier = config$primary_tier, x_cov = NULL) {
  o <- config$outcomes[config$outcomes$outcome == outcome, ]; sp <- config$metrics[config$metrics$metric == metric, ]; fam <- o$family
  Xc <- x_cov %||% siw_tier_matrix(dat, config$tiers[[tier]]); y <- as.numeric(dat[[outcome]]); H <- as.matrix(dat[, siw_daily_names(sp$daily_prefix, lag_end), drop = FALSE]); storage.mode(H) <- "double"
  keep <- which(stats::complete.cases(Xc) & is.finite(y) & apply(H, 1L, function(r) all(is.finite(r))))
  Xs <- Xc[keep, , drop = FALSE]; Xs <- Xs[, !(grepl("^Locality_3group", colnames(Xs)) & apply(Xs, 2L, function(v) all(v == 0))), drop = FALSE]
  ib <- siw_dlm_indep_basis(lag_end, lag_df); B <- ib$B; Hk <- H[keep, , drop = FALSE]; CB <- Hk %*% B; ys <- y[keep]; X <- cbind(Xs, CB); n <- length(ys); p <- ncol(X); pc <- ncol(Xs); bi <- (pc + 1L):p
  if (n <= p + 1L || qr(X)$rank < p) return(list(status = "NOT_ESTIMABLE_RANK", n = n))
  if (fam == "gaussian") {
    XtXi <- chol2inv(chol(crossprod(X))); b <- drop(XtXi %*% crossprod(X, ys)); e <- ys - drop(X %*% b); h <- rowSums((X %*% XtXi) * X)
    V <- XtXi %*% crossprod(X * (e / (1 - h))) %*% XtXi
    XtXi0 <- chol2inv(chol(crossprod(Xs))); e0 <- ys - drop(Xs %*% (XtXi0 %*% crossprod(Xs, ys))); rss1 <- sum(e^2); rss0 <- sum(e0^2); dfa <- p - pc
    gstat <- ((rss0 - rss1) / dfa) / (rss1 / (n - p)); gp <- stats::pf(gstat, dfa, n - p, lower.tail = FALSE)
  } else {
    irls <- function(M) { mu <- (ys + 0.5) / 2; eta <- log(mu / (1 - mu)); b <- rep(0, ncol(M))
      for (it in 1:200) { w <- mu * (1 - mu); z <- eta + (ys - mu) / w; bn <- drop(solve(crossprod(M * w, M), crossprod(M * w, z))); eta <- drop(M %*% bn); mu <- 1 / (1 + exp(-eta)); conv <- max(abs(bn - b)) < 1e-13; b <- bn; if (conv) break }
      list(b = b, dev = -2 * sum(ys * log(mu) + (1 - ys) * log(1 - mu)), w = mu * (1 - mu)) }
    f1 <- irls(X); f0 <- irls(Xs); b <- f1$b; V <- solve(crossprod(X * f1$w, X)); dfa <- p - pc; gstat <- f0$dev - f1$dev; gp <- stats::pchisq(gstat, dfa, lower.tail = FALSE)
  }
  th <- b[bi]; Vt <- V[bi, bi, drop = FALSE]; le <- drop(B %*% th); C <- B %*% Vt %*% t(B); lse <- sqrt(diag(C)); L <- lag_end
  ce <- vapply(seq_len(L), function(k) sum(le[seq_len(k)]), 0); cse <- vapply(seq_len(L), function(k) sqrt(sum(C[seq_len(k), seq_len(k)])), 0)
  pk <- which.max(abs(le)); srt <- sort(abs(le), decreasing = TRUE)
  list(status = "COMPLETED", n = n, family = fam, global_statistic = gstat, global_p = gp, global_df1 = dfa, theta = th, Vtheta = Vt, B = B, lag = data.frame(lag = seq_len(L), estimate = le, standard_error = lse, lower_95 = le - 1.96 * lse, upper_95 = le + 1.96 * lse),
       cumulative = data.frame(lag = seq_len(L), cumulative_effect = ce, cumulative_se = cse, cumulative_lower_95 = ce - 1.96 * cse, cumulative_upper_95 = ce + 1.96 * cse),
       peak_lag = pk, peak_effect = le[pk], peak_margin = if (length(srt) > 1L) srt[2L] / srt[1L] else NA_real_, n_pointwise_excl_null = sum(le - 1.96 * lse > 0 | le + 1.96 * lse < 0))
}

# MASS::mvrnorm rewritten (same algorithm, same random-number consumption): eigen decomposition of Sigma, standard normal matrix of p * n values filled column-wise.
.siw_dv_mvn <- function(n, Sigma) { p <- ncol(Sigma); X <- matrix(stats::rnorm(p * n), n); eS <- eigen(Sigma, symmetric = TRUE); t(eS$vectors %*% diag(sqrt(pmax(eS$values, 0)), p) %*% t(X)) }

#' Independent simultaneous critical value: type-8 0.95 quantile of max_l |(B z)_l / SE_l| over multivariate-normal draws z with covariance Vtheta (own mvrnorm clone, same seed handling as the package).
#' @export
siw_dlm_indep_critical <- function(Vtheta, B, lse, draws, seed, level = 0.95) {
  siw_with_seed(seed, { Z <- .siw_dv_mvn(draws, Vtheta); mt <- apply(abs(Z %*% t(B)) / rep(lse, each = draws), 1L, max); siw_quantile8(mt, level) })
}

#' Monte Carlo stability of the simultaneous critical value: alternative seeds at the declared draw count and one large-draw value (independent covariance).
#' @return list(by_seed = data frame, seeded, large, spread, relative_deviation, exclusion_counts)
#' @export
siw_dlm_mc_stability <- function(fit, seed, draws, n_seeds = 20L, large = 100000L, seed_step = 1000L) {
  Vt <- fit$Vtheta; B <- fit$B; lse <- fit$lag$standard_error; le <- fit$lag$estimate; cnt <- function(cv) sum(le - cv * lse > 0 | le + cv * lse < 0)
  seeded <- siw_dlm_indep_critical(Vt, B, lse, draws, seed); alt_seeds <- seed + seed_step * seq_len(n_seeds); alt <- vapply(alt_seeds, function(s) siw_dlm_indep_critical(Vt, B, lse, draws, s), 0)
  big <- siw_dlm_indep_critical(Vt, B, lse, large, seed); allc <- c(seeded, alt, big)
  list(by_seed = data.frame(seed = c(seed, alt_seeds, seed), draws = c(rep(draws, n_seeds + 1L), large), critical = allc, lags_excluding_zero = vapply(allc, cnt, 0), stringsAsFactors = FALSE), seeded = seeded, large = big, spread = diff(range(c(seeded, alt))),
       relative_deviation = abs(seeded - big) / big, exclusion_counts = unique(vapply(allc, cnt, 0)))
}

.siw_dv_cmp <- function(a, b, abs_tol = 1e-8, rel_tol = 1e-7) { a <- as.numeric(a); b <- as.numeric(b); d <- abs(a - b); both_na <- is.na(a) & is.na(b); ok <- both_na | (is.finite(d) & d <= abs_tol + rel_tol * abs(b))
  c(max_abs = if (all(both_na)) 0 else suppressWarnings(max(d[!both_na], na.rm = FALSE)), max_rel = if (all(both_na)) 0 else suppressWarnings(max((d / pmax(abs(b), 1e-300))[!both_na & is.finite(d)], na.rm = TRUE)), ok = all(ok), n = length(a)) }

#' Verify one package DLM fit (siw_fit_dlm output with curves, accepted glm epsilon) against the independent route.
#' Binary models are additionally checked against the package refitted at glm epsilon 1e-14 (verification only; never a result): at the accepted epsilon the model-based covariance is converged to about 1e-7 relative, which can also flip an eigenvector sign of the MVN draws.
#' @param pkg output of siw_fit_dlm(..., return_curves = TRUE) at the final draw count; pkg10k_summary its summary at the audited draw count (NULL = not available)
#' @return list(checks, critical, peak, bands, mc)
#' @export
siw_dlm_verify_model <- function(dat, config, pkg, pkg10k_summary = NULL, outcome, metric, protocol = siw_dlm_validation_protocol(), mc = TRUE, x_cov = NULL) {
  s <- pkg$summary; id <- s$result_id; f <- siw_dlm_indep_fit(dat, config, outcome, metric, s$lag_end, s$lag_df, s$model_tier, x_cov = x_cov); ci <- protocol$criteria$independent_route; st <- protocol$criteria$simultaneous_mc_stability; binom <- identical(s$outcome_family, "binomial")
  fail1 <- function(g, q) data.frame(result_id = id, group = g, quantity = q, n_compared = 0L, max_abs_diff = NA_real_, max_rel_diff = NA_real_, criterion = "both routes estimable", classification = NA_character_, status = "FAIL", stringsAsFactors = FALSE)
  if (!identical(f$status, "COMPLETED") || !identical(s$status, "COMPLETED")) return(list(checks = fail1("CURVE_VERIFICATION", "estimability"), critical = NULL, peak = NULL, bands = NULL, mc = NULL))
  lc <- pkg$lag_curve; cc <- pkg$cumulative_curve; chk <- list(); L <- s$lag_end
  tolA <- if (binom) c(ci$binomial_fitted_quantities_abs_accepted_epsilon, ci$binomial_fitted_quantities_rel_accepted_epsilon) else c(ci$fitted_quantities_abs, ci$fitted_quantities_rel); tolT <- c(ci$fitted_quantities_abs, ci$fitted_quantities_rel)
  add <- function(group, quantity, a, b, tol = tolA, classification = NA_character_) { r <- .siw_dv_cmp(a, b, tol[1L], tol[2L])
    chk[[length(chk) + 1L]] <<- data.frame(result_id = id, group = group, quantity = quantity, n_compared = as.integer(r[["n"]]), max_abs_diff = r[["max_abs"]], max_rel_diff = r[["max_rel"]], criterion = sprintf("abs <= %g + %g x |reference| per value", tol[1L], tol[2L]), classification = classification, status = if (isTRUE(r[["ok"]] == 1)) "PASS" else "FAIL", stringsAsFactors = FALSE) }
  add("CURVE_VERIFICATION", "sample_size_n", s$n, f$n, c(0, 0)); add("CURVE_VERIFICATION", "global_statistic", s$global_statistic, f$global_statistic); add("CURVE_VERIFICATION", "global_p", s$global_p, f$global_p)
  add("CURVE_VERIFICATION", "lag_effects", lc$estimate, f$lag$estimate); add("CURVE_VERIFICATION", "lag_standard_errors", lc$standard_error, f$lag$standard_error)
  add("CURVE_VERIFICATION", "pointwise_lower_95", lc$lower_95, f$lag$lower_95); add("CURVE_VERIFICATION", "pointwise_upper_95", lc$upper_95, f$lag$upper_95); add("CURVE_VERIFICATION", "n_lags_pointwise_excluding_null", s$n_lags_pointwise_excl_null, f$n_pointwise_excl_null, c(0, 0))
  add("CUMULATIVE_VERIFICATION", "cumulative_effect_curve", cc$cumulative_effect, f$cumulative$cumulative_effect); add("CUMULATIVE_VERIFICATION", "cumulative_se_curve", cc$cumulative_se, f$cumulative$cumulative_se)
  add("CUMULATIVE_VERIFICATION", "cumulative_lower_95_curve", cc$cumulative_lower_95, f$cumulative$cumulative_lower_95); add("CUMULATIVE_VERIFICATION", "cumulative_upper_95_curve", cc$cumulative_upper_95, f$cumulative$cumulative_upper_95)
  add("CUMULATIVE_VERIFICATION", "cumulative_effect_at_L56 (summary)", s$cumulative_effect, f$cumulative$cumulative_effect[L]); add("CUMULATIVE_VERIFICATION", "cumulative_se_at_L56 (summary)", s$cumulative_se, f$cumulative$cumulative_se[L])
  add("CUMULATIVE_VERIFICATION", "cumulative_ci_at_L56 (summary)", c(s$cumulative_ci_lower, s$cumulative_ci_upper), c(f$cumulative$cumulative_lower_95[L], f$cumulative$cumulative_upper_95[L]))
  add("CUMULATIVE_VERIFICATION", "cumulative_effect_equals_sum_of_lag_effects", s$cumulative_effect, sum(lc$estimate), c(1e-12, 1e-12))
  add("PEAK_LAG_VERIFICATION", "peak_lag", s$peak_lag, f$peak_lag, c(0, 0)); add("PEAK_LAG_VERIFICATION", "peak_effect", s$peak_effect, f$peak_effect)
  # the simultaneous-band formula with the package critical value (isolates the band construction from the Monte Carlo draws)
  cp <- s$simultaneous_critical; blo <- f$lag$estimate - cp * f$lag$standard_error; bhi <- f$lag$estimate + cp * f$lag$standard_error
  add("SIMULTANEOUS_EXACT_SEEDED", "simultaneous_lower_95 (formula with the package critical value)", lc$simultaneous_lower_95, blo); add("SIMULTANEOUS_EXACT_SEEDED", "simultaneous_upper_95 (formula with the package critical value)", lc$simultaneous_upper_95, bhi)
  add("SIMULTANEOUS_EXACT_SEEDED", "n_lags_simultaneous_excluding_null (independent estimates, package critical value)", s$n_lags_simultaneous_excl_null, sum(blo > 0 | bhi < 0), c(0, 0))
  cnt <- function(cv) sum(f$lag$estimate - cv * f$lag$standard_error > 0 | f$lag$estimate + cv * f$lag$standard_error < 0)
  # critical values: exact seeded reproduction (c) and cross-implementation classification (iv)
  crit_i <- siw_dlm_indep_critical(f$Vtheta, f$B, f$lag$standard_error, s$simultaneous_draws, s$seed); cls_of <- function(cp_, ci_) { d <- abs(cp_ - ci_); if (d <= ci$critical_seeded_abs) "EXACT" else if (d / cp_ <= st$max_relative_difference_cross_implementation_not_exact && cnt(cp_) == cnt(ci_)) "MC_CONSISTENT_NOT_EXACT" else "FAIL" }
  cls <- cls_of(cp, crit_i); chk[[length(chk) + 1L]] <- data.frame(result_id = id, group = "SIMULTANEOUS_MC_STABILITY", quantity = sprintf("critical_value_%d_draws_seed_%d: package (accepted epsilon) vs independent route", s$simultaneous_draws, s$seed), n_compared = 1L, max_abs_diff = abs(cp - crit_i), max_rel_diff = abs(cp - crit_i) / cp,
    criterion = sprintf("EXACT (<= %g) or relative difference <= %g with identical exclusion counts", ci$critical_seeded_abs, st$max_relative_difference_cross_implementation_not_exact), classification = cls, status = if (cls == "FAIL") "FAIL" else "PASS", stringsAsFactors = FALSE)
  crit_tight <- NA_real_
  if (binom) {
    cfg_t <- config; cfg_t$dlm$final$glm_epsilon <- ci$tight_convergence_check$package_refit_epsilon
    pt <- siw_fit_dlm(dat, cfg_t, outcome, metric, s$lag_end, s$lag_df, s$model_tier, draws = s$simultaneous_draws, seed = s$seed, x_cov = x_cov); stt <- pt$summary
    if (identical(stt$status, "COMPLETED")) { crit_tight <- stt$simultaneous_critical
      add("CONVERGENCE_SENSITIVITY", "lag_effects: package at epsilon 1e-14 vs independent", pt$lag_curve$estimate, f$lag$estimate, tolT); add("CONVERGENCE_SENSITIVITY", "lag_standard_errors: package at epsilon 1e-14 vs independent", pt$lag_curve$standard_error, f$lag$standard_error, tolT)
      add("CONVERGENCE_SENSITIVITY", "cumulative_se_curve: package at epsilon 1e-14 vs independent", pt$cumulative_curve$cumulative_se, f$cumulative$cumulative_se, tolT); add("CONVERGENCE_SENSITIVITY", "global_statistic: package at epsilon 1e-14 vs independent", stt$global_statistic, f$global_statistic, tolT)
      add("SIMULTANEOUS_EXACT_SEEDED", sprintf("critical_value_%d_draws_seed_%d: package at epsilon 1e-14 vs independent (exact seeded)", s$simultaneous_draws, s$seed), crit_tight, crit_i, c(ci$critical_seeded_abs, 0))
    } else chk[[length(chk) + 1L]] <- fail1("CONVERGENCE_SENSITIVITY", "package refit at epsilon 1e-14 estimable")
  } else add("SIMULTANEOUS_EXACT_SEEDED", sprintf("critical_value_%d_draws_seed_%d: package vs independent (exact seeded)", s$simultaneous_draws, s$seed), cp, crit_i, c(ci$critical_seeded_abs, 0))
  crit10_i <- NA_real_; cls10 <- NA_character_
  if (!is.null(pkg10k_summary)) { crit10_i <- siw_dlm_indep_critical(f$Vtheta, f$B, f$lag$standard_error, pkg10k_summary$simultaneous_draws, pkg10k_summary$seed); cls10 <- cls_of(pkg10k_summary$simultaneous_critical, crit10_i)
    chk[[length(chk) + 1L]] <- data.frame(result_id = id, group = "SIMULTANEOUS_MC_STABILITY", quantity = sprintf("critical_value_%d_draws_seed_%d (audited draw count): package vs independent route", pkg10k_summary$simultaneous_draws, pkg10k_summary$seed), n_compared = 1L, max_abs_diff = abs(pkg10k_summary$simultaneous_critical - crit10_i), max_rel_diff = abs(pkg10k_summary$simultaneous_critical - crit10_i) / pkg10k_summary$simultaneous_critical,
      criterion = "EXACT or MC_CONSISTENT_NOT_EXACT as above", classification = cls10, status = if (cls10 == "FAIL") "FAIL" else "PASS", stringsAsFactors = FALSE) }
  checks <- do.call(rbind, chk); mc_row <- NULL
  if (isTRUE(mc)) { mcr <- siw_dlm_mc_stability(f, s$seed, s$simultaneous_draws, st$alternative_seeds, st$large_draw_count, 1000L); alt <- mcr$by_seed$critical[2:(st$alternative_seeds + 1L)]
    cnts <- unique(c(mcr$exclusion_counts, cnt(cp), cnt(crit_i))); ok_dev <- mcr$relative_deviation <= st$max_relative_deviation_of_seeded_from_large; ok_spread <- mcr$spread <= st$max_spread_across_alternative_seeds_absolute; ok_cnt <- length(cnts) == 1L
    mc_row <- data.frame(result_id = id, family = s$outcome_family, draws = s$simultaneous_draws, seed = s$seed, critical_package = cp, critical_independent_same_seed = crit_i, critical_large_draws = mcr$large, large_draws = st$large_draw_count, relative_deviation_package_vs_large = abs(cp - mcr$large) / mcr$large, relative_deviation_independent_vs_large = mcr$relative_deviation,
      alternative_seeds = st$alternative_seeds, alternative_min = min(alt), alternative_max = max(alt), spread_across_seeds = mcr$spread, lags_excluding_zero_all_critical_values = paste(sort(cnts), collapse = "/"),
      pass_relative_deviation = ok_dev && abs(cp - mcr$large) / mcr$large <= st$max_relative_deviation_of_seeded_from_large, pass_spread = ok_spread, pass_conclusion_stability = ok_cnt, stringsAsFactors = FALSE)
    mc_row$status <- if (mc_row$pass_relative_deviation && ok_spread && ok_cnt) "PASS" else "FAIL" }
  peak <- data.frame(result_id = id, peak_lag_package = s$peak_lag, peak_lag_independent = f$peak_lag, peak_effect_package = s$peak_effect, peak_effect_independent = f$peak_effect, peak_at_boundary_L56 = s$peak_lag == s$lag_end, second_to_first_abs_ratio = f$peak_margin, near_tie = is.finite(f$peak_margin) && f$peak_margin >= 1 - 1e-6, stringsAsFactors = FALSE)
  crit <- data.frame(result_id = id, family = s$outcome_family, final_draws = s$simultaneous_draws, final_seed = s$seed, critical_package = cp, critical_independent = crit_i, abs_diff = abs(cp - crit_i), cross_implementation_class = cls, critical_package_at_epsilon_1e14 = crit_tight, independent_minus_package_at_epsilon_1e14 = crit_i - crit_tight,
    audited_draws = pkg10k_summary$simultaneous_draws %||% NA_integer_, audited_critical_package = pkg10k_summary$simultaneous_critical %||% NA_real_, audited_critical_independent = crit10_i, audited_cross_implementation_class = cls10, stringsAsFactors = FALSE)
  bands <- data.frame(result_id = id, n_lags = L, pointwise_excluding_null = s$n_lags_pointwise_excl_null, simultaneous_excluding_null = s$n_lags_simultaneous_excl_null, any_simultaneous_excludes_null = isTRUE(s$any_simultaneous_band_excludes_null), simultaneous_to_pointwise_width_ratio = cp / 1.96, stringsAsFactors = FALSE)
  list(checks = checks, critical = crit, peak = peak, bands = bands, mc = mc_row)
}
