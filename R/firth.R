# Firth (Jeffreys-prior penalised) logistic regression: the documented sensitivity for the binary outcome (SI Table S6: l_F(b) = l(b) + 0.5 log|I(b)|).
# Estimator: the maximiser of the penalised log-likelihood, found by Fisher scoring on the Jeffreys-adjusted score
#   U*(b) = X' (y - p + h (0.5 - p)),  h_i = w_i x_i' I(b)^-1 x_i,  w_i = p_i (1 - p_i),  I(b) = X' W X.
# Inference as documented for the original analysis: SE = sqrt(diag(I(b_F)^-1)) (expected information at the penalised estimate),
# interval = b +/- 1.96 SE ("Wald from penalised expected information"). No p-value and no multiplicity family is defined for this sensitivity.
#
# Difference from the original implementation (documented, see PHASE3B report): the original iteration declares convergence when the
# parameter change is <= 1e-8 *after step-halving*, and the halving test compares a penalised log-likelihood that carries floating-point
# noise, so the loop can stop with an adjusted score of up to about 1e-4. Here convergence is declared only when the Newton step in the
# metric of the information matrix and the adjusted score both fall below a stated tolerance at the *returned* estimate, and the
# score at the returned estimate is reported. Nothing in this file copies the original stopping rule.

#' Penalised log-likelihood l(b) + 0.5 log|I(b)| (numerically stable; no probability clipping).
#' @export
siw_firth_penalised_loglik <- function(X, y, beta) {
  eta <- drop(X %*% beta); p <- stats::plogis(eta); w <- p * (1 - p)
  ll <- sum(y * eta - (pmax(eta, 0) + log1p(exp(-abs(eta)))))
  d <- determinant(crossprod(X, X * w), logarithm = TRUE)
  if (d$sign <= 0 || !is.finite(d$modulus)) return(-Inf)
  ll + 0.5 * as.numeric(d$modulus)
}

#' Jeffreys-adjusted score U*(b) = X'(y - p + h(0.5 - p)); also returns the information inverse and the hat values.
#' @export
siw_firth_adjusted_score <- function(X, y, beta) {
  eta <- drop(X %*% beta); p <- stats::plogis(eta); w <- p * (1 - p)
  info <- crossprod(X, X * w); inv <- tryCatch(solve(info), error = function(e) NULL)
  if (is.null(inv) || any(!is.finite(inv))) return(list(score = rep(NA_real_, ncol(X)), info = info, inverse = NULL, hat = rep(NA_real_, nrow(X)), p = p))
  h <- rowSums((X %*% inv) * X) * w
  list(score = drop(crossprod(X, y - p + h * (0.5 - p))), info = info, inverse = inv, hat = h, p = p)
}

#' Fit the Firth logistic model to a design matrix (intercept included) and a 0/1 outcome.
#' @param tolerance convergence threshold for both the scaled score sqrt(U' I^-1 U) and the largest step, evaluated at the returned estimate.
#' @return list(coefficients, standard_error, covariance, converged, iterations, status, adjusted_score_max_abs, scaled_score_norm, penalised_loglik,
#'   step_halvings, max_abs_linear_predictor, interval_method)
#' @export
siw_firth_fit <- function(X, y, maximum_iterations = 200L, tolerance = 1e-10) {
  p <- ncol(X); nm <- colnames(X); halvings <- 0L; status <- "NOT_CONVERGED"; iters <- 0L
  # The penalised maximum is equivariant under an affine re-parameterisation of the columns (|I| changes by a constant), so the iteration runs on
  # centred and scaled columns (intercept kept) to avoid ill-conditioning from columns on very different scales; results are mapped back.
  ic <- match("(Intercept)", nm); mu <- rep(0, p); sdv <- rep(1, p)
  for (j in setdiff(seq_len(p), ic)) { mu[j] <- mean(X[, j]); s <- stats::sd(X[, j]); sdv[j] <- if (is.finite(s) && s > 0) s else 1 }
  Z <- X; for (j in setdiff(seq_len(p), ic)) Z[, j] <- (X[, j] - mu[j]) / sdv[j]
  to_beta <- function(g) { b <- g / sdv; if (!is.na(ic)) b[ic] <- g[ic] - sum(g[-ic] * mu[-ic] / sdv[-ic]); b }
  g <- rep(0, p); cur <- siw_firth_penalised_loglik(Z, y, g)
  fail <- function(st) list(coefficients = stats::setNames(rep(NA_real_, p), nm), standard_error = stats::setNames(rep(NA_real_, p), nm), covariance = matrix(NA_real_, p, p),
    converged = FALSE, iterations = iters, status = st, adjusted_score_max_abs = NA_real_, scaled_score_norm = NA_real_, penalised_loglik = NA_real_, step_halvings = halvings,
    max_abs_linear_predictor = NA_real_, interval_method = "Wald_from_penalised_expected_information")
  for (iters in seq_len(maximum_iterations)) {
    a <- siw_firth_adjusted_score(Z, y, g)
    if (is.null(a$inverse)) return(fail("SINGULAR_INFORMATION"))
    step <- drop(a$inverse %*% a$score)
    if (any(!is.finite(step))) return(fail("NON_FINITE_STEP"))
    scaled <- sqrt(max(0, sum(a$score * step)))
    if (scaled <= tolerance && max(abs(step)) <= tolerance * (1 + max(abs(g)))) { status <- "CONVERGED"; iters <- iters - 1L; break }
    f <- 1; cand <- g + step; val <- siw_firth_penalised_loglik(Z, y, cand)
    # Step-halving protects the early iterations only. Close to the maximum (scaled step below 1e-6) the full Newton-type step is always taken,
    # because the penalised likelihood can no longer be evaluated more precisely than the gain a step provides.
    if (scaled > 1e-6) {
      noise <- 64 * .Machine$double.eps * (1 + abs(cur))
      while ((!is.finite(val) || val < cur - noise) && f > 2^-30) { f <- f / 2; halvings <- halvings + 1L; cand <- g + f * step; val <- siw_firth_penalised_loglik(Z, y, cand) }
    }
    if (!is.finite(val)) return(fail("NON_FINITE_PENALISED_LIKELIHOOD"))
    g <- cand; cur <- val
  }
  if (status != "CONVERGED") { a <- siw_firth_adjusted_score(Z, y, g); if (is.null(a$inverse)) return(fail("SINGULAR_INFORMATION")); st <- drop(a$inverse %*% a$score)
    if (sqrt(max(0, sum(a$score * st))) <= tolerance && max(abs(st)) <= tolerance * (1 + max(abs(g)))) status <- "CONVERGED" }
  beta <- stats::setNames(to_beta(g), nm)
  a <- siw_firth_adjusted_score(X, y, beta)                       # score, information and standard errors on the original scale at the returned estimate
  if (is.null(a$inverse)) return(fail("SINGULAR_INFORMATION"))
  cov <- a$inverse; se <- sqrt(pmax(0, diag(cov))); names(se) <- nm; dimnames(cov) <- list(nm, nm)
  list(coefficients = beta, standard_error = se, covariance = cov, converged = identical(status, "CONVERGED"), iterations = iters, status = status,
       adjusted_score_max_abs = max(abs(a$score)), scaled_score_norm = sqrt(max(0, sum(a$score * drop(a$inverse %*% a$score)))), penalised_loglik = siw_firth_penalised_loglik(X, y, beta), step_halvings = halvings,
       max_abs_linear_predictor = max(abs(drop(X %*% beta))), interval_method = "Wald_from_penalised_expected_information")
}

#' Independent estimator of the same penalised maximum: iterated pseudo-data augmentation (Firth 1993; Heinze & Schemper 2002).
#' Each observation contributes (y_i, weight 1) plus (1, h_i/2) and (0, h_i/2); the weighted ordinary logistic fit is repeated with the hat values
#' h_i of the current fit until the coefficients stop changing. Uses stats::glm.fit, not the scoring recursion of siw_firth_fit.
#' @export
siw_firth_augmentation <- function(X, y, maximum_iterations = 500L, tolerance = 1e-12) {
  n <- nrow(X); beta <- rep(0, ncol(X)); ctl <- stats::glm.control(epsilon = 1e-14, maxit = 100L)
  for (it in seq_len(maximum_iterations)) {
    eta <- drop(X %*% beta); p <- stats::plogis(eta); w <- p * (1 - p)
    h <- rowSums((X %*% solve(crossprod(X, X * w))) * X) * w
    fit <- suppressWarnings(stats::glm.fit(rbind(X, X, X), c(y, rep(1, n), rep(0, n)), weights = c(rep(1, n), h / 2, h / 2), family = stats::binomial(), start = beta, control = ctl))
    nb <- unname(fit$coefficients)
    if (any(!is.finite(nb))) return(list(coefficients = rep(NA_real_, ncol(X)), converged = FALSE, iterations = it))
    if (max(abs(nb - beta)) <= tolerance) { beta <- nb; return(list(coefficients = beta, converged = TRUE, iterations = it)) }
    beta <- nb
  }
  list(coefficients = beta, converged = FALSE, iterations = maximum_iterations)
}

#' Centre and scale the non-intercept columns (the penalised maximum is equivariant under this affine map; used to condition both the fit and its verification).
#' @return list(Z, to_beta(gamma), to_gamma(beta))
# Richardson extrapolation settings for the finite-difference gradient of the penalised likelihood (the evaluation noise of the log-determinant makes small steps inaccurate).
.siw_fd_args <- list(d = 0.01, r = 6, v = 2)

.siw_firth_standardise <- function(X) {
  p <- ncol(X); ic <- match("(Intercept)", colnames(X)); mu <- rep(0, p); sdv <- rep(1, p)
  for (j in setdiff(seq_len(p), ic)) { mu[j] <- mean(X[, j]); s <- stats::sd(X[, j]); sdv[j] <- if (is.finite(s) && s > 0) s else 1 }
  Z <- X; for (j in setdiff(seq_len(p), ic)) Z[, j] <- (X[, j] - mu[j]) / sdv[j]
  list(Z = Z, to_beta = function(g) { b <- g / sdv; if (!is.na(ic)) b[ic] <- g[ic] - sum(g[-ic] * mu[-ic] / sdv[-ic]); b },
       to_gamma = function(b) { g <- b * sdv; if (!is.na(ic)) g[ic] <- b[ic] + sum(b[-ic] * mu[-ic]); g })
}

#' Mathematical verification of a Firth solution against independent calculations (numDeriv derivatives, augmentation estimator, hand-evaluated penalty).
#' All derivative checks run on the centred and scaled design (the maximum is equivariant), where finite differences are well conditioned.
#' Tolerances are declared here and never adjusted to a result: scaled score <= 1e-8; analytic vs numerical gradient <= 1e-6 (relative to max(1, |g|));
#' penalised Hessian negative definite; augmentation estimator agrees in coefficients <= 1e-7 and relative SE <= 1e-7; no higher value in a local random search.
#' @return one-row data frame of checks (differences and PASS/FAIL).
#' @export
siw_firth_verify <- function(X, y, fit = siw_firth_fit(X, y), tol = list(score = 1e-8, gradient = 1e-6, coefficient = 1e-7, se = 1e-7)) {
  if (!isTRUE(fit$converged)) return(data.frame(converged = FALSE, status = fit$status, result = "FAIL", stringsAsFactors = FALSE))
  st <- .siw_firth_standardise(X); Z <- st$Z; b <- unname(fit$coefficients); g0 <- st$to_gamma(b)
  pl <- function(gg) siw_firth_penalised_loglik(Z, y, gg)
  # (1) analytic adjusted score equals the numerical gradient of the penalised likelihood (Richardson extrapolation) at the solution and at a perturbed point
  g_num <- numDeriv::grad(pl, g0, method = "Richardson", method.args = .siw_fd_args); g_ana <- siw_firth_adjusted_score(Z, y, g0)$score
  g1 <- g0 + 0.05 * seq_along(g0) / length(g0); g_num1 <- numDeriv::grad(pl, g1, method = "Richardson", method.args = .siw_fd_args); g_ana1 <- siw_firth_adjusted_score(Z, y, g1)$score
  # (2) numerical Hessian of the penalised likelihood is negative definite at the solution
  Hn <- numDeriv::hessian(pl, g0); ev <- eigen((Hn + t(Hn)) / 2, symmetric = TRUE, only.values = TRUE)$values
  # (3) penalised likelihood evaluated by hand on the original design: binomial log-density plus half the log-determinant from the eigenvalues of the information
  eta <- drop(X %*% b); pp <- stats::plogis(eta); w <- pp * (1 - pp); hand <- sum(stats::dbinom(y, 1, pp, log = TRUE)) + 0.5 * sum(log(eigen(crossprod(X, X * w), symmetric = TRUE, only.values = TRUE)$values))
  # (4) the augmentation estimator (stats::glm.fit on pseudo-data) reaches the same maximum
  aug <- siw_firth_augmentation(Z, y); ba <- if (aug$converged) st$to_beta(aug$coefficients) else rep(NA_real_, length(b))
  se_aug <- if (aug$converged) { e <- drop(X %*% ba); pq <- stats::plogis(e); sqrt(diag(solve(crossprod(X, X * pq * (1 - pq))))) } else rep(NA_real_, length(b))
  # (5) no higher penalised likelihood in a local random search around the solution
  rs <- .siw_seeded(1L, vapply(1:20, function(i) pl(g0 + stats::rnorm(length(g0), 0, 1e-4)), 0))
  d <- function(x) if (all(is.finite(x))) max(abs(x)) else NA_real_
  out <- data.frame(converged = TRUE, status = fit$status, adjusted_score_max_abs = fit$adjusted_score_max_abs, scaled_score_norm = fit$scaled_score_norm,
    gradient_analytic_vs_numerical_at_solution = d((g_ana - g_num) / pmax(1, abs(g_ana))), gradient_analytic_vs_numerical_off_solution = d((g_ana1 - g_num1) / pmax(1, abs(g_ana1))),
    hessian_max_eigenvalue = max(ev), penalised_loglik_hand_difference = abs(hand - fit$penalised_loglik), augmentation_converged = aug$converged,
    coefficient_difference_vs_augmentation = d((g0 - aug$coefficients)), se_relative_difference_vs_augmentation = d((unname(fit$standard_error) - se_aug) / unname(fit$standard_error)),
    random_search_improvement = max(rs) - pl(g0), stringsAsFactors = FALSE)
  out$result <- if (out$scaled_score_norm <= tol$score && out$gradient_analytic_vs_numerical_at_solution <= tol$gradient && out$gradient_analytic_vs_numerical_off_solution <= tol$gradient &&
                   out$hessian_max_eigenvalue < 0 && isTRUE(out$augmentation_converged) && out$coefficient_difference_vs_augmentation <= tol$coefficient && out$se_relative_difference_vs_augmentation <= tol$se &&
                   out$random_search_improvement <= 1e-9 && out$penalised_loglik_hand_difference <= 1e-8) "PASS" else "FAIL"
  out
}

# Random numbers for the verification search, with the caller's RNG state restored (no dependency on other packages).
.siw_seeded <- function(seed, expr) siw_with_seed(seed, expr)

#' The Firth sensitivity grid: every binary-outcome cumulative specification (all seven tiers), 1 outcome x 8 metrics x 10 windows x 7 tiers = 560.
#' Same samples, design matrices and ordering as the cumulative grid. Failures, non-convergence and dropped terms are recorded per specification.
#' @param core_results results table of the cumulative grid (supplies the separation flag of the ordinary ML fit; NULL leaves it empty)
#' @param verify also run the independent mathematical verification of every converged fit (needs numDeriv)
#' @return list(results, verification)
#' @export
siw_firth_grid <- function(dat, config, core_results = NULL, verify = TRUE, tolerance = 1e-10) {
  tiers <- config$tier_order; tier_x <- lapply(stats::setNames(tiers, tiers), function(t) siw_tier_matrix(dat, config$tiers[[t]])); tier_ok <- lapply(tier_x, stats::complete.cases)
  can_verify <- isTRUE(verify) && requireNamespace("numDeriv", quietly = TRUE); rows <- list(); ver <- list(); k <- 0L
  for (oi in which(config$outcomes$family == "binomial")) {
    o <- config$outcomes[oi, ]; y <- as.numeric(dat[[o$outcome]]); y_ok <- is.finite(y)
    for (mi in seq_len(nrow(config$metrics))) {
      sp <- config$metrics[mi, ]
      for (w in config$windows) {
        src <- siw_cum_name(sp$cumulative_prefix, w); x <- as.numeric(dat[[src]]); x_ok <- is.finite(x)
        for (t in tiers) {
          Xc <- tier_x[[t]]; mask <- tier_ok[[t]] & y_ok & x_ok; Xs <- Xc[mask, , drop = FALSE]
          drop <- grepl("^Locality_3group", colnames(Xs)) & (colSums(Xs != 0) == 0L); dropped <- paste(colnames(Xs)[drop], collapse = "|"); Xs <- Xs[, !drop, drop = FALSE]
          X <- cbind(Xs[, 1L, drop = FALSE], exposure = x[mask], Xs[, -1L, drop = FALSE]); colnames(X)[1L] <- "(Intercept)"; yy <- y[mask]
          k <- k + 1L; rid <- paste("FIR", o$code, sp$code, paste0("W", w), t, sep = "__"); n <- nrow(X); p <- ncol(X)
          est <- .siw_estimable(X, n, 2L, yy, binary = TRUE)
          base <- data.frame(result_id = rid, outcome = o$outcome, outcome_family = o$family, metric = sp$metric, exposure = src, window_or_lag = as.integer(w), model_tier = t, n = n,
                             event_count = sum(yy == 1), non_event_count = sum(yy == 0), parameter_count = p, dropped_terms = dropped, stringsAsFactors = FALSE)
          sep <- if (!is.null(core_results)) { cid <- siw_cum_id(o$code, sp$code, w, t); core_results$separation_status[match(cid, core_results$result_id)] } else NA_character_
          if (!est$ok) {
            rows[[k]] <- cbind(base, data.frame(status = "NOT_ESTIMABLE", not_estimable_reason = est$reason, converged = NA, iterations = NA_integer_, step_halvings = NA_integer_, adjusted_score_max_abs = NA_real_, scaled_score_norm = NA_real_,
              penalised_loglik = NA_real_, max_abs_linear_predictor = NA_real_, separation_status = sep, coefficient = NA_real_, standard_error = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
              covariance_row_norm_exposure = NA_real_, estimator = "Jeffreys_adjusted_score_Firth_logistic", interval_method = "Wald_from_penalised_expected_information", stringsAsFactors = FALSE)); next
          }
          f <- siw_firth_fit(X, yy, tolerance = tolerance); b <- unname(f$coefficients[2L]); s <- unname(f$standard_error[2L])
          ok <- isTRUE(f$converged) && is.finite(b) && is.finite(s)
          rows[[k]] <- cbind(base, data.frame(status = if (ok) "COMPLETED" else "NOT_CONVERGED", not_estimable_reason = if (ok) "" else f$status, converged = isTRUE(f$converged), iterations = as.integer(f$iterations), step_halvings = as.integer(f$step_halvings),
            adjusted_score_max_abs = f$adjusted_score_max_abs, scaled_score_norm = f$scaled_score_norm, penalised_loglik = f$penalised_loglik, max_abs_linear_predictor = f$max_abs_linear_predictor, separation_status = sep,
            coefficient = if (ok) b else NA_real_, standard_error = if (ok) s else NA_real_, ci_lower = if (ok) b - 1.96 * s else NA_real_, ci_upper = if (ok) b + 1.96 * s else NA_real_,
            covariance_row_norm_exposure = if (ok) sqrt(sum(f$covariance[2L, ]^2)) else NA_real_, estimator = "Jeffreys_adjusted_score_Firth_logistic", interval_method = "Wald_from_penalised_expected_information", stringsAsFactors = FALSE))
          if (can_verify && ok) ver[[length(ver) + 1L]] <- cbind(data.frame(result_id = rid, stringsAsFactors = FALSE), siw_firth_verify(X, yy, f))
        }
      }
    }
  }
  list(results = do.call(rbind, rows), verification = if (length(ver)) siw_bind_rows(ver) else data.frame())
}
