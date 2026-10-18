# Batch B: proportional-odds (ordinal logistic) sensitivity for the original 1-4 integration score.
# Documented specification (SI Table S3, S5 C; original code R/090_sensitivity_models.R siecc_ordinal_integration_grid):
#   * outcome  = ordered(integration_score, levels 1:4); levels that are empty in the complete-case sample are dropped (droplevels), as in the original;
#   * model    = MASS::polr(outcome ~ exposure + M2-L covariates, method = "logistic", Hess = TRUE); M2-L only; 8 metrics x 10 windows = 80;
#   * exposure = cumulative field (raw), complete cases over outcome, exposure and the M2-L covariates;
#   * reported = exposure coefficient b and its SE = sqrt(diag(vcov(fit)))[exposure] (inverse numerical Hessian from optim, as polr returns it);
#                odds ratio per exposure SD = exp(b * sd(exposure in the complete-case sample)); Wald z p = 2 * pnorm(-|b / SE|);
#   * multiplicity: Benjamini-Hochberg over the 80 finite p-values: q80 of the ORDINAL family. It is a different family from the primary cumulative
#     q80 (8 metrics x 10 windows within one outcome) and is stored under its own name, q80_ordinal.
# Not changed from the original: the link (logistic), the covariates, reference categories (polr default: lowest category as the reference of the
# cumulative logits; factor reference levels of the covariates as in the validated tiers), the Wald testing procedure.
# Added (recorded, never used to alter a result): optimiser convergence code, warnings raised by polr, log-likelihood, categories used and dropped terms.

#' The ordinal outcome variable of the original analysis.
#' @export
siw_ordinal_outcome <- function(dat) ordered(dat$integration_score, levels = 1:4)

#' One proportional-odds fit: returns the exposure statistics and the fit diagnostics (failures are returned as statuses, never thrown).
#' @param d complete-case data frame holding `outcome`, `exposure` and the covariates
#' @export
siw_polr_one <- function(d, outcome, exposure, covariates) {
  formula <- stats::reformulate(c(exposure, covariates), outcome)
  warn <- character()
  fit <- tryCatch(withCallingHandlers(MASS::polr(formula, data = d, Hess = TRUE, method = "logistic"),
                                      warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }),
                  error = function(e) structure(list(message = conditionMessage(e)), class = "siw_fit_error"))
  if (inherits(fit, "siw_fit_error")) return(list(ok = FALSE, reason = paste0("POLR_ERROR: ", substr(fit$message, 1L, 80)), warnings = paste(unique(warn), collapse = " | ")))
  if (!(exposure %in% names(stats::coef(fit)))) return(list(ok = FALSE, reason = "EXPOSURE_COEFFICIENT_MISSING", warnings = paste(unique(warn), collapse = " | ")))
  V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(V) || !(exposure %in% rownames(V))) return(list(ok = FALSE, reason = "VCOV_UNAVAILABLE", warnings = paste(unique(warn), collapse = " | "), fit = fit))
  se <- sqrt(V[exposure, exposure])
  if (!is.finite(se) || se <= 0) return(list(ok = FALSE, reason = "NON_FINITE_STANDARD_ERROR", warnings = paste(unique(warn), collapse = " | "), fit = fit))
  list(ok = TRUE, fit = fit, coefficient = unname(stats::coef(fit)[[exposure]]), se = se, vcov = V, warnings = paste(unique(warn), collapse = " | "))
}

#' The 80 ordinal specifications (M2-L, 8 metrics x 10 windows) with their own BH family q80_ordinal.
#' @export
siw_ordinal_grid <- function(dat, config, tier = config$primary_tier) {
  dat$integration_ordinal <- siw_ordinal_outcome(dat); covs <- config$tiers[[tier]]; rows <- list()
  for (mi in seq_len(nrow(config$metrics))) {
    sp <- config$metrics[mi, ]
    for (w in config$windows) {
      ex <- siw_cum_name(sp$cumulative_prefix, w); vars <- unique(c("integration_ordinal", ex, covs))
      keep <- stats::complete.cases(dat[, vars, drop = FALSE]); d <- droplevels(dat[keep, vars, drop = FALSE])
      r <- siw_polr_one(d, "integration_ordinal", ex, covs)
      loc_all <- if ("Locality_3group" %in% covs) levels(dat$Locality_3group) else character(); loc_used <- if ("Locality_3group" %in% covs) levels(d$Locality_3group) else character()
      base <- data.frame(result_id = paste("ORD", "INT", sp$code, paste0("W", w), tier, sep = "__"), outcome = "integration_score", outcome_family = "ordinal", metric = sp$metric, exposure = ex, window_or_lag = as.integer(w),
                         model_tier = tier, n = nrow(d), categories_used = length(levels(d$integration_ordinal)), dropped_terms = paste(setdiff(loc_all, loc_used), collapse = "|"), stringsAsFactors = FALSE)
      if (isTRUE(r$ok)) {
        sdx <- stats::sd(d[[ex]]); b <- r$coefficient
        nd <- siw_polr_newton_decrement(r$fit, d, ex, covs)
        rows[[length(rows) + 1L]] <- cbind(base, data.frame(status = "COMPLETED", not_estimable_reason = "", convergence = identical(as.integer(r$fit$convergence), 0L), coefficient = b, se = r$se, odds_ratio = exp(b * sdx),
          p = 2 * stats::pnorm(abs(b / r$se), lower.tail = FALSE), log_likelihood = -r$fit$deviance / 2, parameter_count = length(r$fit$coefficients) + length(r$fit$zeta), exposure_sd = sdx, newton_decrement = nd, fit_warnings = r$warnings, stringsAsFactors = FALSE))
      } else {
        rows[[length(rows) + 1L]] <- cbind(base, data.frame(status = "NOT_ESTIMABLE", not_estimable_reason = r$reason, convergence = NA, coefficient = NA_real_, se = NA_real_, odds_ratio = NA_real_, p = NA_real_, log_likelihood = NA_real_,
          parameter_count = NA_integer_, exposure_sd = NA_real_, newton_decrement = NA_real_, fit_warnings = r$warnings, stringsAsFactors = FALSE))
      }
    }
  }
  out <- do.call(rbind, rows); out$q80_ordinal <- siw_bh(out$p); out$multiplicity_family_q80_ordinal <- ifelse(is.finite(out$q80_ordinal), "ORDINAL_INTEGRATION_q80_8_metrics_x_10_windows", "")
  out
}

#' Log-likelihood only (vectorised); -Inf when a cell probability is not positive.
.siw_po_loglik <- function(theta, y, X) {
  K <- max(y); p <- ncol(X); zeta <- theta[seq_len(K - 1L)]; eta <- drop(X %*% theta[(K - 1L) + seq_len(p)]); zl <- c(-Inf, zeta, Inf)
  pr <- stats::plogis(zl[y + 1L] - eta) - stats::plogis(zl[y] - eta); if (any(!is.finite(pr)) || any(pr <= 0)) -Inf else sum(log(pr))
}

#' Log-likelihood, analytic gradient and analytic Hessian of the proportional-odds model, P(Y <= k) = plogis(zeta_k - x'b), theta = (zeta_1..zeta_(K-1), b).
#' Written from the model definition (no MASS code); used by siw_polr_independent and checked against numerical derivatives in the tests.
#' @export
siw_po_derivatives <- function(theta, y, X) {
  K <- max(y); p <- ncol(X); nobs <- length(y); zeta <- theta[seq_len(K - 1L)]; beta <- theta[(K - 1L) + seq_len(p)]; eta <- drop(X %*% beta)
  zl <- c(-Inf, zeta, Inf); a <- zl[y + 1L] - eta; b <- zl[y] - eta
  Fa <- stats::plogis(a); Fb <- stats::plogis(b); fa <- ifelse(is.finite(a), Fa * (1 - Fa), 0); fb <- ifelse(is.finite(b), Fb * (1 - Fb), 0)
  f1a <- fa * (1 - 2 * Fa); f1b <- fb * (1 - 2 * Fb); pr <- Fa - Fb
  if (any(!is.finite(pr)) || any(pr <= 0)) return(list(loglik = -Inf, gradient = rep(NA_real_, length(theta)), hessian = matrix(NA_real_, length(theta), length(theta))))
  q <- K - 1L + p; G <- matrix(0, nobs, q); H <- matrix(0, q, q)
  for (i in seq_len(nobs)) {
    d1 <- numeric(q); d2 <- matrix(0, q, q); j <- y[i]; l <- y[i] - 1L
    if (j <= K - 1L) { d1[j] <- d1[j] + fa[i]; d2[j, j] <- d2[j, j] + f1a[i] }
    if (l >= 1L)     { d1[l] <- d1[l] - fb[i]; d2[l, l] <- d2[l, l] - f1b[i] }
    xi <- X[i, ]; ib <- (K - 1L) + seq_len(p)
    d1[ib] <- -(fa[i] - fb[i]) * xi
    cz <- numeric(K - 1L); if (j <= K - 1L) cz[j] <- cz[j] + f1a[i]; if (l >= 1L) cz[l] <- cz[l] - f1b[i]       # d2p / (dzeta dbeta) = -cz x'
    d2[seq_len(K - 1L), ib] <- -outer(cz, xi); d2[ib, seq_len(K - 1L)] <- t(d2[seq_len(K - 1L), ib, drop = FALSE]); d2[ib, ib] <- (f1a[i] - f1b[i]) * outer(xi, xi)
    G[i, ] <- d1 / pr[i]; H <- H + d2 / pr[i] - outer(d1, d1) / pr[i]^2
  }
  list(loglik = sum(log(pr)), gradient = colSums(G), hessian = H)
}

#' Independent maximum-likelihood fit of the proportional-odds model. Own likelihood with analytic derivatives (siw_po_derivatives); a robust first stage
#' (stats::nlminb on the (zeta_1, log increments, b) parameterisation) followed by Newton iterations with the exact Hessian on centred and scaled columns;
#' standard errors are the square roots of the diagonal of the inverse observed information. Does not call MASS::polr.
#' @param y integer category codes 1..K (all K present); X numeric matrix without an intercept
#' @return list(coefficients, zeta, log_likelihood, se, converged, gradient_max_abs)
#' @export
siw_polr_independent <- function(y, X) {
  K <- max(y); p <- ncol(X); nobs <- length(y); mu <- colMeans(X); sdv <- apply(X, 2L, stats::sd); sdv[!is.finite(sdv) | sdv <= 0] <- 1
  Z <- sweep(sweep(X, 2L, mu, "-"), 2L, sdv, "/")
  nll_par <- function(par) { zeta <- cumsum(c(par[1L], exp(par[seq_len(K - 2L) + 1L]))); v <- .siw_po_loglik(c(zeta, par[(K - 1L) + seq_len(p)]), y, Z); if (is.finite(v)) -v else 1e10 }
  marg <- stats::qlogis(pmin(pmax(cumsum(tabulate(y, K))[-K] / nobs, 1e-4), 1 - 1e-4))
  o <- stats::nlminb(c(marg[1L], log(pmax(diff(marg), 1e-3)), rep(0, p)), nll_par, control = list(iter.max = 500L, eval.max = 2000L, rel.tol = 1e-14, x.tol = 1e-14))
  theta <- c(cumsum(c(o$par[1L], exp(o$par[seq_len(K - 2L) + 1L]))), o$par[(K - 1L) + seq_len(p)])
  d <- siw_po_derivatives(theta, y, Z)
  for (it in 1:50) {
    if (!is.finite(d$loglik) || max(abs(d$gradient)) <= 1e-11) break
    st <- tryCatch(solve(-d$hessian, d$gradient), error = function(e) NULL); if (is.null(st) || any(!is.finite(st))) break
    f <- 1; repeat { cand <- theta + f * st; dc <- siw_po_derivatives(cand, y, Z); if (is.finite(dc$loglik) && dc$loglik >= d$loglik - 1e-13) break; f <- f / 2; if (f < 1e-10) break }
    if (f < 1e-10) break; theta <- cand; d <- dc
  }
  V <- tryCatch(solve(-d$hessian), error = function(e) matrix(NA_real_, length(theta), length(theta))); ib <- (K - 1L) + seq_len(p)
  gz <- theta[ib]; se_z <- sqrt(diag(V))[ib]
  list(coefficients = stats::setNames(gz / sdv, colnames(X)), zeta = theta[seq_len(K - 1L)] + sum(gz * mu / sdv), log_likelihood = d$loglik, se = stats::setNames(se_z / sdv, colnames(X)),
       converged = is.finite(d$loglik) && max(abs(d$gradient)) <= 1e-8 && all(is.finite(se_z)), gradient_max_abs = max(abs(d$gradient)))
}

#' Newton decrement sqrt(g' (-H)^-1 g) of the proportional-odds log-likelihood at a polr solution (affine invariant; 0 at the maximum).
#' Computed with the analytic derivatives of siw_po_derivatives. A recorded measure of how far polr's optimiser stopped from the maximum; never used to change a result.
#' @export
siw_polr_newton_decrement <- function(fit, d, exposure, covariates) {
  X <- stats::model.matrix(stats::reformulate(c(exposure, covariates)), d)[, -1L, drop = FALSE]
  if (!identical(colnames(X), names(fit$coefficients))) return(NA_real_)
  dv <- siw_po_derivatives(c(fit$zeta, fit$coefficients), as.integer(d[[as.character(fit$terms[[2L]])]]), X)
  tryCatch(sqrt(max(0, sum(dv$gradient * solve(-dv$hessian, dv$gradient)))), error = function(e) NA_real_)
}

#' Check each ordinal fit against the independent maximum-likelihood fit. Declared tolerances, tied to the accuracy of polr's own optimiser (optim BFGS, reltol 1e-8):
#' log-likelihood difference <= 1e-6, coefficient and SE differences <= 1e-4. They are not adjusted to any result.
#' Per-specification status:
#'   PASS                   polr and the independent fit agree within the tolerances;
#'   POLR_NOT_AT_MAXIMUM    they do not agree, the independent fit is converged (gradient <= 1e-8) and has the higher log-likelihood: MASS::polr stopped short of the maximum
#'                          (a documented limitation of its default optimiser tolerance; the polr result is reported as polr returns it, the independent values are listed);
#'   FAIL                   anything else (independent fit failed, or lower likelihood than polr, i.e. a discrepancy that cannot be attributed to polr's stopping rule).
#' @return data frame, one row per ordinal specification
#' @export
siw_ordinal_verify <- function(dat, config, grid, tier = config$primary_tier, tol = list(loglik = 1e-6, coefficient = 1e-4, se = 1e-4)) {
  dat$integration_ordinal <- siw_ordinal_outcome(dat); covs <- config$tiers[[tier]]; rows <- list()
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]; if (g$status != "COMPLETED") { rows[[i]] <- data.frame(result_id = g$result_id, status = "NOT_VERIFIED_NOT_ESTIMABLE", stringsAsFactors = FALSE); next }
    ex <- g$exposure; vars <- unique(c("integration_ordinal", ex, covs)); d <- droplevels(dat[stats::complete.cases(dat[, vars, drop = FALSE]), vars, drop = FALSE])
    X <- stats::model.matrix(stats::reformulate(c(ex, covs)), d)[, -1L, drop = FALSE]
    r <- tryCatch(siw_polr_independent(as.integer(d$integration_ordinal), X), error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$converged)) { rows[[i]] <- data.frame(result_id = g$result_id, status = "FAIL", reason = "independent fit did not converge", stringsAsFactors = FALSE); next }
    dl <- r$log_likelihood - g$log_likelihood; db <- abs(r$coefficients[[ex]] - g$coefficient); ds <- abs(r$se[[ex]] - g$se)
    agree <- abs(dl) <= tol$loglik && db <= tol$coefficient && ds <= tol$se
    st <- if (agree) "PASS" else if (dl > 0) "POLR_NOT_AT_MAXIMUM" else "FAIL"
    rows[[i]] <- data.frame(result_id = g$result_id, status = st, reason = if (st == "FAIL") "disagreement not attributable to polr stopping short of the maximum" else "", loglik_difference = abs(dl), coefficient_difference = db, se_difference = ds,
                            independent_loglik_minus_polr = dl, independent_coefficient = unname(r$coefficients[[ex]]), independent_se = unname(r$se[[ex]]), independent_gradient_max_abs = r$gradient_max_abs,
                            polr_newton_decrement = g$newton_decrement, stringsAsFactors = FALSE)
  }
  siw_bind_rows(rows)
}
