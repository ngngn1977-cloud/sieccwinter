# Cross-check of the analytic derivatives used by the ordinal refinement (siw_po_derivatives: Newton decrement, independent fit, information matrices)
# against INDEPENDENTLY calculated numerical derivatives of an independently written likelihood, on FULLY SYNTHETIC data only.
#
#  * likelihood: explicit loops and exp(), written separately from siw_po_derivatives and from .siw_po_loglik (no shared code);
#  * derivatives: numDeriv::grad / numDeriv::hessian (Richardson) of that likelihood, on the centred and scaled design of each of the 80 refined fits (well-conditioned; the Newton decrement is
#    affine invariant, so it is the same number in either parameterisation);
#  * MASS side: the log-likelihood reported by MASS::polr (-deviance/2) and the Hessian MASS computes (finite differences of MASS's own analytic gradient, in its log-spacing
#    parameterisation, mapped with the Jacobian MASS itself uses) are compared with the same quantities from the analytic code.
# Checks per fit: (1) analytic gradient = numeric gradient at the refined solution and at a perturbed point; (2) analytic Hessian = numeric Hessian at both; (3) Newton decrement from the analytic
# derivatives = Newton decrement from the numeric derivatives; (4) plain-loop log-likelihood = MASS log-likelihood = refined table log-likelihood (original parameterisation); (5) analytic observed
# information = MASS Hessian after MASS's Jacobian (all-parameter SE); (6) the numeric gradient of the plain-loop likelihood at the refined solution is ~0 (stationarity without the analytic code).
#   Rscript validation/ordinal_derivative_crosscheck_synthetic.R [--out DIR] [--seeds 1234,1,2]
args <- commandArgs(trailingOnly = TRUE); oi <- match("--out", args); out <- if (!is.na(oi)) args[oi + 1L] else NULL; si <- match("--seeds", args); seeds <- if (!is.na(si)) as.integer(strsplit(args[si + 1L], ",")[[1L]]) else c(1234L, 1L, 2L)
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); root <- normalizePath(file.path(dirname(script_arg[1L]), ".."), winslash = "/")
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root); stopifnot(requireNamespace("numDeriv", quietly = TRUE))
cfg <- siw_load_config(); plan <- siw_ordinal_refinement_plan()
ll_plain <- function(theta, y, X) { K <- max(y); zeta <- theta[seq_len(K - 1L)]; beta <- theta[K:length(theta)]; eta <- drop(X %*% beta); s <- 0
  for (i in seq_along(y)) { k <- y[i]; hi <- if (k == K) 1 else 1 / (1 + exp(-(zeta[k] - eta[i]))); lo <- if (k == 1L) 0 else 1 / (1 + exp(-(zeta[k - 1L] - eta[i]))); s <- s + log(hi - lo) }; s }
rel <- function(a, b) max(abs(a - b)) / max(1, max(abs(b)))
rows <- list()
for (sd in seeds) {
  raw <- siw_make_synthetic(n = 60L, seed = sd, config = cfg); dat <- siw_attach_cumulative(siw_prepare_data(raw, cfg), cfg)$data; G <- siw_ordinal_refined_grid(dat, cfg, plan); covs <- cfg$tiers[[cfg$primary_tier]]
  for (rid in G$results$result_id) {
    r <- G$fits[[rid]]; d <- G$data[[rid]]; ex <- G$results$exposure[G$results$result_id == rid]; X <- stats::model.matrix(stats::reformulate(c(ex, covs)), d)[, -1L, drop = FALSE]; y <- as.integer(d$integration_ordinal); q <- length(r$zeta)
    Z <- sweep(sweep(X, 2L, colMeans(X)), 2L, apply(X, 2L, stats::sd), "/"); colnames(Z) <- paste0("V", seq_len(ncol(Z))); dd <- data.frame(y = d$integration_ordinal, Z)
    fit <- MASS::polr(stats::reformulate(colnames(Z), "y"), dd, Hess = TRUE, method = "logistic", control = list(reltol = plan$protocol$optim_control$reltol, maxit = 5000L, ndeps = rep(plan$protocol$optim_control$ndeps, ncol(Z) + q)))
    th <- c(fit$zeta, fit$coefficients); th2 <- th + 0.02 * sin(seq_along(th)); f <- function(t) ll_plain(t, y, Z)           # parameter order of siw_po_derivatives: zeta, then coefficients
    ga <- siw_po_derivatives(th, y, Z); gb <- siw_po_derivatives(th2, y, Z)
    gn <- numDeriv::grad(f, th, method = "Richardson"); gn2 <- numDeriv::grad(f, th2, method = "Richardson"); Hn <- numDeriv::hessian(f, th2, method = "Richardson"); Hn0 <- numDeriv::hessian(f, th, method = "Richardson")
    nd_an <- sqrt(max(0, sum(ga$gradient * solve(-ga$hessian, ga$gradient)))); nd_num <- sqrt(max(0, sum(gn * solve(-Hn0, gn))))
    ll_mass <- -fit$deviance / 2; ll_plain_s <- f(th); ll_plain_orig <- ll_plain(c(r$zeta, r$coefficients), y, X)
    theta_log <- c(fit$zeta[1L], log(diff(fit$zeta))); Jac <- { k <- q; et <- exp(theta_log); m <- matrix(0, k, k); m[, 1L] <- 1; for (i in 2L:k) m[i:k, i] <- et[i]; m }       # the Jacobian MASS uses in vcov.polr
    A <- diag(ncol(Z) + q); A[ncol(Z) + seq_len(q), ncol(Z) + seq_len(q)] <- Jac; Vmass <- A %*% MASS::ginv(fit$Hessian) %*% t(A)                                      # covariance (gamma, zeta_s) as MASS reports it
    Van <- solve(-ga$hessian); ord <- c(q + seq_len(ncol(Z)), seq_len(q)); Van <- Van[ord, ord]                                                                          # analytic: (zeta, gamma) -> (gamma, zeta_s)
    rows[[length(rows) + 1L]] <- data.frame(seed = sd, result_id = rid, numeric_gradient_max_abs_at_solution = max(abs(gn)), analytic_gradient_max_abs_at_solution = max(abs(ga$gradient)),
      gradient_rel_diff_perturbed = rel(gb$gradient, gn2), hessian_rel_diff_perturbed = rel(gb$hessian, Hn), hessian_rel_diff_at_solution = rel(ga$hessian, Hn0),
      newton_decrement_analytic = nd_an, newton_decrement_numeric = nd_num, newton_decrement_abs_diff = abs(nd_an - nd_num), loglik_plain_minus_mass = ll_plain_s - ll_mass, loglik_plain_original_minus_refined_table = ll_plain_orig - r$loglik,
      se_rel_diff_mass_vs_analytic = max(abs(sqrt(diag(Vmass)) - sqrt(diag(Van))) / sqrt(diag(Van))))
  }
}
R <- do.call(rbind, rows); options(width = 200)
S <- data.frame(check = c("numeric gradient of the independent likelihood at the refined solution (max abs; stationarity)", "analytic gradient at the refined solution (max abs)", "analytic vs numeric gradient at a perturbed point (relative)", "analytic vs numeric Hessian at a perturbed point (relative)",
  "analytic vs numeric Hessian at the solution (relative)", "Newton decrement: analytic vs numeric (absolute)", "independent plain-loop log-likelihood vs MASS log-likelihood (absolute)", "plain-loop log-likelihood at the back-transformed solution vs refined table (absolute)",
  "MASS Hessian (own Jacobian) vs analytic information: all-parameter SE (relative)"),
  max_value = c(max(R$numeric_gradient_max_abs_at_solution), max(R$analytic_gradient_max_abs_at_solution), max(R$gradient_rel_diff_perturbed), max(R$hessian_rel_diff_perturbed), max(R$hessian_rel_diff_at_solution), max(R$newton_decrement_abs_diff),
                max(abs(R$loglik_plain_minus_mass)), max(abs(R$loglik_plain_original_minus_refined_table)), max(R$se_rel_diff_mass_vs_analytic)), stringsAsFactors = FALSE)
S$n_fits <- nrow(R); print(S, right = FALSE); cat("datasets:", paste(seeds, collapse = ","), "; fits:", nrow(R), "; non-finite entries:", sum(!is.finite(as.matrix(R[, -(1:2)]))), "\n")
if (!is.null(out)) { dir.create(out, recursive = TRUE, showWarnings = FALSE); siw_write_csv(R, file.path(out, "ordinal_derivative_crosscheck_per_fit.csv")); siw_write_csv(S, file.path(out, "ordinal_derivative_crosscheck_summary.csv")) }
