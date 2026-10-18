# Regression tests for the ordinal verification column-name collision (Phase 3B.4) and the independent derivative cross-check. Synthetic data only.
PLAN16 <- siw_ordinal_refinement_plan()
RG16 <- siw_ordinal_refined_grid(DAT, CFG, PLAN16)
RV16 <- siw_ordinal_refined_verify(RG16, CFG, PLAN16)
CRIT <- paste0("criterion_", c("optimiser_code_zero", "newton_decrement", "loglik_agreement", "thresholds_ordered", "information_matrix", "coefficients_thresholds_agree", "exposure_coefficient_agrees", "exposure_se_agrees", "independent_fit_converged", "all_estimates_finite"), "_pass")

test_that("every schema and the verification table have unique column names; measurement and logical criterion are distinct fields", {
  sch <- siw_output_columns(); for (nm in names(sch)) expect_false(anyDuplicated(sch[[nm]]) > 0)
  expect_false(anyDuplicated(names(RV16)) > 0); expect_true(all(CRIT %in% names(RV16))); expect_true("newton_decrement_value" %in% names(RV16)); expect_false("newton_decrement" %in% names(RV16))
  expect_true(is.numeric(RV16$newton_decrement_value)); expect_true(is.logical(RV16$criterion_newton_decrement_pass)); expect_true(is.logical(RV16$all_criteria_pass))
  expect_identical(names(siw_conform(RV16, siw_ordinal_refinement_output_columns()$ordinal_refined_verification)), siw_ordinal_refinement_output_columns()$ordinal_refined_verification)
  # every criterion is an explicit logical, never NA, and the status is exactly their conjunction
  for (cn in CRIT) { expect_true(is.logical(RV16[[cn]])); expect_false(anyNA(RV16[[cn]])) }
  expect_identical(RV16$all_criteria_pass, apply(as.matrix(RV16[, CRIT]), 1L, all)); expect_identical(RV16$status == "ACCEPTED", RV16$all_criteria_pass)
})

test_that("the collision affected reporting only: the status is computed from the criterion vector (old and new code give the same statuses and failed criteria)", {
  # the same tampering as in test-15 produces the same REJECTED status and failed criterion as before the rename
  rid <- RG16$results$result_id[RG16$results$metric == "UTCI" & RG16$results$window_or_lag == 7L]
  g <- RG16; g$fits[[rid]]$newton_decrement <- 1e-3; v <- siw_ordinal_refined_verify(g, CFG, PLAN16); i <- v$result_id == rid
  expect_identical(v$status[i], "REJECTED"); expect_identical(v$failed_criteria[i], "newton_decrement"); expect_identical(sum(v$status == "REJECTED"), 1L)
  expect_false(v$criterion_newton_decrement_pass[i]); expect_true(all(unlist(v[i, setdiff(CRIT, "criterion_newton_decrement_pass")]))); expect_false(v$all_criteria_pass[i])
  expect_equal(v$newton_decrement_value[i], 1e-3)                                                         # the measurement is reported under its own name
})

test_that("CSV round trip preserves names, logical criteria, measurements and status for all 80 models", {
  f <- tempfile(fileext = ".csv"); utils::write.csv(siw_conform(RV16, siw_ordinal_refinement_output_columns()$ordinal_refined_verification), f, row.names = FALSE, na = "")
  b <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE); expect_identical(names(b), siw_ordinal_refinement_output_columns()$ordinal_refined_verification); expect_false(anyDuplicated(names(b)) > 0); expect_equal(nrow(b), 80L)
  for (cn in c(CRIT, "all_criteria_pass", "inherited_batch_b_criteria_met")) { expect_true(is.logical(b[[cn]])); expect_identical(b[[cn]], RV16[[cn]]) }
  num <- c("newton_decrement_value", "loglik_difference", "max_coefficient_threshold_difference", "exposure_coefficient_difference", "exposure_se_relative_difference", "max_se_relative_difference_all_parameters", "min_threshold_gap", "polr_hessian_min_eigenvalue", "polr_hessian_condition", "analytic_information_min_eigenvalue", "independent_newton_decrement", "independent_gradient_max_abs")
  for (cn in num) expect_equal(b[[cn]], RV16[[cn]], tolerance = 1e-13); expect_identical(b$status, RV16$status); expect_identical(ifelse(is.na(b$failed_criteria), "", as.character(b$failed_criteria)), ifelse(is.na(RV16$failed_criteria), "", RV16$failed_criteria))      # an all-empty text column is read back as NA by read.csv; expect_identical(b$result_id, RV16$result_id)
  # the written table of a run goes through siw_write_table with the same schema
  d <- tempfile(); dir.create(d); siw_write_table(RV16, "ordinal_refined_verification", d); w <- utils::read.csv(file.path(d, "ordinal_refined_verification.csv"), check.names = FALSE); expect_identical(names(w), names(b)); expect_identical(w$criterion_newton_decrement_pass, RV16$criterion_newton_decrement_pass)
})

test_that("an excessive Newton decrement fails the model even when every other criterion passes; the boundary of the declared limit is exact", {
  rid <- RG16$results$result_id[RG16$results$metric == "MRT" & RG16$results$window_or_lag == 14L]
  chk <- function(value) { g <- RG16; g$fits[[rid]]$newton_decrement <- value; v <- siw_ordinal_refined_verify(g, CFG, PLAN16); i <- v$result_id == rid
    f <- tempfile(fileext = ".csv"); utils::write.csv(siw_conform(v, siw_ordinal_refinement_output_columns()$ordinal_refined_verification), f, row.names = FALSE, na = ""); list(v = v[i, ], csv = utils::read.csv(f, check.names = FALSE)[which(i), ]) }
  for (val in c(1.0001e-5, 1e-4, 1e-2, 1)) { r <- chk(val); expect_identical(r$v$status, "REJECTED"); expect_false(r$v$criterion_newton_decrement_pass); expect_true(all(unlist(r$v[, setdiff(CRIT, "criterion_newton_decrement_pass")]))); expect_identical(r$csv$status, "REJECTED"); expect_false(r$csv$criterion_newton_decrement_pass); expect_equal(r$csv$newton_decrement_value, val, tolerance = 1e-13) }
  r <- chk(1e-5); expect_identical(r$v$status, "ACCEPTED"); expect_true(r$v$criterion_newton_decrement_pass)                        # exactly at the declared limit: accepted
  r <- chk(NA_real_); expect_identical(r$v$status, "REJECTED"); expect_false(r$v$criterion_newton_decrement_pass)                  # a missing decrement never passes
  # a model failing only another criterion leaves the Newton criterion TRUE
  g <- RG16; g$fits[[rid]]$loglik <- g$fits[[rid]]$loglik - 1e-6; v <- siw_ordinal_refined_verify(g, CFG, PLAN16); i <- v$result_id == rid; expect_true(v$criterion_newton_decrement_pass[i]); expect_false(v$criterion_loglik_agreement_pass[i]); expect_identical(v$status[i], "REJECTED")
})

test_that("duplicate column names are refused by the schema machinery instead of silently selecting the first column", {
  df <- data.frame(a = 1:2, b = 3:4); names(df) <- c("a", "a"); expect_error(siw_conform(df, c("a")), "DUPLICATE_COLUMN_NAMES_IN_TABLE")
  expect_error(siw_conform(data.frame(a = 1:2), c("a", "a")), "DUPLICATE_COLUMN_NAMES_IN_SCHEMA")
  expect_identical(names(siw_conform(data.frame(a = 1:2), c("a", "b"))), c("a", "b"))
  # the old defect, reproduced on purpose: a measurement and a criterion with the same name cannot be built into the verification table
  expect_true(!anyDuplicated(names(RV16)))
})

test_that("independent numerical derivatives confirm the analytic derivatives used by the Newton decrement, the independent fit and the information matrices", {
  skip_nd <- !requireNamespace("numDeriv", quietly = TRUE); if (skip_nd) return(invisible(NULL))
  ll_plain <- function(theta, y, X) { K <- max(y); zeta <- theta[seq_len(K - 1L)]; beta <- theta[K:length(theta)]; eta <- drop(X %*% beta); s <- 0       # explicit loops and exp(): shares no code with siw_po_derivatives
    for (i in seq_along(y)) { k <- y[i]; hi <- if (k == K) 1 else 1 / (1 + exp(-(zeta[k] - eta[i]))); lo <- if (k == 1L) 0 else 1 / (1 + exp(-(zeta[k - 1L] - eta[i]))); s <- s + log(hi - lo) }; s }
  covs <- CFG$tiers[[CFG$primary_tier]]; pick <- c(1L, 22L, 37L, 50L, 63L, 80L); rel <- function(a, b) max(abs(a - b)) / max(1, max(abs(b)))
  for (i in pick) {
    rid <- RG16$results$result_id[i]; r <- RG16$fits[[rid]]; d <- RG16$data[[rid]]; ex <- RG16$results$exposure[i]; X <- stats::model.matrix(stats::reformulate(c(ex, covs)), d)[, -1L, drop = FALSE]; y <- as.integer(d$integration_ordinal); q <- length(r$zeta)
    Z <- sweep(sweep(X, 2L, colMeans(X)), 2L, apply(X, 2L, stats::sd), "/"); gam <- (r$coefficients) * apply(X, 2L, stats::sd); zs <- r$zeta - sum(r$coefficients * colMeans(X))       # scaled-coordinates solution from the back-transformed one
    th <- c(zs, gam); f <- function(t) ll_plain(t, y, Z); th2 <- th + 0.02 * sin(seq_along(th)); ga <- siw_po_derivatives(th, y, Z); gb <- siw_po_derivatives(th2, y, Z)
    expect_equal(ll_plain(c(r$zeta, r$coefficients), y, X), r$loglik, tolerance = 1e-10); expect_equal(f(th), r$loglik, tolerance = 1e-10)                  # independent likelihood = refined log-likelihood, in both parameterisations
    gn <- numDeriv::grad(f, th, method = "Richardson"); expect_true(max(abs(gn)) < 1e-5)                                                                       # stationarity without the analytic code
    expect_true(rel(gb$gradient, numDeriv::grad(f, th2, method = "Richardson")) < 1e-5); expect_true(rel(gb$hessian, numDeriv::hessian(f, th2, method = "Richardson")) < 1e-3)
    Hn <- numDeriv::hessian(f, th, method = "Richardson"); nd_an <- sqrt(max(0, sum(ga$gradient * solve(-ga$hessian, ga$gradient)))); nd_num <- sqrt(max(0, sum(gn * solve(-Hn, gn))))
    expect_true(abs(nd_an - nd_num) < 1e-6)                                                                                                                   # far below the declared limit 1e-5
  }
  # the analytic Hessian also equals the Hessian that MASS itself computes (separate code), mapped with MASS's own Jacobian
  r <- RG16$fits[[RG16$results$result_id[22]]]; d <- RG16$data[[RG16$results$result_id[22]]]; ex <- RG16$results$exposure[22]; X <- stats::model.matrix(stats::reformulate(c(ex, covs)), d)[, -1L, drop = FALSE]
  Z <- sweep(sweep(X, 2L, colMeans(X)), 2L, apply(X, 2L, stats::sd), "/"); colnames(Z) <- paste0("V", seq_len(ncol(Z))); dd <- data.frame(y = d$integration_ordinal, Z)
  fit <- MASS::polr(stats::reformulate(colnames(Z), "y"), dd, Hess = TRUE, method = "logistic", control = list(reltol = 1e-14, maxit = 5000L, ndeps = rep(1e-5, ncol(Z) + 3L))); q <- 3L
  et <- exp(c(fit$zeta[1L], log(diff(fit$zeta)))); Jac <- matrix(0, q, q); Jac[, 1L] <- 1; for (k in 2:q) Jac[k:q, k] <- et[k]; A <- diag(ncol(Z) + q); A[ncol(Z) + 1:q, ncol(Z) + 1:q] <- Jac
  Vm <- A %*% MASS::ginv(fit$Hessian) %*% t(A); ga <- siw_po_derivatives(c(fit$zeta, fit$coefficients), as.integer(dd$y), Z); Va <- solve(-ga$hessian); o <- c(q + seq_len(ncol(Z)), seq_len(q)); Va <- Va[o, o]
  expect_true(max(abs(sqrt(diag(Vm)) - sqrt(diag(Va))) / sqrt(diag(Va))) < 1e-6)
})
