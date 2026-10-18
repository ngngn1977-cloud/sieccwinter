# Ordinal refinement (Phase 3B.2): synthetic data only.
PLAN <- siw_ordinal_refinement_plan()
LEG <- siw_ordinal_grid(DAT, CFG)
RG <- siw_ordinal_refined_grid(DAT, CFG, PLAN)
RV <- siw_ordinal_refined_verify(RG, CFG, PLAN)

test_that("the refinement plan declares the protocol and the acceptance criteria; the inherited Batch B criteria are not loosened", {
  expect_identical(PLAN$protocol$id, "POLR_SCALED_TIGHT_V1"); expect_true(isTRUE(PLAN$protocol$same_protocol_for_all_models)); expect_true(nzchar(PLAN$plan_hash))
  a <- PLAN$acceptance_criteria; expect_equal(a$newton_decrement_max, 1e-5); expect_equal(a$loglik_abs_diff_vs_independent_max, 1e-9); expect_equal(a$coefficients_and_thresholds_abs_diff_vs_independent_max, 1e-6)
  expect_true(isTRUE(a$optimiser_code_zero)); expect_equal(a$inherited_batch_b_criteria_still_reported$loglik, 1e-6); expect_equal(a$inherited_batch_b_criteria_still_reported$coefficient, 1e-4); expect_equal(a$inherited_batch_b_criteria_still_reported$se, 1e-4)
  expect_true(a$loglik_abs_diff_vs_independent_max < 1e-6 && a$exposure_coefficient_abs_diff_vs_independent_max < 1e-4)                     # strictly tighter than the existing verification
  expect_equal(PLAN$protocol$optim_control$reltol, 1e-14); expect_equal(PLAN$protocol$restarts$max, 3)
})

test_that("back-transformation: thresholds, coefficients and covariance map exactly from the scaled to the original parameterisation", {
  set.seed(2); n <- 80; X <- cbind(a = stats::rnorm(n, 50, 10), b = stats::rbinom(n, 1, 0.4), c = stats::runif(n, 0, 0.01)); mu <- colMeans(X); s <- apply(X, 2, stats::sd); q <- 3
  T <- siw_polr_backtransform_matrix(mu, s, q); gam <- c(0.3, -0.5, 0.2); zs <- c(-1, 0.2, 1.1); th <- as.numeric(T %*% c(gam, zs)); beta <- th[1:3]; zeta <- th[4:6]
  Z <- sweep(sweep(X, 2, mu), 2, s, "/"); for (k in 1:3) expect_equal(stats::plogis(zs[k] - drop(Z %*% gam)), stats::plogis(zeta[k] - drop(X %*% beta)), tolerance = 1e-12)       # same cumulative probabilities
  expect_equal(beta, gam / s, tolerance = 1e-14)
  f <- function(v) { g <- v[1:3]; z <- v[4:6]; b <- g / s; c(b, z + sum(b * mu)) }
  if (requireNamespace("numDeriv", quietly = TRUE)) expect_true(max(abs(numDeriv::jacobian(f, c(gam, zs)) - T)) < 1e-6)
  Vs <- crossprod(matrix(stats::rnorm(36), 6)); V <- T %*% Vs %*% t(T); expect_true(isSymmetric(V)); expect_equal(sqrt(diag(V))[1:3], sqrt(diag(Vs))[1:3] / s, tolerance = 1e-12)
  # refined fit equals a tight fit on the unscaled design (same optimum, original parameterisation)
  v <- c("integration_ordinal", "UTCI_AVG_7D", CFG$tiers$M2_L); dat <- DAT; dat$integration_ordinal <- siw_ordinal_outcome(dat); d <- droplevels(dat[stats::complete.cases(dat[, v]), v])
  r <- siw_polr_refined_one(d, "integration_ordinal", "UTCI_AVG_7D", CFG$tiers$M2_L, PLAN); expect_true(r$ok)
  Xo <- stats::model.matrix(stats::reformulate(c("UTCI_AVG_7D", CFG$tiers$M2_L)), d)[, -1, drop = FALSE]; ind <- siw_polr_independent(as.integer(d$integration_ordinal), Xo)
  expect_true(max(abs(r$coefficients - ind$coefficients)) < 1e-6); expect_true(max(abs(r$zeta - ind$zeta)) < 1e-6); expect_equal(unname(r$se[["UTCI_AVG_7D"]]), unname(ind$se[["UTCI_AVG_7D"]]), tolerance = 1e-6)
  expect_true(isSymmetric(unname(r$vcov), tol = 1e-8)); expect_equal(rownames(r$vcov), c(colnames(Xo), names(r$zeta)))
})

test_that("refined grid: the same 80 specifications and participants, own ids, own BH q80, legacy table untouched", {
  R <- RG$results; expect_equal(nrow(R), 80L); expect_true(all(R$status == "COMPLETED")); expect_true(!anyDuplicated(R$result_id)); expect_true(all(R$result_id != R$legacy_result_id)); expect_identical(R$legacy_result_id, LEG$result_id)
  expect_identical(R$n, LEG$n); expect_identical(R$categories_used, LEG$categories_used); expect_identical(R$dropped_terms, LEG$dropped_terms); expect_identical(R$metric, LEG$metric); expect_identical(R$window_or_lag, LEG$window_or_lag); expect_true(all(R$model_tier == "M2_L"))
  expect_true("q80_ordinal_refined" %in% names(R)); expect_false(any(c("q80", "q80_ordinal") %in% names(R))); expect_equal(R$q80_ordinal_refined, siw_bh(R$p), tolerance = 1e-12); expect_equal(sum(is.finite(R$q80_ordinal_refined)), 80L)
  expect_true(all(abs(R$p - 2 * stats::pnorm(abs(R$coefficient / R$se), lower.tail = FALSE)) < 1e-12)); expect_true(all(abs(R$odds_ratio - exp(R$coefficient * R$exposure_sd)) < 1e-12)); expect_true(all(R$protocol_id == PLAN$protocol$id))
  expect_identical(siw_ordinal_grid(DAT, CFG), LEG); expect_true("q80_ordinal" %in% names(LEG)); expect_false("q80_ordinal_refined" %in% names(LEG))
})

test_that("refined verification: every criterion is evaluated per model and all 80 synthetic models are accepted", {
  expect_equal(nrow(RV), 80L); expect_true(all(RV$status == "ACCEPTED")); crit <- siw_ordinal_refinement_output_columns()$ordinal_refined_verification; crit <- crit[(length(crit) - 9):length(crit)]
  expect_true(all(crit %in% names(RV))); expect_true(all(as.matrix(RV[, crit]))); expect_true(all(RV$newton_decrement_value <= 1e-5)); expect_true(all(abs(RV$loglik_difference) <= 1e-9)); expect_true(all(RV$min_threshold_gap > 0))
  expect_true(all(RV$polr_hessian_min_eigenvalue > 0)); expect_true(all(RV$analytic_information_min_eigenvalue > 0)); expect_true(all(RV$inherited_batch_b_criteria_met)); expect_true(all(RV$independent_gradient_max_abs <= 1e-8))
  # the legacy polr fits that are not at the maximum are corrected: the refined log-likelihood is never lower
  C <- siw_ordinal_refined_vs_legacy(RG$results, LEG); expect_true(all(C$loglik_gain >= -1e-9)); expect_true(any(C$loglik_gain > 1e-6))
})

test_that("a successful optimiser flag alone is insufficient: tampered fits are REJECTED for the right criterion", {
  rid <- RG$results$result_id[RG$results$metric == "UTCI" & RG$results$window_or_lag == 7L]; ex <- RG$results$exposure[RG$results$result_id == rid]
  chk <- function(mod, what) { g <- RG; f <- g$fits[[rid]]; g$fits[[rid]] <- mod(f); v <- siw_ordinal_refined_verify(g, CFG, PLAN); i <- v$result_id == rid
    expect_identical(v$status[i], "REJECTED"); expect_true(grepl(what, v$failed_criteria[i])); expect_true(all(v$status[!i] == "ACCEPTED")) }
  chk(function(f) { f$coefficients[[ex]] <- f$coefficients[[ex]] + 0.01; f }, "coefficients_thresholds_agree")
  chk(function(f) { f$newton_decrement <- 1e-3; f }, "newton_decrement")
  chk(function(f) { f$loglik <- f$loglik - 1e-6; f }, "loglik_agreement")
  chk(function(f) { f$zeta[2] <- f$zeta[1] - 0.1; f }, "thresholds_ordered")
  chk(function(f) { f$hessian_min_eigenvalue <- -1; f }, "information_matrix")
  chk(function(f) { f$se <- f$se * 1.01; f }, "information_matrix")
  chk(function(f) { f$se[[ex]] <- f$se[[ex]] * (1 + 1e-5); f }, "exposure_se_agrees")
  chk(function(f) { f$code <- 1L; f }, "optimiser_code_zero")
  chk(function(f) { f$loglik <- NA_real_; f }, "all_estimates_finite")
  # an under-converged protocol (default optimiser tolerances) cannot pass the criteria although polr reports convergence code 0
  weak <- PLAN; weak$protocol$optim_control$reltol <- 1.5e-8; weak$protocol$optim_control$maxit <- 100L; weak$protocol$optim_control$ndeps <- 1e-3; weak$protocol$restarts$max <- 0L
  g <- siw_ordinal_refined_grid(DAT, CFG, weak); v <- siw_ordinal_refined_verify(g, CFG, PLAN)
  expect_true(all(v$optimiser_code_zero)); expect_true(sum(v$status == "REJECTED") >= 1L)
})

test_that("restarts: forced restarts keep the protocol and the result accepted", {
  p <- PLAN; p$protocol$restarts$restart_if_newton_decrement_above <- 0; p$protocol$restarts$max <- 2L
  v <- c("integration_ordinal", "UTCI_AVG_7D", CFG$tiers$M2_L); dat <- DAT; dat$integration_ordinal <- siw_ordinal_outcome(dat); d <- droplevels(dat[stats::complete.cases(dat[, v]), v])
  r <- siw_polr_refined_one(d, "integration_ordinal", "UTCI_AVG_7D", CFG$tiers$M2_L, p); expect_equal(r$restarts, 2L); expect_true(r$newton_decrement < 1e-5)
  r0 <- siw_polr_refined_one(d, "integration_ordinal", "UTCI_AVG_7D", CFG$tiers$M2_L, PLAN); expect_equal(r$coefficients[["UTCI_AVG_7D"]], r0$coefficients[["UTCI_AVG_7D"]], tolerance = 1e-6)
})

test_that("failures and degenerate specifications are recorded per model, not thrown", {
  d3 <- DAT; d3$integration_score[d3$integration_score == 3] <- 4; g3 <- siw_ordinal_refined_grid(d3, CFG, PLAN); R <- g3$results
  expect_equal(nrow(R), 80L); expect_true(all(R$status %in% c("COMPLETED", "NOT_ESTIMABLE"))); expect_true(all(R$categories_used == 3L)); expect_true(all(nzchar(R$not_estimable_reason[R$status == "NOT_ESTIMABLE"])))
  expect_equal(sum(is.finite(R$q80_ordinal_refined)), sum(is.finite(R$p))); v3 <- siw_ordinal_refined_verify(g3, CFG, PLAN); expect_true(all(v3$status[R$status == "NOT_ESTIMABLE"] == "NOT_VERIFIED_NOT_ESTIMABLE")); expect_true(all(v3$status[R$status == "COMPLETED"] %in% c("ACCEPTED", "REJECTED")))
  dc <- DAT; dc$UTCI_AVG_7D <- 5; gc <- siw_ordinal_refined_grid(dc, CFG, PLAN)$results; i <- which(gc$metric == "UTCI" & gc$window_or_lag == 7L); expect_identical(gc$status[i], "NOT_ESTIMABLE"); expect_true(grepl("ZERO_VARIANCE", gc$not_estimable_reason[i])); expect_true(is.na(gc$q80_ordinal_refined[i]))
})

test_that("comparison with the legacy table: joined on metric and window, differences and crossings reported, not a pass/fail", {
  lv <- siw_ordinal_verify(DAT, CFG, LEG); C <- siw_ordinal_refined_vs_legacy(RG$results, LEG, lv); expect_equal(nrow(C), 80L); expect_identical(names(C), siw_ordinal_refinement_output_columns()$ordinal_refined_vs_legacy)
  expect_equal(C$coefficient_difference, C$coefficient_refined - C$coefficient_legacy); expect_equal(C$q80_difference, C$q80_refined - C$q80_legacy); expect_identical(C$legacy_result_id, LEG$result_id)
  expect_true(all(C$legacy_numerical_status %in% c("PASS", "POLR_NOT_AT_MAXIMUM"))); expect_true(all(abs(C$coefficient_difference[C$legacy_numerical_status == "PASS"]) < 1e-3))
  expect_identical(C$sign_change, sign(C$coefficient_refined) != sign(C$coefficient_legacy))
})

test_that("end to end: the step reports separate statuses, reproduces the legacy table, writes only to its own new folder", {
  if (identical(Sys.getenv("SIW_SKIP_SLOW_TESTS"), "1")) return(invisible(NULL))
  tmp <- file.path(tempdir(), "ordref_e2e"); unlink(tmp, recursive = TRUE); dir.create(tmp); refdir <- file.path(tmp, "ref"); legdir <- file.path(tmp, "legacy_run"); dir.create(refdir); dir.create(legdir)
  raw <- siw_read_study_csv(SYN_CSV, CFG); dat <- siw_attach_cumulative(siw_prepare_data(raw, CFG), CFG)$data; lg <- siw_ordinal_grid(dat, CFG)
  utils::write.csv(data.frame(outcome = lg$outcome, metric = lg$metric, window_or_lag = lg$window_or_lag, n = lg$n, coefficient = lg$coefficient, se = lg$se, odds_ratio = lg$odds_ratio, p = lg$p, q80_ordinal = lg$q80_ordinal), file.path(refdir, "batchB_ordinal_reference.csv"), row.names = FALSE, na = "")
  utils::write.csv(lg, file.path(legdir, "ordinal_integration.csv"), row.names = FALSE, na = ""); before <- c(list.files(refdir), list.files(legdir)); hb <- unname(tools::md5sum(list.files(c(refdir, legdir), full.names = TRUE)))
  out <- file.path(tmp, "out"); r <- siw_run_ordinal_refinement(SYN_CSV, out, refdir, CFG, legacy_run_dir = legdir)
  expect_identical(r$statuses[["EXECUTION_STATUS"]], "PASS"); expect_identical(r$statuses[["LEGACY_REPRODUCTION_STATUS"]], "PASS"); expect_identical(r$statuses[["REFINED_NUMERICAL_VERIFICATION_STATUS"]], "PASS")
  expect_true(r$statuses[["LEGACY_NUMERICAL_VERIFICATION_STATUS"]] %in% c("PASS", "DOCUMENTED_LIMITATION")); expect_true(length(unique(names(r$statuses))) == length(r$statuses))
  for (f in c("ordinal_refined.csv", "ordinal_refined_verification.csv", "ordinal_refined_vs_legacy.csv", "ordinal_legacy_reproduction.csv", "ordinal_refinement_summary.csv", "ORDINAL_REFINEMENT_STATUS.txt", "ordinal_refinement_metadata.json", "session_info.txt", "MANIFEST.csv")) expect_true(file.exists(file.path(out, f)))
  expect_equal(nrow(utils::read.csv(file.path(out, "ordinal_refined.csv"))), 80L); expect_identical(c(list.files(refdir), list.files(legdir)), before); expect_identical(unname(tools::md5sum(list.files(c(refdir, legdir), full.names = TRUE))), hb)   # inputs untouched
  all_txt <- paste(unlist(lapply(list.files(out, pattern = "[.](csv|md|txt|json)$", full.names = TRUE), function(p) readLines(p, warn = FALSE))), collapse = "\n"); expect_false(any(vapply(unique(as.character(SYN$participant_code)), function(cd) grepl(cd, all_txt, fixed = TRUE), logical(1))))
  # a legacy run folder whose table differs, or a reference that differs, FAILS the legacy reproduction (separate from the refined verification)
  bad <- lg; bad$coefficient[3] <- bad$coefficient[3] + 1e-3; utils::write.csv(bad, file.path(legdir, "ordinal_integration.csv"), row.names = FALSE, na = "")
  r2 <- siw_run_ordinal_refinement(SYN_CSV, file.path(tmp, "out2"), refdir, CFG, legacy_run_dir = legdir); expect_identical(r2$statuses[["LEGACY_REPRODUCTION_STATUS"]], "FAIL"); expect_identical(r2$statuses[["REFINED_NUMERICAL_VERIFICATION_STATUS"]], "PASS")
  e <- file.path(tmp, "emptyref"); dir.create(e); r3 <- siw_run_ordinal_refinement(SYN_CSV, file.path(tmp, "out3"), e, CFG); expect_identical(r3$statuses[["LEGACY_REPRODUCTION_STATUS"]], "FAIL")      # a missing required reference fails
})
