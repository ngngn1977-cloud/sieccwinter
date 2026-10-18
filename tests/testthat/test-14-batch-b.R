# Batch B: Firth sensitivity, ordinal integration, exposure transformations, nonlinear forms, their independent verification and the Batch B validator.
# Synthetic data only. Expensive objects are computed once and shared.
CORE <- siw_cumulative_grid(DAT, CFG)$results
FG <- siw_firth_grid(DAT, CFG, CORE, verify = FALSE)$results
OG <- siw_ordinal_grid(DAT, CFG)
TG <- siw_transformation_grid(DAT, CFG)
NG <- siw_nonlinear_grid(DAT, CFG)
design <- function(x, ...) cbind("(Intercept)" = 1, x = x, ...)

test_that("Firth closed forms: intercept-only Jeffreys estimate and the 2x2 table with half added to every cell", {
  f <- siw_firth_fit(matrix(1, 20, 1, dimnames = list(NULL, "(Intercept)")), c(rep(1, 3), rep(0, 17)))
  expect_true(f$converged)
  expect_equal(unname(f$coefficients), stats::qlogis(3.5 / 21), tolerance = 1e-9)                     # penalised ML p = (events + 1/2) / (n + 1)
  expect_equal(unname(f$standard_error), 1 / sqrt(20 * (3.5 / 21) * (1 - 3.5 / 21)), tolerance = 1e-9)
  a <- 6; b <- 0; c <- 2; d <- 5; x <- c(rep(1, a + b), rep(0, c + d)); y <- c(rep(1, a), rep(0, b), rep(1, c), rep(0, d))
  g <- siw_firth_fit(design(x), y)
  expect_equal(unname(g$coefficients[2]), log(((a + .5) * (d + .5)) / ((b + .5) * (c + .5))), tolerance = 1e-9)   # zero cell: ordinary ML diverges
  expect_equal(unname(g$coefficients[1]), log((c + .5) / (d + .5)), tolerance = 1e-9)
})

test_that("Firth: separation and sparse outcomes give finite, converged, verified estimates where ordinary ML fails", {
  set.seed(3); xs <- c(stats::rnorm(10, -3), stats::rnorm(10, 3)); ys <- rep(0:1, each = 10); X <- design(xs)
  ml <- suppressWarnings(stats::glm.fit(X, ys, family = stats::binomial()))
  expect_true(abs(ml$coefficients[2]) > 10)                              # ordinary ML: separation, runaway slope
  f <- siw_firth_fit(X, ys); expect_true(f$converged); expect_true(all(is.finite(f$coefficients))); expect_true(abs(f$coefficients[2]) < 3)
  expect_identical(siw_firth_verify(X, ys, f)$result, "PASS")
  set.seed(4); Xs <- design(stats::rnorm(40), z = stats::rnorm(40)); ysp <- c(1, rep(0, 39)); fs <- siw_firth_fit(Xs, ysp)
  expect_true(fs$converged); expect_true(all(is.finite(fs$standard_error))); expect_identical(siw_firth_verify(Xs, ysp, fs)$result, "PASS")
})

test_that("Firth: adjusted score vanishes at the returned estimate, the penalty is the half log-determinant, and the augmentation estimator agrees", {
  set.seed(11); n <- 45; X <- design(stats::rnorm(n), z = stats::rnorm(n)); y <- stats::rbinom(n, 1, stats::plogis(0.4 * X[, 2]))
  f <- siw_firth_fit(X, y); st <- siw_firth_adjusted_score(X, y, f$coefficients)
  expect_true(max(abs(st$score)) < 1e-8); expect_true(f$scaled_score_norm < 1e-9)
  p <- stats::plogis(drop(X %*% f$coefficients)); w <- p * (1 - p)
  expect_equal(siw_firth_penalised_loglik(X, y, f$coefficients), sum(stats::dbinom(y, 1, p, log = TRUE)) + 0.5 * log(det(crossprod(X, X * w))), tolerance = 1e-10)
  aug <- siw_firth_augmentation(X, y); expect_true(aug$converged); expect_true(max(abs(aug$coefficients - f$coefficients)) < 1e-8)
  expect_equal(unname(f$standard_error), unname(sqrt(diag(solve(crossprod(X, X * w))))), tolerance = 1e-10)       # SE from the expected information at the penalised estimate
  expect_true(siw_firth_penalised_loglik(X, y, f$coefficients) >= siw_firth_penalised_loglik(X, y, f$coefficients + c(1e-3, -1e-3, 1e-3)))
})

test_that("Firth: singular designs and non-estimable specifications are reported, never thrown", {
  f <- siw_firth_fit(cbind("(Intercept)" = 1, x = 1:20 / 20, x2 = 2 * (1:20) / 20), rep(0:1, 10)); expect_false(f$converged); expect_identical(f$status, "SINGULAR_INFORMATION")
  d0 <- DAT; d0$Sex <- 0; g0 <- siw_firth_grid(d0, CFG, verify = FALSE)$results
  expect_true(all(g0$status[g0$model_tier != "M0"] == "NOT_ESTIMABLE")); expect_true(all(grepl("RANK_DEFICIENT|ZERO_VARIANCE", g0$not_estimable_reason[g0$status == "NOT_ESTIMABLE"]))); expect_true(all(g0$status[g0$model_tier == "M0"] == "COMPLETED"))
  d1 <- DAT; d1$Int_dummy <- 0L; g1 <- siw_firth_grid(d1, CFG, verify = FALSE)$results
  expect_true(all(g1$status == "NOT_ESTIMABLE")); expect_true(all(g1$not_estimable_reason == "NO_BINARY_OUTCOME_VARIATION"))
})

test_that("Firth grid: 560 specifications, unique ids, no p-value or multiplicity family, convergence and dropped terms recorded", {
  expect_equal(nrow(FG), 560L); expect_true(!anyDuplicated(FG$result_id)); expect_true(all(FG$outcome == "Int_dummy")); expect_equal(sort(unique(FG$model_tier)), sort(CFG$tier_order))
  expect_true(all(FG$status == "COMPLETED")); expect_true(all(FG$converged)); expect_true(all(FG$scaled_score_norm < 1e-9)); expect_true(all(is.finite(FG$coefficient) & is.finite(FG$standard_error)))
  expect_true(all(abs(FG$ci_upper - FG$ci_lower - 2 * 1.96 * FG$standard_error) < 1e-12)); expect_false(any(c("p", "nominal_p", "q", "q90") %in% names(FG)))
  expect_true(all(FG$interval_method == "Wald_from_penalised_expected_information")); expect_true(all(c("dropped_terms", "step_halvings", "separation_status") %in% names(FG)))
  # the 560 keys are the binary rows of the cumulative grid
  expect_identical(sort(paste(FG$metric, FG$window_or_lag, FG$model_tier)), sort(paste(CORE$metric, CORE$window_or_lag, CORE$model_tier)[CORE$outcome == "Int_dummy"]))
})

test_that("Firth: the grid reproduces independent verification for sampled specifications", {
  pick <- c(1L, 123L, 337L, 560L); tier_x <- lapply(stats::setNames(CFG$tier_order, CFG$tier_order), function(t) siw_tier_matrix(DAT, CFG$tiers[[t]]))
  for (i in pick) {
    r <- FG[i, ]; sp <- CFG$metrics[CFG$metrics$metric == r$metric, ]; x <- DAT[[siw_cum_name(sp$cumulative_prefix, r$window_or_lag)]]; y <- as.numeric(DAT$Int_dummy)
    Xc <- tier_x[[r$model_tier]]; m <- stats::complete.cases(Xc) & is.finite(x) & is.finite(y); Xs <- Xc[m, , drop = FALSE]; Xs <- Xs[, !(grepl("^Locality_3group", colnames(Xs)) & colSums(Xs != 0) == 0), drop = FALSE]
    X <- cbind(Xs[, 1L, drop = FALSE], exposure = x[m], Xs[, -1L, drop = FALSE]); colnames(X)[1L] <- "(Intercept)"
    v <- siw_firth_verify(X, y[m]); expect_identical(v$result, "PASS"); expect_equal(unname(siw_firth_fit(X, y[m])$coefficients[2]), r$coefficient, tolerance = 1e-9)
  }
})

test_that("ordinal: analytic derivatives equal numerical derivatives; the independent fit matches polr where polr is converged", {
  skip_if_not_installed <- function(p) requireNamespace(p, quietly = TRUE)
  set.seed(5); n <- 50; X <- cbind(a = stats::rnorm(n), b = stats::rnorm(n)); y <- sample(1:4, n, TRUE); th <- c(-1, 0.2, 1.1, 0.3, -0.4); d <- siw_po_derivatives(th, y, X)
  nl <- function(t) { zl <- c(-Inf, t[1:3], Inf); e <- drop(X %*% t[4:5]); -sum(log(stats::plogis(zl[y + 1L] - e) - stats::plogis(zl[y] - e))) }
  expect_true(!skip_if_not_installed("numDeriv") || max(abs(d$gradient + numDeriv::grad(nl, th))) < 1e-7)
  expect_true(!skip_if_not_installed("numDeriv") || max(abs(d$hessian + numDeriv::hessian(nl, th))) < 1e-6)
  expect_equal(d$loglik, -nl(th), tolerance = 1e-12)
  ok <- OG$status == "COMPLETED"; ov <- siw_ordinal_verify(DAT, CFG, OG)
  expect_true(all(ov$status %in% c("PASS", "POLR_NOT_AT_MAXIMUM"))); expect_true(all(ov$independent_gradient_max_abs[ov$status != "NOT_VERIFIED_NOT_ESTIMABLE"] < 1e-8))
  expect_true(all(ov$independent_loglik_minus_polr[ov$status == "POLR_NOT_AT_MAXIMUM"] > 0))     # whenever polr disagrees, the independent fit has the higher likelihood
  expect_true(sum(ov$status == "PASS") >= 60L)
})

test_that("ordinal grid: 80 polr specifications, own q80 family, outcome ordering and dropped categories preserved, failures recorded", {
  expect_equal(nrow(OG), 80L); expect_true(all(OG$outcome == "integration_score")); expect_true(all(OG$model_tier == "M2_L")); expect_true(!anyDuplicated(OG$result_id))
  expect_true("q80_ordinal" %in% names(OG)); expect_false("q80" %in% names(OG)); expect_equal(sum(is.finite(OG$q80_ordinal)), sum(is.finite(OG$p)))
  expect_equal(OG$q80_ordinal, siw_bh(OG$p), tolerance = 1e-12); expect_true(all(OG$categories_used == 4L)); expect_true(all(OG$convergence))
  expect_true(all(abs(OG$p - 2 * stats::pnorm(abs(OG$coefficient / OG$se), lower.tail = FALSE)) < 1e-12)); expect_true(all(abs(OG$odds_ratio - exp(OG$coefficient * OG$exposure_sd)) < 1e-12))
  expect_identical(levels(siw_ordinal_outcome(DAT)), as.character(1:4)); expect_true(is.ordered(siw_ordinal_outcome(DAT)))
  d3 <- DAT; d3$integration_score[d3$integration_score == 3] <- 4; o3 <- siw_ordinal_grid(d3, CFG)
  expect_true(all(o3$categories_used == 3L))                                                            # an empty category is dropped as in the original, not an error
  expect_true(all(o3$status %in% c("COMPLETED", "NOT_ESTIMABLE"))); expect_true(any(o3$status == "COMPLETED")); expect_true(all(nzchar(o3$not_estimable_reason[o3$status == "NOT_ESTIMABLE"])))
  expect_true(all(nzchar(o3$fit_warnings[o3$status == "NOT_ESTIMABLE"])) || all(grepl("POLR_ERROR|NON_FINITE|VCOV|EXPOSURE", o3$not_estimable_reason[o3$status == "NOT_ESTIMABLE"])))   # sparse categories: polr's own failures are recorded per specification
  dc <- DAT; dc$UTCI_AVG_7D <- 5; oc <- siw_ordinal_grid(dc, CFG); i <- which(oc$metric == "UTCI" & oc$window_or_lag == 7L)
  expect_identical(oc$status[i], "NOT_ESTIMABLE"); expect_true(nzchar(oc$not_estimable_reason[i])); expect_true(is.na(oc$q80_ordinal[i]))
  # polr reference definition: logit P(Y <= k) = zeta_k - x'b, p from the Wald z of the exposure
  dat <- DAT; dat$io <- siw_ordinal_outcome(dat); v <- c("io", "UTCI_AVG_7D", CFG$tiers$M2_L); dd <- droplevels(dat[stats::complete.cases(dat[, v]), v])
  fit <- MASS::polr(stats::reformulate(c("UTCI_AVG_7D", CFG$tiers$M2_L), "io"), dd, Hess = TRUE, method = "logistic"); j <- which(OG$metric == "UTCI" & OG$window_or_lag == 7L)
  expect_equal(OG$coefficient[j], unname(fit$coefficients[["UTCI_AVG_7D"]]), tolerance = 1e-12); expect_equal(OG$se[j], sqrt(diag(stats::vcov(fit)))[["UTCI_AVG_7D"]], tolerance = 1e-12)
})

test_that("transformations: 1,080 specifications; sets per metric type; domain and missing-value rules; raw equals the validated M2-L results", {
  expect_equal(nrow(TG), 1080L); expect_true(!anyDuplicated(TG$result_id)); expect_true(all(TG$model_tier == "M2_L"))
  tab <- table(TG$metric, TG$transformation); expect_true(all(tab[CFG$metrics$metric[CFG$metrics$aggregation == "mean"], c("raw", "z", "signed_log_z", "exp_z")] == 30L))
  expect_true(all(tab[CFG$metrics$metric[CFG$metrics$aggregation == "mean"], c("log1p", "sqrt")] == 0L)); expect_true(all(tab[CFG$metrics$metric[CFG$metrics$aggregation == "sum"], ] == 30L))
  r <- TG[TG$transformation == "raw", ]; m <- CORE[CORE$model_tier == "M2_L", ]; i <- match(paste(r$outcome, r$metric, r$window_or_lag), paste(m$outcome, m$metric, m$window_or_lag))
  expect_true(max(abs(r$coefficient - m$coefficient[i]), abs(r$nominal_p - m$nominal_p[i]), abs(r$n - m$n[i])) < 1e-12)
  # definitions
  x <- c(1, 2, NA, 4, 10); z <- (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)                       # z uses all non-missing values, missing stays missing
  expect_equal(siw_transform_exposure(x, "z")$value, z); expect_equal(siw_transform_exposure(x, "signed_log_z")$value, sign(z) * log1p(abs(z))); expect_equal(siw_transform_exposure(x, "exp_z")$value, exp(z))
  expect_equal(siw_transform_exposure(x, "log1p")$value, log1p(x)); expect_equal(siw_transform_exposure(x, "sqrt")$value, sqrt(x)); expect_identical(siw_transform_exposure(x, "raw")$value, x)
  expect_identical(siw_transform_exposure(c(1, -2, 3), "log1p")$status, "UNDEFINED"); expect_identical(siw_transform_exposure(c(1, -2, 3), "sqrt")$status, "UNDEFINED")
  expect_identical(siw_transform_exposure(c(5, 5, 5), "z")$status, "UNDEFINED"); expect_identical(siw_transform_exposure(c(5, NA, NA), "exp_z")$reason, "TRANSFORMATION_Z_INSUFFICIENT_VALUES")
  expect_identical(siw_transform_exposure(c(0, 0, 4, NA), "sqrt")$status, "OK"); expect_error(siw_transform_exposure(1:3, "cube"), "UNKNOWN_TRANSFORMATION")
  expect_identical(siw_transformations_for("mean"), c("raw", "z", "signed_log_z", "exp_z")); expect_identical(siw_transformations_for("sum"), c("raw", "z", "signed_log_z", "exp_z", "log1p", "sqrt"))
})

test_that("transformations: domain violations and constant exposures are recorded per specification and do not stop the grid", {
  d <- DAT; d$IMS_RAIN_IDW_TOTAL_14D[3] <- -1; d$UTCI_AVG_21D <- 7; g <- siw_transformation_grid(d, CFG)
  bad <- g[g$metric == "IMS_rainfall_total" & g$window_or_lag == 14L & g$transformation %in% c("log1p", "sqrt"), ]
  expect_true(all(bad$status == "NOT_ESTIMABLE")); expect_true(all(grepl("NONNEGATIVE", bad$not_estimable_reason))); expect_true(all(is.na(bad$coefficient)))
  ok <- g[g$metric == "IMS_rainfall_total" & g$window_or_lag == 14L & g$transformation %in% c("raw", "z", "signed_log_z", "exp_z"), ]; expect_true(all(ok$status == "COMPLETED"))
  cz <- g[g$metric == "UTCI" & g$window_or_lag == 21L, ]; expect_true(all(cz$status[cz$transformation != "raw"] == "NOT_ESTIMABLE")); expect_true(all(grepl("TRANSFORMATION", cz$not_estimable_reason[cz$transformation != "raw"])))
  expect_true(all(cz$status[cz$transformation == "raw"] == "NOT_ESTIMABLE"))      # a constant raw exposure is not estimable either (zero variance)
  expect_equal(sum(is.finite(g$q_transformation_global)), sum(is.finite(g$nominal_p)))
  # missing exposure values are excluded from the model but the z scale uses all non-missing participants
  dm <- DAT; dm$UTCI_AVG_7D[c(2, 9)] <- NA; gm <- siw_transformation_grid(dm, CFG); a <- gm[gm$outcome == "UCLA_Lonliness_SCORE" & gm$metric == "UTCI" & gm$window_or_lag == 7L & gm$transformation == "z", ]
  expect_equal(a$n, sum(stats::complete.cases(siw_tier_matrix(dm, CFG$tiers$M2_L)) & is.finite(dm$UCLA_Lonliness_SCORE) & is.finite(dm$UTCI_AVG_7D)))
  tx <- siw_transform_exposure(dm$UTCI_AVG_7D, "z")$value; expect_equal(stats::sd(tx, na.rm = TRUE), 1, tolerance = 1e-12)
})

test_that("transformations: independent recalculation with lm / glm and a hand-written HC3 agrees for every specification; BH families have the documented sizes", {
  tv <- siw_transformation_verify(DAT, CFG, TG); expect_equal(nrow(tv), sum(TG$status == "COMPLETED")); expect_true(all(tv$status == "PASS"))
  for (o in unique(TG$outcome)) { s <- TG[TG$outcome == o, ]; expect_equal(sum(is.finite(s$q_transformation_outcome)), 360L); expect_equal(s$q_transformation_outcome, siw_bh(s$nominal_p), tolerance = 1e-12) }
  expect_equal(TG$q_transformation_global, siw_bh(TG$nominal_p), tolerance = 1e-12)
})

test_that("nonlinear forms: 360 specifications, ns basis, centred quadratic, joint Wald, AIC/AICc, families 60 and 360", {
  expect_equal(nrow(NG), 360L); expect_true(!anyDuplicated(NG$result_id)); expect_true(all(NG$status == "COMPLETED")); expect_equal(sort(unique(NG$form)), c("quadratic", "spline_df2"))
  expect_true(all(NG$joint_df[NG$form == "quadratic"] == 1) && all(NG$joint_df[NG$form == "spline_df2"] == 2)); expect_equal(sort(unique(NG$metric)), sort(CFG$metrics$metric[CFG$metrics$aggregation == "mean"]))
  for (o in unique(NG$outcome)) for (f in unique(NG$form)) { s <- NG[NG$outcome == o & NG$form == f, ]; expect_equal(nrow(s), 60L); expect_equal(s$q_outcome_form, siw_bh(s$joint_p), tolerance = 1e-12) }
  expect_equal(NG$q_global, siw_bh(NG$joint_p), tolerance = 1e-12)
  nv <- siw_nonlinear_verify(DAT, CFG, NG); expect_equal(nrow(nv), 360L); expect_true(all(nv$status == "PASS"))
  # spline definition: ns(x, df = 2) of the complete-case exposure, interior knot at the median, boundary knots at the range
  i <- which(NG$outcome == "UCLA_Lonliness_SCORE" & NG$metric == "UTCI" & NG$window_or_lag == 14L & NG$form == "spline_df2")
  v <- c("UCLA_Lonliness_SCORE", "UTCI_AVG_14D", CFG$tiers$M2_L); d <- droplevels(DAT[stats::complete.cases(DAT[, v]), v]); b <- splines::ns(d$UTCI_AVG_14D, df = 2)
  expect_equal(unname(attr(b, "knots")), unname(stats::median(d$UTCI_AVG_14D)), tolerance = 1e-12); expect_equal(attr(b, "Boundary.knots"), range(d$UTCI_AVG_14D))
  d$s1 <- b[, 1]; d$s2 <- b[, 2]; fit <- stats::lm(stats::reformulate(c("s1", "s2", CFG$tiers$M2_L), "UCLA_Lonliness_SCORE"), d); X <- stats::model.matrix(fit); e <- stats::resid(fit)
  xi <- solve(crossprod(X)); h <- rowSums((X %*% xi) * X); V <- xi %*% crossprod(X, X * (e^2 / (1 - h)^2)) %*% xi; bb <- stats::coef(fit)[c("s1", "s2")]
  expect_equal(NG$joint_statistic[i], as.numeric(t(bb) %*% solve(V[c("s1", "s2"), c("s1", "s2")]) %*% bb), tolerance = 1e-9)
})

test_that("nonlinear forms: too few cases, constant exposure and rank deficiency are reported per specification", {
  d <- DAT; d$UTCI_AVG_28D <- 3; d$MRT_AVG_1D[-(1:10)] <- NA; g <- siw_nonlinear_grid(d, CFG)
  a <- g[g$metric == "UTCI" & g$window_or_lag == 28L, ]; expect_true(all(a$status == "NOT_ESTIMABLE")); expect_true(all(a$not_estimable_reason == "ZERO_VARIANCE_EXPOSURE")); expect_true(all(is.na(a$joint_p)))
  b <- g[g$metric == "MRT" & g$window_or_lag == 1L, ]; expect_true(all(b$status == "NOT_ESTIMABLE")); expect_true(all(b$not_estimable_reason == "FEWER_THAN_15_COMPLETE_CASES"))
  dr <- DAT; dr$UTCI_AVG_42D <- dr$Age; gr <- siw_nonlinear_grid(dr, CFG); r <- gr[gr$metric == "UTCI" & gr$window_or_lag == 42L, ]; expect_true(all(r$status == "NOT_ESTIMABLE")); expect_true(all(nzchar(r$not_estimable_reason)))
  expect_equal(sum(is.finite(g$q_global)), sum(is.finite(g$joint_p)))
})

mk_ref <- function(tabs, dir, firth_score = 1e-3) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  f <- tabs$firth_sensitivity; fr <- data.frame(outcome = f$outcome, metric = f$metric, window_or_lag = f$window_or_lag, model_tier = f$model_tier, coefficient = f$coefficient, standard_error = f$standard_error, ci_lower = f$ci_lower, ci_upper = f$ci_upper,
    legacy_adjusted_score_max_abs = firth_score, stringsAsFactors = FALSE)
  o <- tabs$ordinal_integration; orf <- data.frame(outcome = o$outcome, metric = o$metric, window_or_lag = o$window_or_lag, n = o$n, coefficient = o$coefficient, se = o$se, odds_ratio = o$odds_ratio, p = o$p, q80_ordinal = o$q80_ordinal)
  t <- tabs$exposure_transformations; tr <- t[, intersect(c("outcome", "metric", "window_or_lag", "transformation", .siw_cum_stats, "q_transformation_outcome", "q_transformation_global"), names(t))]
  n <- tabs$nonlinear_forms; nl <- n[, c("outcome", "metric", "window_or_lag", "form", "n", "joint_statistic", "joint_df", "joint_p", "aic", "aicc", "q_outcome_form", "q_global")]
  w <- function(x, f) utils::write.csv(x, file.path(dir, f), row.names = FALSE, na = ""); w(fr, "batchB_firth_reference.csv"); w(orf, "batchB_ordinal_reference.csv"); w(tr, "batchB_transformations_reference.csv"); w(nl, "batchB_nonlinear_reference.csv")
  invisible(list(firth = fr, ordinal = orf, transformation = tr, nonlinear = nl))
}
TABS <- list(firth_sensitivity = FG, ordinal_integration = OG, exposure_transformations = TG, nonlinear_forms = NG)

test_that("Batch B plan: four required references with the documented counts; every key and required column is part of the output schema", {
  p <- siw_load_batch_b_plan(); expect_equal(length(p$references), 4L); expect_true(all(vapply(p$references, function(r) isTRUE(r$required), logical(1))))
  expect_equal(vapply(p$references, function(r) as.integer(unlist(r$expected_records_by_file)), 1L), c(560L, 80L, 1080L, 360L)); expect_true(nzchar(p$plan_hash))
  sch <- siw_batch_b_output_columns()
  for (r in p$references) { expect_true(all(unlist(r$keys) %in% sch[[r$result]])); expect_true(all(unlist(r$required_columns) %in% sch[[r$result]])) }
  expect_true(isTRUE(p$references[[3]]$retain_all_rows)); expect_true(isTRUE(p$references[[1]]$legacy_convergence_bound))
  expect_identical(siw_load_config()$tolerances$cumulative_abs, 1e-7); expect_identical(siw_load_config()$tolerances$cumulative_rel, 1e-6)       # existing tolerances unchanged
})

test_that("Batch B validator: exact match passes; absent, undeclared, duplicated, missing, non-finite and short references FAIL (never pass)", {
  d <- file.path(tempdir(), "bb_ref_ok"); mk_ref(TABS, d); v <- siw_validate_batch_b(TABS, CFG, d)
  expect_true(all(grepl("^PASS", v$summary$status))); expect_equal(v$summary$n_compared > 0, rep(TRUE, 4)); expect_equal(v$summary$n_records_reference, c(560L, 80L, 1080L, 360L)); expect_equal(siw_overall_validation_status(v$summary), "PASS")
  expect_true(all(v$summary$n_different == 0L)); expect_true(all(v$summary$n_missing_required == 0L))
  e <- file.path(tempdir(), "bb_ref_empty"); dir.create(e, showWarnings = FALSE); ve <- siw_validate_batch_b(TABS, CFG, e); expect_true(all(ve$summary$status == "FAIL")); expect_equal(siw_overall_validation_status(ve$summary), "FAIL")
  # a reference file that is not declared in the plan
  u <- file.path(tempdir(), "bb_ref_undeclared"); mk_ref(TABS, u); file.rename(file.path(u, "batchB_ordinal_reference.csv"), file.path(u, "batchB_ordinal_reference_other.csv")); expect_true(siw_validate_batch_b(TABS, CFG, u)$summary$status[2] == "FAIL")
  # missing record, duplicated key, missing required column, non-finite disagreement
  r <- mk_ref(TABS, file.path(tempdir(), "bb_ref_x")); x <- file.path(tempdir(), "bb_ref_x")
  utils::write.csv(r$ordinal[-5, ], file.path(x, "batchB_ordinal_reference.csv"), row.names = FALSE, na = ""); s <- siw_validate_batch_b(TABS, CFG, x)$summary; expect_identical(s$status[2], "FAIL"); expect_true(s$n_unmatched[2] >= 1L)
  utils::write.csv(rbind(r$ordinal, r$ordinal[1, ]), file.path(x, "batchB_ordinal_reference.csv"), row.names = FALSE, na = ""); s <- siw_validate_batch_b(TABS, CFG, x)$summary; expect_identical(s$status[2], "FAIL"); expect_true(s$n_duplicate[2] >= 1L)
  utils::write.csv(r$ordinal[, setdiff(names(r$ordinal), "q80_ordinal")], file.path(x, "batchB_ordinal_reference.csv"), row.names = FALSE, na = ""); s <- siw_validate_batch_b(TABS, CFG, x)$summary; expect_identical(s$status[2], "FAIL"); expect_true(s$n_missing_required[2] >= 1L)
  o2 <- r$ordinal; o2$p[3] <- NA; utils::write.csv(o2, file.path(x, "batchB_ordinal_reference.csv"), row.names = FALSE, na = ""); s <- siw_validate_batch_b(TABS, CFG, x)$summary; expect_identical(s$status[2], "FAIL"); expect_true(s$n_nonfinite[2] >= 1L)
  utils::write.csv(r$nonlinear[1:359, ], file.path(x, "batchB_nonlinear_reference.csv"), row.names = FALSE, na = ""); utils::write.csv(r$ordinal, file.path(x, "batchB_ordinal_reference.csv"), row.names = FALSE, na = "")
  s <- siw_validate_batch_b(TABS, CFG, x)$summary; expect_identical(s$status[4], "FAIL")
  # a result table that is not produced is a FAIL, not a skip
  expect_identical(siw_validate_batch_b(TABS[-3], CFG, d)$summary$status[3], "FAIL")
  # cell outside the cumulative tolerance in a non-Firth table is DIFFERENT and fails; the Firth bound is never applied to other references
  n2 <- r$nonlinear; n2$joint_p[7] <- n2$joint_p[7] + 1e-4; utils::write.csv(n2, file.path(x, "batchB_nonlinear_reference.csv"), row.names = FALSE, na = ""); s <- siw_validate_batch_b(TABS, CFG, x)$summary
  expect_identical(s$status[4], "FAIL"); expect_true(s$n_different[4] == 1L); expect_equal(s$n_explained_by_legacy_bound[4], 0L)
})

test_that("Batch B validator: transformation rows other than raw are retained (retain_all_rows) while the core validator still filters them", {
  d <- file.path(tempdir(), "bb_ref_trn"); mk_ref(TABS, d); s <- siw_validate_batch_b(TABS, CFG, d)$summary; expect_equal(s$n_records_reference[3], 1080L)
  ref <- utils::read.csv(file.path(d, "batchB_transformations_reference.csv"), check.names = FALSE); pl_keep <- list(required = TRUE, scope = "t", expected_records_by_file = list(f.csv = 1080L), required_columns = list("coefficient"), retain_all_rows = TRUE)
  tmp <- file.path(tempdir(), "bb_filter"); dir.create(tmp, showWarnings = FALSE); utils::write.csv(ref, file.path(tmp, "f.csv"), row.names = FALSE)
  spec <- list(name = "t", files = list("f.csv"), keys = list("outcome", "metric", "window_or_lag", "transformation"), result = "exposure_transformations", tolerance = "cumulative", aliases = list(), plan = pl_keep)
  expect_equal(.siw_validate_one(TABS, CFG, tmp, spec)$summary$n_records_reference, 1080L)
  spec$plan$retain_all_rows <- NULL; spec$plan$expected_records_by_file <- list(f.csv = 240L)
  r0 <- .siw_validate_one(TABS, CFG, tmp, spec)$summary; expect_equal(r0$n_records_reference, 240L)        # without the flag only the raw rows (3 outcomes x 8 metrics x 10 windows) are kept, as for the core references
})

test_that("Firth frozen comparison: differences inside the declared legacy-convergence bound are labelled and never plain PASS; differences outside it, and standard-error differences, FAIL", {
  d <- file.path(tempdir(), "bb_ref_firth"); r <- mk_ref(TABS, d, firth_score = 1e-3); bound <- sqrt(FG$parameter_count) * FG$covariance_row_norm_exposure * 1e-3
  expect_true(all(bound > 1e-5))
  shift <- function(frac, col = "coefficient", rows = 1:5) { x <- r$firth; x[[col]][rows] <- x[[col]][rows] + frac * bound[rows]; utils::write.csv(x, file.path(d, "batchB_firth_reference.csv"), row.names = FALSE, na = ""); siw_validate_batch_b(TABS, CFG, d) }
  v <- shift(0.5); s <- v$summary[1, ]; expect_identical(s$status, "PASS_EXPLAINED_BY_LEGACY_CONVERGENCE"); expect_equal(s$n_explained_by_legacy_bound, 5L); expect_equal(s$n_different, 0L)
  expect_true(any(v$detail$class == "DIFFERENT_EXPLAINED_BY_LEGACY_CONVERGENCE")); expect_equal(sum(v$attribution$explained), 5L); expect_true(all(v$attribution$abs_diff_over_bound < 1)); expect_identical(siw_overall_validation_status(v$summary), "PASS")
  expect_false(identical(s$status, "PASS_EXACT")); expect_false(identical(s$status, "PASS_WITHIN_TOLERANCE"))
  v2 <- shift(5); s2 <- v2$summary[1, ]; expect_identical(s2$status, "FAIL"); expect_equal(s2$n_different, 5L); expect_equal(s2$n_explained_by_legacy_bound, 0L)
  v3 <- shift(0.5, "standard_error"); expect_identical(v3$summary$status[1], "FAIL"); expect_true(v3$summary$n_different[1] >= 1L)       # standard errors are never excused
  v4 <- shift(0.5, "ci_upper"); expect_identical(v4$summary$status[1], "PASS_EXPLAINED_BY_LEGACY_CONVERGENCE")
  x <- r$firth; x$legacy_adjusted_score_max_abs <- NULL; x$coefficient[1] <- x$coefficient[1] + 0.5 * bound[1]; utils::write.csv(x, file.path(d, "batchB_firth_reference.csv"), row.names = FALSE, na = ""); expect_identical(siw_validate_batch_b(TABS, CFG, d)$summary$status[1], "FAIL")   # no recorded score, no excuse
  x <- r$firth; x$legacy_adjusted_score_max_abs <- 1e-12; x$coefficient[1] <- x$coefficient[1] + 1e-3; utils::write.csv(x, file.path(d, "batchB_firth_reference.csv"), row.names = FALSE, na = ""); expect_identical(siw_validate_batch_b(TABS, CFG, d)$summary$status[1], "FAIL")  # a converged frozen fit cannot excuse a difference
})

test_that("Batch B self-checks and verification summary: consistent tables pass; NOT_TESTED is never PASS", {
  bt <- siw_batch_b_tables(DAT, CFG, CORE, verify = FALSE); expect_true(all(c("firth_sensitivity", "ordinal_integration", "exposure_transformations", "nonlinear_forms", "batch_b_math_verification") %in% names(bt)))
  sc <- siw_batch_b_self_checks(bt, CFG, CORE); expect_true(all(sc$status == "PASS")); expect_true(all(bt$batch_b_math_verification$status == "NOT_TESTED")); expect_equal(siw_batch_b_math_status(bt$batch_b_math_verification), "NOT_TESTED")
  m <- data.frame(component = c("a", "b"), status = c("PASS", "PASS_WITH_DOCUMENTED_POLR_LIMITATION")); expect_equal(siw_batch_b_math_status(m), "PASS_WITH_DOCUMENTED_LIMITATION")
  expect_equal(siw_batch_b_math_status(data.frame(component = "a", status = c("PASS", "FAIL"))), "FAIL"); expect_equal(siw_batch_b_math_status(data.frame(component = "a", status = c("PASS", "NOT_TESTED"))), "NOT_TESTED")
  bad <- bt; ir <- which(bad$exposure_transformations$transformation == "raw")[1]; bad$exposure_transformations$coefficient[ir] <- bad$exposure_transformations$coefficient[ir] + 1
  expect_true(siw_batch_b_self_checks(bad, CFG, CORE)$status[siw_batch_b_self_checks(bad, CFG, CORE)$check == "raw_transformation_equals_core_M2L"] == "FAIL")   # the cross-check against the validated M2-L results detects a corrupted raw row
  bt2 <- bt; bt2$ordinal_integration$q80_ordinal[1] <- NA; expect_true(any(siw_batch_b_self_checks(bt2, CFG, CORE)$status == "FAIL"))
  sch <- siw_output_columns(); expect_true(all(names(siw_batch_b_output_columns()) %in% names(sch))); for (nm in names(siw_batch_b_output_columns())) if (nm %in% names(bt) && is.data.frame(bt[[nm]]) && nrow(bt[[nm]])) expect_identical(names(siw_conform(bt[[nm]], sch[[nm]])), sch[[nm]])
})

test_that("Batch B leaves the data, the primary results and Batch A untouched", {
  d0 <- DAT; invisible(siw_firth_grid(DAT, CFG, CORE, verify = FALSE)); invisible(siw_ordinal_grid(DAT, CFG)); invisible(siw_transformation_grid(DAT, CFG)); invisible(siw_nonlinear_grid(DAT, CFG)); expect_identical(DAT, d0)
  core2 <- siw_cumulative_grid(DAT, CFG)$results; expect_identical(core2, CORE); expect_identical(siw_load_config()$config_hash, CFG$config_hash)
  a <- siw_cumulative_diagnostics(DAT, CFG); expect_equal(nrow(a), 1680L); expect_equal(nrow(siw_bivariate_grid(DAT, CFG)), 240L)
  expect_true(exists("siw_run_batch_a", mode = "function") && exists("siw_batch_a_tables", mode = "function")); expect_identical(names(siw_batch_a_output_columns())[1], "batch_a_input_checks")
})

test_that("end to end: Batch B run on the synthetic CSV with synthetic references reports the separate statuses and a verified manifest", {
  skip_slow <- identical(Sys.getenv("SIW_SKIP_SLOW_TESTS"), "1"); if (skip_slow) return(invisible(NULL))
  out <- file.path(tempdir(), "bb_e2e_run"); refs <- file.path(tempdir(), "bb_e2e_refs"); dir.create(refs, showWarnings = FALSE)
  # synthetic references for the dry run: the legacy-equivalent tables themselves (mechanics only; the legacy comparison is validation/legacy_equivalence_batch_b_synthetic.R)
  coreref <- file.path(refs, "core"); dir.create(coreref, showWarnings = FALSE); utils::write.csv(CORE, file.path(coreref, "cumulative_regression_grid.csv"), row.names = FALSE)
  dlm <- siw_final_dlm(DAT, CFG, draws = CFG$dlm$final$draws)$summary; utils::write.csv(dlm[, setdiff(names(dlm), c("simultaneous_critical", "n_lags_simultaneous_excl_null"))], file.path(coreref, "DLM_L01_L56_nine_representative_summary.csv"), row.names = FALSE)
  ar <- file.path(refs, "a"); b <- file.path(refs, "b"); bt <- siw_batch_b_tables(DAT, CFG, CORE, verify = FALSE); mk_ref(list(firth_sensitivity = bt$firth_sensitivity, ordinal_integration = bt$ordinal_integration, exposure_transformations = bt$exposure_transformations, nonlinear_forms = bt$nonlinear_forms), b)
  at <- siw_batch_a_tables(SYN, DAT, CFG, CORE); dir.create(ar, showWarnings = FALSE)
  pa <- siw_load_batch_a_plan(); for (pl in pa$references) { tb <- at[[pl$result]]; utils::write.csv(tb, file.path(ar, unlist(pl$files)[1]), row.names = FALSE, na = "") }
  r <- siw_run_batch_b(SYN_CSV, out, coreref, ar, b, CFG, expected_assessment_days = NA_integer_)
  expect_true(all(c("EXECUTION_STATUS", "INPUT_CHECK_STATUS", "CORE_COMPARISON_STATUS", "BATCH_A_COMPARISON_STATUS", "BATCH_B_COMPARISON_STATUS", "BATCH_B_MATH_VERIFICATION_STATUS") %in% names(r$statuses)))
  expect_identical(r$statuses[["EXECUTION_STATUS"]], "PASS"); expect_identical(r$statuses[["BATCH_B_COMPARISON_STATUS"]], "PASS"); expect_true(r$statuses[["BATCH_B_MATH_VERIFICATION_STATUS"]] %in% c("PASS", "PASS_WITH_DOCUMENTED_LIMITATION"))
  for (f in c("firth_sensitivity.csv", "firth_verification.csv", "ordinal_integration.csv", "ordinal_verification.csv", "exposure_transformations.csv", "transformation_verification.csv", "nonlinear_forms.csv", "nonlinear_verification.csv",
              "batch_b_math_verification.csv", "batch_b_validation_summary.csv", "batch_b_validation_detail.csv", "batch_b_self_checks.csv", "batch_b_timings.csv", "BATCH_B_STATUS.txt", "batch_b_metadata.json", "batch_a_validation_summary.csv", "validation_summary.csv")) expect_true(file.exists(file.path(out, f)))
  expect_equal(nrow(utils::read.csv(file.path(out, "firth_sensitivity.csv"))), 560L); expect_equal(nrow(utils::read.csv(file.path(out, "ordinal_integration.csv"))), 80L)
  expect_equal(nrow(utils::read.csv(file.path(out, "exposure_transformations.csv"))), 1080L); expect_equal(nrow(utils::read.csv(file.path(out, "nonlinear_forms.csv"))), 360L)
  txt <- paste(readLines(file.path(out, "BATCH_B_STATUS.txt")), collapse = "\n"); expect_true(grepl("Results were not altered to fit any reference", txt))
  all_txt <- paste(unlist(lapply(list.files(out, pattern = "[.](csv|md|txt|json)$", full.names = TRUE), function(p) readLines(p, warn = FALSE))), collapse = "\n")
  expect_false(any(vapply(unique(as.character(SYN$participant_code)), function(cd) grepl(cd, all_txt, fixed = TRUE), logical(1))))      # no participant identifier value appears in any aggregate output
})
