LAG_L <- 56L
fit_ref <- function(outcome, metric) {
  sp <- CFG$metrics[CFG$metrics$metric == metric, ]
  Xc <- siw_tier_matrix(DAT, CFG$tiers$M2_L); H <- as.matrix(DAT[, siw_daily_names(sp$daily_prefix, LAG_L)]); y <- DAT[[outcome]]
  ok <- complete.cases(Xc) & is.finite(y) & rowSums(!is.finite(H)) == 0
  B <- splines::ns(1:LAG_L, df = 3, intercept = TRUE); cb <- H[ok, ] %*% B
  list(X = cbind(Xc[ok, ], cb), Xc = Xc[ok, ], y = y[ok], cb = cb, B = B)
}

test_that("lag basis and cross-basis have the expected dimensions", {
  B <- siw_lag_basis(LAG_L, 3); expect_equal(dim(B), c(56L, 3L))
  expect_equal(unname(B), unname(matrix(as.numeric(splines::ns(1:56, df = 3, intercept = TRUE)), 56)), tolerance = 1e-14)
  H <- as.matrix(DAT[, siw_daily_names("UTCI", 56)]); expect_equal(dim(H), c(nrow(DAT), 56L))
  expect_equal(dim(siw_crossbasis(H, B)), c(nrow(DAT), 3L))
  expect_equal(dim(siw_lag_basis(59, 3)), c(59L, 3L))
  expect_error(siw_crossbasis(H[, 1:55], B))
})

test_that("nested F global test equals anova for the continuous outcome", {
  r <- fit_ref("UCLA_Lonliness_SCORE", "UTCI")
  a <- anova(lm(r$y ~ r$Xc - 1), lm(r$y ~ r$X - 1))
  f <- siw_fit_dlm(DAT, CFG, "UCLA_Lonliness_SCORE", "UTCI", draws = 0L)$summary
  expect_equal(f$global_statistic, a$F[2], tolerance = 1e-8); expect_equal(f$global_p, a$`Pr(>F)`[2], tolerance = 1e-8)
  expect_equal(f$global_df1, 3); expect_equal(f$n, nrow(r$X))
})

test_that("likelihood-ratio global test equals anova for the binary outcome", {
  r <- fit_ref("Int_dummy", "UTCI")
  ctl <- glm.control(epsilon = 1e-12, maxit = 200)
  g1 <- glm(r$y ~ r$X - 1, family = binomial(), control = ctl); g0 <- glm(r$y ~ r$Xc - 1, family = binomial(), control = ctl)
  f <- siw_fit_dlm(DAT, CFG, "Int_dummy", "UTCI", draws = 0L)$summary
  expect_equal(f$global_statistic, deviance(g0) - deviance(g1), tolerance = 1e-6)
  expect_equal(f$global_p, anova(g0, g1, test = "Chisq")$`Pr(>Chi)`[2], tolerance = 1e-6)
})

test_that("lag-specific and cumulative effects follow B %*% beta", {
  r <- fit_ref("UCLA_Lonliness_SCORE", "UTCI"); b <- coef(lm(r$y ~ r$X - 1)); bc <- b[(ncol(r$Xc) + 1):length(b)]
  o <- siw_fit_dlm(DAT, CFG, "UCLA_Lonliness_SCORE", "UTCI", draws = 0L)
  expect_equal(o$lag_curve$estimate, drop(r$B %*% bc), tolerance = 1e-8)
  expect_equal(o$cumulative_curve$cumulative_effect, cumsum(drop(r$B %*% bc)), tolerance = 1e-8)
  expect_equal(o$summary$cumulative_effect, sum(drop(r$B %*% bc)), tolerance = 1e-8)
  expect_equal(nrow(o$lag_curve), 56L)
})

test_that("simultaneous bands are deterministic for a fixed seed and wider than pointwise bands", {
  a <- siw_fit_dlm(DAT, CFG, "UCLA_Lonliness_SCORE", "UTCI", draws = 500L); b <- siw_fit_dlm(DAT, CFG, "UCLA_Lonliness_SCORE", "UTCI", draws = 500L)
  expect_identical(a$summary$simultaneous_critical, b$summary$simultaneous_critical); expect_identical(a$lag_curve, b$lag_curve)
  expect_true(a$summary$simultaneous_critical > 1.96)
  expect_true(all(a$lag_curve$simultaneous_upper_95 >= a$lag_curve$upper_95))
  expect_equal(a$summary$seed, 20260729L + 100L * 1L + 1L)
  c2 <- siw_fit_dlm(DAT, CFG, "UCLA_Lonliness_SCORE", "UTCI", draws = 500L, seed = 99L)
  expect_false(identical(a$summary$simultaneous_critical, c2$summary$simultaneous_critical))
})

test_that("sign-normalised MVN draws are stable under tiny covariance perturbations", {
  set.seed(3); A <- matrix(rnorm(9), 3); S <- crossprod(A); S2 <- S + 1e-10 * crossprod(matrix(rnorm(9), 3))
  d1 <- siw_with_seed(5, siw_mvn_draws(200, S, "eigen_sign_normalised")); d2 <- siw_with_seed(5, siw_mvn_draws(200, S2, "eigen_sign_normalised"))
  expect_equal(d1, d2, tolerance = 1e-6)
  expect_equal(dim(siw_with_seed(5, siw_mvn_draws(10, S, "MASS_mvrnorm"))), c(10L, 3L))
  expect_error(siw_mvn_draws(5, S, "nope"), "UNKNOWN_MVN")
})

test_that("a missing lag value excludes the participant from every DLM (complete-history rule)", {
  full <- siw_fit_dlm(DAT, CFG, "UCLA_Lonliness_SCORE", "ERA5_air_temperature", draws = 0L)$summary$n
  d <- DAT; had <- is.finite(DAT$T2M_L20[5]); d$T2M_L20[5] <- NA
  expect_equal(siw_fit_dlm(d, CFG, "UCLA_Lonliness_SCORE", "ERA5_air_temperature", draws = 0L)$summary$n, full - as.integer(had))
})
