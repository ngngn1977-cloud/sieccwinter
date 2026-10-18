mk <- function(binary = FALSE) {
  X <- siw_tier_matrix(DAT, CFG$tiers$M2_L); x <- DAT$UTCI_AVG_14D
  ok <- complete.cases(X) & is.finite(x)
  Xf <- cbind(X[ok, 1, drop = FALSE], exposure = x[ok], X[ok, -1, drop = FALSE])
  list(X = Xf, y = if (binary) DAT$Int_dummy[ok] else DAT$UCLA_Lonliness_SCORE[ok])
}

test_that("OLS coefficient, model SE and HC3 SE match a direct calculation", {
  m <- mk(); f <- siw_fit_gaussian(m$X, m$y)
  fit <- lm.fit(m$X, m$y); e <- fit$residuals; XtXi <- solve(crossprod(m$X)); h <- rowSums((m$X %*% XtXi) * m$X)
  V3 <- XtXi %*% crossprod(m$X * (e / (1 - h))) %*% XtXi
  expect_equal(f$coefficient, unname(fit$coefficients[2]), tolerance = 1e-10)
  expect_equal(f$robust_hc3_se, sqrt(V3[2, 2]), tolerance = 1e-10)
  s2 <- sum(e^2) / (nrow(m$X) - ncol(m$X)); expect_equal(f$model_based_se, sqrt(s2 * XtXi[2, 2]), tolerance = 1e-10)
  expect_equal(f$nominal_p, 2 * pt(abs(f$coefficient / f$robust_hc3_se), nrow(m$X) - ncol(m$X), lower.tail = FALSE), tolerance = 1e-12)
})

test_that("logistic extraction matches glm", {
  m <- mk(TRUE); f <- siw_fit_logistic(m$X, m$y)
  g <- glm(m$y ~ m$X - 1, family = binomial(), control = glm.control(epsilon = 1e-8, maxit = 25)); s <- summary(g)$coefficients
  expect_equal(f$coefficient, unname(coef(g)[2]), tolerance = 1e-7)
  expect_equal(f$model_based_se, unname(s[2, 2]), tolerance = 1e-6)
  expect_equal(f$odds_ratio_raw, exp(unname(coef(g)[2])), tolerance = 1e-7)
  g0 <- glm(m$y ~ m$X[, -2] - 1, family = binomial()); lr <- deviance(g0) - deviance(g)
  expect_equal(f$lr_statistic, lr, tolerance = 1e-6); expect_equal(f$lr_df, 1)
})

test_that("non-estimable models are reported, not fitted", {
  m <- mk(); X <- cbind(m$X, dup = m$X[, 2]); f <- siw_fit_gaussian(X, m$y)
  expect_false(f$estimable); expect_equal(f$reason, "RANK_DEFICIENT")
  f2 <- siw_fit_logistic(m$X, rep(0, nrow(m$X))); expect_false(f2$estimable)
})

test_that("reduced-model cache does not change results", {
  m <- mk(); cache <- new.env()
  a <- siw_fit_gaussian(m$X, m$y, cache = cache, cache_key = "k"); b <- siw_fit_gaussian(m$X, m$y, cache = cache, cache_key = "k"); c0 <- siw_fit_gaussian(m$X, m$y)
  expect_identical(a$partial_r_squared_weather, b$partial_r_squared_weather); expect_equal(a$partial_r_squared_weather, c0$partial_r_squared_weather, tolerance = 1e-14)
})
