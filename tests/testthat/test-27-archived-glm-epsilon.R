# J02: every archived L01-L59 entry point uses the archived glm control (epsilon 1e-10, maxit 200); the final L01-L56 models keep 1e-12. Synthetic data only.

test_that("J02 archived glm control is 1e-10 / 200, equals the archived protocol trace, and the final epsilon stays 1e-12", {
  g <- siw_archived_glm_control(CFG)
  expect_equal(g$glm_epsilon, 1e-10); expect_identical(g$glm_maxit, 200L)
  b <- siw_archived_validation_protocol()$specification_trace$binary_fit_control
  expect_equal(g$glm_epsilon, b$glm_epsilon); expect_equal(g$glm_maxit, as.integer(b$glm_maxit))
  expect_equal(CFG$dlm$final$glm_epsilon, 1e-12); expect_equal(CFG$dlm$final$glm_maxit, 200L)
})

test_that("J02 siw_archived_dlm_grid fits binary models at epsilon 1e-10 and leaves the caller's configuration untouched", {
  cfg0 <- CFG; a <- siw_archived_dlm_grid(DAT, CFG, tiers = "M2_L", draws = 0L)
  expect_identical(CFG, cfg0)
  c10 <- CFG; c10$dlm$final$glm_epsilon <- 1e-10; c10$dlm$final$glm_maxit <- 200L; xc <- siw_tier_matrix(DAT, CFG$tiers$M2_L)
  for (m in CFG$metrics$metric[1:3]) {
    ref <- siw_fit_dlm(DAT, c10, "Int_dummy", m, 59L, 3L, "M2_L", draws = 0L, return_curves = FALSE, x_cov = xc)$summary
    got <- a[a$outcome == "Int_dummy" & a$metric == m, ]
    for (cn in c("global_statistic", "global_p", "cumulative_effect", "cumulative_se", "peak_effect")) expect_identical(got[[cn]], ref[[cn]])
  }
})

test_that("J02 siw_archived_dlm_sensitivity fits binary models at epsilon 1e-10", {
  s <- siw_archived_dlm_sensitivity(DAT, CFG); c10 <- CFG; c10$dlm$final$glm_epsilon <- 1e-10; xc <- siw_tier_matrix(DAT, CFG$tiers$M2_L)
  ref <- siw_fit_dlm(DAT, c10, "Int_dummy", "UTCI", 45L, 4L, "M2_L", draws = 0L, return_curves = FALSE, x_cov = xc)$summary
  got <- s[s$outcome == "Int_dummy" & s$metric == "UTCI" & s$sensitivity_specification == "L45_DF4", ]
  expect_identical(got$global_statistic, ref$global_statistic); expect_identical(got$cumulative_se, ref$cumulative_se)
  expect_equal(nrow(s), 216L)
})

test_that("J02 the archived validator's own 1e-10 configuration copy gives identical archived results (Phase 3E outputs unchanged)", {
  pv <- siw_archived_validation_protocol(); cv <- .siw_av_config(CFG, pv)
  a1 <- siw_archived_dlm_grid(DAT, CFG, tiers = c("M1", "M2_L"), draws = 0L); a2 <- siw_archived_dlm_grid(DAT, cv, tiers = c("M1", "M2_L"), draws = 0L)
  expect_identical(a1, a2)
})

test_that("J02 Gaussian archived rows are bit-identical to the pre-J02 behaviour; binary rows change only at convergence level", {
  old <- CFG; old$dlm$archived$glm_epsilon <- 1e-12; old$dlm$archived$glm_maxit <- 200L
  n <- siw_archived_dlm_grid(DAT, CFG, tiers = "M2_L", draws = 0L); o <- siw_archived_dlm_grid(DAT, old, tiers = "M2_L", draws = 0L)
  gi <- n$outcome_family == "gaussian"; expect_identical(n[gi, ], o[gi, ])
  bi <- n$outcome_family == "binomial"
  expect_identical(n$status[bi], o$status[bi]); expect_identical(n$separation[bi], o$separation[bi]); expect_identical(n$convergence[bi], o$convergence[bi])
  expect_equal(n$global_p[bi], o$global_p[bi], tolerance = 1e-6); expect_equal(n$cumulative_effect[bi], o$cumulative_effect[bi], tolerance = 1e-6)
})

test_that("J02 the final L01-L56 models are unaffected", {
  f <- siw_final_dlm(DAT, CFG, draws = 0L)$summary; xc <- siw_tier_matrix(DAT, CFG$tiers$M2_L)
  r <- siw_fit_dlm(DAT, CFG, "Int_dummy", "UTCI", 56L, 3L, "M2_L", draws = 0L, return_curves = FALSE, x_cov = xc)$summary
  expect_identical(f$global_statistic[f$outcome == "Int_dummy" & f$metric == "UTCI"], r$global_statistic)
})

test_that("J02 archived checkpoints from the 1e-12 era cannot be resumed (stage versions bumped); other stages unchanged", {
  v <- .siw_stage_versions
  expect_identical(unname(v[c("archived_dlm", "archived_dlm_sensitivity")]), c("2", "2"))
  expect_identical(unname(v[c("cumulative", "dlm_final", "rainfall_sensitivity")]), c("1", "1", "1"))
})
