G <- siw_cumulative_grid(DAT, CFG)
RES <- G$results

test_that("BH function equals p.adjust", {
  p <- c(0.001, 0.02, 0.03, 0.5, 0.9, NA)
  expect_equal(siw_bh(p[1:5]), p.adjust(p[1:5], "BH"), tolerance = 1e-14)
  set.seed(1); p <- runif(40); expect_equal(siw_bh(p), p.adjust(p, "BH"), tolerance = 1e-14)
})

test_that("the cumulative grid has 1,680 specifications with unique stable ids", {
  expect_equal(nrow(RES), 1680L); expect_equal(anyDuplicated(RES$result_id), 0L)
  expect_true("CUM__UCLA__UTCI__W7__M2_L" %in% RES$result_id)
  expect_equal(siw_cum_id("INT", "IMS_RAIN", 56, "M4_CC"), "CUM__INT__IMS_RAIN__W56__M4_CC")
  expect_equal(siw_dlm_id("UCLA", "UTCI", 56, 3, "M2_L"), "DLM__UCLA__UTCI__L56__DF3__M2_L")
  G2 <- siw_cumulative_grid(DAT, CFG); expect_identical(G2$results$result_id, RES$result_id)          # deterministic order
  expect_equal(sum(RES$model_tier == "M2_L"), 240L)
})

test_that("q10, q80 and q90 are assigned only within their families", {
  P <- RES[RES$model_tier == "M2_L" & RES$status == "COMPLETED", ]
  expect_equal(sum(!is.na(RES$q10)), nrow(P)); expect_true(all(is.na(RES$q10[RES$model_tier != "M2_L"])))
  for (o in unique(P$outcome)) {
    for (m in unique(P$metric)) { i <- P$outcome == o & P$metric == m; expect_equal(P$q10[i], p.adjust(P$nominal_p[i], "BH"), tolerance = 1e-12); expect_equal(sum(i), 10L) }
    i <- P$outcome == o; expect_equal(P$q80[i], p.adjust(P$nominal_p[i], "BH"), tolerance = 1e-12); expect_equal(sum(i), 80L)
  }
  i <- P$metric %in% CFG$representative_metrics; expect_equal(sum(i), 90L); expect_equal(P$q90[i], p.adjust(P$nominal_p[i], "BH"), tolerance = 1e-12)
  expect_true(all(is.na(P$q90[!i])))
})

test_that("q9 is BH across the nine final DLM global tests; q24 and q216 across the archived families", {
  f <- siw_final_dlm(DAT, CFG, draws = 50L)$summary
  expect_equal(nrow(f), 9L); expect_equal(f$q9, p.adjust(f$global_p, "BH"), tolerance = 1e-12)
  a <- siw_archived_dlm_grid(DAT, CFG, tiers = "M2_L", draws = 20L)
  k <- !is.na(a$q24); expect_equal(sum(k), 24L); expect_equal(a$q24[k], p.adjust(a$global_p[k], "BH"), tolerance = 1e-12)
  s <- siw_archived_dlm_sensitivity(DAT, CFG)
  expect_equal(nrow(s), 216L); expect_equal(s$q216, p.adjust(s$global_p, "BH"), tolerance = 1e-12)
})
