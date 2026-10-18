test_that("cumulative mean, sum and count follow the declared rule", {
  rec <- siw_reconstruct_cumulative(SYN, CFG)
  H <- as.matrix(SYN[, sprintf("UTCI_L%02d", 1:7)]); expect_equal(rec$UTCI_AVG_7D, rowMeans(H), tolerance = 1e-12)
  R <- as.matrix(SYN[, sprintf("IMS_IDW_RAIN_TOTAL_L%02d", 1:56)]); expect_equal(rec$IMS_RAIN_IDW_TOTAL_56D, rowSums(R, na.rm = TRUE), tolerance = 1e-10)
  C <- as.matrix(SYN[, sprintf("IMS_IDW_RAIN_DAYS1_L%02d", 1:28)]); expect_equal(rec$IMS_RAIN_DAYS_GE1MM_IDW_28D, rowSums(C), tolerance = 1e-12)
  expect_true(all(rec$IMS_RAIN_DAYS_GE1MM_IDW_56D == round(rec$IMS_RAIN_DAYS_GE1MM_IDW_56D)))     # counts are integers
})

test_that("missing days are omitted, not imputed; all-missing windows are missing", {
  x <- SYN[1:3, ]; x$T2M_L02[1] <- NA
  x[2, sprintf("T2M_L%02d", 1:3)] <- NA; x[3, sprintf("T2M_L%02d", 1:7)] <- NA
  rec <- siw_reconstruct_cumulative(x, CFG)
  expect_equal(rec$AIR_TEMP_AVG_3D[1], mean(unlist(x[1, sprintf("T2M_L%02d", c(1, 3))])), tolerance = 1e-12)   # available-day mean
  expect_true(is.na(rec$AIR_TEMP_AVG_3D[2]))                                                              # all three days missing
  expect_true(is.na(rec$AIR_TEMP_AVG_7D[3]) && is.finite(rec$AIR_TEMP_AVG_14D[3]))
})

test_that("supplied cumulative fields are used as given; otherwise they are reconstructed", {
  d <- siw_prepare_data(SYN, CFG)
  expect_equal(siw_attach_cumulative(d, CFG)$source, "supplied")
  cn <- siw_cumulative_columns(CFG); d2 <- d[, !(names(d) %in% cn)]
  a <- siw_attach_cumulative(d2, CFG); expect_equal(a$source, "reconstructed")
  expect_equal(a$data$UTCI_AVG_21D, SYN$UTCI_AVG_21D, tolerance = 1e-12)
  d$UTCI_AVG_7D[1] <- 99; expect_equal(siw_attach_cumulative(d, CFG)$data$UTCI_AVG_7D[1], 99)
})

test_that("complete-case selection and accounting are correct", {
  g <- siw_cumulative_grid(DAT, CFG, tiers = c("M2_L", "M3", "M4_CC"))
  acc <- g$accounting; res <- g$results
  n3 <- acc$n_complete[res$model_tier == "M3"]; n4 <- acc$n_complete[res$model_tier == "M4_CC"]; n2 <- acc$n_complete[res$model_tier == "M2_L"]
  expect_true(all(n2 == 60L)); expect_true(all(n3 == 59L)); expect_true(all(n4 == 58L))         # missing PSQI (M3, M4) and Outdoor_Mobility (M4)
  expect_true(all(acc$n_excluded == acc$n_input - acc$n_complete))
  expect_true(all(res$n[res$model_tier == "M4_CC"] == 58L))
  expect_true(all(grepl("PSQI", acc$covariates_with_missing[res$model_tier == "M3"])))
})
