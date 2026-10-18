test_that("rainfall anomaly is detected and reported without altering values", {
  a <- siw_rainfall_anomalies(SYN, CFG); expect_equal(a$n_cells, 1L); expect_equal(a$n_participants, 1L); expect_equal(a$by_lag$lag, 35L)
  before <- SYN; r <- siw_rainfall_anomaly_report(SYN, CFG); expect_identical(SYN, before)
  expect_true(any(grepl("lag_35", r$item)))
  expect_false(grepl("SYN0", paste(unlist(r), collapse = " ")))
  clean <- SYN; clean$IMS_IDW_RAIN_TOTAL_L35[is.na(clean$IMS_IDW_RAIN_TOTAL_L35)] <- 0
  expect_equal(siw_rainfall_anomalies(clean, CFG)$n_cells, 0L)
})

test_that("rainfall total is not imputed in the primary preparation; the optional sensitivity is explicit", {
  d <- prep(); i <- which(is.na(SYN$IMS_IDW_RAIN_TOTAL_L35)); expect_true(is.na(d$IMS_IDW_RAIN_TOTAL_L35[i]))
  s <- siw_rainfall_zero_reconstruction_sensitivity(siw_prepare_data(SYN, CFG), CFG)
  expect_equal(s$IMS_IDW_RAIN_TOTAL_L35[i], 0); expect_equal(attr(s, "sensitivity")$cells_replaced, 1L)
  expect_true(is.na(SYN$IMS_IDW_RAIN_TOTAL_L35[i]))
})
