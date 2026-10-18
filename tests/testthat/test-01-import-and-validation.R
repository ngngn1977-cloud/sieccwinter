test_that("CSV import enforces numeric types and refuses SPSS files", {
  d <- siw_read_study_csv(SYN_CSV, CFG)
  expect_true(is.numeric(d$UCLA_Lonliness_SCORE)); expect_true(is.numeric(d$UTCI_L01)); expect_true(is.character(d$participant_code))
  expect_equal(nrow(d), 60L)
  expect_error(siw_read_study_csv("study.sav", CFG), "SPSS")
  tmp <- tempfile(fileext = ".csv"); x <- SYN; x$Age <- as.character(x$Age); x$Age[2] <- "abc"; siw_write_csv(x, tmp)
  r <- siw_validate_input(siw_read_study_csv(tmp, CFG), CFG)
  expect_equal(r$status[r$check_id == "V03"], "FAIL")
})

test_that("clean synthetic data pass validation with only the intended warning", {
  r <- siw_validate_input(siw_read_study_csv(SYN_CSV, CFG), CFG)
  expect_equal(sum(r$status == "FAIL"), 0L)
  expect_equal(r$status[r$check_id == "V14"], "WARN")            # the synthetic rainfall edge case
  expect_equal(r$n_affected[r$check_id == "V14"], 1L)
})

test_that("validation detects structural problems without altering the data", {
  chk <- function(x, id, status) { r <- siw_validate_input(x, CFG); expect_equal(r$status[r$check_id == id], status) }
  x <- SYN; x$Sex <- NULL;                       chk(x, "V01", "FAIL")
  x <- SYN; x$participant_code[2] <- x$participant_code[1]; chk(x, "V02", "FAIL")
  x <- SYN; x$Area[3] <- 4;                      chk(x, "V04_Area", "FAIL")
  x <- SYN; x$int_dummy[1] <- 1 - x$int_dummy[1]; chk(x, "V10", "FAIL")
  x <- SYN; x$UTCI_L05 <- NULL;                  chk(x, "V05", "FAIL")
  x <- SYN; x$IMS_IDW_RAIN_TOTAL_L10[4] <- -1;   chk(x, "V08", "FAIL")
  x <- SYN; i <- which(x$Jewish == 0)[1]; x$Muslim[i] <- 0; x$Christian[i] <- 0; chk(x, "V11", "WARN")
  x <- SYN; x$Memory_assess[1] <- 1 - x$Memory_assess[1]; chk(x, "V12", "WARN")
  x <- SYN; x$UTCI_AVG_7D[2] <- x$UTCI_AVG_7D[2] + 1; chk(x, "V13", "WARN")
  x <- SYN; before <- x; invisible(siw_validate_input(x, CFG)); expect_identical(x, before)
})

test_that("validation reports never contain participant codes or row values", {
  x <- SYN; x$participant_code[2] <- x$participant_code[1]
  r <- siw_validate_input(x, CFG); txt <- paste(unlist(r), collapse = " ")
  expect_false(grepl("SYN0", txt))
})

test_that("incomplete cumulative field sets are rejected", {
  x <- SYN; x$UTCI_AVG_7D <- NULL
  expect_equal(siw_validate_input(x, CFG)$status[siw_validate_input(x, CFG)$check_id == "V13"], "FAIL")
})
