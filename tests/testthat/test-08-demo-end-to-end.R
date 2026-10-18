test_that("the synthetic demo runs end to end and outputs conform to the schemas", {
  out <- file.path(tempdir(), "demo_test"); r <- siw_run_demo(out, CFG, resume = FALSE)
  expect_equal(r$status, "OK")
  need <- c("cumulative_results.csv", "dlm_final_summary.csv", "dlm_final_lag_curves.csv", "input_validation_report.csv", "run_metadata.json", "MANIFEST.csv", "self_checks.csv", "timings.csv")
  expect_true(all(file.exists(file.path(out, need))))
  cr <- read.csv(file.path(out, "cumulative_results.csv")); expect_equal(nrow(cr), 1680L)
  ds <- read.csv(file.path(out, "dlm_final_summary.csv")); expect_equal(nrow(ds), 9L)
  expect_equal(nrow(read.csv(file.path(out, "dlm_final_lag_curves.csv"))), 9L * 56L)
  for (nm in c("cumulative_results", "dlm_final_summary", "dlm_final_lag_curves", "complete_case_accounting")) {
    sch <- file.path(ROOT, "output_schema", paste0(nm, ".schema.json"))
    # schemas live in the repository root; they are not present in an installed package
    if (file.exists(sch)) expect_true(siw_check_schema(read.csv(file.path(out, paste0(nm, ".csv"))), sch)$ok)
  }
  files <- list.files(out, full.names = TRUE, recursive = TRUE)
  txt <- paste(unlist(lapply(files[grepl("\\.(csv|json|txt|log|md)$", files)], readLines, warn = FALSE)), collapse = "\n")
  expect_false(grepl("SYN0[0-9][0-9]", txt))                                                  # no participant codes in any output
  out2 <- file.path(tempdir(), "demo_test2"); siw_run_demo(out2, CFG, resume = FALSE)
  for (f in c("cumulative_results.csv", "dlm_final_summary.csv", "dlm_final_lag_curves.csv"))
    expect_identical(unname(tools::md5sum(file.path(out, f))), unname(tools::md5sum(file.path(out2, f))))     # deterministic
})

test_that("a checkpointed stage is resumed and gives identical results", {
  out <- file.path(tempdir(), "demo_resume"); siw_run_demo(out, CFG, resume = FALSE); h1 <- unname(tools::md5sum(file.path(out, "cumulative_results.csv")))
  r2 <- siw_run_demo(out, CFG, resume = TRUE)
  expect_true(any(r2$timings$status == "RESTORED"))
  expect_identical(unname(tools::md5sum(file.path(out, "cumulative_results.csv"))), h1)
})

test_that("validate mode reports zero discrepancies against its own results", {
  out <- file.path(tempdir(), "demo_val"); siw_run_demo(out, CFG, resume = FALSE)
  refd <- file.path(tempdir(), "ref_dir"); dir.create(refd, showWarnings = FALSE)
  file.copy(file.path(out, "cumulative_results.csv"), file.path(refd, "cumulative_regression_grid.csv"), overwrite = TRUE)
  file.copy(file.path(out, "dlm_final_summary.csv"), file.path(refd, "DLM_L01_L56_nine_representative_summary.csv"), overwrite = TRUE)
  v <- file.path(tempdir(), "val_out")
  res <- siw_run_validate(SYN_CSV, refd, v, CFG, resume = FALSE)
  expect_true(file.exists(file.path(v, "validation_summary.csv")))
  s <- read.csv(file.path(v, "validation_summary.csv"))
  expect_equal(sum(grepl("^PASS", s$status)), 2L); expect_equal(sum(s$status == "NOT_TESTED"), 1L)
  expect_true(all(s$n_different == 0L))
  # a perturbed reference is reported as a discrepancy and the results are not changed
  bad <- read.csv(file.path(refd, "cumulative_regression_grid.csv")); bad$coefficient[3] <- bad$coefficient[3] + 1
  write.csv(bad, file.path(refd, "cumulative_regression_grid.csv"), row.names = FALSE)
  v2 <- file.path(tempdir(), "val_out2"); r2 <- siw_run_validate(SYN_CSV, refd, v2, CFG, resume = FALSE)
  expect_equal(r2$overall, "FAIL"); expect_true(sum(read.csv(file.path(v2, "validation_summary.csv"))$n_different) >= 1L)
  expect_identical(unname(tools::md5sum(file.path(v2, "cumulative_results.csv"))), unname(tools::md5sum(file.path(out, "cumulative_results.csv"))))
})
