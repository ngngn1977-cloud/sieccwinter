test_that("the two copies of the configuration are identical (source checkout)", {
  a <- file.path(ROOT, "config", "sieccwinter_config.json"); b <- file.path(ROOT, "inst", "extdata", "sieccwinter_config.json")
  if (file.exists(a) && file.exists(b)) expect_identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
  expect_true(file.exists(SYN_CSV))
})

test_that("analytical defaults are the frozen ones", {
  expect_identical(CFG$dlm$final$mvn_method, "MASS_mvrnorm")
  expect_equal(CFG$glm$cumulative_epsilon, 1e-8); expect_equal(CFG$dlm$final$glm_epsilon, 1e-12)
  expect_equal(CFG$dlm$final$lag_end, 56L); expect_equal(CFG$dlm$final$draws, 5000L); expect_equal(CFG$seed, 20260729L)
})

test_that("no absolute development path is embedded in the R code (source checkout)", {
  rd <- file.path(ROOT, "R")
  if (dir.exists(rd)) {
    txt <- unlist(lapply(list.files(rd, full.names = TRUE, pattern = "[.]R$"), readLines, warn = FALSE))
    expect_false(any(grepl("[A-Za-z]:[/\\](Users|Program)", txt)))
  }
})
