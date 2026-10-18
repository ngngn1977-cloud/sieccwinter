# Phase 2D regression tests of the validator on fully synthetic tables: every defect must give FAIL; a complete valid comparison passes.
V_G <- NULL; V_D <- NULL
v_tables <- function() {
  if (is.null(V_G)) { V_G <<- siw_cumulative_grid(DAT, CFG)$results; V_D <<- siw_final_dlm(DAT, CFG, draws = 50L)$summary }
  list(cumulative_results = V_G, dlm_final_summary = V_D)
}
v_refdir <- function(mutate = function(g, d) list(g = g, d = d), drop_files = character()) {
  tb <- v_tables(); d <- file.path(tempdir(), paste0("vref_", sample.int(1e6, 1))); dir.create(d)
  m <- mutate(tb$cumulative_results, tb$dlm_final_summary)
  if (!is.null(m$g) && !("cumulative_regression_grid.csv" %in% drop_files)) write.csv(m$g, file.path(d, "cumulative_regression_grid.csv"), row.names = FALSE)
  if (!is.null(m$d) && !("DLM_L01_L56_nine_representative_summary.csv" %in% drop_files)) write.csv(m$d, file.path(d, "DLM_L01_L56_nine_representative_summary.csv"), row.names = FALSE)
  d
}
v_run <- function(...) { rd <- v_refdir(...); siw_validate_results(v_tables(), CFG, rd) }
st <- function(v, name) v$summary$status[v$summary$reference == name]
cls_present <- function(v, cls) any(v$detail$class == cls)

test_that("a complete valid comparison passes; the archived reference is explicitly out of scope (NOT_TESTED, never PASS)", {
  v <- v_run()
  expect_true(grepl("^PASS", st(v, "cumulative"))); expect_true(grepl("^PASS", st(v, "dlm_final_L01_L56")))
  expect_equal(st(v, "dlm_archived_L01_L59"), "NOT_TESTED"); expect_false(v$summary$required[v$summary$reference == "dlm_archived_L01_L59"])
  expect_equal(siw_overall_validation_status(v$summary), "PASS")
  s <- v$summary; expect_equal(s$n_duplicate, c(0L, 0L, 0L)); expect_equal(s$n_missing_required[1:2], c(0L, 0L)); expect_equal(s$n_unclassified[1:2], c(0L, 0L))
  expect_equal(s$n_records_reference[1:2], c(1680L, 9L)); expect_equal(s$expected_records[1:2], c(1680L, 9L)); expect_true(all(s$n_compared[1:2] > 0))
  expect_equal(s$n_exact[1:2] + s$n_within_tolerance[1:2], s$n_compared[1:2])
  expect_equal(siw_overall_validation_status(data.frame(status = "NOT_TESTED", required = FALSE)), "NOT_TESTED")      # nothing compared is never PASS
  expect_equal(siw_overall_validation_status(data.frame(status = c("PASS_EXACT", "NOT_TESTED"), required = c(TRUE, TRUE))), "FAIL")   # a required NOT_TESTED is never PASS
})

test_that("an absent required reference table FAILS (not NOT_TESTED, not PASS)", {
  v <- v_run(drop_files = "DLM_L01_L56_nine_representative_summary.csv")
  expect_equal(st(v, "dlm_final_L01_L56"), "FAIL"); expect_equal(siw_overall_validation_status(v$summary), "FAIL")
  v2 <- v_run(drop_files = c("DLM_L01_L56_nine_representative_summary.csv", "cumulative_regression_grid.csv"))
  expect_equal(sum(v2$summary$status == "FAIL"), 2L); expect_equal(siw_overall_validation_status(v2$summary), "FAIL")
  tb <- v_tables(); tb$dlm_final_summary <- NULL; v3 <- siw_validate_results(tb, CFG, v_refdir()); expect_equal(st(v3, "dlm_final_L01_L56"), "FAIL")   # required result table not produced
})

test_that("missing required columns, missing keys and record-count mismatches FAIL", {
  v <- v_run(function(g, d) list(g = g[, setdiff(names(g), "q90")], d = d)); expect_equal(st(v, "cumulative"), "FAIL"); expect_true(cls_present(v, "REQUIRED_COLUMN_MISSING_REFERENCE"))
  v <- v_run(function(g, d) list(g = g, d = d[, setdiff(names(d), "global_p")])); expect_equal(st(v, "dlm_final_L01_L56"), "FAIL")
  v <- v_run(function(g, d) list(g = g[, setdiff(names(g), "outcome")], d = d)); expect_equal(st(v, "cumulative"), "FAIL"); expect_true(cls_present(v, "KEY_COLUMN_MISSING"))
  v <- v_run(function(g, d) list(g = g[-5, ], d = d)); expect_equal(st(v, "cumulative"), "FAIL"); expect_true(cls_present(v, "RECORD_COUNT_MISMATCH")); expect_true(cls_present(v, "UNMATCHED_RESULT_ONLY"))
  v <- v_run(function(g, d) list(g = g, d = d[-3, ])); expect_equal(st(v, "dlm_final_L01_L56"), "FAIL")
  extra <- function(g, d) { e <- g[1, ]; e$outcome <- "EXTRA_OUTCOME"; list(g = rbind(g, e), d = d) }; v <- v_run(extra); expect_equal(st(v, "cumulative"), "FAIL"); expect_true(cls_present(v, "UNMATCHED_REFERENCE_ONLY"))
  # a required column absent from the RESULTS table
  tb <- v_tables(); tb$cumulative_results$q10 <- NULL; v <- siw_validate_results(tb, CFG, v_refdir()); expect_equal(st(v, "cumulative"), "FAIL"); expect_true(cls_present(v, "REQUIRED_COLUMN_MISSING_RESULTS"))
  # a required column that is entirely missing on both sides is a skipped comparison, not a pass
  v <- v_run(function(g, d) list(g = replace(g, "q90", NA), d = d)); tb <- v_tables(); tb$cumulative_results$q90 <- NA_real_
  v2 <- siw_validate_results(tb, CFG, v_refdir(function(g, d) list(g = replace(g, "q90", NA), d = d))); expect_equal(st(v2, "cumulative"), "FAIL"); expect_true(cls_present(v2, "REQUIRED_COLUMN_NOT_COMPARED"))
})

test_that("duplicated or blank keys in results or references FAIL", {
  v <- v_run(function(g, d) { g[2, c("outcome", "metric", "window_or_lag", "model_tier")] <- g[1, c("outcome", "metric", "window_or_lag", "model_tier")]; list(g = g, d = d) }); expect_equal(st(v, "cumulative"), "FAIL"); expect_true(cls_present(v, "DUPLICATE_KEY_REFERENCE"))
  tb <- v_tables(); tb$dlm_final_summary[2, c("outcome", "metric", "model_tier")] <- tb$dlm_final_summary[1, c("outcome", "metric", "model_tier")]
  v <- siw_validate_results(tb, CFG, v_refdir()); expect_equal(st(v, "dlm_final_L01_L56"), "FAIL"); expect_true(cls_present(v, "DUPLICATE_KEY_RESULTS")); expect_true(v$summary$n_duplicate[2] >= 1L)
  v <- v_run(function(g, d) { g$metric[3] <- ""; list(g = g, d = d) }); expect_equal(st(v, "cumulative"), "FAIL"); expect_true(cls_present(v, "MISSING_KEY"))
  # direct: duplicates alone (otherwise identical tables) must not pass
  a <- data.frame(k = c("a", "a", "b"), x = c(1, 1, 2)); r <- siw_compare_tables(a, a, "k", list(exact_abs = 1e-12, exact_rel = 1e-12, tol_abs = 1e-7, tol_rel = 1e-6, name = "tol"), name = "t")
  expect_equal(r$summary$status, "FAIL"); expect_equal(r$summary$n_duplicate, 2L)
})

test_that("missing or non-finite values that disagree FAIL; matching missing values do not", {
  v <- v_run(function(g, d) { g$coefficient[10] <- NA; list(g = g, d = d) }); expect_equal(st(v, "cumulative"), "FAIL"); expect_true(v$summary$n_nonfinite[1] >= 1L)
  tb <- v_tables(); tb$cumulative_results$nominal_p[7] <- NA; v <- siw_validate_results(tb, CFG, v_refdir()); expect_equal(st(v, "cumulative"), "FAIL"); expect_true(cls_present(v, "NONFINITE_ONE_SIDED"))
  v <- v_run(function(g, d) { g$ci_lower[20] <- Inf; list(g = g, d = d) }); expect_equal(st(v, "cumulative"), "FAIL")
  tb <- v_tables(); tb$cumulative_results$ci_lower[20] <- Inf; v <- siw_validate_results(tb, CFG, v_refdir(function(g, d) { g$ci_lower[20] <- Inf; list(g = g, d = d) })); expect_equal(st(v, "cumulative"), "FAIL")   # both non-finite is still unresolved
  v <- v_run(function(g, d) { g$q10[g$model_tier != "M2_L"] <- NA; list(g = g, d = d) }); expect_true(grepl("^PASS", st(v, "cumulative")))                                 # NA on both sides agrees
  v <- v_run(function(g, d) { g$odds_ratio_raw[1] <- "not-a-number"; list(g = g, d = d) }); expect_equal(st(v, "cumulative"), "FAIL")                                     # unparseable reference value
})

test_that("tolerance classification is unchanged: exact, within tolerance, different", {
  tol <- list(exact_abs = 1e-12, exact_rel = 1e-12, cumulative_abs = 1e-7, cumulative_rel = 1e-6, name = "cumulative")
  a <- data.frame(k = letters[1:4], x = c(1, 1, 1, 1)); b <- data.frame(k = letters[1:4], x = c(1, 1 + 1e-13, 1 + 1e-8, 1 + 1e-4))
  r <- siw_compare_tables(a, b, "k", tol, name = "t"); expect_equal(c(r$summary$n_exact, r$summary$n_within_tolerance, r$summary$n_different), c(2L, 1L, 1L)); expect_equal(r$summary$status, "FAIL")
  expect_identical(sort(r$detail$class), c("DIFFERENT", "WITHIN_TOLERANCE"))                                  # sub-tolerance differences are listed, not hidden
  r2 <- siw_compare_tables(a[1:3, ], b[1:3, ], "k", tol, name = "t"); expect_equal(r2$summary$status, "PASS_WITHIN_TOLERANCE")
  v <- v_run(function(g, d) { g$coefficient[5] <- g$coefficient[5] + 1e-4; list(g = g, d = d) }); expect_equal(st(v, "cumulative"), "FAIL"); expect_true(v$summary$n_different[1] >= 1L)
  v <- v_run(function(g, d) { g$coefficient[5] <- g$coefficient[5] + 1e-9; list(g = g, d = d) }); expect_equal(st(v, "cumulative"), "PASS_WITHIN_TOLERANCE")
  v <- v_run(function(g, d) { d$global_p[1] <- d$global_p[1] + 1e-3; list(g = g, d = d) }); expect_equal(st(v, "dlm_final_L01_L56"), "FAIL")
})

test_that("undeclared reference files and failing archived material are handled explicitly", {
  d <- v_refdir(); file.copy(file.path(d, "cumulative_regression_grid.csv"), file.path(d, "fixed_DLM_summary.csv"))      # an archived-named file not matching the archived table
  v <- siw_validate_results(c(v_tables(), list(archived_dlm_summary = v_tables()$dlm_final_summary)), CFG, d)
  expect_equal(st(v, "dlm_archived_L01_L59"), "FAIL")                       # present but wrong (168 expected records): a comparison that is attempted must pass
  expect_equal(siw_overall_validation_status(v$summary), "FAIL")
  d2 <- v_refdir(); file.rename(file.path(d2, "DLM_L01_L56_nine_representative_summary.csv"), file.path(d2, "DLM_L01_L56_nine_representative_summary_RENAMED.csv"))
  v2 <- siw_validate_results(v_tables(), CFG, d2); expect_equal(st(v2, "dlm_final_L01_L56"), "FAIL")
  p <- siw_load_validation_plan(); expect_true(isTRUE(p$references[[1]]$required) && isTRUE(p$references[[2]]$required) && !isTRUE(p$references[[3]]$required))
  expect_equal(p$references[[1]]$expected_records_by_file[["cumulative_regression_grid.csv"]], 1680L)
})

test_that("validate mode end to end: complete comparison passes, a missing required table fails, results unchanged", {
  mc <- function(g, d) list(g = g, d = d[, setdiff(names(d), c("simultaneous_critical", "n_lags_simultaneous_excl_null"))])   # Monte-Carlo fields are not references (draws differ here)
  csv <- SYN_CSV; refd <- v_refdir(mc); out <- file.path(tempdir(), "vmode_ok"); r <- siw_run_validate(csv, refd, out, CFG, resume = FALSE)
  expect_equal(r$overall, "PASS"); s <- read.csv(file.path(out, "validation_summary.csv")); expect_true(all(c("n_duplicate", "n_missing_required", "required", "reason") %in% names(s)))
  expect_true(any(grepl("REQUIRED_REFERENCES_NOT_PASSED=0", readLines(file.path(out, "VALIDATION_STATUS.txt")))))
  refd2 <- v_refdir(mc, drop_files = "DLM_L01_L56_nine_representative_summary.csv"); out2 <- file.path(tempdir(), "vmode_bad"); r2 <- siw_run_validate(csv, refd2, out2, CFG, resume = FALSE)
  expect_equal(r2$overall, "FAIL"); expect_identical(unname(tools::md5sum(file.path(out2, "cumulative_results.csv"))), unname(tools::md5sum(file.path(out, "cumulative_results.csv"))))
})
