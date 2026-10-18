# Coded model record, version 2 (synthetic only): stable codes from the configuration, declared families, attachment of run statuses, safe writing.

CMR <- siw_coded_model_record(CFG)

test_that("the record covers the declared scopes with the declared sizes, and every internal check passes", {
  r <- CMR$record
  expect_equal(nrow(r), 1680L + 9L + 168L + 216L)
  expect_equal(as.integer(table(r$scope)[c("CUMULATIVE_PRIMARY_M2_L", "CUMULATIVE_OTHER_TIER", "FINAL_DLM_L01_L56", "ARCHIVED_DLM_L01_L59_GRID", "ARCHIVED_DLM_LAG_FORM_SENSITIVITY")]), c(240L, 1440L, 9L, 168L, 216L))
  ck <- siw_coded_model_record_checks(CMR, CFG); expect_true(all(ck$status == "PASS")); expect_equal(nrow(ck), 16L)
  expect_true(all(r$record_version == "2")); expect_false(any(r$run_found))
})

test_that("codes are stable (configuration only), unique, and final and archived scopes never share a code or a family", {
  expect_identical(siw_coded_model_record(CFG)$record$model_code, CMR$record$model_code)
  r <- CMR$record; expect_false(anyDuplicated(r$model_code) > 0)
  expect_true(all(grepl("^SIW2[.](CUM|DLM56|ARCH59|ARCHLF)[.]", r$model_code)))
  fin <- r[r$scope == "FINAL_DLM_L01_L56", ]; arc <- r[grepl("^ARCHIVED", r$scope), ]
  expect_true(all(fin$lag_end == 56L) && all(arc$lag_end[arc$scope == "ARCHIVED_DLM_L01_L59_GRID"] == 59L))
  expect_identical(unique(fin$declared_families), "DLM_FINAL_L01_L56_M2_L_q9")
  expect_false(any(grepl("DLM_FINAL", arc$declared_families))); expect_false(any(grepl("ARCHIVED", fin$declared_families)))
  expect_identical(fin$package_result_id, siw_final_dlm(DAT, CFG, draws = 0L)$summary$result_id)      # same identifiers as the package results
  expect_equal(sum(nzchar(arc$same_model_as)), 24L)
})

test_that("binary epsilon recorded per scope: cumulative 1e-8, final 1e-12, archived 1e-10; Gaussian rows NA", {
  r <- CMR$record; b <- r$outcome_family == "binomial"
  expect_true(all(is.na(r$binary_glm_epsilon[!b])))
  expect_equal(unique(r$binary_glm_epsilon[b & grepl("^CUMULATIVE", r$scope)]), 1e-8); expect_equal(unique(r$binary_glm_epsilon[b & r$scope == "FINAL_DLM_L01_L56"]), 1e-12)
  expect_equal(unique(r$binary_glm_epsilon[b & grepl("^ARCHIVED", r$scope)]), 1e-10)
})

test_that("statuses, n and in-run families are attached from a synthetic paper folder; archived validation classes from a synthetic archived folder", {
  pd <- file.path(tempdir(), paste0("cmr_paper_", sample.int(1e6, 1))); dir.create(pd)
  cg <- siw_cumulative_grid(DAT, CFG)$results; fd <- siw_final_dlm(DAT, CFG, draws = 0L)$summary
  siw_write_csv(cg, file.path(pd, "cumulative_results.csv")); siw_write_csv(fd, file.path(pd, "dlm_final_summary.csv"))
  ad <- file.path(tempdir(), paste0("cmr_arch_", sample.int(1e6, 1))); dir.create(ad)
  ml <- data.frame(comparison = c("SUMMARY_168", "SUMMARY_216"), key = c("Int_dummy|UTCI|M3", "UCLA_Lonliness_SCORE|MRT|L45_DF2"), worst_class = c("WITHIN_TOLERANCE", "EXACT"), stringsAsFactors = FALSE)
  utils::write.csv(ml, file.path(ad, "archived_model_level_comparison.csv"), row.names = FALSE)
  x <- siw_coded_model_record(CFG, pd, ad); r <- x$record
  expect_equal(sum(r$run_found[r$source_step == "PAPER"]), 1689L)
  expect_identical(r$run_status[r$scope == "FINAL_DLM_L01_L56"], fd$status[match(r$package_result_id[r$scope == "FINAL_DLM_L01_L56"], fd$result_id)])
  expect_true(all(siw_coded_model_record_checks(x, CFG)$status == "PASS"))
  expect_identical(r$run_archived_validation_class[r$model_code == "SIW2.ARCH59.INT.UTCI.M3.L59DF3"], "WITHIN_TOLERANCE")
  expect_identical(r$run_archived_validation_class[r$model_code == "SIW2.ARCHLF.UCLA.MRT.M2_L.L45DF2"], "EXACT")
  expect_equal(sum(r$run_found[grepl("^ARCHIVED", r$scope)]), 2L)
  fam <- x$families; expect_equal(fam$members_in_run[fam$family == "RAW_CUMULATIVE_q90_all_primary_pairs"], sum(cg$multiplicity_family_q90 != ""))
  bad <- cg; bad$multiplicity_family_q80[bad$multiplicity_family_q80 != ""][1] <- "WRONG_FAMILY"; siw_write_csv(bad, file.path(pd, "cumulative_results.csv"))
  expect_true(any(siw_coded_model_record_checks(siw_coded_model_record(CFG, pd, NULL), CFG)$status == "FAIL"))      # a family that differs from the declaration is caught
})

test_that("writing: new folder only, the legacy file name is never written, aggregate content only", {
  od <- file.path(tempdir(), paste0("cmr_out_", sample.int(1e6, 1))); w <- siw_write_coded_model_record(od, CFG, run_label = "synthetic")
  expect_identical(w$status, "PASS")
  f <- list.files(od); expect_true(all(c("SIECC_coded_model_record_v2.csv", "SIECC_coded_model_record_v2_families.csv", "SIECC_coded_model_record_v2_checks.csv", "SIECC_coded_model_record_v2_metadata.json", "CODED_MODEL_RECORD_STATUS.txt", "MANIFEST.csv") %in% f))
  expect_false("SIECC_All_Models_with_Codes.csv" %in% f)
  expect_error(siw_write_coded_model_record(od, CFG), "NOT_EMPTY")
  txt <- unlist(lapply(file.path(od, f), readLines, warn = FALSE)); expect_false(any(grepl("SYN0[0-9]", txt)))
  expect_equal(nrow(utils::read.csv(file.path(od, "SIECC_coded_model_record_v2.csv"))), 2073L)
})
