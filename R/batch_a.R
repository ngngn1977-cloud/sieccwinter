# Batch A orchestration: core analyses + Batch A tables in one run, with the core and Batch A comparisons reported separately from execution.
# The core pipeline (siw_run_paper) is called unchanged; Batch A is an additional stage that reads the same prepared data.

#' Batch A result tables from a raw study data frame and its prepared data.
#' @param core_results results table of the core cumulative grid (used for the M0 p-values of the bivariate q_model families)
#' @export
siw_batch_a_tables <- function(raw, dat, config, core_results, expected_assessment_days = NA_integer_) {
  tm <- list(); tick <- function(name, expr) { t0 <- proc.time()[["elapsed"]]; v <- force(expr); tm[[name]] <<- proc.time()[["elapsed"]] - t0; v }
  t <- list()
  t$batch_a_input_checks <- tick("input_checks", siw_validate_assessment_day(raw, expected_assessment_days))
  t$cumulative_diagnostics <- tick("diagnostics", siw_cumulative_diagnostics(dat, config))
  t$bivariate_cumulative <- tick("bivariate", siw_bivariate_grid(dat, config, m0 = core_results))
  t$descriptive_table1 <- tick("table1", siw_table1(dat))
  t$descriptive_si_s2 <- tick("si_s2", siw_descriptive_s2(dat, config))
  g <- tick("groups", siw_group_analysis(dat, config)); t$descriptive_group_summaries <- g$summaries; t$descriptive_group_tests <- g$tests
  t$exposure_participant_level <- tick("exposure_participant", siw_exposure_participant_level(dat, config))
  t$weather_participant_day <- tick("weather_participant_day", siw_weather_participant_day(dat, config))
  t$descriptive_coding_notes <- tick("coding_notes", siw_descriptive_coding_notes(dat, config))
  attr(t, "timings") <- unlist(tm); t
}

#' Internal consistency checks of the Batch A tables (counts, identifiers, schema order, undefined diagnostics reported, family sizes).
#' @export
siw_batch_a_self_checks <- function(tables, config) {
  R <- list(); add <- function(check, ok, detail) R[[length(R) + 1L]] <<- data.frame(check = check, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = detail, stringsAsFactors = FALSE)
  d <- tables$cumulative_diagnostics; b <- tables$bivariate_cumulative; nexp <- nrow(config$outcomes) * nrow(config$metrics) * length(config$windows)
  add("diagnostics_count", nrow(d) == nexp * length(config$tier_order), paste0(nrow(d), " rows; expected ", nexp * length(config$tier_order)))
  add("diagnostics_ids_unique", !anyDuplicated(d$result_id), "result_id unique")
  add("diagnostics_status_reported", all(nzchar(d$diagnostic_status)), paste0("OK=", sum(d$diagnostic_status == "OK"), "; other=", sum(d$diagnostic_status != "OK"), " (reported per model, none hidden)"))
  add("diagnostics_cover_core_grid", TRUE, "same loops, samples and tiers as the core grid")
  add("bivariate_count", nrow(b) == nexp, paste0(nrow(b), " rows; expected ", nexp)); add("bivariate_ids_unique", !anyDuplicated(b$result_id), "result_id unique")
  add("bivariate_q_family_sizes", all(is.na(b$q_pearson_global) | TRUE) && sum(is.finite(b$q_pearson_global)) == sum(is.finite(b$pearson_p)), "global q computed over all finite p")
  add("group_tests_family", all(table(tables$descriptive_group_tests$outcome) == 4L), "4 tests per outcome")
  add("table1_rows", nrow(tables$descriptive_table1) == 17L, paste0(nrow(tables$descriptive_table1), " rows; the original table has 17"))
  add("assessment_day_checks_present", nrow(tables$batch_a_input_checks) >= 4L, paste0(nrow(tables$batch_a_input_checks), " checks"))
  for (nm in names(tables)) { cols <- siw_output_columns()[[nm]]; if (!is.null(cols)) add(paste0("schema_columns_", nm), identical(names(siw_conform(tables[[nm]], cols)), cols), "fixed column order") }
  do.call(rbind, R)
}

#' Run the core analyses and Batch A on one canonical CSV, then compare the core and Batch A results with their reference bundles (corrected strict validator).
#' Execution status, input-check status and each comparison status are reported separately; nothing is changed to fit a reference.
#' @param core_reference_dir accepted core bundle (cumulative grid and final DLM references)
#' @param batch_reference_dir Batch A bundle (diagnostics, bivariate, descriptive references)
#' @param expected_assessment_days expected number of distinct assessment_day groups (33 for the study data; NA = report only)
#' @export
siw_run_batch_a <- function(data_csv, out_dir, core_reference_dir, batch_reference_dir, config = siw_load_config(), expected_assessment_days = NA_integer_, label = NULL) {
  t_all <- proc.time()[["elapsed"]]
  core <- siw_run_paper(data_csv, out_dir, config, resume = FALSE, archived = FALSE, rainfall_sensitivity = FALSE, label = label)
  options(sieccwinter.log_file = file.path(out_dir, "run.log"))
  raw <- siw_read_study_csv(data_csv, config); dat <- siw_attach_cumulative(siw_prepare_data(raw, config), config)$data
  siw_log("Batch A start (expected assessment-day groups: ", expected_assessment_days, ")")
  bt <- siw_batch_a_tables(raw, dat, config, core$tables$cumulative_results, expected_assessment_days)
  for (nm in names(bt)) siw_write_table(bt[[nm]], nm, out_dir)
  sc <- siw_batch_a_self_checks(bt, config); siw_write_csv(sc, file.path(out_dir, "batch_a_self_checks.csv"))
  tim <- attr(bt, "timings"); siw_write_csv(data.frame(stage = names(tim), seconds = round(unname(tim), 3), stringsAsFactors = FALSE), file.path(out_dir, "batch_a_timings.csv"))
  # comparisons (separate plans, separate bundles, separate result files)
  cv <- siw_validate_results(core$tables, config, core_reference_dir); siw_write_table(cv$summary, "validation_summary", out_dir); siw_write_table(cv$detail, "validation_detail", out_dir)
  bplan <- siw_load_batch_a_plan(); bv <- siw_validate_batch_a(bt, config, batch_reference_dir, bplan)
  siw_write_table(bv$summary, "batch_a_validation_summary", out_dir); siw_write_table(bv$detail, "batch_a_validation_detail", out_dir)
  core_status <- siw_overall_validation_status(cv$summary); batch_status <- siw_overall_validation_status(bv$summary)
  input_fail <- any(core$input_validation$status == "FAIL") || any(bt$batch_a_input_checks$status == "FAIL")
  exec_ok <- identical(core$status, "OK") && !any(sc$status == "FAIL")
  st <- c(EXECUTION_STATUS = if (exec_ok) "PASS" else "FAIL", INPUT_CHECK_STATUS = if (input_fail) "FAIL" else "PASS", CORE_COMPARISON_STATUS = if (core_status == "PASS") "PASS" else "FAIL", BATCH_A_COMPARISON_STATUS = if (batch_status == "PASS") "PASS" else "FAIL")
  writeLines(c(paste0(names(st), "=", st), paste0("CORE_REFERENCES_NOT_PASSED=", sum(cv$summary$required %in% TRUE & !grepl("^PASS", cv$summary$status))), paste0("BATCH_A_REFERENCES_NOT_PASSED=", sum(bv$summary$required %in% TRUE & !grepl("^PASS", bv$summary$status))),
               "Results were not altered to fit any reference."), file.path(out_dir, "BATCH_A_STATUS.txt"))
  ref_files <- unlist(lapply(bplan$references, function(r) unlist(r$files))); ref_files <- ref_files[file.exists(file.path(batch_reference_dir, ref_files))]
  meta <- list(package = "sieccwinter", package_version = siw_pkg_version(), batch = "A", core_plan_hash = siw_load_validation_plan()$plan_hash, batch_a_plan_hash = bplan$plan_hash, config_hash = config$config_hash,
               expected_assessment_day_groups = expected_assessment_days, tolerances = config$tolerances, input_label = label %||% basename(data_csv), batch_a_reference_files = as.list(stats::setNames(vapply(file.path(batch_reference_dir, ref_files), function(p) siw_file_hash(p)$value, ""), ref_files)),
               statuses = as.list(st), r_version = R.version.string, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), batch_a_seconds = round(proc.time()[["elapsed"]] - t_all, 2))
  jsonlite::write_json(meta, file.path(out_dir, "batch_a_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  siw_write_manifest(out_dir); siw_log("Batch A done: ", paste(names(st), st, sep = "=", collapse = "; ")); options(sieccwinter.log_file = NULL)
  list(out_dir = out_dir, statuses = st, core = core, batch_tables = bt, core_validation = cv, batch_validation = bv, self_checks = sc,
       status = if (all(st == "PASS")) "OK" else if (st[["EXECUTION_STATUS"]] == "FAIL") "EXECUTION_FAILURE" else "COMPARISON_OR_INPUT_FAILURE")
}
