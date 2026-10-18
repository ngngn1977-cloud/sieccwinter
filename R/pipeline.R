# Pipeline: import once, validate once, precompute cumulative exposures once, run stages with resumable checkpoints, write few files.

.siw_stage_versions <- c(cumulative = "1", dlm_final = "1", archived_dlm = "2", archived_dlm_sensitivity = "2", rainfall_sensitivity = "1")

siw_pkg_version <- function() {
  f <- file.path(siw_root(), "DESCRIPTION")
  if (file.exists(f)) read.dcf(f, fields = "Version")[1, 1] else NA_character_
}

#' Run a stage with a checkpoint. The checkpoint holds aggregate results only and is reused when the signature
#' (input hash, configuration hash, package version, stage version, options) is unchanged.
#' @export
siw_stage <- function(name, signature, out_dir, fun, resume = TRUE) {
  cp <- file.path(out_dir, "checkpoints"); dir.create(cp, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(cp, paste0(name, ".rds")); sig <- siw_string_hash(signature)
  if (resume && file.exists(f)) {
    obj <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(obj) && identical(obj$signature, sig)) { siw_log("stage ", name, ": RESTORED from checkpoint"); return(list(value = obj$value, status = "RESTORED", seconds = 0)) }
  }
  siw_log("stage ", name, ": START")
  t0 <- proc.time()[["elapsed"]]; v <- fun(); secs <- proc.time()[["elapsed"]] - t0
  saveRDS(list(signature = sig, value = v, created = format(Sys.time(), tz = "UTC", usetz = TRUE)), f)
  siw_log("stage ", name, ": DONE in ", round(secs, 2), " s")
  list(value = v, status = "COMPUTED", seconds = secs)
}

#' Internal consistency checks of a run (structure of the outputs, family sizes, identifiers, schema order).
#' @export
siw_self_checks <- function(tables, config, archived = FALSE) {
  R <- list(); add <- function(check, ok, detail) R[[length(R) + 1L]] <<- data.frame(check = check, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = detail, stringsAsFactors = FALSE)
  cr <- tables$cumulative_results; nexp <- nrow(config$outcomes) * nrow(config$metrics) * length(config$windows) * length(config$tier_order)
  add("cumulative_specification_count", nrow(cr) == nexp, paste0(nrow(cr), " rows; expected ", nexp))
  add("cumulative_ids_unique", !anyDuplicated(cr$result_id), "result_id unique")
  el <- cr$model_tier == config$primary_tier & cr$status == "COMPLETED" & is.finite(cr$nominal_p)
  add("q10_family_sizes", all(table(cr$multiplicity_family_q10[cr$multiplicity_family_q10 != ""]) == length(config$windows)), "each q10 family has 10 tests")
  add("q80_family_sizes", all(table(cr$multiplicity_family_q80[cr$multiplicity_family_q80 != ""]) == nrow(config$metrics) * length(config$windows)), "each q80 family has 80 tests")
  add("q90_family_size", sum(cr$multiplicity_family_q90 != "") == nrow(config$outcomes) * length(config$representative_metrics) * length(config$windows), paste0(sum(cr$multiplicity_family_q90 != ""), " tests; expected 90"))
  add("q_only_on_primary_tier", all(is.na(cr$q10[cr$model_tier != config$primary_tier])) && all(is.na(cr$q80[cr$model_tier != config$primary_tier])) && all(is.na(cr$q90[cr$model_tier != config$primary_tier])), "q values exist only for M2-L")
  ds <- tables$dlm_final_summary
  add("final_dlm_count", nrow(ds) == nrow(config$outcomes) * length(config$representative_metrics), paste0(nrow(ds), " models; expected 9"))
  add("final_dlm_lag_range", all(ds$lag_end == config$dlm$final$lag_end) && all(ds$lag_df == config$dlm$final$lag_df), "all final DLMs use L01-L56, df 3")
  add("final_dlm_curves_dimension", nrow(tables$dlm_final_lag_curves) == sum(ds$status == "COMPLETED") * config$dlm$final$lag_end, "one row per lag per estimable model")
  add("q9_family_size", sum(is.finite(ds$q9)) == sum(is.finite(ds$global_p)), "q9 computed over all finite global tests")
  add("band_metadata_recorded", all(ds$simultaneous_draws == config$dlm$final$draws, na.rm = TRUE) && all(!is.na(ds$seed[ds$status == "COMPLETED"])), "draw count and seed stored for every model")
  if (archived) {
    a <- tables$archived_dlm_summary
    add("archived_dlm_count", nrow(a) == nrow(config$outcomes) * nrow(config$metrics) * length(config$tier_order), paste0(nrow(a), " rows; expected 168"))
    add("archived_lag_range", all(a$lag_end == config$dlm$archived$lag_end), "archived models are L01-L59")
    s <- tables$archived_dlm_sensitivity_summary
    add("archived_sensitivity_count", nrow(s) == 216L, paste0(nrow(s), " rows; expected 216"))
  }
  for (nm in intersect(names(tables), names(siw_output_columns()))) add(paste0("schema_columns_", nm), identical(names(siw_conform(tables[[nm]], siw_output_columns()[[nm]])), siw_output_columns()[[nm]]), "fixed column order")
  do.call(rbind, R)
}

#' Optional sensitivity: deterministic zero reconstruction of the missing rainfall total. Never part of the primary run.
#' @export
siw_run_rainfall_sensitivity <- function(dat, config, draws = config$dlm$final$draws) {
  an <- siw_rainfall_anomalies(dat, config, config$dlm$final$lag_end)
  sens <- siw_rainfall_zero_reconstruction_sensitivity(dat, config, config$dlm$final$lag_end)
  rain_metrics <- c("IMS_rainfall_total", "IMS_rainy_day_count_ge_1_mm")
  g0 <- siw_cumulative_grid(dat, config, tiers = config$primary_tier)$results; g1 <- siw_cumulative_grid(sens, config, tiers = config$primary_tier)$results
  rows <- list()
  for (i in which(g0$metric %in% rain_metrics)) {
    j <- match(g0$result_id[i], g1$result_id)
    rows[[length(rows) + 1L]] <- data.frame(result_id = g0$result_id[i], outcome = g0$outcome[i], metric = g0$metric[i], model_tier = g0$model_tier[i], analysis = "cumulative", window_or_lag = g0$window_or_lag[i],
      n_primary = g0$n[i], n_sensitivity = g1$n[j], effect_primary = g0$coefficient[i], effect_sensitivity = g1$coefficient[j], p_primary = g0$nominal_p[i], p_sensitivity = g1$nominal_p[j],
      q_primary = if (g0$metric[i] %in% config$representative_metrics) g0$q90[i] else g0$q80[i], q_sensitivity = if (g0$metric[i] %in% config$representative_metrics) g1$q90[j] else g1$q80[j],
      note = if (g0$metric[i] %in% config$representative_metrics) "q90" else "q80", stringsAsFactors = FALSE)
  }
  d0 <- siw_final_dlm(dat, config, draws = 0L)$summary; d1 <- siw_final_dlm(sens, config, draws = 0L)$summary
  for (i in seq_len(nrow(d0))) rows[[length(rows) + 1L]] <- data.frame(result_id = d0$result_id[i], outcome = d0$outcome[i], metric = d0$metric[i], model_tier = d0$model_tier[i], analysis = "dlm_final_L01_L56",
      window_or_lag = 56L, n_primary = d0$n[i], n_sensitivity = d1$n[i], effect_primary = d0$cumulative_effect[i], effect_sensitivity = d1$cumulative_effect[i], p_primary = d0$global_p[i], p_sensitivity = d1$global_p[i],
      q_primary = d0$q9[i], q_sensitivity = d1$q9[i], note = paste0("q9; zero-reconstructed cells=", attr(sens, "sensitivity")$cells_replaced), stringsAsFactors = FALSE)
  do.call(rbind, rows)
}

#' Paper workflow from an authorised canonical CSV.
#' @param archived Also run the archived L01-L59 programme (168 fixed models, q24, and the 216 lag-form sensitivities).
#' @param rainfall_sensitivity Also run the optional rainfall zero-reconstruction sensitivity.
#' @export
siw_run_paper <- function(data_csv, out_dir, config = siw_load_config(), resume = TRUE, archived = FALSE, rainfall_sensitivity = FALSE, label = NULL) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); options(sieccwinter.log_file = file.path(out_dir, "run.log"))
  t_all <- proc.time()[["elapsed"]]; timings <- list()
  siw_log("sieccwinter ", siw_pkg_version(), " paper workflow; archived=", archived, "; rainfall_sensitivity=", rainfall_sensitivity)
  ih <- siw_file_hash(data_csv); lag_need <- if (archived) 59L else config$dlm$final$lag_end
  t0 <- proc.time()[["elapsed"]]
  raw <- siw_read_study_csv(data_csv, config)
  rep <- siw_validate_input(raw, config, lag_end = lag_need)
  timings$import_validate <- proc.time()[["elapsed"]] - t0
  siw_write_table(rep, "input_validation_report", out_dir)
  siw_log("validation: ", sum(rep$status == "PASS"), " PASS, ", sum(rep$status == "WARN"), " WARN, ", sum(rep$status == "FAIL"), " FAIL")
  if (any(rep$status == "FAIL")) stop("SIW_INPUT_VALIDATION_FAILED: see input_validation_report.csv (", paste(rep$check_id[rep$status == "FAIL"], collapse = ", "), ")", call. = FALSE)
  t0 <- proc.time()[["elapsed"]]
  dat <- siw_prepare_data(raw, config); att <- siw_attach_cumulative(dat, config); dat <- att$data
  timings$prepare <- proc.time()[["elapsed"]] - t0
  base_sig <- c(input = ih$value, config = config$config_hash, version = siw_pkg_version())
  draws <- config$dlm$final$draws
  st <- list()
  st$cumulative <- siw_stage("cumulative", c(base_sig, .siw_stage_versions[["cumulative"]], att$source), out_dir, function() siw_cumulative_grid(dat, config), resume)
  st$dlm_final <- siw_stage("dlm_final", c(base_sig, .siw_stage_versions[["dlm_final"]], att$source, draws), out_dir, function() siw_final_dlm(dat, config, draws), resume)
  tables <- list(input_validation_report = rep, cumulative_results = st$cumulative$value$results, complete_case_accounting = st$cumulative$value$accounting, cumulative_design_audit = st$cumulative$value$designs,
                 dlm_final_summary = st$dlm_final$value$summary, dlm_final_lag_curves = st$dlm_final$value$lag_curves, dlm_final_cumulative_curves = st$dlm_final$value$cumulative_curves,
                 rainfall_anomaly_report = siw_rainfall_anomaly_report(dat, config, lag_need))
  if (archived) {
    st$archived_dlm <- siw_stage("archived_dlm", c(base_sig, .siw_stage_versions[["archived_dlm"]], att$source, config$dlm$archived$draws), out_dir, function() siw_archived_dlm_grid(dat, config), resume)
    st$archived_dlm_sens <- siw_stage("archived_dlm_sensitivity", c(base_sig, .siw_stage_versions[["archived_dlm_sensitivity"]], att$source), out_dir, function() siw_archived_dlm_sensitivity(dat, config), resume)
    tables$archived_dlm_summary <- st$archived_dlm$value; tables$archived_dlm_sensitivity_summary <- st$archived_dlm_sens$value
  }
  if (rainfall_sensitivity) {
    st$rainfall <- siw_stage("rainfall_sensitivity", c(base_sig, .siw_stage_versions[["rainfall_sensitivity"]], att$source, draws), out_dir, function() siw_run_rainfall_sensitivity(dat, config), resume)
    tables$rainfall_sensitivity_summary <- st$rainfall$value
  }
  for (nm in setdiff(names(tables), "input_validation_report")) siw_write_table(tables[[nm]], nm, out_dir)
  sc <- siw_self_checks(tables, config, archived); siw_write_csv(sc, file.path(out_dir, "self_checks.csv"))
  timings <- c(timings, lapply(st, function(s) s$seconds)); timings$total <- proc.time()[["elapsed"]] - t_all
  tim <- data.frame(stage = names(timings), seconds = round(unlist(timings), 3), status = c("COMPUTED", "COMPUTED", vapply(st, function(s) s$status, character(1)), "-")[seq_along(timings)], stringsAsFactors = FALSE)
  siw_write_csv(tim, file.path(out_dir, "timings.csv"))
  writeLines(utils::capture.output(print(utils::sessionInfo())), file.path(out_dir, "session_info.txt"))
  siw_write_csv(siw_dependency_report(), file.path(out_dir, "dependency_report.csv"))
  meta <- list(package = "sieccwinter", package_version = siw_pkg_version(), input_label = label %||% basename(data_csv), input_hash_algorithm = ih$algorithm, input_hash = ih$value, config_hash = config$config_hash,
               config_version = config$config_version, seed = config$seed, rng = config$rng, dlm_final = config$dlm$final[c("lag_end", "lag_df", "draws", "confidence", "quantile_type", "band_method", "mvn_method", "seed_rule")],
               cumulative_source = att$source, n_participants = nrow(raw), archived_run = archived, rainfall_sensitivity_run = rainfall_sensitivity, r_version = R.version.string,
               validation_status = list(pass = sum(rep$status == "PASS"), warn = sum(rep$status == "WARN"), fail = sum(rep$status == "FAIL")), self_checks_failed = sum(sc$status == "FAIL"),
               tolerances = config$tolerances, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  if (archived) meta$archived_glm_control <- siw_archived_glm_control(config)
  siw_write_metadata(out_dir, meta)
  siw_write_manifest(out_dir)
  siw_log("done in ", round(timings$total, 2), " s; self-checks failed: ", sum(sc$status == "FAIL"))
  options(sieccwinter.log_file = NULL)
  list(out_dir = out_dir, tables = tables, self_checks = sc, timings = tim, input_validation = rep, status = if (any(sc$status == "FAIL")) "SELF_CHECK_FAILURE" else "OK")
}

#' Demo workflow on the shipped fully synthetic data.
#' @export
siw_run_demo <- function(out_dir, config = siw_load_config(), resume = FALSE, archived = FALSE) {
  csv <- file.path(siw_root(), "inst", "extdata", "synthetic_siecc_winter.csv")
  if (!file.exists(csv)) csv <- file.path(siw_root(), "extdata", "synthetic_siecc_winter.csv")
  if (!file.exists(csv)) { csv <- tempfile(fileext = ".csv"); siw_write_csv(siw_make_synthetic(config = config), csv) }
  siw_run_paper(csv, out_dir, config, resume = resume, archived = archived, rainfall_sensitivity = TRUE, label = "synthetic_siecc_winter.csv (fully synthetic)")
}
