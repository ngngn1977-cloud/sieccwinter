# Phase 3E: validation of the archived L01-L59 grid (168) and lag-form sensitivity (216). Synthetic data and synthetic frozen-format reference files only; no study data, no frozen V1.4 file is read here.
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
if (!exists("expect_gt", mode = "function")) expect_gt <- function(a, b) expect_true(a > b)
if (!exists("expect_lt", mode = "function")) expect_lt <- function(a, b) expect_true(a < b)
if (!exists("expect_null", mode = "function")) expect_null <- function(x) expect_true(is.null(x))
CFG <- siw_load_config(); PA <- siw_archived_validation_protocol(); hsh <- function(f) siw_file_hash(file.path(ROOT, f))$value

# ---- synthetic builders: a data CSV, a legacy-format reference folder and a protocol copy describing that folder
syn_archived <- function(base, seed = 7L, n = 60L, noise = 0, config = CFG) {
  unlink(base, recursive = TRUE); dir.create(base, recursive = TRUE); raw <- siw_make_synthetic(n = n, seed = seed, config = config); csv <- file.path(base, "syn.csv"); utils::write.csv(raw, csv, row.names = FALSE, na = ""); dat <- siw_prepare_data(raw, config)
  fit <- siw_archived_validation_fits(dat, config, PA)
  ls <- function(s) data.frame(model_id = s$result_id, outcome = s$outcome, outcome_family = s$outcome_family, metric = s$metric, model_tier = s$model_tier, n = s$n, event_count = s$event_count, lag_start = s$lag_start, lag_end = s$lag_end, lag_df = s$lag_df,
    global_statistic = s$global_statistic, global_df = s$global_df1, global_p = s$global_p, cumulative_effect = s$cumulative_effect, cumulative_se = s$cumulative_se, cumulative_ci_lower = s$cumulative_ci_lower, cumulative_ci_upper = s$cumulative_ci_upper,
    peak_lag = s$peak_lag, peak_effect = s$peak_effect, simultaneous_critical = rep(2.5, nrow(s)), any_simultaneous_band_excludes_null = FALSE, convergence = s$convergence, separation = s$separation, rank = s$rank, parameter_count = s$parameter_count,
    residual_df = s$residual_df, condition_number = 100, status = s$status, stringsAsFactors = FALSE)
  g <- ls(fit$grid); g$q9 <- fit$grid$q9; g$q24 <- fit$grid$q24; g$evidence_status <- "SENSITIVITY_ONLY"
  s <- ls(fit$sensitivity); s$sensitivity_specification <- fit$sensitivity$sensitivity_specification; s$q216 <- fit$sensitivity$q216; s$simultaneous_critical <- NA_real_
  curve <- function(d, sens) { o <- data.frame(model_id = "x", outcome = d$outcome, metric = d$metric, model_tier = d$model_tier, lag = d$lag, estimate = d$estimate, standard_error = d$standard_error, lower_95 = d$lower_95, upper_95 = d$upper_95,
    simultaneous_lower_95 = d$estimate - 2.5 * d$standard_error, simultaneous_upper_95 = d$estimate + 2.5 * d$standard_error, exponentiated_estimate = NA_real_, stringsAsFactors = FALSE); if (sens) o$sensitivity_specification <- d$sensitivity_specification; o }
  lc <- curve(fit$lag_curves_168, FALSE); sc <- curve(fit$lag_curves_216, TRUE); sc$simultaneous_lower_95 <- NA_real_; sc$simultaneous_upper_95 <- NA_real_; k <- fit$cumulative_curves_168
  cc <- data.frame(model_id = "x", outcome = k$outcome, metric = k$metric, model_tier = k$model_tier, lag = k$lag, cumulative_effect = k$cumulative_effect, cumulative_se = k$cumulative_se, cumulative_lower_95 = k$cumulative_lower_95, cumulative_upper_95 = k$cumulative_upper_95, stringsAsFactors = FALSE)
  if (noise > 0) { set.seed(11); jig <- function(v) v * (1 + noise * stats::runif(length(v), -1, 1)); for (nm in c("cumulative_ci_lower", "cumulative_ci_upper")) { g[[nm]] <- jig(g[[nm]]); s[[nm]] <- jig(s[[nm]]) }; for (nm in c("estimate", "standard_error", "lower_95", "upper_95")) { lc[[nm]] <- jig(lc[[nm]]); sc[[nm]] <- jig(sc[[nm]]) } }
  refdir <- file.path(base, "ref"); dir.create(refdir); W <- function(x, f) utils::write.csv(x, file.path(refdir, f), row.names = FALSE, na = "")
  W(g, "fixed_DLM_summary.csv"); W(s, "DLM_lag_length_df_sensitivity_summary.csv"); W(lc, "fixed_DLM_lag_curves.csv"); W(cc, "fixed_DLM_cumulative_curves.csv"); W(sc, "DLM_lag_length_df_sensitivity_curves.csv")
  list(base = base, csv = csv, refdir = refdir, dat = dat, fit = fit, config = config)
}
syn_protocol <- function(refdir, out_json, tamper_cells = 0L) {
  p <- siw_archived_validation_protocol(); for (i in seq_along(p$reference_files)) { f <- file.path(refdir, p$reference_files[[i]]$file); if (file.exists(f)) { p$reference_files[[i]]$sha256 <- siw_file_hash(f)$value; p$reference_files[[i]]$bytes <- file.info(f)$size } }
  rd <- function(f) if (file.exists(file.path(refdir, f))) utils::read.csv(file.path(refdir, f), stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")) else NULL
  tabs <- list(archived_grid_summary = rd("fixed_DLM_summary.csv"), sensitivity_summary = rd("DLM_lag_length_df_sensitivity_summary.csv"), archived_grid_lag_curves = rd("fixed_DLM_lag_curves.csv"), archived_grid_cumulative_curves = rd("fixed_DLM_cumulative_curves.csv"), sensitivity_lag_curves = rd("DLM_lag_length_df_sensitivity_curves.csv"))
  nn <- function(d, f) sum(vapply(intersect(f, names(d)), function(z) sum(!is.na(d[[z]])), 0L)); ids <- c(SUMMARY_168 = "archived_grid_summary", SUMMARY_216 = "sensitivity_summary", LAG_CURVES_168 = "archived_grid_lag_curves", CUMULATIVE_CURVES_168 = "archived_grid_cumulative_curves", LAG_CURVES_216 = "sensitivity_lag_curves")
  for (i in seq_along(p$comparisons)) { d <- tabs[[ids[[p$comparisons[[i]]$id]]]]; if (!is.null(d)) { p$comparisons[[i]]$expected_compared_cells <- nn(d, unlist(p$comparisons[[i]]$fields)) + (if (i == 1L) tamper_cells else 0L); p$comparisons[[i]]$expected_records <- nrow(d) } }
  for (i in seq_along(p$reference_files)) { d <- tabs[[p$reference_files[[i]]$name]]; if (!is.null(d)) p$reference_files[[i]]$expected_records <- nrow(d) }
  p$protocol_path <- NULL; p$protocol_hash <- NULL; jsonlite::write_json(p, out_json, auto_unbox = TRUE, pretty = TRUE, null = "null"); out_json
}
run_syn <- function(S, refdir = S$refdir, pj = NULL, out = NULL) { if (is.null(pj)) pj <- syn_protocol(refdir, tempfile(fileext = ".json")); out <- out %||% file.path(S$base, paste0("run_", basename(tempfile()))); suppressMessages(siw_run_archived_dlm_validation(S$csv, refdir, out, S$config, protocol_path = pj)) }
variant <- function(S, edit, name, rebuild_protocol = TRUE) { d <- file.path(S$base, paste0("ref_", name)); unlink(d, recursive = TRUE); dir.create(d); file.copy(list.files(S$refdir, full.names = TRUE), d); edit(d); d }
rw <- function(d, f, fun) { p <- file.path(d, f); x <- utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")); x <- fun(x); utils::write.csv(x, p, row.names = FALSE, na = "") }

S0 <- syn_archived(file.path(tempdir(), "syn_arch_exact")); R0 <- run_syn(S0)
Sn <- syn_archived(file.path(tempdir(), "syn_arch_noise"), noise = 1e-9)

test_that("the protocol is declared before the run: five hashed references, expected counts, traced specification, not-deterministic items, and nothing derived from a result", {
  if (siw_skip_source_only("test-23-archived-dlm-validation: the protocol is declared before the run: five hashed references, expec")) return(invisible(NULL))
  expect_match(PA$status_of_this_document, "DECLARED BEFORE THE MANUAL RUN"); expect_match(PA$status_of_this_document, "does NOT extend the V2.1 acceptance"); expect_equal(length(PA$reference_files), 5L)
  for (rf in PA$reference_files) { expect_match(rf$sha256, "^[0-9A-F]{64}$"); expect_gt(rf$bytes, 1000) }
  expect_equal(vapply(PA$reference_files, function(z) z$expected_records, 0L), c(168L, 216L, 9912L, 9912L, 10008L)); expect_equal(vapply(PA$comparisons, function(z) z$expected_compared_cells, 0L), c(2609L, 3528L, 39648L, 39648L, 40032L))
  expect_private(PA$reference_files[[1]]$sha256, "archived_dlm_reference_1")
  expect_equal(PA$specification_trace$binary_fit_control$glm_epsilon, 1e-10); expect_equal(PA$specification_trace$binary_fit_control$glm_maxit, 200); expect_equal(CFG$dlm$final$glm_epsilon, 1e-12)
  expect_equal(PA$tolerance$within_tolerance$abs, CFG$tolerances$dlm_abs); expect_equal(PA$tolerance$within_tolerance$rel, CFG$tolerances$dlm_rel); expect_equal(PA$tolerance$exact$abs, CFG$tolerances$exact_abs)
  expect_match(PA$specification_trace$multiplicity$q24, "24 global tests"); expect_match(PA$specification_trace$multiplicity$q216, "216 global tests"); expect_match(PA$specification_trace$missing_data_rule, "rainfall missingness is preserved")
  it <- vapply(PA$not_deterministic_or_not_validated_here, function(z) z$item, ""); expect_true(any(grepl("simultaneous_critical", it))); expect_true(any(grepl("simultaneous_lower_95", it))); expect_true(any(grepl("evidence_status", it))); expect_true(any(grepl("independent-route", it)))
  expect_false("simultaneous_critical" %in% unlist(lapply(PA$comparisons, function(z) z$fields))); expect_false(any(grepl("simultaneous", unlist(lapply(PA$comparisons, function(z) z$fields)))))
  expect_match(PA$scope$kept_separate_from, "nine final L01-L56"); expect_identical(hsh("config/archived_dlm_validation_protocol.json"), PA$protocol_hash); expect_identical(hsh("inst/extdata/archived_dlm_validation_protocol.json"), PA$protocol_hash)
})

test_that("accepted V1/V2/V2.1 sources, protocols and the archived functions are untouched", {
  if (siw_skip_source_only("test-23-archived-dlm-validation: accepted V1/V2/V2.1 sources, protocols and the archived functions are ")) return(invisible(NULL))
  expect_private(hsh("config/dlm_validation_protocol.json"), "dlm_validation_protocol"); expect_private(hsh("config/dlm_validation_protocol_v2.json"), "dlm_validation_protocol_v2"); expect_private(hsh("config/dlm_validation_protocol_v2_1.json"), "dlm_validation_protocol_v2_1")
  expect_identical(hsh("R/dlm_v21.R"), "E143D245051200E4B2EBA0E3579F2D1487DB752759A4BB1DFD53D1B5CD6E11BD"); expect_identical(hsh("R/ordinal.R"), "010882F8DFD0756A8577429F8A3E5423F45220F2D39AA0A4067CD2ED73704441")
  expect_identical(hsh("config/validation_plan_dlm.json"), "1AE045B8CC9E538D9F86621C8E583FBE2A54BB6F745B0A7A839E9B69168A7348")
})

test_that("a reference that equals the package output passes every status; counts, cells and classification are complete; the final-model acceptance is not extended", {
  expect_equal(unname(R0$statuses[c("EXECUTION_STATUS", "INPUT_PROVENANCE_STATUS", "ARCHIVED_168_STATUS", "LAG_FORM_216_STATUS", "GLOBAL_TESTS_STATUS", "MULTIPLICITY_STATUS", "CURVES_STATUS", "LAG_BASIS_STATUS", "COVERAGE_STATUS", "SOURCE_INTEGRITY_STATUS")]), rep("PASS", 10L)); expect_identical(R0$status, "OK_WITH_SEPARATE_VALIDATION_ITEMS_OPEN")
  s <- R0$summary; expect_equal(s$n_compared, c(2609L, 3528L, 39648L, 39648L, 40032L)); expect_true(all(s$n_exact == s$n_compared)); expect_true(all(s$cells_as_declared)); expect_true(all(s$n_unclassified == 0L)); expect_true(all(s$status == "PASS_EXACT"))
  expect_equal(nrow(R0$flags), 3L * (168L + 216L)); expect_true(all(R0$flags$class == "EXACT")); expect_true(all(R0$peak$class == "EXACT")); expect_equal(nrow(R0$peak), 168L + 216L)
  st <- readLines(file.path(R0$out_dir, "ARCHIVED_DLM_STATUS.txt")); expect_true(any(grepl("^SIMULTANEOUS_CRITICAL_VALUES=NOT_VALIDATED_SEPARATE_PROTOCOL_REQUIRED", st))); expect_true(any(grepl("^SIMULTANEOUS_BANDS_AND_EVIDENCE_STATUS=NOT_VALIDATED", st)))
  expect_true(any(grepl("^FINAL_NINE_L56_NUMERICAL_VALIDATION=ACCEPTED_UNDER_V2_1 .*not extended", st))); expect_true(any(grepl("^MANUSCRIPT_CONCORDANCE_STATUS=REVIEW", st))); expect_true(any(grepl("^FIGURE_PROVENANCE_STATUS=NOT_TESTED", st))); expect_true(any(grepl("^DLNM_EQUALITY_STATUS=NOT_TESTED", st)))
  nv <- utils::read.csv(file.path(R0$out_dir, "archived_not_validated.csv")); expect_equal(nrow(nv), length(PA$not_deterministic_or_not_validated_here)); expect_true(all(grepl("^NOT_VALIDATED", nv$status)))
  expect_true(file.exists(file.path(R0$out_dir, "archived_environment.json"))); expect_true(file.exists(file.path(R0$out_dir, "archived_sessionInfo.txt"))); expect_true(file.exists(file.path(R0$out_dir, "source_manifest_archived.csv"))); expect_match(readLines(file.path(R0$out_dir, "SOURCE_FINGERPRINT.txt"))[3], "FALSE$")
})

test_that("simultaneous critical values, bands and the derived evidence status are not compared: wild reference values change nothing; no simulated draw is made", {
  d <- variant(S0, function(d) { rw(d, "fixed_DLM_summary.csv", function(x) { x$simultaneous_critical <- 99; x$any_simultaneous_band_excludes_null <- TRUE; x$evidence_status <- "SUPPORTED_PRIMARY_DLM"; x }); rw(d, "fixed_DLM_lag_curves.csv", function(x) { x$simultaneous_lower_95 <- -9; x$simultaneous_upper_95 <- 9; x }) }, "sim")
  R <- run_syn(S0, d); expect_equal(unname(R$statuses[["ARCHIVED_168_STATUS"]]), "PASS"); expect_equal(unname(R$statuses[["CURVES_STATUS"]]), "PASS"); expect_false(any(grepl("simultaneous|evidence", R$detail$statistic))); expect_true(all(is.na(R0$fits$grid$simultaneous_critical))); expect_true(all(R0$fits$config_used$draws == 0L))
  expect_false(any(names(R0$summary) == "simultaneous_critical")); expect_true(all(is.na(R0$fits$sensitivity$simultaneous_critical)))
})

test_that("differences inside the declared tolerance class are classified WITHIN_TOLERANCE and pass; none is classified exact by mistake", {
  Rn <- run_syn(Sn); expect_equal(unname(Rn$statuses[["ARCHIVED_168_STATUS"]]), "PASS"); expect_equal(unname(Rn$statuses[["CURVES_STATUS"]]), "PASS"); s <- Rn$summary; expect_true(all(s$n_within_tolerance + s$n_exact == s$n_compared)); expect_gt(s$n_within_tolerance[3], 1000L); expect_true(all(grepl("^PASS", s$status))); expect_true(all(s$n_different == 0L))
  expect_true(any(s$status == "PASS_WITHIN_TOLERANCE"))
})

test_that("a difference beyond tolerance in one model fails that comparison, is located by model and stratum, and nothing else is blamed", {
  k <- which(S0$fit$grid$outcome_family == "binomial" & S0$fit$grid$model_tier == "M3")[1L]; key <- paste(S0$fit$grid$outcome[k], S0$fit$grid$metric[k], S0$fit$grid$model_tier[k], sep = "|")
  d <- variant(S0, function(d) rw(d, "fixed_DLM_summary.csv", function(x) { i <- which(paste(x$outcome, x$metric, x$model_tier, sep = "|") == key); x$cumulative_effect[i] <- x$cumulative_effect[i] * 1.001 + 1e-3; x }), "diff")
  R <- run_syn(S0, d); expect_equal(unname(R$statuses[["ARCHIVED_168_STATUS"]]), "FAIL"); expect_equal(unname(R$statuses[["LAG_FORM_216_STATUS"]]), "PASS"); expect_equal(unname(R$statuses[["CURVES_STATUS"]]), "PASS"); expect_identical(R$status, "VERIFICATION_FAILURE")
  ml <- R$model_level; bad <- ml[ml$comparison == "SUMMARY_168" & ml$n_cells_outside_tolerance > 0L, ]; expect_equal(nrow(bad), 1L); expect_identical(bad$key, key); expect_identical(bad$outcome_family, "binomial"); expect_identical(bad$worst_class, "OUTSIDE_TOLERANCE")
  expect_true(file.exists(file.path(R$out_dir, "archived_stratum_summary.csv"))); expect_equal(sum(R$summary$n_different[R$summary$reference == "SUMMARY_168"]), 1L)
})

test_that("sample size, global tests, multiplicity adjustments and flags each fail on their own", {
  one <- function(f, fun, nm) { d <- variant(S0, function(d) rw(d, f, fun), nm); run_syn(S0, d) }
  Rn <- one("fixed_DLM_summary.csv", function(x) { x$n[5] <- x$n[5] - 1L; x }, "n"); expect_equal(unname(Rn$statuses[["ARCHIVED_168_STATUS"]]), "FAIL")
  Rg <- one("fixed_DLM_summary.csv", function(x) { x$global_p[9] <- x$global_p[9] * 1.01; x }, "gp"); expect_equal(unname(Rg$statuses[["GLOBAL_TESTS_STATUS"]]), "FAIL")
  Rd <- one("DLM_lag_length_df_sensitivity_summary.csv", function(x) { x$global_df[7] <- x$global_df[7] + 1; x }, "gdf"); expect_equal(unname(Rd$statuses[["GLOBAL_TESTS_STATUS"]]), "FAIL"); expect_equal(unname(Rd$statuses[["LAG_FORM_216_STATUS"]]), "FAIL")
  Rq <- one("DLM_lag_length_df_sensitivity_summary.csv", function(x) { x$q216[3] <- min(1, x$q216[3] + 0.01); x }, "q216"); expect_equal(unname(Rq$statuses[["MULTIPLICITY_STATUS"]]), "FAIL")
  Rq24 <- one("fixed_DLM_summary.csv", function(x) { i <- which(is.finite(x$q24))[2]; x$q24[i] <- min(1, x$q24[i] + 0.02); x }, "q24"); expect_equal(unname(Rq24$statuses[["MULTIPLICITY_STATUS"]]), "FAIL")
  Rq9 <- one("fixed_DLM_summary.csv", function(x) { i <- which(is.finite(x$q9))[1]; x$q9[i] <- NA; x }, "q9"); expect_equal(unname(Rq9$statuses[["MULTIPLICITY_STATUS"]]), "FAIL")                             # a missing family member is a failure, never a pass
  Rs <- one("fixed_DLM_summary.csv", function(x) { i <- which(x$separation %in% FALSE)[1]; x$separation[i] <- TRUE; x }, "sep"); expect_equal(unname(Rs$statuses[["ARCHIVED_168_STATUS"]]), "FAIL"); expect_true(any(Rs$flags$class == "DIFFERENT"))
  Rc <- one("fixed_DLM_lag_curves.csv", function(x) { x$estimate[100] <- x$estimate[100] + 0.01; x }, "curve"); expect_equal(unname(Rc$statuses[["CURVES_STATUS"]]), "FAIL"); expect_equal(unname(Rc$statuses[["ARCHIVED_168_STATUS"]]), "PASS")
  Rcc <- one("fixed_DLM_cumulative_curves.csv", function(x) { x$cumulative_se[40] <- x$cumulative_se[40] * 1.5; x }, "cumcurve"); expect_equal(unname(Rcc$statuses[["CURVES_STATUS"]]), "FAIL")
})

test_that("peak lag: exact integer or a numerical tie of |estimate| passes; any other integer fails", {
  keys <- c("outcome", "metric", "model_tier"); new <- data.frame(outcome = "o", metric = "m", model_tier = c("A", "B", "C"), peak_lag = c(3L, 4L, 5L), stringsAsFactors = FALSE); ref <- data.frame(outcome = "o", metric = "m", model_tier = c("A", "B", "C"), peak_lag = c(3L, 3L, 3L), stringsAsFactors = FALSE)
  cur <- do.call(rbind, lapply(c("A", "B", "C"), function(t) data.frame(outcome = "o", metric = "m", model_tier = t, lag = 1:6, estimate = c(0, 0.1, if (t == "B") -0.5 else 0.5, if (t == "B") 0.5 + 1e-9 else 0.3, if (t == "C") 0.31 else 0.1, 0), stringsAsFactors = FALSE)))
  tol <- list(dlm_abs = CFG$tolerances$dlm_abs, dlm_rel = CFG$tolerances$dlm_rel); r <- .siw_av_peak(new, ref, cur, keys, c(keys, "lag"), tol)
  expect_equal(r$class, c("EXACT", "PEAK_LAG_TIE_WITHIN_TOLERANCE", "DIFFERENT"))
})

test_that("missing, altered or incomplete reference evidence fails the provenance and nothing is compared or passed", {
  # missing file
  d <- variant(S0, function(d) unlink(file.path(d, "fixed_DLM_lag_curves.csv")), "missing"); R <- run_syn(S0, d); expect_equal(unname(R$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL"); expect_true(all(R$statuses[c("ARCHIVED_168_STATUS", "LAG_FORM_216_STATUS", "GLOBAL_TESTS_STATUS", "MULTIPLICITY_STATUS", "CURVES_STATUS", "LAG_BASIS_STATUS")] == "NOT_TESTED")); expect_equal(unname(R$statuses[["COVERAGE_STATUS"]]), "FAIL"); expect_identical(R$status, "VERIFICATION_FAILURE")
  expect_false(file.exists(file.path(R$out_dir, "archived_comparison_summary.csv"))); expect_true(any(grepl("REQUIRED REFERENCE ABSENT", R$provenance$item)))
  # altered file with the protocol of the original: hash and byte mismatch
  d2 <- variant(S0, function(d) rw(d, "fixed_DLM_summary.csv", function(x) { x$n[1] <- x$n[1] + 1L; x }), "hash"); R2 <- run_syn(S0, d2, pj = syn_protocol(S0$refdir, tempfile(fileext = ".json"))); expect_equal(unname(R2$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL"); expect_true(any(grepl("SHA-256", R2$provenance$item) & !R2$provenance$ok))
  # duplicate key; dropped record; missing required column
  d3 <- variant(S0, function(d) rw(d, "fixed_DLM_summary.csv", function(x) { x[2, c("outcome", "metric", "model_tier")] <- x[1, c("outcome", "metric", "model_tier")]; x }), "dup"); R3 <- run_syn(S0, d3); expect_equal(unname(R3$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL"); expect_true(any(R3$provenance$item == "keys unique" & !R3$provenance$ok))
  d4 <- variant(S0, function(d) rw(d, "DLM_lag_length_df_sensitivity_summary.csv", function(x) x[-10, ]), "drop"); R4 <- run_syn(S0, d4); expect_equal(unname(R4$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL"); expect_true(any(R4$provenance$item == "key set equals the complete expected grid" & !R4$provenance$ok))
  d5 <- variant(S0, function(d) rw(d, "fixed_DLM_summary.csv", function(x) { x$q24 <- NULL; x }), "nocol"); R5 <- run_syn(S0, d5); expect_equal(unname(R5$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL"); expect_true(any(grepl("required columns present", R5$provenance$item) & !R5$provenance$ok))
  d6 <- variant(S0, function(d) rw(d, "fixed_DLM_summary.csv", function(x) { x$outcome[3] <- ""; x }), "blank"); R6 <- run_syn(S0, d6); expect_equal(unname(R6$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL")
})

test_that("a compared-cell count that differs from the declared count fails coverage (a silently skipped comparison cannot pass)", {
  R <- run_syn(S0, pj = syn_protocol(S0$refdir, tempfile(fileext = ".json"), tamper_cells = 1L)); expect_equal(unname(R$statuses[["COVERAGE_STATUS"]]), "FAIL"); expect_equal(unname(R$statuses[["ARCHIVED_168_STATUS"]]), "FAIL"); expect_false(R$summary$cells_as_declared[1])
})

test_that("supplied rainfall missingness is preserved: the data passed to the fits are unchanged, the rainfall-total models lose the incomplete participant, nothing is imputed", {
  if (siw_skip_source_only("test-23-archived-dlm-validation: supplied rainfall missingness is preserved: the data passed to the fit")) return(invisible(NULL))
  rep <- siw_rainfall_anomalies(S0$dat, CFG, 59L); expect_gt(rep$n_cells, 0L); g <- R0$fits$grid; n_rain <- unique(g$n[g$metric == "IMS_rainfall_total"]); n_other <- unique(g$n[g$metric != "IMS_rainfall_total"]); expect_lt(max(n_rain), max(n_other))
  st <- readLines(file.path(R0$out_dir, "ARCHIVED_DLM_STATUS.txt")); expect_true(any(grepl("DATA_PASSED_TO_FITS_UNCHANGED=TRUE", st))); expect_true(any(grepl("RAINFALL_CELLS_MISSING_SUPPLIED=1", st)))
  src <- readLines(file.path(ROOT, "R", "archived_validate.R")); code <- src[!grepl("^\\s*#", src)]; expect_false(any(grepl("siw_rainfall_zero_reconstruction_sensitivity\\(", code)))
})

test_that("the archived glm epsilon is applied to a copy of the configuration only; the final-model configuration and the package functions are unchanged", {
  cfg0 <- CFG; cfg1 <- .siw_av_config(CFG, PA); expect_equal(cfg1$dlm$final$glm_epsilon, 1e-10); expect_equal(cfg1$dlm$final$glm_maxit, 200); expect_identical(CFG, cfg0); expect_equal(CFG$dlm$final$glm_epsilon, 1e-12)
  md <- jsonlite::fromJSON(file.path(R0$out_dir, "archived_metadata.json")); expect_equal(md$glm_epsilon_used, 1e-10); expect_equal(md$draws, 0L); expect_match(md$protocol_hash, "^[0-9A-F]{64}$")                 # the synthetic runs use a protocol copy describing the synthetic reference folder
  g <- R0$fits$grid; expect_equal(nrow(g), 168L); expect_equal(nrow(R0$fits$sensitivity), 216L); expect_equal(sum(is.finite(g$q24)), 24L); expect_equal(sum(is.finite(g$q9)), 9L); expect_equal(sum(is.finite(R0$fits$sensitivity$q216)), 216L)
  expect_equal(length(unique(paste(g$outcome, g$metric, g$model_tier))), 168L); expect_equal(sort(unique(g$model_tier)), sort(CFG$tier_order)); expect_equal(sort(unique(R0$fits$sensitivity$sensitivity_specification)), sort(c("L35_DF2", "L35_DF3", "L35_DF4", "L45_DF2", "L45_DF3", "L45_DF4", "L59_DF2", "L59_DF3", "L59_DF4")))
  expect_true(all(g$lag_end == 59L) && all(g$lag_df == 3L) && all(g$global_df1 == 3L)); s <- R0$fits$sensitivity; expect_true(all(s$global_df1 == s$lag_df)); expect_true(all(s$model_tier == "M2_L"))
})

test_that("the lag bases of all nine (L, df) combinations are verified against the independent construction; dlnm equality is NOT_TESTED, never PASS", {
  b <- R0$bases; expect_equal(nrow(unique(b[, c("lag_end", "lag_df")])), 9L); expect_true(setequal(paste(b$lag_end, b$lag_df), paste(rep(c(35, 45, 59), each = 3), rep(c(2, 3, 4), 3)))); expect_true(all(b$status[!grepl("dlnm", b$check)] == "PASS")); d <- b[grepl("dlnm", b$check), ]
  if (!requireNamespace("dlnm", quietly = TRUE)) expect_true(all(d$status == "NOT_TESTED")); expect_equal(unname(R0$statuses[["LAG_BASIS_STATUS"]]), "PASS"); expect_true(all(b$value[b$check == "package basis has df columns"] == b$lag_df[b$check == "package basis has df columns"]))
  B <- siw_lag_basis(35L, 2L); expect_equal(dim(B), c(35L, 2L))
})

test_that("internal consistency: reference and package q families recompute with p.adjust BH; L59_DF3 repeats the M2_L grid; branch reuse is identical", {
  i <- R0$internal; expect_true(all(i$status == "PASS")); expect_true(any(grepl("q24 equals stats::p.adjust", i$check))); expect_true(any(grepl("L59_DF3", i$check))); expect_true(any(grepl("identical 'global_p'", i$check))); expect_true(sum(grepl("^package: archived functions", i$check)) >= 8L)
  g <- R0$fits$grid; m2 <- g[g$model_tier == "M2_L", ]; expect_equal(m2$q24, stats::p.adjust(m2$global_p, "BH"), tolerance = 1e-12); r9 <- m2$metric %in% CFG$representative_metrics; expect_equal(m2$q9[r9], stats::p.adjust(m2$global_p[r9], "BH"), tolerance = 1e-12)
})

test_that("the run writes only to its own folder, refuses an output folder that overlaps the reference folder, and never touches the reference files", {
  if (siw_skip_source_only("test-23-archived-dlm-validation: the run writes only to its own folder, refuses an output folder that o")) return(invisible(NULL))
  before <- vapply(list.files(S0$refdir, full.names = TRUE), function(f) siw_file_hash(f)$value, ""); R <- run_syn(S0); after <- vapply(list.files(S0$refdir, full.names = TRUE), function(f) siw_file_hash(f)$value, ""); expect_identical(before, after)
  expect_error(siw_run_archived_dlm_validation(S0$csv, S0$refdir, file.path(S0$refdir, "inside"), S0$config), "OUTPUT_AND_REFERENCE_FOLDERS_OVERLAP")
  src <- readLines(file.path(ROOT, "R", "archived_validate.R")); code <- src[!grepl("^\\s*#", src)]; expect_false(any(grepl("unlink\\(|file\\.remove|set\\.seed|mvrnorm|siw_mvn_draws|siw_with_seed", code)))
  expect_false(any(grepl("MASS::", code)))
})

test_that("the entry point and wrapper exist and the exit codes are declared", {
  if (siw_skip_source_only("test-23-archived-dlm-validation: the entry point and wrapper exist and the exit codes are declared")) return(invisible(NULL))
  expect_true(file.exists(file.path(ROOT, "run_archived_dlm_validation.ps1"))); expect_true(file.exists(file.path(ROOT, "scripts", "_run_archived_dlm.ps1"))); r <- readLines(file.path(ROOT, "run.R")); expect_true(any(grepl("mode == \"dlmarchived\"", r)))
  w <- paste(readLines(file.path(ROOT, "scripts", "_run_archived_dlm.ps1")), collapse = "\n"); expect_match(w, "ARCHIVED_168_STATUS"); expect_match(w, "SIMULTANEOUS_CRITICAL_VALUES"); expect_match(w, "finalise_dlm_validation.R")
})
