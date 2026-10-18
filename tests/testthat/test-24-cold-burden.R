# Phase 3F: cold-burden scan: burden calculations, thresholds, fits, scales, inference, multiplicity, reference handling and failure paths. Synthetic data only; no study data and no frozen file is read here.
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
if (!exists("expect_gt", mode = "function")) expect_gt <- function(a, b) expect_true(a > b)
if (!exists("expect_lt", mode = "function")) expect_lt <- function(a, b) expect_true(a < b)
CFG <- siw_load_config(); PC <- siw_cold_burden_protocol(); hsh <- function(f) siw_file_hash(file.path(ROOT, f))$value

test_that("the protocol is declared before the run: two hashed references, expected counts, traced specification, interpretation not assessed", {
  if (siw_skip_source_only("test-24-cold-burden: the protocol is declared before the run: two hashed references, expect")) return(invisible(NULL))
  expect_match(PC$status_of_this_document, "DECLARED BEFORE THE MANUAL RUN"); expect_match(PC$status_of_this_document, "scientific interpretation"); expect_equal(length(PC$reference_files), 2L)
  expect_private(PC$reference_files[[1]]$sha256, "cold_burden_reference_1"); expect_private(PC$reference_files[[2]]$sha256, "cold_burden_reference_2")
  expect_equal(vapply(PC$reference_files, function(z) z$expected_records, 0L), c(22302L, 15L)); expect_equal(vapply(PC$comparisons, function(z) z$expected_compared_cells, 0L), c(892943L, 57L)); expect_equal(PC$comparisons[[1]]$expected_infinite_equal_pairs, 3450L)
  P <- PC$parameters; expect_equal(unlist(P$thermal_metrics), c("UTCI", "MRT", "ERA5_air_temperature", "IMS_temperature")); expect_equal(unlist(P$percentiles), c(P10 = 0.10, P05 = 0.05, P025 = 0.025)); expect_equal(P$quantile_type, 7); expect_equal(unlist(P$utci_standard_thresholds), c(9, 0, -13)); expect_equal(P$min_participant_days_below, 20); expect_equal(P$min_support_participants, 8); expect_equal(P$lag_end, 59)
  expect_equal(P$glm_epsilon, 1e-8); expect_equal(P$glm_maxit, 25); expect_equal(CFG$glm$cumulative_epsilon, P$glm_epsilon); expect_equal(CFG$glm$cumulative_maxit, P$glm_maxit); expect_equal(CFG$dlm$final$glm_epsilon, 1e-12)
  expect_equal(P$expected_grid$windows * P$expected_grid$thresholds * P$expected_grid$variants * P$expected_grid$outcomes, 22302L); expect_equal(14L * 1593L, 22302L); expect_equal(4L * 3L * 59L * 3L * 3L, 6372L); expect_equal(12744L + 6372L + 3186L, 22302L)
  expect_match(PC$scope$reconciliation_with_the_earlier_archive, "12,744"); expect_match(PC$scope$reconciliation_with_the_earlier_archive, "NOT used as a reference"); expect_match(PC$scientific_interpretation, "NOT assessed")
  expect_match(PC$specification_trace$reported_findings_to_check, "22,302"); expect_match(PC$specification_trace$reported_findings_to_check, "no source conflict"); expect_equal(PC$tolerance$within_tolerance$abs, CFG$tolerances$cumulative_abs); expect_equal(PC$tolerance$within_tolerance$rel, CFG$tolerances$cumulative_rel)
  expect_identical(hsh("config/cold_burden_validation_protocol.json"), PC$protocol_hash); expect_identical(hsh("inst/extdata/cold_burden_validation_protocol.json"), PC$protocol_hash)
  expect_identical(hsh("R/dlm_v21.R"), "E143D245051200E4B2EBA0E3579F2D1487DB752759A4BB1DFD53D1B5CD6E11BD"); expect_identical(hsh("R/ordinal.R"), "010882F8DFD0756A8577429F8A3E5423F45220F2D39AA0A4067CD2ED73704441"); expect_private(hsh("config/archived_dlm_validation_protocol.json"), "archived_dlm_validation_protocol")
})

test_that("burden features match hand-calculated examples, including the strict threshold boundary, a missing day, an infinite day and runs", {
  x <- c(6, 4, 3, 5, 2, 2, NA, 1); thr <- 5                                              # cold: lags 2, 3, 5, 6, 8 (5 is not below 5; NA is never cold); deficits 1, 2, 3, 3, 4
  f <- siw_cold_feature_curves(matrix(x, 1L), thr); expect_equal(f$degree_burden[1, ], c(0, 1, 3, 3, 6, 9, 9, 13)); expect_equal(f$cold_day_count[1, ], c(0, 1, 2, 2, 3, 4, 4, 5)); expect_equal(f$persistent_cold_days_ge2[1, ], c(0, 0, 2, 2, 2, 4, 4, 4)); expect_equal(f$longest_cold_spell[1, ], c(0, 1, 2, 2, 2, 2, 2, 2))
  expect_equal(f$mean_cold_intensity[1, ], c(0, 1, 1.5, 1.5, 2, 2.25, 2.25, 2.6)); r8 <- siw_cold_features_reference(x, thr); expect_equal(unname(r8), c(13, 5, 4, 2, 2.6)); r3 <- siw_cold_features_reference(x[1:3], thr); expect_equal(unname(r3), c(3, 2, 2, 2, 1.5))
  expect_equal(unname(siw_cold_features_reference(rep(5, 4), 5)), c(0, 0, 0, 0, 0)); expect_equal(unname(siw_cold_features_reference(c(NA, NA, NA), 5)), c(0, 0, 0, 0, 0)); expect_equal(unname(siw_cold_features_reference(c(-Inf, 4, Inf), 5)), c(1, 1, 0, 1, 1))     # -Inf is not finite, so not cold
  expect_equal(unname(siw_cold_features_reference(c(4, NA, 4), 5)), c(2, 2, 0, 1, 1)); g <- siw_cold_feature_curves(matrix(c(4, NA, 4), 1L), 5); expect_equal(g$longest_cold_spell[1, ], c(1, 1, 1)); expect_equal(g$persistent_cold_days_ge2[1, ], c(0, 0, 0))   # a missing day ends a run
  expect_equal(unname(siw_cold_features_reference(c(4, 3, 3, 4, 4, 5, 4), 5)), c(1 + 2 + 2 + 1 + 1 + 1, 6, 5, 5, 8 / 6)); one <- siw_cold_feature_curves(matrix(4, 1L, 1L), 5); expect_equal(one$degree_burden[1, 1], 1); expect_equal(one$longest_cold_spell[1, 1], 1)
  expect_equal(siw_cold_feature_curves(matrix(c(-3, -2, 0), 1L), -2)$degree_burden[1, ], c(1, 1, 1)); expect_equal(siw_cold_feature_curves(matrix(c(-3, -2, 0), 1L), -2)$cold_day_count[1, ], c(1, 1, 1))
})

test_that("the optimised cumulative features equal the independent per-participant route for every window, on random histories with missing values, ties and boundary values", {
  set.seed(5); H <- matrix(round(stats::rnorm(40 * 30, 8, 3)), 40, 30); H[sample(length(H), 90)] <- NA; H[sample(length(H), 10)] <- Inf; thr <- 8; f <- siw_cold_feature_curves(H, thr); md <- 0
  for (i in 1:40) for (w in 1:30) { r <- siw_cold_features_reference(H[i, 1:w], thr); for (m in c("degree_burden", "cold_day_count", "persistent_cold_days_ge2", "longest_cold_spell", "mean_cold_intensity")) md <- max(md, abs(f[[m]][i, w] - r[[m]])) }
  expect_lt(md, 1e-12); expect_true(all(f$longest_cold_spell <= f$cold_day_count)); expect_true(all(f$persistent_cold_days_ge2 <= f$cold_day_count)); expect_true(all(apply(f$degree_burden, 1L, function(z) all(diff(z) >= 0))))
})

test_that("thresholds: pooled type-7 percentiles verified independently, strict counts below, fixed UTCI thresholds, the 20-participant-day fitting rule at its boundary", {
  raw <- siw_make_synthetic(n = 55L, seed = 21L, config = CFG); dat <- siw_prepare_data(raw, CFG); thr <- siw_cold_thresholds(dat, CFG, PC); expect_equal(nrow(thr), 15L); expect_equal(thr$threshold_name[thr$metric == "UTCI"], c("P10", "P05", "P025", "UTCI_LT_9", "UTCI_LT_0", "UTCI_LT_-13")); expect_equal(unique(thr$metric), c("UTCI", "MRT", "ERA5_air_temperature", "IMS_temperature"))
  v <- siw_cold_verify_thresholds(dat, CFG, thr, PC); expect_true(all(v$status == "PASS")); expect_true(all(v$abs_diff <= 1e-12)); u <- as.numeric(as.matrix(dat[, siw_daily_names("UTCI", 59L)])); u <- u[is.finite(u)]; expect_equal(thr$threshold[1], stats::quantile(u, 0.10, type = 7, names = FALSE)); expect_equal(thr$participant_days_below[1], sum(u < thr$threshold[1]))
  expect_equal(.siw_dv_q7(c(1, 2, 3, 4, 10), 0.25), stats::quantile(c(1, 2, 3, 4, 10), 0.25, type = 7, names = FALSE)); expect_equal(thr$participant_days_total[thr$metric == "ERA5_air_temperature"][1], sum(is.finite(as.numeric(as.matrix(dat[, siw_daily_names("T2M", 59L)])))))
  expect_equal(thr$participant_days_below[thr$threshold_name == "UTCI_LT_9"], sum(u < 9)); expect_equal(thr$participant_days_below[thr$threshold_name == "UTCI_LT_-13"], 0L)
  bt <- thr; bt$participant_days_below[1:2] <- c(20L, 19L); sc <- siw_cold_burden_scan(dat, CFG, PC, thresholds = bt[bt$metric == "UTCI" & bt$threshold_name %in% c("P10", "P05"), ], windows = 1L, diagnostics = FALSE); expect_equal(unique(sc$results$threshold_name), "P10")     # exactly 20 is fitted, 19 is not
  bad <- thr; bad$participant_days_below[1] <- bad$participant_days_below[1] + 1L; expect_equal(siw_cold_verify_thresholds(dat, CFG, bad, PC)$status[1], "FAIL")
})

S <- local({ raw <- siw_make_synthetic(n = 55L, seed = 21L, config = CFG); dat <- siw_prepare_data(raw, CFG); sc <- siw_cold_burden_scan(dat, CFG, PC, windows = c(1L, 29L)); list(dat = dat, sc = sc) })

test_that("the scan has the declared structure: counts, keys, order, support flags, estimability, sample size and parameters", {
  r <- S$sc$results; thr <- S$sc$thresholds; nfit <- sum(thr$participant_days_below >= 20L); expect_equal(nrow(r), nfit * 2L * 9L * 3L); expect_equal(nrow(unique(r[, c("outcome", "metric", "threshold_name", "burden_measure", "transformation", "window_or_lag")])), nrow(r)); expect_equal(length(unique(r$model_id)), nrow(r))
  expect_equal(table(paste(r$burden_measure, r$transformation))[["degree_burden raw"]], nfit * 2L * 3L); expect_equal(sum(r$transformation != "raw"), nfit * 2L * 2L * 2L * 3L); expect_equal(unique(r$parameter_count[r$status == "COMPLETED"]), 8L); expect_true(all(r$n == 55L)); expect_equal(unique(r$model_tier), "M2_L"); expect_equal(unique(r$exposure_unit), "cold_burden")
  expect_equal(r$outcome[1:3], c("UCLA_Lonliness_SCORE", "Isolation_cont", "Int_dummy")); expect_true(all(r$support_eligible == (r$participants_with_positive_burden >= 8L))); ne <- r[r$status == "NOT_ESTIMABLE", ]; expect_gt(nrow(ne), 0L); expect_true(all(ne$participants_with_positive_burden == 0L)); expect_true(all(grepl("^RANK_DEFICIENT", ne$not_estimable_reason)))
  expect_true(all(is.infinite(ne$condition_number) & is.infinite(ne$maximum_vif))); expect_true(all(is.na(ne$coefficient))); expect_true(all(ne$exposure_sd == 0)); expect_true(all(ne$residual_df == 55L - ne$rank)); expect_true(all(is.na(ne$q_global))); expect_true(all(r$status[r$participants_with_positive_burden > 0L] == "COMPLETED") || any(r$status[r$participants_with_positive_burden > 0L] == "NOT_ESTIMABLE"))
  expect_true(all(r$transformation[r$burden_measure %in% c("cold_day_count", "persistent_cold_days_ge2", "longest_cold_spell")] == "raw")); expect_equal(max(r$window_or_lag), 29L)
})

test_that("the optimised fit equals a direct call of the validated engine and the independent formula-based refit (coefficient scale, SE, p, CI, AIC, F / LR)", {
  r <- S$sc$results; thr <- S$sc$thresholds; rf <- siw_cold_independent_refit(S$dat, CFG, r, thr, PC); expect_gt(nrow(rf), 100L); expect_true(all(rf$status == "PASS")); expect_true(all(c("coefficient", "model_based_se", "nominal_p", "aic") %in% rf$field)); expect_true(any(rf$field == "lr_p")); expect_true(any(rf$field == "robust_hc3_se"))
  i <- which(r$status == "COMPLETED" & r$outcome == "UCLA_Lonliness_SCORE")[7L]; tr <- thr[thr$metric == r$metric[i] & thr$threshold_name == r$threshold_name[i], ]; sp <- CFG$metrics[CFG$metrics$metric == r$metric[i], ]; H <- as.matrix(S$dat[, siw_daily_names(sp$daily_prefix, 59L)]); x <- siw_cold_feature_curves(H, tr$threshold)[[r$burden_measure[i]]][, r$window_or_lag[i]]
  x <- switch(r$transformation[i], raw = x, log1p = log1p(x), sqrt = sqrt(x)); Xc <- siw_tier_matrix(S$dat, CFG$tiers$M2_L); X <- cbind(Xc[, 1L, drop = FALSE], exposure = x, Xc[, -1L, drop = FALSE]); st <- siw_fit_gaussian(X, S$dat$UCLA_Lonliness_SCORE, 2L); expect_equal(r$coefficient[i], st$coefficient, tolerance = 1e-12); expect_equal(r$robust_hc3_se[i], st$robust_hc3_se, tolerance = 1e-12)
  # identities and multiplicity on the scan; tampering is detected
  id <- siw_cold_identities(r, NULL); expect_true(all(id$status == "PASS")); r2 <- r; r2$standardised_effect[which(r2$status == "COMPLETED")[3]] <- 99; expect_true(any(siw_cold_identities(r2, NULL)$status == "FAIL")); r3 <- r; r3$odds_ratio_sd[which(r3$outcome_family == "binomial" & r3$status == "COMPLETED")[2]] <- 1e3; expect_true(any(siw_cold_identities(r3, NULL)$status == "FAIL"))
  r4 <- r; j <- which(r4$status == "COMPLETED")[5]; r4$nominal_p[j] <- min(1, r4$nominal_p[j] + 0.05); expect_true(any(siw_cold_identities(r4, NULL)$status == "FAIL"))
  m <- siw_cold_verify_multiplicity(r); expect_true(all(m$status %in% c("PASS", "INFO"))); r5 <- r; r5$q_global[j] <- r5$q_global[j] / 2; expect_true(any(siw_cold_verify_multiplicity(r5)$status == "FAIL")); r6 <- r; r6$q_curve[j] <- NA; expect_true(any(siw_cold_verify_multiplicity(r6)$status == "FAIL"))
  e <- r[r$status == "COMPLETED", ]; expect_equal(e$q_global, stats::p.adjust(e$nominal_p, "BH"), tolerance = 1e-12); expect_equal(sum(is.finite(r$q_outcome)), sum(r$status == "COMPLETED"))
  sdy <- vapply(CFG$outcomes$outcome, function(o) stats::sd(as.numeric(S$dat[[o]])), 0); expect_true(all(siw_cold_identities(r, sdy)$status == "PASS"))
})

test_that("numerical-failure paths are classified and never crash: all-zero burden, constant covariate, separation, non-finite inputs", {
  cache <- .siw_new_cache(); Xc <- siw_tier_matrix(S$dat, CFG$tiers$M2_L); ok <- stats::complete.cases(Xc); y <- as.numeric(S$dat$UCLA_Lonliness_SCORE)
  z <- .siw_cold_one(y, "gaussian", rep(0, nrow(Xc)), Xc, ok, "UCLA_Lonliness_SCORE", "M2_L", cache, CFG, PC$parameters); expect_false(z$est); expect_match(z$st$reason, "RANK_DEFICIENT"); expect_true(is.infinite(z$d$condition_number))
  yb <- as.numeric(S$dat$Int_dummy); xs <- rep(0, nrow(Xc)); xs[yb == 1] <- 1; sp <- .siw_cold_one(yb, "binomial", xs, Xc, ok, "Int_dummy", "M2_L", cache, CFG, PC$parameters); expect_true(sp$est); expect_identical(sp$st$separation_status, "SUSPECTED")
  yy <- rep(1, length(yb)); no <- .siw_cold_one(yy, "binomial", seq_along(yb) * 1, Xc, ok, "Int_dummy", "M2_L", cache, CFG, PC$parameters); expect_false(no$est)
  xna <- seq_along(yb) * 1; xna[c(2, 9)] <- NA; na <- .siw_cold_one(y, "gaussian", xna, Xc, ok, "UCLA_Lonliness_SCORE", "M2_L", cache, CFG, PC$parameters); expect_equal(na$n, sum(ok) - 2L)         # a missing exposure is excluded by the complete-case rule
  Xz <- Xc; Xz[, "Econ_STS"] <- 1; zv <- .siw_cold_one(y, "gaussian", seq_along(y) * 1, Xz, stats::complete.cases(Xz), "UCLA_Lonliness_SCORE", "M2_L", .siw_new_cache(), CFG, PC$parameters); expect_false(zv$est)
  q <- siw_cold_assign_q(data.frame(outcome = "o", metric = "m", threshold_name = "t", burden_measure = "b", transformation = "raw", nominal_p = c(0.01, NA, 0.5, NaN), status = c("COMPLETED", "NOT_ESTIMABLE", "COMPLETED", "COMPLETED"), stringsAsFactors = FALSE)); expect_equal(q$q_global[c(1, 3)], c(0.02, 0.5)); expect_true(is.na(q$q_global[2]) && is.na(q$q_global[4]))
})

# ---- a synthetic frozen-format reference built from the scan (circular by construction: it exercises the runner, the strict rules and the failure paths; the non-circular check is validation/legacy_equivalence_cold_burden_synthetic.R)
syn_ref <- function(base, windows = c(1L, 29L), noise = 0) {
  unlink(base, recursive = TRUE); dir.create(base, recursive = TRUE); raw <- siw_make_synthetic(n = 55L, seed = 21L, config = CFG); csv <- file.path(base, "syn.csv"); utils::write.csv(raw, csv, row.names = FALSE, na = ""); res <- S$sc$results; thr <- S$sc$thresholds
  if (noise > 0) { set.seed(3); for (f in c("ci_lower", "ci_upper", "aic")) res[[f]] <- res[[f]] * (1 + noise * stats::runif(nrow(res), -1, 1)) }
  refdir <- file.path(base, "ref"); dir.create(refdir); utils::write.csv(res, file.path(refdir, "cold_burden_results.csv"), row.names = FALSE, na = ""); utils::write.csv(thr, file.path(refdir, "cold_thresholds.csv"), row.names = FALSE, na = ""); list(base = base, csv = csv, refdir = refdir, res = res, thr = thr, windows = windows)
}
syn_protocol <- function(Sy, refdir, out_json, tamper_cells = 0L, windows = Sy$windows) {
  p <- siw_cold_burden_protocol(); for (i in 1:2) { f <- file.path(refdir, p$reference_files[[i]]$file); if (file.exists(f)) { p$reference_files[[i]]$sha256 <- siw_file_hash(f)$value; p$reference_files[[i]]$bytes <- file.info(f)$size } }
  rd <- function(f) if (file.exists(file.path(refdir, f))) utils::read.csv(file.path(refdir, f), stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")) else NULL; d <- rd("cold_burden_results.csv"); t <- rd("cold_thresholds.csv"); fields <- unlist(p$comparisons[[1]]$fields)
  if (!is.null(d)) { fin <- vapply(fields, function(f) sum(is.finite(suppressWarnings(as.numeric(d[[f]])))), 0L); inf <- vapply(fields, function(f) sum(is.infinite(suppressWarnings(as.numeric(d[[f]])))), 0L); p$comparisons[[1]]$expected_compared_cells <- sum(fin) + tamper_cells; p$comparisons[[1]]$expected_infinite_equal_pairs <- sum(inf); p$comparisons[[1]]$expected_records <- nrow(d); p$reference_files[[1]]$expected_records <- nrow(d) }
  if (!is.null(t)) { p$comparisons[[2]]$expected_compared_cells <- sum(vapply(unlist(p$comparisons[[2]]$fields), function(f) sum(!is.na(t[[f]])), 0L)); p$comparisons[[2]]$expected_records <- nrow(t); p$reference_files[[2]]$expected_records <- nrow(t) }
  p$parameters$windows_subset <- as.list(windows); p$parameters$expected_grid <- list(windows = length(windows), thresholds = sum(Sy$thr$participant_days_below >= 20L), variants = 9L, outcomes = 3L)
  r <- Sy$res; k <- which(r$status == "COMPLETED" & is.finite(r$maximum_leverage))[4L]; p$parameters$si_row <- list(outcome = r$outcome[k], metric = r$metric[k], threshold_name = r$threshold_name[k], burden_measure = r$burden_measure[k], transformation = r$transformation[k], window_or_lag = r$window_or_lag[k], n_supported = sum(r$q_global <= 0.05, na.rm = TRUE),
    q_global_range = c(r$q_global[k] * 0.999, r$q_global[k] * 1.001), positive_burden = r$participants_with_positive_burden[k], leverage_range = c(r$maximum_leverage[k] - 0.001, r$maximum_leverage[k] + 0.001), influential = r$influential_case_count[k])
  p$protocol_path <- NULL; p$protocol_hash <- NULL; jsonlite::write_json(p, out_json, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA); out_json
}
run_syn <- function(Sy, refdir = Sy$refdir, pj = NULL) { if (is.null(pj)) pj <- syn_protocol(Sy, refdir, tempfile(fileext = ".json")); suppressMessages(siw_run_cold_burden_validation(Sy$csv, refdir, file.path(Sy$base, paste0("run_", basename(tempfile()))), CFG, protocol_path = pj)) }
vref <- function(Sy, edit, nm) { d <- file.path(Sy$base, paste0("ref_", nm)); unlink(d, recursive = TRUE); dir.create(d); file.copy(list.files(Sy$refdir, full.names = TRUE), d); edit(d); d }
rw <- function(d, f, fun) { p <- file.path(d, f); x <- utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")); x <- fun(x); utils::write.csv(x, p, row.names = FALSE, na = "") }
Sy0 <- syn_ref(file.path(tempdir(), "syn_cold_exact")); R0 <- run_syn(Sy0)

test_that("a reference equal to the scan passes every status; counts, cells, infinite pairs and categorical fields are complete; interpretation stays unassessed; runtime is recorded", {
  s <- R0$statuses; expect_true(all(s[c("EXECUTION_STATUS", "INPUT_PROVENANCE_STATUS", "THRESHOLDS_STATUS", "NUMERICAL_REPRODUCTION_STATUS", "FEATURE_VERIFICATION_STATUS", "SCALE_AND_INFERENCE_STATUS", "MULTIPLICITY_STATUS", "INDEPENDENT_REFIT_STATUS", "SI_STATEMENT_STATUS", "COVERAGE_STATUS", "SOURCE_INTEGRITY_STATUS")] == "PASS")); expect_identical(R0$status, "OK_WITH_INTERPRETATION_NOT_ASSESSED")
  m <- R0$summary; expect_true(all(m$cells_as_declared)); expect_true(all(m$n_unclassified == 0L)); expect_true(all(m$n_exact == m$n_compared)); expect_equal(m$infinite_equal_pairs[1], 2L * sum(R0$results$status == "NOT_ESTIMABLE")); expect_true(all(R0$categorical$n_different == 0L)); expect_equal(nrow(R0$categorical), 10L)
  st <- readLines(file.path(R0$out_dir, "COLDBURDEN_STATUS.txt")); expect_true(any(grepl("^SCIENTIFIC_INTERPRETATION=NOT_ASSESSED", st))); expect_true(any(grepl("^MANUSCRIPT_CONCORDANCE_STATUS=REVIEW", st))); expect_true(any(grepl("^FIGURE_PROVENANCE_STATUS=NOT_TESTED", st))); expect_true(any(grepl("^DLNM_EQUALITY_STATUS=NOT_TESTED", st))); expect_true(any(grepl("^FINAL_NINE_L56_NUMERICAL_VALIDATION=ACCEPTED_UNDER_V2_1", st))); expect_true(any(grepl("^RUNTIME_SECONDS_SCAN=", st)))
  expect_true(all(is.finite(unlist(R0$seconds)))); expect_gt(R0$seconds[["scan_total"]], 0); expect_true(file.exists(file.path(R0$out_dir, "coldburden_descriptive_flags.csv"))); expect_true(file.exists(file.path(R0$out_dir, "coldburden_package_results.csv"))); expect_match(readLines(file.path(R0$out_dir, "SOURCE_FINGERPRINT.txt"))[3], "FALSE$")
  expect_true(all(R0$features$status == "PASS")); expect_true(all(R0$refit$status == "PASS")); expect_true(all(R0$si$status == "PASS"))
})

test_that("differences within the declared class pass as WITHIN_TOLERANCE; differences beyond it fail the numerical reproduction and nothing else", {
  Sn <- syn_ref(file.path(tempdir(), "syn_cold_noise"), noise = 1e-9); Rn <- run_syn(Sn); expect_equal(unname(Rn$statuses[["NUMERICAL_REPRODUCTION_STATUS"]]), "PASS"); expect_gt(Rn$summary$n_within_tolerance[1], 100L); expect_equal(Rn$summary$n_different[1], 0L)
  d <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) { i <- which(x$status == "COMPLETED")[10]; x$coefficient[i] <- x$coefficient[i] * 1.01; x }), "diff"); R <- run_syn(Sy0, d); expect_equal(unname(R$statuses[["NUMERICAL_REPRODUCTION_STATUS"]]), "FAIL"); expect_equal(R$summary$n_different[1], 1L); expect_equal(unname(R$statuses[["FEATURE_VERIFICATION_STATUS"]]), "PASS"); expect_identical(R$status, "VERIFICATION_FAILURE")
  d2 <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) { i <- which(x$status == "COMPLETED")[3]; x$q_curve[i] <- x$q_curve[i] / 2; x }), "q"); R2 <- run_syn(Sy0, d2); expect_equal(unname(R2$statuses[["NUMERICAL_REPRODUCTION_STATUS"]]), "FAIL")
  d3 <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) { x$status[5] <- "NOT_ESTIMABLE"; x }), "cat"); R3 <- run_syn(Sy0, d3); expect_equal(unname(R3$statuses[["NUMERICAL_REPRODUCTION_STATUS"]]), "FAIL"); expect_gt(sum(R3$categorical$n_different), 0L)
  d4 <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) { i <- which(x$status == "COMPLETED")[2]; x$robust_hc3_se[which(!is.na(x$robust_hc3_se))[1]] <- NA; x }), "na"); R4 <- run_syn(Sy0, d4); expect_equal(unname(R4$statuses[["NUMERICAL_REPRODUCTION_STATUS"]]), "FAIL")
  d5 <- vref(Sy0, function(d) rw(d, "cold_thresholds.csv", function(x) { x$threshold[2] <- x$threshold[2] + 0.01; x }), "thr"); R5 <- run_syn(Sy0, d5); expect_equal(unname(R5$statuses[["THRESHOLDS_STATUS"]]), "FAIL"); expect_equal(unname(R5$statuses[["NUMERICAL_REPRODUCTION_STATUS"]]), "PASS")
  d6 <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) { i <- which(is.infinite(x$condition_number))[1]; x$condition_number[i] <- 1e6; x }), "inf"); R6 <- run_syn(Sy0, d6); expect_equal(unname(R6$statuses[["NUMERICAL_REPRODUCTION_STATUS"]]), "FAIL")      # Inf on one side only fails
})

test_that("mathematical verification and scientific interpretation stay separate: scale, multiplicity, SI statement and refit failures fail their own status", {
  d <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) { i <- which(x$outcome_family == "binomial" & x$status == "COMPLETED")[3]; x$odds_ratio_sd[i] <- x$odds_ratio_sd[i] * 1.5; x }), "scale"); R <- run_syn(Sy0, d); expect_equal(unname(R$statuses[["SCALE_AND_INFERENCE_STATUS"]]), "FAIL"); expect_equal(unname(R$statuses[["NUMERICAL_REPRODUCTION_STATUS"]]), "FAIL")
  d2 <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) { i <- which(x$status == "COMPLETED")[6]; x$q_global[i] <- x$q_global[i] * 0.5; x }), "mult"); R2 <- run_syn(Sy0, d2); expect_equal(unname(R2$statuses[["MULTIPLICITY_STATUS"]]), "FAIL")
  sit <- .siw_cold_si_check(S$sc$results, PC)                                                  # the SI row does not exist in synthetic data: the check must FAIL there
  expect_true(any(sit$status == "FAIL")); expect_equal(unname(R0$statuses[["SI_STATEMENT_STATUS"]]), "PASS")
  expect_match(PC$scientific_interpretation, "not promoted|no model is promoted"); expect_false("SCIENTIFIC_INTERPRETATION_STATUS" %in% names(R0$statuses))
})

test_that("missing, altered or incomplete reference evidence fails the provenance and nothing is compared or passed", {
  d <- vref(Sy0, function(d) unlink(file.path(d, "cold_thresholds.csv")), "missing"); R <- run_syn(Sy0, d); expect_equal(unname(R$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL"); expect_equal(unname(R$statuses[["COVERAGE_STATUS"]]), "FAIL"); expect_true(all(R$statuses[c("THRESHOLDS_STATUS", "NUMERICAL_REPRODUCTION_STATUS", "FEATURE_VERIFICATION_STATUS", "MULTIPLICITY_STATUS")] == "NOT_TESTED")); expect_false(file.exists(file.path(R$out_dir, "coldburden_comparison_summary.csv")))
  d2 <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) { x$n[1] <- x$n[1] + 1L; x }), "hash"); R2 <- run_syn(Sy0, d2, pj = syn_protocol(Sy0, Sy0$refdir, tempfile(fileext = ".json"))); expect_equal(unname(R2$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL"); expect_true(any(grepl("SHA-256", R2$provenance$item) & !R2$provenance$ok))
  d3 <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) { x[2, c("outcome", "metric", "threshold_name", "burden_measure", "transformation", "window_or_lag")] <- x[1, c("outcome", "metric", "threshold_name", "burden_measure", "transformation", "window_or_lag")]; x }), "dup"); R3 <- run_syn(Sy0, d3); expect_equal(unname(R3$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL"); expect_true(any(R3$provenance$item == "keys unique" & !R3$provenance$ok))
  d4 <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) x[-7, ]), "drop"); R4 <- run_syn(Sy0, d4); expect_equal(unname(R4$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL"); d5 <- vref(Sy0, function(d) rw(d, "cold_burden_results.csv", function(x) { x$q_curve <- NULL; x }), "col"); R5 <- run_syn(Sy0, d5); expect_equal(unname(R5$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL")
  R6 <- run_syn(Sy0, pj = syn_protocol(Sy0, Sy0$refdir, tempfile(fileext = ".json"), tamper_cells = 1L)); expect_equal(unname(R6$statuses[["COVERAGE_STATUS"]]), "FAIL"); expect_equal(unname(R6$statuses[["NUMERICAL_REPRODUCTION_STATUS"]]), "FAIL")
  expect_error(siw_run_cold_burden_validation(Sy0$csv, Sy0$refdir, file.path(Sy0$refdir, "inside"), CFG), "OUTPUT_AND_REFERENCE_FOLDERS_OVERLAP")
})

test_that("the run reads and writes only what it should: references untouched, no resampling, no Firth, no imputation, no deletion; entry points exist", {
  if (siw_skip_source_only("test-24-cold-burden: the run reads and writes only what it should: references untouched, no")) return(invisible(NULL))
  before <- vapply(list.files(Sy0$refdir, full.names = TRUE), function(f) siw_file_hash(f)$value, ""); run_syn(Sy0); expect_identical(before, vapply(list.files(Sy0$refdir, full.names = TRUE), function(f) siw_file_hash(f)$value, ""))
  src <- readLines(file.path(ROOT, "R", "cold_burden.R")); code <- src[!grepl("^\\s*#", src)]; expect_false(any(grepl("unlink\\(|file\\.remove|set\\.seed|[^a-z_]sample\\(|siw_with_seed|mvrnorm|siw_firth|run_firth|siw_rainfall_zero_reconstruction_sensitivity\\(", code))); expect_false(any(grepl("read_sav|haven::|foreign::", code)))
  expect_true(file.exists(file.path(ROOT, "run_cold_burden_validation.ps1"))); expect_true(file.exists(file.path(ROOT, "scripts", "_run_cold_burden.ps1"))); expect_true(any(grepl("mode == \"coldburden\"", readLines(file.path(ROOT, "run.R"))))); expect_true(file.exists(file.path(ROOT, "validation", "legacy_equivalence_cold_burden_synthetic.R")))
})
