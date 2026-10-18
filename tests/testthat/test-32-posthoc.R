# Post hoc steps of config/posthoc_sensitivity_protocol_v1.json (13 Oct 2026). Synthetic data only; every comparison uses references built here from synthetic results.
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
P32 <- siw_posthoc_protocol(); D32 <- DAT; D32$EarlyLate <- siw_early_late(D32$AssessmentDay)
ctx32 <- function(synthetic = FALSE) { td <- file.path(tempdir(), paste0("ph32_", sample.int(1e6, 1))); dir.create(td, recursive = TRUE)
  list(synthetic = synthetic, run_id = basename(td), input_sha256 = "X", fingerprint = "F", out_dir = td, W = function(x, f) siw_write_csv(x, file.path(td, f)), options = list(), raw_participant_code = SYN$participant_code) }
pcopy <- function(mod) { p <- jsonlite::fromJSON(P32$protocol_path, simplifyVector = FALSE); p <- mod(p); f <- tempfile(fileext = ".json"); jsonlite::write_json(p, f, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null"); f }
refspec <- function(p, file, path, n) { for (i in seq_along(p$reference_files)) if (identical(p$reference_files[[i]]$file, file)) { p$reference_files[[i]]$sha256 <- siw_file_hash(path)$value; p$reference_files[[i]]$bytes <- file.info(path)$size; p$reference_files[[i]]$expected_records <- n }; p }
no_codes <- function(dir) !any(vapply(list.files(dir, recursive = TRUE, full.names = TRUE), function(f) any(grepl("SYN0[0-9][0-9]", readLines(f, warn = FALSE))), NA))

test_that("calendar anchor: interview and lag dates for different assessment days, including day 0", {
  expect_identical(P32$calendar_anchor$assessment_day_0, "2026-01-29")
  expect_identical(format(siw_interview_date(c(0, 30, 31, 54, 55, 61, 62, 91, 92), P32)), c("2026-01-29", "2026-02-28", "2026-03-01", "2026-03-24", "2026-03-25", "2026-03-31", "2026-04-01", "2026-04-30", "2026-05-01"))
  ld <- siw_lag_date(c(0, 30, 62), c(1, 2, 56), P32); expect_identical(ld[1, ], c("2026-01-28", "2026-01-27", "2025-12-04")); expect_identical(ld[2, 1], "2026-02-27"); expect_identical(ld[3, 3], "2026-02-04")
  expect_identical(format(siw_lag_date(31, 1, P32)), "2026-02-28"); expect_identical(format(siw_lag_date(40, 9, P32)), "2026-03-01"); expect_identical(format(siw_lag_date(40, 10, P32)), "2026-02-28")
  expect_equal(siw_lag_date(c(0, 10), 1:2, P32, as_offset = TRUE), matrix(c(-1, 9, -2, 8), 2))
  expect_identical(as.character(siw_early_late(c(0, 54, 55, 78))), c("Early", "Early", "Late", "Late"))
  expect_identical(format(siw_interview_date(c(61, 62, 91, 92), P32), "%m"), c("03", "04", "04", "05"))                 # the no-April rule: interview month April = days 62-91
})

test_that("UTCI bands (9 and 26 inclusive in no thermal stress), rain categories, seasons and the minimum-UCLA count on a hand-built history", {
  u <- c(9, 26, 26.001, 8.999, 0, -0.001, -13, -13.1); b <- siw_utci_band(u)
  expect_identical(b, c("no_thermal_stress", "no_thermal_stress", "any_heat_stress", "slight_cold_stress", "slight_cold_stress", "moderate_cold_stress", "moderate_cold_stress", "strong_cold_or_colder")); expect_true(is.na(siw_utci_band(NA)))
  d <- data.frame(AssessmentDay = c(0, 40), UCLA_Lonliness_SCORE = c(3, 3)); U <- matrix(10, 2, 56); U[1, 1:8] <- u; U[2, 56] <- NA; R <- matrix(0, 2, 56); R[1, 1:4] <- c(0.5, 1, 1.5, 0.99); R[2, 3] <- NA
  for (k in 1:56) { d[[sprintf("UTCI_L%02d", k)]] <- U[, k]; d[[sprintf("IMS_IDW_RAIN_TOTAL_L%02d", k)]] <- R[, k] }
  ex <- siw_exposure_aggregates(d, CFG, P32); g <- function(q) ex$value[ex$quantity == q]
  expect_equal(g("participant_days_L01_L56"), 112); expect_equal(g("utci_valid_days"), 111); expect_equal(g("utci_no_thermal_stress_days"), 2 + 103); expect_equal(g("utci_any_heat_stress_days"), 1); expect_equal(g("utci_strong_cold_or_colder_days"), 1)
  expect_equal(g("rain_valid_days"), 111); expect_equal(g("rain_rainy_ge1mm_days"), 2); expect_equal(g("rain_between_0_and_1mm_days"), 2); expect_equal(g("rain_dry_0mm_days"), 107)
  expect_equal(g("spring_participant_days"), 9); expect_equal(g("winter_participant_days"), 111 - 9)                      # participant on day 40: lags 1-9 fall on or after 1 Mar
  expect_identical(ex$value_text[ex$quantity == "exposure_date_span_first"], "2025-12-04"); expect_identical(ex$value_text[ex$quantity == "exposure_date_span_last"], "2026-03-09")
  expect_equal(g("ucla_participants_at_minimum"), 2); expect_false(any(c("participant_code", "assessment_day") %in% names(ex)))
})

test_that("exposure export: the independent recomputation agrees; acceptance PASS on declared values and FAIL on a wrong one (end to end)", {
  ex <- siw_exposure_aggregates(D32, CFG, P32); v <- .siw_ph_export_verify(D32, CFG, P32, ex); expect_true(all(v$status == "PASS"))
  pr <- pcopy(function(p) { i <- which(vapply(p$items, function(z) z$id, "") == "exposure_export"); for (j in seq_along(p$items[[i]]$acceptance)) { q <- p$items[[i]]$acceptance[[j]]; p$items[[i]]$acceptance[[j]]$value <- round(ex$value[ex$quantity == q$quantity], q$decimals) }; p })
  r <- siw_run_posthoc("exposure_export", SYN_CSV, file.path(tempdir(), paste0("ph32e_", sample.int(1e6, 1))), CFG, protocol_path = pr)
  expect_identical(unname(r$statuses[["EXPORT_ACCEPTANCE_STATUS"]]), "PASS"); expect_identical(unname(r$statuses[["MIN_UCLA_ARCHIVE_REPRODUCTION_STATUS"]]), "NOT_TESTED"); expect_identical(r$overall, "REVIEW")      # absent optional reference: never PASS
  expect_true(no_codes(r$out_dir)); st <- readLines(file.path(r$out_dir, "POSTHOC_STATUS.txt")); expect_true(any(grepl("^PRIMARY_INFERENCE_EFFECT=NONE", st))); expect_true(any(grepl("^POSTHOC_LABEL=POST_HOC$", st)))
  pr2 <- pcopy(function(p) { p <- jsonlite::fromJSON(pr, simplifyVector = FALSE); i <- which(vapply(p$items, function(z) z$id, "") == "exposure_export"); p$items[[i]]$acceptance[[1]]$value <- p$items[[i]]$acceptance[[1]]$value + 1; p })
  r2 <- siw_run_posthoc("exposure_export", SYN_CSV, file.path(tempdir(), paste0("ph32f_", sample.int(1e6, 1))), CFG, protocol_path = pr2)
  expect_identical(unname(r2$statuses[["EXPORT_ACCEPTANCE_STATUS"]]), "FAIL"); expect_identical(r2$overall, "FAIL")
  expect_error(siw_run_posthoc("exposure_export", { f <- tempfile(fileext = ".csv"); file.copy(SYN_CSV, f); cat("\n", file = f, append = TRUE); f }, tempfile(), CFG, synthetic_demo = TRUE), "SYNTHETIC_DEMO_REQUIRES")
})

test_that("reference verification: declared hash and size PASS, a changed file FAILS, an absent role is NOT_TESTED", {
  td <- file.path(tempdir(), paste0("ph32r_", sample.int(1e6, 1))); dir.create(td); f <- file.path(td, "WEATHER_SOURCE_AGREEMENT.csv"); a <- siw_source_agreement(D32); utils::write.csv(a, f, row.names = FALSE)
  p <- siw_posthoc_protocol(pcopy(function(p) refspec(p, "WEATHER_SOURCE_AGREEMENT.csv", f, 80L)))
  expect_identical(siw_posthoc_reference(td, p, "WEATHER_SOURCE_AGREEMENT.csv")$status, "PASS"); expect_identical(siw_posthoc_reference(NULL, p, "WEATHER_SOURCE_AGREEMENT.csv")$status, "NOT_TESTED")
  expect_identical(nrow(siw_posthoc_reference(td, p, "WEATHER_SOURCE_AGREEMENT.csv", list(group = "ALL"))$table), 20L)
  cat("x\n", file = f, append = TRUE); expect_identical(siw_posthoc_reference(td, p, "WEATHER_SOURCE_AGREEMENT.csv")$status, "FAIL")
  expect_identical(siw_posthoc_reference(td, P32, "WEATHER_SOURCE_AGREEMENT.csv")$status, "FAIL")                         # the declared study hash never matches a synthetic file
})

test_that("(c) deletion: six selected models (one per outcome and domain), 6 x (n + dates) refits, original route equals the final estimator, reproduction PASS / FAIL, no identifier written", {
  sel <- siw_selected_xy(D32, CFG, P32); expect_equal(nrow(sel), 6L); dom <- unlist(siw_posthoc_item(P32, "deletion")$metric_domain)
  expect_identical(unname(dom[sel$metric]), rep(c("thermal", "moisture"), 3)); expect_identical(sel$outcome, rep(CFG$outcomes$outcome, each = 2))
  del <- siw_deletion_refits(D32, CFG, sel); nd <- length(unique(D32$AssessmentDay)); expect_equal(nrow(del), 6L * (nrow(D32) + nd))
  expect_true(max(abs(del$coefficient - del$final_coefficient)) < 1e-8); expect_true(all(del$n[del$mode == "participant"] <= nrow(D32) - 1L))
  ref <- data.frame(mode = del$mode, omitted = ifelse(del$mode == "participant", SYN$participant_code[suppressWarnings(as.integer(sub("^P", "", del$deletion_key)))], format(siw_interview_date(suppressWarnings(as.integer(sub("^D", "", del$deletion_key))), P32))), coefficient = del$coefficient, outcome = del$outcome, metric = del$metric, window = del$window, stringsAsFactors = FALSE)
  td <- file.path(tempdir(), paste0("ph32d_", sample.int(1e6, 1))); dir.create(td); f <- file.path(td, "selected_leave_out.csv"); utils::write.csv(ref, f, row.names = FALSE)
  p <- siw_posthoc_protocol(pcopy(function(p) { p <- refspec(p, "selected_leave_out.csv", f, nrow(ref)); i <- which(vapply(p$items, function(z) z$id, "") == "deletion"); p$items[[i]]$reference$expected_compared <- nrow(ref); p }))
  it <- siw_posthoc_item(p, "deletion"); cx <- ctx32(); r <- .siw_ph_item_deletion(D32, CFG, p, it, list(posthoc_v14_reference = td), cx)
  expect_identical(unname(r$statuses[["DELETION_REPRODUCTION_STATUS"]]), "PASS"); expect_identical(unname(r$statuses[["DELETION_VERIFICATION_STATUS"]]), "PASS"); expect_identical(unname(r$statuses[["DELETION_SELECTION_STATUS"]]), "FAIL")   # synthetic selection differs from the SI list
  expect_true(no_codes(cx$out_dir))
  ref$coefficient[5] <- ref$coefficient[5] + 1e-3; utils::write.csv(ref, f, row.names = FALSE); p <- siw_posthoc_protocol(pcopy(function(p) { p <- refspec(p, "selected_leave_out.csv", f, nrow(ref)); i <- which(vapply(p$items, function(z) z$id, "") == "deletion"); p$items[[i]]$reference$expected_compared <- nrow(ref); p }))
  r2 <- .siw_ph_item_deletion(D32, CFG, p, siw_posthoc_item(p, "deletion"), list(posthoc_v14_reference = td), ctx32()); expect_identical(unname(r2$statuses[["DELETION_REPRODUCTION_STATUS"]]), "FAIL")
})

test_that("(d) archived DLM bootstrap: the original seed rule and sampling sequence, sorted ISO date clusters, reproducible, the full-sample fit equals the archived DLM", {
  bs <- siw_l59_bootstrap(D32, CFG, P32, replicates = list(participant = 25L, assessment_date = 21L)); expect_equal(nrow(bs), 18L)
  expect_identical(unique(bs$target_index), as.integer(11L * (1:9))); expect_identical(bs$seed, as.integer(CFG$seed + 100L * bs$target_index + (bs$resampling_type == "assessment_date")))
  expect_identical(unique(bs$cluster_count[bs$resampling_type == "assessment_date"]), length(unique(D32$AssessmentDay)))
  bs2 <- siw_l59_bootstrap(D32, CFG, P32, replicates = list(participant = 25L, assessment_date = 21L)); expect_identical(bs, bs2)
  # independent replicate loop for the first participant target and the first date target (set.seed once; sample.int / sample of sorted ISO dates)
  sp <- CFG$metrics[CFG$metrics$metric == CFG$representative_metrics[1], ]; ln <- siw_daily_names(sp$daily_prefix, 59L); covs <- CFG$tiers$M2_L; B <- siw_lag_basis(59L, 3L); o <- CFG$outcomes[1, ]
  e1 <- siw_with_seed(bs$seed[1], vapply(1:25, function(b) .siw_v14_dlm_cum(D32[sample.int(nrow(D32), nrow(D32), TRUE), , drop = FALSE], o$outcome, o$family, ln, covs, B), 0))
  expect_equal(stats::median(e1[is.finite(e1)]), bs$estimate_median[1], tolerance = 1e-12); expect_equal(sum(is.finite(e1)), bs$replicates_successful[1])
  iso <- format(siw_interview_date(D32$AssessmentDay, P32)); g <- sort(unique(iso)); expect_identical(g, names(split(seq_len(nrow(D32)), iso)))
  e2 <- siw_with_seed(bs$seed[2], vapply(1:21, function(b) { s <- sample(g, length(g), TRUE); .siw_v14_dlm_cum(D32[unlist(lapply(s, function(z) which(iso == z))), , drop = FALSE], o$outcome, o$family, ln, covs, B) }, 0))
  expect_equal(stats::median(e2[is.finite(e2)]), bs$estimate_median[2], tolerance = 1e-12)
  a <- .siw_v14_dlm_cum(D32, o$outcome, o$family, ln, covs, B); f <- siw_fit_dlm(D32, .siw_archived_cfg(CFG), o$outcome, CFG$representative_metrics[1], 59L, 3L, "M2_L", draws = 0L, return_curves = FALSE)$summary$cumulative_effect; expect_equal(a, f, tolerance = 1e-10)
})

test_that("(a) no-April: participants interviewed in April removed, 90 rows, the independent refit agrees; the stage-17 estimator is used", {
  g <- siw_s17_grid(D32, CFG, "NO_APRIL", P32); expect_equal(nrow(g), 90L); keep <- format(siw_interview_date(D32$AssessmentDay, P32), "%m") != "04"
  expect_true(sum(!keep) > 0); expect_true(all(g$n <= sum(keep))); expect_true(all(g$status == "COMPLETED"))
  for (i in c(1, 45, 90)) { iv <- .siw_s17_independent(D32, CFG, g[i, ], P32); expect_equal(g$estimate[i], iv[["estimate"]], tolerance = 1e-8); expect_equal(g$standard_error[i], iv[["standard_error"]], tolerance = 1e-8) }
  expect_equal(g$p, 2 * stats::pnorm(abs(g$estimate / g$standard_error), lower.tail = FALSE)); expect_equal(g$ci_upper - g$estimate, 1.96 * g$standard_error)
})

test_that("(f) source agreement: 80 rows; Lin's concordance and Bland-Altman limits by formula", {
  a <- siw_source_agreement(D32); expect_equal(nrow(a), 80L); expect_identical(sort(unique(a$group)), sort(c("ALL", CFG$coding$locality_levels)))
  r <- a[a$comparison == "ERA5_vs_IMS_temperature" & a$window_days == 56 & a$group == "ALL", ]; x <- D32$AIR_TEMP_AVG_56D; y <- D32$IMS_TEMP_IDW_AVG_56D; k <- is.finite(x) & is.finite(y); x <- x[k]; y <- y[k]
  expect_equal(r$lin_concordance_correlation, 2 * stats::cov(x, y) / (stats::var(x) + stats::var(y) + (mean(x) - mean(y))^2)); expect_equal(r$bland_altman_upper - r$bland_altman_lower, 2 * 1.96 * stats::sd(x - y)); expect_equal(r$n_pairwise, length(x))
})

test_that("(g) exposure residuals: 27 regressions; the common-range fraction by hand", {
  g <- siw_exposure_residuals(D32); expect_equal(nrow(g), 27L); expect_true(all(g$r_squared >= 0 & g$r_squared <= 1))
  expect_equal(.siw_common_range_fraction(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), rep(c("a", "b"), each = 5)), 0); expect_equal(.siw_common_range_fraction(c(1, 3, 6, 8), rep(c("a", "b"), 2)), (6 - 3) / 7)
  expect_equal(g$percent_variance_remaining, 100 * (1 - g$r_squared))
})

test_that("(h) profile likelihood (base R): for a well-behaved model the interval ends solve the profile equation and agree with the 1-df LRT", {
  r <- siw_profile_likelihood_one(D32, CFG, "UTCI", 1L); expect_identical(r$row$status, "COMPLETED"); expect_true(abs(r$verification$objective_at_lower) < 1e-4); expect_true(abs(r$verification$objective_at_upper) < 1e-4)
  expect_true(r$verification$lrt_interval_consistent); expect_equal(r$row$profile_lrt_p, stats::pchisq(r$row$profile_lrt_statistic, 1L, lower.tail = FALSE)); expect_false("logistf" %in% loadedNamespaces())
})

test_that("(e) early/late: 180 subgroup fits, the two declared q families, interaction rows and Panel A", {
  sg <- siw_early_late_subgroups(D32, CFG); expect_equal(nrow(sg), 180L); expect_equal(sg$q180_subgroup, stats::p.adjust(sg$nominal_p, "BH"))
  i <- sg$window == 56; expect_equal(sum(i), 18L); expect_equal(sg$q18_subgroup[i], stats::p.adjust(sg$nominal_p[i], "BH")); expect_true(all(is.na(sg$q18_subgroup[!i])))
  pa <- siw_early_late_panel_a(D32, CFG); for (gg in c("Early", "Late")) { n <- pa$value[pa$group == gg & pa$quantity == "participants"]; expect_equal(sum(pa$value[pa$group == gg & pa$quantity %in% paste0("utci_", SIW_UTCI_BANDS, "_days")]), 56 * n) }
  ia <- siw_s17_grid(D32, CFG, "EARLY_LATE_INTERACTION", P32); expect_equal(nrow(ia), 90L); expect_equal(ia$late_estimate, ia$early_estimate + ia$interaction_estimate)
})

test_that("(D) H3 nested test: nesting verified, F / LR equal anova / deviance difference, HC3 Wald invariant to the basis of the complement, q9-nested, declared columns", {
  h <- siw_h3_nested(D32, CFG, "run", "fp"); expect_true(all(h$nesting$status == "PASS")); tb <- h$table; expect_identical(names(tb), unlist(siw_posthoc_item(P32, "h3_nested")$output_columns))
  expect_equal(tb$q9_nested, stats::p.adjust(tb$p_model_based, "BH")); expect_identical(tb$stat_type, rep(c("F", "F", "LR"), each = 3)); expect_true(all(tb$df_num == 2))
  # F against stats::anova on the same 55-type complete sample
  d <- D32; B <- siw_lag_basis(56L, 3L); H <- as.matrix(d[, siw_daily_names("UTCI", 56L)]); cb <- H %*% B; d$cb1 <- cb[, 1]; d$cb2 <- cb[, 2]; d$cb3 <- cb[, 3]; cv <- CFG$tiers$M2_L
  f0 <- stats::lm(stats::reformulate(c("UTCI_AVG_56D", cv), "UCLA_Lonliness_SCORE"), d); f1 <- stats::lm(stats::reformulate(c(cv, "cb1", "cb2", "cb3"), "UCLA_Lonliness_SCORE"), d); a <- stats::anova(f0, f1)
  expect_equal(tb$statistic[1], a$F[2], tolerance = 1e-8); expect_equal(tb$p_model_based[1], a$`Pr(>F)`[2], tolerance = 1e-8); expect_equal(tb$df_den_or_NA[1], a$Res.Df[2])
  g0 <- stats::glm(stats::reformulate(c("UTCI_AVG_56D", cv), "Int_dummy"), stats::binomial(), d, control = stats::glm.control(1e-12, 200)); g1 <- stats::glm(stats::reformulate(c(cv, "cb1", "cb2", "cb3"), "Int_dummy"), stats::binomial(), d, control = stats::glm.control(1e-12, 200))
  expect_equal(tb$statistic[7], g0$deviance - g1$deviance, tolerance = 1e-7)
  # HC3 Wald: same statistic with another basis of the complement of a
  X <- stats::model.matrix(f1); e <- stats::residuals(f1); V <- .siw_hc3(X, e)$cov; bi <- match(c("cb1", "cb2", "cb3"), colnames(X)); av <- qr.solve(B, rep(1, 56)); Q <- qr.Q(qr(matrix(av, ncol = 1)), complete = TRUE); C <- t(Q[, 2:3])
  C2 <- matrix(c(2, 1, 1, -1), 2) %*% C; w <- function(C) { cb_ <- C %*% stats::coef(f1)[bi]; drop(t(cb_) %*% solve(C %*% V[bi, bi] %*% t(C)) %*% cb_) }; expect_equal(w(C), w(C2), tolerance = 1e-8)
  expect_equal(tb$p_HC3_wald[1], stats::pf(w(C) / 2, 2, nrow(X) - ncol(X), lower.tail = FALSE), tolerance = 1e-8)
  # negative: a reduced exposure that is not the 56-day sum / mean breaks the nesting check
  d2 <- D32; d2$UTCI_AVG_56D <- d2$UTCI_AVG_49D; n2 <- siw_h3_nested_one(d2, CFG, "UCLA_Lonliness_SCORE", "UTCI")$nesting; expect_identical(n2$status, "FAIL")
})

test_that("(D) H3 acceptance reads the core reference through the input declaration (regression: run_20261013_165748_38af stopped with SIW_POSTHOC_REFERENCE_NOT_DECLARED)", {
  core <- c("cumulative_regression_grid.csv", "DLM_L01_L56_nine_representative_summary.csv"); it <- siw_posthoc_item(P32, "h3_nested")
  expect_identical(c(it$acceptance$reduced_coefficients$role, it$acceptance$full_global_p$role), rep("core_reference", 2)); expect_false(any(vapply(P32$reference_files, function(z) z$file, "") %in% core))   # the exact failing path
  sp <- siw_core_reference_spec(core[1], P32); expect_false(is.null(sp)); expect_identical(sp$expected_records, 1680L); expect_identical(siw_core_reference_spec(core[2], P32)$expected_records, 9L)
  if (!siw_skip_public("test-32: the declared core-reference hashes")) { expect_true(grepl("^[0-9A-F]{64}$", sp$sha256)); expect_identical(sp$identification, "CONFIRMED_BY_FROZEN_SOURCE") }
  expect_true(is.null(siw_core_reference_spec("not_declared_anywhere.csv", P32))); expect_error(siw_posthoc_reference(NULL, P32, "not_declared_anywhere.csv"), "SIW_POSTHOC_REFERENCE_NOT_DECLARED")
  expect_identical(siw_posthoc_reference(NULL, P32, core[1])$status, "NOT_TESTED")
  r0 <- .siw_ph_item_h3_nested(D32, CFG, P32, it, list(), ctx32()); expect_identical(unname(r0$statuses[["H3_REDUCED_COEFFICIENT_STATUS"]]), "NOT_TESTED"); red <- r0$tables$acceptance
  td <- file.path(tempdir(), paste0("ph32h_", sample.int(1e6, 1))); dir.create(td); fg <- file.path(td, core[1]); fd <- file.path(td, core[2])
  utils::write.csv(red[, c("outcome", "metric", "window_or_lag", "model_tier", "coefficient")], fg, row.names = FALSE); utils::write.csv(red[, c("outcome", "metric", "model_tier", "global_p")], fd, row.names = FALSE)
  decl <- function(status = "CONFIRMED_BY_FROZEN_SOURCE") { f <- tempfile(fileext = ".json"); e <- function(p, st) list(file = basename(p), bytes = file.info(p)$size, sha256 = siw_file_hash(p)$value, status = st, records = 9L)
    jsonlite::write_json(list(bundles = list(list(role = "core_reference", files = list(e(fg, status), e(fd, "AUTHOR_ACCEPTED_EVIDENCE"))))), f, auto_unbox = TRUE, pretty = TRUE); f }
  dc <- decl(); expect_identical(siw_posthoc_reference(td, P32, core[1])$status, "FAIL")                                    # the declared study hash never matches a synthetic file (or no declaration: public build)
  # end to end, the study-run path (not the synthetic demo): run_posthoc h3_nested with a core reference folder
  pr <- pcopy(function(p) { p$core_reference_declaration <- dc; p }); out <- file.path(tempdir(), paste0("ph32H_", sample.int(1e6, 1)))
  r <- siw_run_posthoc("h3_nested", SYN_CSV, out, CFG, refs = list(core_reference = td), protocol_path = pr)
  expect_identical(unname(r$statuses[c("INPUT_PROVENANCE_STATUS", "H3_NESTING_STATUS", "H3_TEST_STATUS", "H3_REDUCED_COEFFICIENT_STATUS", "H3_FULL_GLOBAL_P_STATUS")]), rep("PASS", 5))
  expect_identical(unname(r$statuses[["H3_TABLE3_STATUS"]]), "FAIL")                                                   # synthetic values are not the Table 3 values
  pv <- utils::read.csv(file.path(r$out_dir, "posthoc_reference_provenance.csv"), stringsAsFactors = FALSE); expect_true(all(pv$ok)); expect_true(any(grepl("project_inputs_declaration_v1.json", pv$value)))
  cmp <- utils::read.csv(file.path(r$out_dir, "h3_reduced_coefficient_comparison.csv"), stringsAsFactors = FALSE); expect_match(cmp$status[1], "^PASS")
  # same acceptance checks: an unaccepted identification status, a changed coefficient (re-declared) and a changed file all FAIL
  p2 <- P32; p2$core_reference_declaration <- decl("UNCONFIRMED"); r2 <- .siw_ph_item_h3_nested(D32, CFG, p2, it, list(core_reference = td), ctx32())
  expect_identical(unname(r2$statuses[c("INPUT_PROVENANCE_STATUS", "H3_REDUCED_COEFFICIENT_STATUS")]), c("FAIL", "FAIL"))
  g <- utils::read.csv(fg); g$coefficient[2] <- g$coefficient[2] * (1 + 1e-3); utils::write.csv(g, fg, row.names = FALSE); p3 <- P32; p3$core_reference_declaration <- decl()
  r3 <- .siw_ph_item_h3_nested(D32, CFG, p3, it, list(core_reference = td), ctx32()); expect_identical(unname(r3$statuses[["H3_REDUCED_COEFFICIENT_STATUS"]]), "FAIL"); expect_identical(unname(r3$statuses[["H3_FULL_GLOBAL_P_STATUS"]]), "PASS")
  cat("x\n", file = fd, append = TRUE); r4 <- .siw_ph_item_h3_nested(D32, CFG, p3, it, list(core_reference = td), ctx32()); expect_identical(unname(r4$statuses[["H3_FULL_GLOBAL_P_STATUS"]]), "FAIL")
})

test_that("(b) nearest station: raw-export aliases, declared variants run separately, the reproducing variant is named, REVIEW when none reproduces", {
  nr <- .siw_nearest_synthetic(D32, P32); raw <- data.frame(FEATURE_ID = nr$participant_code, METHOD = "NEAREST", JERUSALEM_LOCAL_DATE = paste0(nr$LOCAL_DATE, "T00:00:00"), nr[, c("TD_MEAN", "RH_MEAN", "RAIN_TOTAL", "RAIN_DAYS1")], WS_MEAN = 1, stringsAsFactors = FALSE)
  a <- siw_nearest_substitute(D32, CFG, raw, P32); expect_equal(a$IMS_IDW_RH_MEAN_L09, siw_nearest_substitute(D32, CFG, nr, P32)$IMS_IDW_RH_MEAN_L09); expect_identical(attr(a, "nearest_id_column"), "FEATURE_ID")
  expect_identical(attr(a, "nearest_date_column"), "JERUSALEM_LOCAL_DATE"); expect_identical(attr(a, "nearest_participants_matched"), nrow(D32))
  b <- raw; b$JERUSALEM_LOCAL_DATE[3] <- "28/01/2026"; expect_error(siw_nearest_substitute(D32, CFG, b, P32), "SIW_NEAREST_DATE_NOT_ISO: 1 row")
  expect_error(siw_nearest_substitute(D32, CFG, raw[, setdiff(names(raw), "FEATURE_ID")], P32), "SIW_NEAREST_SCHEMA")
  td <- file.path(tempdir(), paste0("ph32n_", sample.int(1e6, 1))); dir.create(td); fA <- file.path(td, "a.csv"); fB <- file.path(td, "b.csv"); utils::write.csv(raw, fA, row.names = FALSE)
  rb <- raw; names(rb)[3] <- "LOCAL_DATE"; rb$RH_MEAN <- 0.9 * rb$RH_MEAN; utils::write.csv(rb, fB, row.names = FALSE)   # a scale change (a constant shift would leave every slope unchanged)
  it <- siw_posthoc_item(P32, "nearest_station"); r1 <- .siw_ph_item_nearest_station(D32, CFG, P32, it, list(nearest_station_histories = fA), ctx32())
  expect_identical(unname(r1$statuses[["INPUT_PROVENANCE_STATUS"]]), "REVIEW")                                                      # not a declared variant: header only
  pv <- r1$tables$variants[[1]]$si$package_value; k <- 0L; nx <- function(d) { k <<- k + 1L; round(pv[k], d) }
  pr <- pcopy(function(p) { i <- which(vapply(p$items, function(z) z$id, "") == "nearest_station"); s <- p$items[[i]]$si_concordance
    for (j in seq_along(s$w56)) { s$w56[[j]]$estimate <- nx(2); s$w56[[j]]$ci <- list(nx(2), nx(2)) }
    for (j in seq_along(s$dlm)) { s$dlm[[j]]$sum <- nx(3); s$dlm[[j]]$ci <- list(nx(3), nx(3)); s$dlm[[j]]$global_p <- nx(3) }
    s$statements$cumulative_converged <- nx(0); s$statements$integration_heuristic_flags <- nx(0); p$items[[i]]$si_concordance <- s
    p$items[[i]]$variants <- list(list(id = "va", label = "A", sha256 = siw_file_hash(fA)$value, bytes = file.info(fA)$size), list(id = "vb", label = "B", sha256 = siw_file_hash(fB)$value, bytes = file.info(fB)$size)); p })
  p <- siw_posthoc_protocol(pr); it <- siw_posthoc_item(p, "nearest_station"); cx <- ctx32()
  r2 <- .siw_ph_item_nearest_station(D32, CFG, p, it, list(nearest_station_histories = paste(fA, fB, sep = "|")), cx)
  expect_identical(unname(r2$statuses), c("PASS", "PASS", "PASS")); expect_identical(r2$tables$summary$variant, c("va", "vb")); expect_identical(r2$tables$summary$reproduces_si, c(TRUE, FALSE))
  expect_true(any(r2$info == "NEAREST_REPRODUCING_VARIANT=va")); expect_identical(r2$tables$summary$date_column, c("JERUSALEM_LOCAL_DATE", "LOCAL_DATE"))
  expect_true(all(file.exists(file.path(cx$out_dir, c("nearest_station_variant_summary.csv", "nearest_station_variant_differences.csv", "nearest_station_si_concordance_va.csv", "nearest_station_dlm_6_vb.csv")))))
  expect_true(no_codes(cx$out_dir)); expect_error(.siw_ph_item_nearest_station(D32, CFG, p, it, list(nearest_station_histories = paste(fA, fA, sep = "|")), ctx32()), "DUPLICATE_VARIANT")
  r3 <- .siw_ph_item_nearest_station(D32, CFG, p, it, list(nearest_station_histories = fB), ctx32())
  expect_identical(unname(r3$statuses[["NEAREST_SI_CONCORDANCE_STATUS"]]), "REVIEW"); expect_true(any(r3$info == "NEAREST_REPRODUCING_VARIANT=NONE (differences in nearest_station_variant_differences.csv)"))
  expect_true(r3$tables$summary$si_values_agreeing < r3$tables$summary$si_values); expect_true(any(r3$tables$differences$agrees == FALSE))
  expect_identical(vapply(siw_posthoc_item(P32, "nearest_station")$variants, function(z) z$id, ""), c("jul19", "jul25"))
})

test_that("(b) nearest station: histories keyed by participant and date substitute the IMS IDW histories; schema and duplicate errors; 120 + 6 fits", {
  nr <- .siw_nearest_synthetic(D32, P32); dn <- siw_nearest_substitute(D32, CFG, nr, P32)
  expect_equal(dn$IMS_IDW_TD_MEAN_L05, D32$IMS_IDW_TD_MEAN_L05 + 0.3); expect_equal(dn$UTCI_L05, D32$UTCI_L05); expect_equal(dn$IMS_TEMP_IDW_AVG_7D, rowMeans(as.matrix(dn[, siw_daily_names("IMS_IDW_TD_MEAN", 7L)]), na.rm = TRUE))
  expect_equal(dn$AIR_TEMP_AVG_56D, D32$AIR_TEMP_AVG_56D)
  expect_error(siw_nearest_substitute(D32, CFG, nr[, setdiff(names(nr), "RH_MEAN")], P32), "SIW_NEAREST_SCHEMA"); expect_error(siw_nearest_substitute(D32, CFG, rbind(nr, nr[1, ]), P32), "DUPLICATE")
  ch <- siw_nearest_station_check(dn, CFG); expect_equal(nrow(ch$cumulative), 120L); expect_equal(nrow(ch$dlm), 6L); expect_true(all(ch$dlm$status == "COMPLETED"))
})

test_that("post hoc protocol: declared after the family lock, labelled items (13 in v2), every reference with size and SHA-256; the shipped copies are identical", {
  expect_match(P32$status_of_this_document, "after the 30 July 2026 family lock"); expect_identical(vapply(P32$items, function(z) z$id, ""), SIW_PH_ITEMS)
  expect_true(all(vapply(P32$items, function(z) z$label, "") %in% c("SENSITIVITY", "POST_HOC", "RECONSTRUCTION"))); expect_identical(siw_posthoc_item(P32, "early_late")$label, "RECONSTRUCTION")
  if (!siw_skip_public("test-32: declared reference hashes and sizes")) expect_true(all(vapply(P32$reference_files, function(z) grepl("^[0-9A-F]{64}$", z$sha256) && z$bytes > 0, NA))); if (SIW_PUBLIC_BUILD) expect_true(all(vapply(P32$reference_files, function(z) z$sha256 == SIW_REDACTED, NA))); expect_identical(siw_posthoc_overall(c(a = "PASS", b = SIW_PH_NA_SYN)), "PASS"); expect_identical(siw_posthoc_overall(c(a = "PASS", b = "NOT_TESTED")), "REVIEW")
  if (SIW_SOURCE_CHECKOUT) expect_identical(siw_file_hash(file.path(ROOT, "config", "posthoc_sensitivity_protocol_v1.json"))$value, siw_file_hash(file.path(ROOT, "inst", "extdata", "posthoc_sensitivity_protocol_v1.json"))$value)
})
