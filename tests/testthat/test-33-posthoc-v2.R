# Post hoc protocol v2 (14 Oct 2026): steps 27-29 (exposure QA, VIF summary, calendar spline). Synthetic data only; every GIS input is built here from the synthetic CSV
# and written under neutral names; every reference is built from synthetic results.
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
P33 <- siw_posthoc_protocol(); D33 <- DAT
ctx33 <- function(synthetic = FALSE) { td <- file.path(tempdir(), paste0("ph33_", sample.int(1e6, 1))); dir.create(td, recursive = TRUE)
  list(synthetic = synthetic, run_id = basename(td), input_sha256 = "X", fingerprint = "F", out_dir = td, W = function(x, f) siw_write_csv(x, file.path(td, f)), options = list(), raw_participant_code = SYN$participant_code) }
pcopy33 <- function(mod) { p <- jsonlite::fromJSON(P33$protocol_path, simplifyVector = FALSE); p <- mod(p); f <- tempfile(fileext = ".json"); jsonlite::write_json(p, f, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null"); f }
no_codes33 <- function(dir) !any(vapply(list.files(dir, recursive = TRUE, full.names = TRUE), function(f) any(grepl("SYN0[0-9][0-9]", readLines(f, warn = FALSE))), NA))
full_counts <- function(gis, r) { o <- as.character(unlist(r$one_minute_rain_stations)); d <- gis$counts$date == r$dst_day; full <- ifelse(d, r$expected_10min_dst, r$expected_10min)
  gis$counts$n_td <- full; gis$counts$n_rh <- full; gis$counts$n_rain <- ifelse(gis$counts$station_id %in% o, ifelse(d, r$expected_1min_dst, r$expected_1min), full); gis }

test_that("protocol v2: declared 14 Oct 2026 after the family lock; version 1 kept unchanged; three labelled items with the spec labels", {
  expect_identical(P33$protocol_version, "2"); expect_match(P33$status_of_this_document, "14 Oct 2026"); expect_match(P33$status_of_this_document, "after the 30 July 2026 family lock")
  expect_identical(vapply(P33$items, function(z) z$id, ""), SIW_PH_ITEMS); expect_identical(length(SIW_PH_ITEMS), 14L)
  lab <- vapply(P33$items, function(z) z$label, ""); names(lab) <- vapply(P33$items, function(z) z$step, "")
  expect_identical(unname(lab[c("PH_EXPOSURE_QA", "PH_VIF_SUMMARY", "PH_CALENDAR_SPLINE")]), c("RECONSTRUCTION", "RECONSTRUCTION", "SENSITIVITY"))
  v1 <- jsonlite::fromJSON(sub("_v2[.]json$", "_v1.json", P33$protocol_path), simplifyVector = FALSE); expect_identical(v1$items, P33$items[1:10]); expect_identical(v1$reference_files, P33$reference_files[seq_along(v1$reference_files)])
  if (!siw_skip_public("test-33: v2 extends the declared v1 hash")) expect_identical(P33$extends$sha256, siw_file_hash(sub("_v2[.]json$", "_v1.json", P33$protocol_path))$value)
  g <- Filter(function(z) identical(z$role, SIW_GIS_ROLE), P33$reference_files); expect_identical(length(g), 11L); expect_identical(sort(unique(vapply(g, function(z) z$content, ""))), c("audit", "daily", "period_coverage", "plan", "raw_10min", "stage_a", "stage_b")); expect_identical(unique(vapply(g[vapply(g, function(z) z$content %in% c("period_coverage", "raw_10min", "stage_a", "stage_b"), NA)], function(z) z$level, "")), "station-level")
  if (SIW_PUBLIC_BUILD) expect_true(all(vapply(g, function(z) z$sha256 == SIW_REDACTED && z$file == SIW_REDACTED, NA)))
  if (SIW_SOURCE_CHECKOUT) expect_identical(siw_file_hash(file.path(ROOT, "config", "posthoc_sensitivity_protocol_v2.json"))$value, siw_file_hash(file.path(ROOT, "inst", "extdata", "posthoc_sensitivity_protocol_v2.json"))$value)
})

test_that("(27) raw 10-minute counts: only non-missing values counted per station and Jerusalem local date; CSV and gzip read alike", {
  d <- data.frame(station_id = c(rep("7", 5), rep("9", 3)), station_name = "x", JERUSALEM_LOCAL_DATE = c(rep("2026-03-27", 3), "2026-03-28", "2026-03-28", "2026-03-27T00:00", "2026-03-27", "2026-03-27"),
                  TD_C = c(1, NA, 3, 4, 5, 6, NA, NA), RH_PCT = c(50, 51, NA, NA, NA, 60, 61, 62), RAIN_MM = c(0, 0, 0, NA, 0, 0.1, 0.2, NA), WS_MS = 1, stringsAsFactors = FALSE)
  f <- tempfile(fileext = ".csv.gz"); z <- gzfile(f, "w"); utils::write.csv(d, z, row.names = FALSE); close(z); f2 <- tempfile(fileext = ".csv"); utils::write.csv(d, f2, row.names = FALSE)
  a <- siw_raw_station_day_counts(f); a <- a[order(a$station_id, a$date), ]; expect_identical(a$date, c("2026-03-27", "2026-03-28", "2026-03-27"))
  expect_equal(a$n_td, c(2, 2, 1)); expect_equal(a$n_rh, c(2, 0, 3)); expect_equal(a$n_rain, c(3, 1, 2)); b <- siw_raw_station_day_counts(f2); expect_equal(unname(as.matrix(b[order(b$station_id, b$date), c("n_td", "n_rh", "n_rain")])), unname(as.matrix(a[, c("n_td", "n_rh", "n_rain")])))
  expect_error(siw_raw_station_day_counts({ g <- tempfile(fileext = ".csv"); utils::write.csv(d[, setdiff(names(d), "RH_PCT")], g, row.names = FALSE); g }), "SIW_EXPOSURE_QA_SCHEMA")
})

test_that("(27) exposure QA: source identity is required (1e-9; rows; missing pattern); coverage uses 144 / 138 (DST) and 1,440 / 1,380 (1-minute rain) with the 0.80 threshold", {
  it <- siw_posthoc_item(P33, "exposure_qa"); r <- it$rules; gis <- full_counts(.siw_exposure_qa_synthetic(D33, P33), r); q <- siw_exposure_qa(D33, gis, it, P33)
  expect_identical(q$identity$strict_1e9_status, rep("PASS", 3)); expect_identical(q$identity$stored_precision_status, rep("PASS", 3)); expect_equal(q$participant_days, nrow(D33) * 56); expect_true(q$computation_ok)
  a <- q$coverage[q$coverage$scope == "ALL", ]; expect_equal(a$n_3, rep(nrow(D33) * 56, 3))                        # every assigned station covers every day at the full count
  expect_identical(r$dst_day, "2026-03-27"); expect_equal(c(r$expected_10min, r$expected_10min_dst, r$expected_1min, r$expected_1min_dst), c(144, 138, 1440, 1380)); expect_identical(unlist(r$one_minute_rain_stations), c("312", "315", "317", "318", "320"))
  ld <- siw_lag_date(D33$AssessmentDay, 1:56, P33); au <- gis$audit; pdst <- as.character(D33$participant_code)[rowSums(ld == r$dst_day) > 0]
  s <- au$STATION_ID[au$VARIABLE == "TD"][1]; k <- sum(au$FEATURE_ID[au$VARIABLE == "TD" & au$STATION_ID == s] %in% pdst); expect_true(k > 0)
  set <- function(g, st, col, v) { i <- g$counts$station_id == st & g$counts$date == r$dst_day; g$counts[[col]][i] <- v; g }
  q1 <- siw_exposure_qa(D33, set(gis, s, "n_td", 111), it, P33); expect_equal(q1$coverage$n_3[q1$coverage$scope == "ALL" & q1$coverage$variable == "td"], nrow(D33) * 56)      # 111 / 138 >= 0.80
  q2 <- siw_exposure_qa(D33, set(gis, s, "n_td", 110), it, P33); expect_equal(q2$coverage$n_3[q2$coverage$scope == "ALL" & q2$coverage$variable == "td"], nrow(D33) * 56 - k)  # 110 / 138 < 0.80 (110 / 144 would be the same; 111 / 144 would not)
  om <- unlist(r$one_minute_rain_stations)[1]; kr <- sum(au$FEATURE_ID[au$VARIABLE == "Rain" & au$STATION_ID == om] %in% pdst); expect_true(kr > 0)
  q3 <- siw_exposure_qa(D33, set(gis, om, "n_rain", 1104), it, P33); expect_equal(q3$coverage$n_3[q3$coverage$scope == "ALL" & q3$coverage$variable == "rain"], nrow(D33) * 56)   # 1,104 / 1,380 = 0.80
  q4 <- siw_exposure_qa(D33, set(gis, om, "n_rain", 1103), it, P33); expect_equal(q4$coverage$n_3[q4$coverage$scope == "ALL" & q4$coverage$variable == "rain"], nrow(D33) * 56 - kr)
  g5 <- gis; g5$daily$TD_MEAN[7] <- g5$daily$TD_MEAN[7] + 1e-6; x5 <- siw_exposure_qa(D33, g5, it, P33)$identity; expect_identical(x5$strict_1e9_status, c("FAIL", "PASS", "PASS")); expect_identical(x5$stored_precision_status, c("FAIL", "PASS", "PASS"))
  g6 <- gis; g6$daily <- g6$daily[-3, ]; x6 <- siw_exposure_qa(D33, g6, it, P33)$identity; expect_identical(x6$strict_1e9_status, rep("FAIL", 3)); expect_identical(x6$stored_precision_status, rep("FAIL", 3))
  g7 <- gis; i7 <- which(is.finite(g7$daily$RH_MEAN))[2]; g7$daily$RH_MEAN[i7] <- NA; x7 <- siw_exposure_qa(D33, g7, it, P33)$identity; expect_identical(c(x7$strict_1e9_status[2], x7$stored_precision_status[2]), c("FAIL", "FAIL")); expect_equal(x7$missing_pattern_mismatch[2], 1)
  # regression (run_20261014_124607_389c): the equality count and the maximum come from the same aligned differences, so they must agree; a few rows differing above / below 1e-9
  g8 <- gis; i8 <- which(is.finite(g8$daily$TD_MEAN))[c(3, 50, 99, 140)]; g8$daily$TD_MEAN[i8] <- g8$daily$TD_MEAN[i8] + c(3e-9, -2.5e-9, 4e-10, 6e-9)
  x8 <- siw_exposure_qa(D33, g8, it, P33)$identity[1, ]; expect_identical(x8$strict_1e9_status, "FAIL"); expect_equal(x8$differ_gt_1e_9, 3); expect_equal(x8$equal_1e_9, x8$both_finite - 3)
  expect_equal(x8$max_abs_difference, 6e-9, tolerance = 1e-6); expect_identical(x8$max_abs_difference_text, "6.000e-09"); expect_equal(x8$differ_gt_5e_9, 1); expect_equal(x8$equal_1e_9 + x8$differ_gt_1e_9, x8$both_finite); expect_equal(x8$exactly_equal, x8$both_finite - 4)
  g9 <- gis; g9$daily$TD_MEAN[i8[3]] <- g9$daily$TD_MEAN[i8[3]] + 4e-10; x9 <- siw_exposure_qa(D33, g9, it, P33)$identity[1, ]; expect_identical(x9$strict_1e9_status, "PASS"); expect_true(x9$max_abs_difference > 0 && x9$max_abs_difference <= 1e-9)
  # a canonical CSV holding 8 decimals of the run's values: |run - canonical| <= 5e-9 everywhere, FAIL at 1e-9 (tolerance unchanged), and equal after rounding the run to 8 decimals
  d10 <- D33; nm <- siw_daily_names("IMS_IDW_TD_MEAN", 56L); g10 <- gis; ld10 <- siw_lag_date(D33$AssessmentDay, 1:56, P33); jj <- match(paste(rep(as.character(D33$participant_code), 56), as.vector(ld10)), paste(g10$daily$FEATURE_ID, g10$daily$LOCAL_DATE))
  u <- (seq_along(jj) %% 9 - 4) * 1.1e-9; g10$daily$TD_MEAN[jj] <- g10$daily$TD_MEAN[jj] + u; d10[, nm] <- matrix(round(g10$daily$TD_MEAN[jj], 8), nrow(D33))
  x10 <- siw_exposure_qa(d10, g10, it, P33)$identity[1, ]; expect_identical(x10$strict_1e9_status, "FAIL"); expect_true(x10$max_abs_difference <= 5e-9 + 1e-12); expect_equal(x10$equal_after_rounding_run_to_8_decimals, x10$both_finite)
  # declared post hoc criterion (author decision 14 Oct 2026): 8-decimal storage passes stored precision and fails strict 1e-9; a 1e-7 difference fails both
  expect_identical(x10$stored_precision_status, "PASS"); expect_equal(x10$equal_after_rounding_both_to_8_decimals, x10$both_finite)
  g11 <- g10; g11$daily$TD_MEAN[jj[c(5, 77)]] <- g11$daily$TD_MEAN[jj[c(5, 77)]] + 1e-7; x11 <- siw_exposure_qa(d10, g11, it, P33)$identity[1, ]
  expect_identical(c(x11$strict_1e9_status, x11$stored_precision_status), c("FAIL", "FAIL")); expect_equal(x11$differ_gt_1e_8, 2)
  # second amendment (rule ill-posed at rounding boundaries): a pair straddling an 8th-decimal rounding boundary PASSES; a 2e-8 difference FAILS
  d12 <- D33; g12 <- gis; k12 <- jj[c(4, 40)]; base <- floor(abs(g12$daily$TD_MEAN[k12]) * 1e8) / 1e8 * sign(g12$daily$TD_MEAN[k12])
  g12$daily$TD_MEAN[k12] <- base + 4.9e-9 * sign(base); m12 <- matrix(g12$daily$TD_MEAN[jj], nrow(D33)); m12[match(k12, jj)] <- base + 5.1e-9 * sign(base); d12[, nm] <- m12
  x12 <- siw_exposure_qa(d12, g12, it, P33)$identity[1, ]; expect_identical(x12$stored_precision_status, "PASS"); expect_true(x12$equal_after_rounding_both_to_8_decimals < x12$both_finite); expect_identical(x12$strict_1e9_status, "PASS")
  g13 <- g12; g13$daily$TD_MEAN[k12[1]] <- g13$daily$TD_MEAN[k12[1]] + 2e-8; x13 <- siw_exposure_qa(d12, g13, it, P33)$identity[1, ]; expect_identical(c(x13$stored_precision_status, x13$strict_1e9_status), c("FAIL", "FAIL"))
  i11 <- .siw_ph_item_exposure_qa(d10, CFG, P33, it, list(), ctx33(TRUE)); expect_identical(unname(i11$statuses[["EXPOSURE_SOURCE_IDENTITY_STORED_PRECISION_STATUS"]]), "PASS"); expect_true(any(grepl("^EXPOSURE_SOURCE_IDENTITY_STRICT_1E9=PASS \\(informational, kept for the record", i11$info)))
  # distances and pool by hand
  td <- au[au$VARIABLE == "TD", ]; km <- as.numeric(td$DISTANCE_M) / 1000; near <- tapply(km, td$FEATURE_ID, min); dd <- q$distances[q$distances$variable == "TD" & q$distances$group == "ALL", ]
  expect_equal(c(dd$km_min, dd$km_max, dd$nearest_median, dd$farthest_median, dd$distinct_stations), c(min(km), max(km), stats::median(near), stats::median(tapply(km, td$FEATURE_ID, max)), length(unique(td$STATION_ID))))
  expect_equal(q$pool$value[q$pool$quantity == "stations_requested_coverage_file"], length(unique(gis$coverage$STATION_ID))); expect_equal(q$pool$value[q$pool$quantity == "stations_returned_any_valid"], length(unique(gis$coverage$STATION_ID[gis$coverage$valid_count > 0])))
  expect_true(all(c("ALL", "BY_GROUP") %in% q$coverage$scope)); expect_false(any(c("participant_code", "FEATURE_ID", "date") %in% unlist(lapply(q[c("coverage", "distances", "support", "pool")], names))))
})

test_that("(27) exposure QA end to end: files identified by SHA-256 (folder or | list), provenance PASS / FAIL / NOT_TESTED, file path equals the in-memory path, no identifier written", {
  it <- siw_posthoc_item(P33, "exposure_qa"); r <- it$rules; gis <- .siw_exposure_qa_synthetic(D33, P33); keep <- sort(unique(gis$counts$date))[c(1:3, which(sort(unique(gis$counts$date)) == r$dst_day))]
  gis$counts <- gis$counts[gis$counts$date %in% keep, ]; td <- file.path(tempdir(), paste0("ph33g_", sample.int(1e6, 1))); dir.create(td)
  W <- function(x, n) { f <- file.path(td, n); utils::write.csv(x, f, row.names = FALSE); f }
  fd <- W(gis$daily, "d.csv"); fa <- W(gis$audit, "a.csv"); fp <- W(gis$plan, "p.csv"); fc <- W(gis$coverage, "c.csv")
  rows <- do.call(rbind, lapply(seq_len(nrow(gis$counts)), function(i) { x <- gis$counts[i, ]; m <- max(x$n_td, x$n_rh, x$n_rain); data.frame(station_id = x$station_id, JERUSALEM_LOCAL_DATE = x$date, TD_C = ifelse(seq_len(m) <= x$n_td, 10, NA), RH_PCT = ifelse(seq_len(m) <= x$n_rh, 70, NA), RAIN_MM = ifelse(seq_len(m) <= x$n_rain, 0, NA)) }))
  fr <- file.path(td, "r1.csv.gz"); fr2 <- file.path(td, "r2.csv.gz"); h <- nrow(rows) %/% 2; for (z in list(list(fr, rows[1:h, ]), list(fr2, rows[-(1:h), ]))) { con <- gzfile(z[[1]], "w"); utils::write.csv(z[[2]], con, row.names = FALSE); close(con) }
  fsa <- file.path(td, "sa.json"); fsb <- file.path(td, "sb.json"); jsonlite::write_json(list(stage = "A", details = list(station_inventory = 41, coordinate_valid_stations = 40, metadata_candidate_union = 11, download_station_ids = 11)), fsa, auto_unbox = TRUE)
  jsonlite::write_json(list(stage = "B", details = list(completed_stations = 10, fetch_status = c(stats::setNames(as.list(c(rep("range_ok", 9), "range_failed:204", "range_partial")), as.character(101:111)), list(candidate_expansion = list(rounds = 0))))), fsb, auto_unbox = TRUE)
  ss <- siw_read_station_stages(fsa, fsb); expect_equal(ss$value[match(c("coordinate_valid_stations_stage_a", "stations_requested_stage_a", "stations_returned_stage_b", "fetch_status_stations_stage_b", "fetch_status_range_ok_stage_b"), ss$quantity)], c(40, 11, 10, 11, 9))
  fbad <- file.path(td, "bad.json"); jsonlite::write_json(list(details = list(participant_code = "x")), fbad, auto_unbox = TRUE); expect_error(siw_read_station_stages(fsa, fbad), "SIW_EXPOSURE_QA_STATION_FILE_HAS_PARTICIPANT_FIELD")
  fl <- list(daily = fd, audit = fa, plan = fp, period_coverage = fc, raw_10min = fr, raw_10min = fr2, stage_a = fsa, stage_b = fsb)
  pr <- pcopy33(function(p) { p$reference_files <- c(Filter(function(z) !identical(z$role, SIW_GIS_ROLE), p$reference_files), lapply(seq_along(fl), function(i) list(role = SIW_GIS_ROLE, file = basename(fl[[i]]), content = names(fl)[i], bytes = file.info(fl[[i]])$size, sha256 = siw_file_hash(fl[[i]])$value))); p })
  p <- siw_posthoc_protocol(pr); it <- siw_posthoc_item(p, "exposure_qa")
  expect_identical(unname(siw_posthoc_role_files(td, p, SIW_GIS_ROLE)), unname(unlist(fl))); expect_identical(unname(siw_posthoc_role_files(paste(rev(unlist(fl)), collapse = "|"), p, SIW_GIS_ROLE)), unname(unlist(fl)))
  rd <- siw_read_gis_inputs(siw_posthoc_role_files(td, p, SIW_GIS_ROLE), p); qf <- siw_exposure_qa(D33, rd, it, p); qm <- siw_exposure_qa(D33, gis, it, p)
  expect_equal(qf$coverage, qm$coverage); expect_equal(qf$identity, qm$identity); expect_equal(qf$distances, qm$distances)
  cx <- ctx33(); x <- .siw_ph_item_exposure_qa(D33, CFG, p, it, list(gis_ims_v8_1_reference = paste(unlist(fl), collapse = "|")), cx)
  expect_identical(unname(x$statuses[c("INPUT_PROVENANCE_STATUS", "EXPOSURE_SOURCE_IDENTITY_STORED_PRECISION_STATUS", "EXPOSURE_QA_COMPUTATION_STATUS", "EXPOSURE_QA_SI_CONCORDANCE_STATUS")]), c("PASS", "PASS", "PASS", "REVIEW")); expect_false("EXPOSURE_SOURCE_IDENTITY_STATUS" %in% names(x$statuses)); expect_true(any(grepl("^EXPOSURE_SOURCE_IDENTITY_STRICT_1E9=PASS \\(informational, kept for the record", x$info)))
  si <- utils::read.csv(file.path(cx$out_dir, "exposure_qa_si_concordance.csv"), stringsAsFactors = FALSE); expect_identical(names(si)[1:6], c("item", "quantity", "package_value", "manuscript_value", "decimals", "agrees"))
  expect_true(no_codes33(cx$out_dir)); expect_true(any(grepl("^record only", si$note))); expect_equal(si$package_value[si$quantity == "coordinate-valid stations (stage A)"], 40)
  x0 <- .siw_ph_item_exposure_qa(D33, CFG, p, it, list(), ctx33()); expect_identical(unname(x0$statuses), rep("NOT_TESTED", 4))
  cat("x\n", file = fp, append = TRUE); x1 <- .siw_ph_item_exposure_qa(D33, CFG, p, it, list(gis_ims_v8_1_reference = td), ctx33()); expect_identical(unname(x1$statuses[["INPUT_PROVENANCE_STATUS"]]), "FAIL"); expect_identical(unname(x1$statuses[["EXPOSURE_SOURCE_IDENTITY_STORED_PRECISION_STATUS"]]), "NOT_TESTED")
})

test_that("(28) VIF: per-column VIF = 1 / (1 - R^2) of the column on the others; the maximum equals the Batch A value; 240 models, 90 representative", {
  X <- siw_tier_matrix(D33, CFG$tiers$M2_L); X <- X[stats::complete.cases(X), , drop = FALSE]; X <- X[, !(grepl("^Locality_3group", colnames(X)) & colSums(X != 0) == 0L), drop = FALSE]
  X <- cbind(X[, 1L, drop = FALSE], exposure = D33$UTCI_AVG_42D[seq_len(nrow(X))], X[, -1L, drop = FALSE]); colnames(X)[1L] <- "(Intercept)"; v <- siw_vif_columns(X); P <- X[, -1L]
  for (j in seq_len(ncol(P))) expect_equal(unname(v[j]), 1 / (1 - summary(stats::lm(P[, j] ~ P[, -j]))$r.squared), tolerance = 1e-8)
  expect_equal(max(v), .siw_max_vif(X)); expect_identical(.siw_vif_term(c("exposure", "AssessmentDay_centered", "Locality_3groupB", "Sex", "Age", "Econ_STS")), c("exposure", "assessment_day", "locality", "sex", "age", "economic_status"))
  vs <- siw_vif_summary(D33, CFG); expect_equal(nrow(vs$models), 240L); expect_equal(sum(vs$models$representative), 90L)
  dg <- siw_cumulative_diagnostics(D33, CFG, tiers = "M2_L"); expect_equal(vs$models$maximum_vif, dg$maximum_vif[match(vs$models$result_id, dg$result_id)], tolerance = 1e-12)
  cx <- ctx33(); r <- .siw_ph_item_vif_summary(D33, CFG, P33, siw_posthoc_item(P33, "vif_summary"), list(), cx); expect_identical(unname(r$statuses[["VIF_COMPUTATION_STATUS"]]), "PASS")
  expect_identical(unname(r$statuses[["VIF_SI_CONCORDANCE_STATUS"]]), "REVIEW"); expect_true(file.exists(file.path(cx$out_dir, "vif_si_concordance.csv"))); expect_true(no_codes33(cx$out_dir))
})

test_that("(29) calendar spline: ns(assessment_day, df) replaces the linear term; t-based p on the residual df; BH within each df; reference PASS / FAIL at 1e-10", {
  r2 <- siw_s17_cumulative(D33, CFG, "UCLA_Lonliness_SCORE", "gaussian", "UTCI", 28L, "DATE_NS_DF2", P33); d <- D33[stats::complete.cases(D33[, c("UCLA_Lonliness_SCORE", "UTCI_AVG_28D", "Sex", "Age", "AssessmentDay", "Locality_3group", "Econ_STS")]), ]
  f <- stats::lm(UCLA_Lonliness_SCORE ~ UTCI_AVG_28D + Sex + Age + Locality_3group + Econ_STS + splines::ns(AssessmentDay, df = 2), data = d); expect_equal(r2$estimate, unname(stats::coef(f)[["UTCI_AVG_28D"]]), tolerance = 1e-10)
  expect_equal(r2$residual_df, stats::df.residual(f)); expect_equal(r2$p_t, 2 * stats::pt(abs(r2$estimate / r2$standard_error), r2$residual_df, lower.tail = FALSE)); expect_equal(r2$p_primary, r2$p_t)
  r3 <- siw_s17_cumulative(D33, CFG, "Int_dummy", "binomial", "UTCI", 28L, "DATE_NS_DF3", P33); expect_equal(r3$p_primary, r3$p); expect_true(is.na(r3$p_t)); expect_equal(r3$residual_df, r2$residual_df - 1L)
  na <- siw_s17_cumulative(D33, CFG, "UCLA_Lonliness_SCORE", "gaussian", "UTCI", 28L, "NO_APRIL", P33); expect_false(any(c("p_t", "residual_df", "p_primary") %in% names(na)))   # existing variants unchanged
  it <- siw_posthoc_item(P33, "calendar_spline"); cx <- ctx33(); x <- .siw_ph_item_calendar_spline(D33, CFG, P33, it, list(), cx); G <- x$tables$grid
  expect_equal(nrow(G), 180L); expect_true(all(x$tables$verification$status == "PASS")); for (v in c("DATE_NS_DF2", "DATE_NS_DF3")) expect_equal(G$q90_calspline[G$variant == v], stats::p.adjust(G$p_primary[G$variant == v], "BH"))
  expect_identical(unname(x$statuses[["CALENDAR_SPLINE_REPRODUCTION_STATUS"]]), "NOT_TESTED")
  ref <- data.frame(source_model_id = paste0("SRC__W", G$window, "__", G$variant), outcome = G$outcome, metric = G$metric, variant = G$variant, estimate = G$estimate, standard_error = G$standard_error, p = G$p, stringsAsFactors = FALSE)
  td <- file.path(tempdir(), paste0("ph33c_", sample.int(1e6, 1))); dir.create(td); rf <- file.path(td, it$reference$file); utils::write.csv(ref, rf, row.names = FALSE)
  decl <- function() pcopy33(function(p) { for (i in seq_along(p$reference_files)) if (identical(p$reference_files[[i]]$file, it$reference$file)) { p$reference_files[[i]]$sha256 <- siw_file_hash(rf)$value; p$reference_files[[i]]$bytes <- file.info(rf)$size; p$reference_files[[i]]$expected_records <- nrow(ref) }; p })
  p <- siw_posthoc_protocol(decl()); y <- .siw_ph_item_calendar_spline(D33, CFG, p, siw_posthoc_item(p, "calendar_spline"), list(archive_engine_reference = td), ctx33())
  expect_identical(unname(y$statuses[c("INPUT_PROVENANCE_STATUS", "CALENDAR_SPLINE_REPRODUCTION_STATUS", "CALENDAR_SPLINE_VERIFICATION_STATUS", "CALENDAR_SPLINE_SI_CONCORDANCE_STATUS")]), c("PASS", "PASS", "PASS", "REVIEW"))
  ref$estimate[5] <- ref$estimate[5] + 1e-9; utils::write.csv(ref, rf, row.names = FALSE); p <- siw_posthoc_protocol(decl())
  z <- .siw_ph_item_calendar_spline(D33, CFG, p, siw_posthoc_item(p, "calendar_spline"), list(archive_engine_reference = td), ctx33()); expect_identical(unname(z$statuses[["CALENDAR_SPLINE_REPRODUCTION_STATUS"]]), "FAIL")
  si <- utils::read.csv(file.path(cx$out_dir, "calendar_spline_si_concordance.csv"), stringsAsFactors = FALSE); expect_identical(names(si)[1:6], c("item", "quantity", "package_value", "manuscript_value", "decimals", "agrees")); expect_true(no_codes33(cx$out_dir))
})

test_that("v2 steps through siw_run_posthoc: the synthetic demo runs all three; NOT_APPLICABLE_SYNTHETIC_INPUT only for comparisons with study values", {
  for (i in c("exposure_qa", "vif_summary", "calendar_spline")) { r <- siw_run_posthoc(i, SYN_CSV, file.path(tempdir(), paste0("ph33r_", i, sample.int(1e6, 1))), CFG, synthetic_demo = TRUE)
    expect_identical(r$overall, "PASS"); expect_true(any(r$statuses == SIW_PH_NA_SYN)); st <- readLines(file.path(r$out_dir, "POSTHOC_STATUS.txt")); expect_true(any(grepl("^PRIMARY_INFERENCE_EFFECT=NONE", st)))
    expect_true(length(list.files(r$out_dir, pattern = "_si_concordance[.]csv$")) == 1L); expect_true(no_codes33(r$out_dir)) }
})
