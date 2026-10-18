# Phase 3I: the exploratory ANOM and ALT families. Synthetic data only; no study data and no frozen file is read here.
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
if (!exists("expect_gt", mode = "function")) expect_gt <- function(a, b) expect_true(a > b)
if (!exists("expect_null", mode = "function")) expect_null <- function(x) expect_true(is.null(x))
if (!exists("expect_lt", mode = "function")) expect_lt <- function(a, b) expect_true(a < b)
if (siw_skip_source_only("test-26-anomaly-history: whole file (needs validation/make_anomaly_history_synthetic.R, which is not part of the built package)")) {} else {
CFG <- siw_load_config(); PH <- siw_ah_protocol(); hsh <- function(f) siw_file_hash(file.path(ROOT, f))$value
source(file.path(ROOT, "validation", "make_anomaly_history_synthetic.R"), local = TRUE)
RAW <- ah_rich_rain(siw_make_synthetic(n = 55L, seed = 21L, config = CFG)); DAT <- siw_prepare_data(RAW, CFG); WIN <- c(1L, 7L, 35L, 59L)
SC <- siw_anomaly_scan(DAT, CFG, PH, windows = WIN); AL <- siw_alt_scan(DAT, CFG, PH)
tiny <- function(vals, days, loc = rep("A", length(days)), prefix = "UTCI", metric = "UTCI") {            # a three-lag history of one metric for the hand-calculated examples
  d <- data.frame(assessment_day = days, Locality_3group = factor(loc, levels = c("A", "B"))); for (l in 1:3) d[[sprintf("%s_L%02d", prefix, l)]] <- vals[, l]; cfg1 <- CFG; cfg1$metrics <- CFG$metrics[CFG$metrics$metric == metric, ]; p3 <- PH; p3$parameters$lag_end <- 3L; list(dat = d, cfg = cfg1, protocol = p3) }

test_that("the protocol is declared before the run: hashed references, counts, traced grid, horizons, text support, statuses", {
  expect_match(PH$status_of_this_document, "DECLARED BEFORE THE MANUAL RUN"); expect_equal(length(PH$reference_files), 4L); expect_equal(vapply(PH$reference_files, function(z) z$expected_records, 0L), c(7080L, 312L, 64L, 439L)); expect_equal(vapply(PH$comparisons, function(z) z$expected_records, 0L), c(7080L, 312L, 64L, 439L))
  expect_equal(vapply(PH$comparisons, function(z) z$expected_compared_cells, 0L), c(285560L, 1536L, 400L, 878L)); expect_equal(length(unlist(PH$comparisons[[1]]$fields)), 52L)
  expect_private(vapply(PH$reference_files, function(z) z$sha256, ""), "anomhist_reference_sha256")
  expect_equal(2 * (6 * 1 + 2 * 2) * 2 * 59 * 3, 7080); expect_equal(8 * 3 * 13, 312); expect_equal(PH$parameters$lag_end, 59); expect_equal(PH$parameters$alt_history_end, 56); expect_equal(PH$parameters$calendar_axis, "assessment_day - lag")
  expect_match(PH$scope$text_support, "does not describe either family"); expect_match(PH$scope$text_support, "59 .*2,360.*7,080"); expect_match(PH$scope$kept_separate_from, "primary inference"); expect_match(PH$specification_trace$ANOM$exposure_history, "not verifiable from the canonical CSV")
  expect_match(PH$specification_trace$ALT$trajectory_pca, "sign of a principal component is arbitrary"); expect_match(PH$statuses$ALT_REPRODUCTION_STATUS, "sign difference of a principal component is a difference"); expect_equal(PH$tolerance$class, "cumulative"); expect_equal(PH$tolerance$within_tolerance$abs, CFG$tolerances$cumulative_abs)
  expect_equal(PH$parameters$glm_epsilon, CFG$glm$cumulative_epsilon); expect_identical(hsh("config/anomaly_history_validation_protocol.json"), PH$protocol_hash); expect_identical(hsh("inst/extdata/anomaly_history_validation_protocol.json"), PH$protocol_hash)
  expect_identical(hsh("R/daily_layer.R"), "AF2D8BE7F933A21376C747D668376DDDCBAEA579E9DA40E2CFB948D66E250F8B"); expect_identical(hsh("R/cold_burden.R"), "8DE4A3A287B0039783D7A714639EC40F28114A531763F5E3D031F42EA1F191CB"); expect_identical(hsh("R/engine.R"), "3D1F3A801FE3957134A29B086BBD08E3A81663FE5D76C3EB169E2EF143974082")
  expect_identical(hsh("R/diagnostics.R"), "109E5D9E04DC671F753080E3CCA43699426AE296089472160B740D2C44928D1B"); expect_identical(hsh("config/sieccwinter_config.json"), "5A8C605469AE0AA8133215E8E1666182BEAE17F92F01A00CD0BDE0612E3DC5DE"); expect_private(hsh("config/daily_layer_validation_protocol.json"), "daily_layer_validation_protocol")
})

test_that("reference anomalies: hand-calculated leave-one-out example (all and locality reference, singleton group, literal ratio and stable scale)", {
  vals <- rbind(c(1, 5, 9), c(2, 6, 10), c(3, 7, 11), c(4, 8, 12)); tn <- tiny(vals, days = c(10, 10, 11, 12)); a <- siw_anomaly_values(tn$dat, tn$cfg, tn$protocol, "all"); M <- a$metrics$UTCI
  loo <- rbind(c(7, 8.5, 10), c(20 / 3, 8, 9), c(8, 5, 5.5), c(NA, 3, 10 / 3)); expect_equal(M$literal_ratio, (vals - loo) / loo, tolerance = 1e-12)                                    # calendar day 9 holds {1, 2, 7, 12} (total 22), day 8 {5, 6, 11}, day 7 {9, 10}, day 10 {3, 8}, day 11 {4} alone
  r <- as.vector(vals - loo); pooled <- stats::sd(r[is.finite(r)]); expect_equal(M$stable_anomaly, (vals - loo) / pooled, tolerance = 1e-12); expect_true(is.na(M$stable_anomaly[4, 1])); expect_equal(a$diagnostics$n_total[a$diagnostics$method == "stable_anomaly"], 12); expect_equal(a$diagnostics$n_finite[a$diagnostics$method == "stable_anomaly"], 11)
  expect_true(all(is.na(M$zero_safe_log_anomaly))); expect_equal(a$diagnostics$minimum[a$diagnostics$method == "stable_anomaly"], min((vals - loo)[is.finite(loo)] / pooled), tolerance = 1e-12)
  tl <- tiny(vals, days = c(10, 10, 11, 12), loc = c("A", "A", "B", "B")); b <- siw_anomaly_values(tl$dat, tl$cfg, tl$protocol, "locality")$metrics$UTCI$literal_ratio                                  # day 9: A = {1, 2}, B = {7, 12}
  expect_equal(b[1, 1], 1 / 2 - 1); expect_equal(b[2, 1], 2 / 1 - 1); expect_equal(b[3, 2], 7 / 12 - 1); expect_equal(b[4, 3], 12 / 7 - 1); expect_true(is.na(b[4, 1]))
  vm <- vals; vm[2, 2] <- NA; vm[1, 3] <- Inf; tm <- tiny(vm, days = c(10, 10, 11, 12)); c1 <- siw_anomaly_values(tm$dat, tm$cfg, tm$protocol, "all")$metrics$UTCI$literal_ratio; expect_true(is.na(c1[2, 2])); expect_true(is.na(c1[1, 3]))                  # a missing or non-finite value is neither reference nor anomaly
  d2 <- tiny(rbind(c(1, 1, 1), c(1, 1, 1)), days = c(10, 10)); e <- siw_anomaly_values(d2$dat, d2$cfg, d2$protocol, "all")$metrics$UTCI; expect_true(all(is.na(e$stable_anomaly) | is.nan(e$stable_anomaly)))                  # identical values: zero pooled scale, no finite anomaly
  d3 <- tiny(rbind(c(1, 2, 3)), days = 10); expect_true(all(is.na(siw_anomaly_values(d3$dat, d3$cfg, d3$protocol, "all")$metrics$UTCI$stable_anomaly)))                                                   # one participant: no reference
  expect_error(siw_anomaly_values(transform(tn$dat, assessment_day = c(10, NA, 11, 12)), tn$cfg, tn$protocol, "all"), "ASSESSMENT_DAY_MISSING")
})

test_that("rainfall zero-safe anomaly: hand-calculated value", {
  d <- data.frame(assessment_day = c(5, 5), Locality_3group = factor(c("A", "A"), levels = c("A", "B")), IMS_IDW_RAIN_TOTAL_L01 = c(3, 1), IMS_IDW_RAIN_TOTAL_L02 = c(0, 0), IMS_IDW_RAIN_TOTAL_L03 = c(2, 2)); cfg1 <- CFG; cfg1$metrics <- CFG$metrics[CFG$metrics$metric == "IMS_rainfall_total", ]; p3 <- PH; p3$parameters$lag_end <- 3L
  zz <- siw_anomaly_values(d, cfg1, p3, "all")$metrics$IMS_rainfall_total$zero_safe_log_anomaly; expect_equal(zz[1, 1], log1p(3) - log1p(1)); expect_equal(zz[2, 1], log1p(1) - log1p(3)); expect_equal(zz[1, 2], log1p(0) - log1p(0)); expect_equal(zz[1, 3], log1p(2) - log1p(2))
})

test_that("window features: hand-calculated availability rule, signed and absolute means, the original per-participant function on random matrices with gaps", {
  A <- rbind(c(1, NA, 3, NA, 5), c(1, 2, NA, 4, 5), c(-1, 2, NA, -4, 5), c(NA, NA, NA, NA, NA)); F <- siw_anomaly_features(A)
  expect_equal(F$signed_mean[1, ], c(1, NA, NA, NA, NA)); expect_equal(F$signed_mean[2, ], c(1, 1.5, NA, NA, 3)); expect_equal(F$signed_mean[3, 5], 0.5); expect_equal(F$absolute_mean[3, 5], 3); expect_true(all(is.na(F$signed_mean[4, ])))                      # ceiling(0.8 W): W = 3 needs 3, W = 4 needs 4, W = 5 needs 4
  expect_equal(F$signed_mean[3, 2], 0.5); expect_equal(F$absolute_mean[3, 2], 1.5)
  set.seed(8); B <- matrix(stats::rnorm(40 * 59), 40, 59); B[sample(length(B), 400)] <- NA; B[3, ] <- NA; B[5, 1:50] <- NA; G <- siw_anomaly_features(B); md <- 0
  for (bu in c("signed_mean", "absolute_mean")) for (w in c(1, 2, 5, 14, 35, 59)) for (i in 1:40) { a <- G[[bu]][i, w]; b <- siw_anomaly_feature_reference(B[i, ], w, bu); expect_equal(is.na(a), is.na(b)); if (!is.na(a)) md <- max(md, abs(a - b)) }; expect_lt(md, 1e-14)
  expect_equal(dim(siw_anomaly_features(matrix(c(1, 2), 2, 1))$signed_mean), c(2L, 1L))
})

test_that("scan structure: 7,080-model grid arithmetic, order, keys, unique ids, multiplicity families, q recomputed", {
  R <- SC$results; expect_equal(nrow(R), 2 * 10 * 2 * length(WIN) * 3); expect_false(anyDuplicated(R$model_id) > 0); kk <- do.call(paste, R[c("outcome", "metric", "reference_definition", "anomaly_method", "burden_direction", "window_or_lag")]); expect_false(anyDuplicated(kk) > 0)
  expect_identical(unique(R$reference_definition), c("all", "locality")); expect_identical(unique(R$metric), CFG$metrics$metric); expect_equal(sum(R$anomaly_method == "zero_safe_log_anomaly"), 2 * 2 * 2 * length(WIN) * 3); expect_true(all(R$anomaly_method[R$anomaly_method == "zero_safe_log_anomaly"] == "zero_safe_log_anomaly"))
  expect_identical(R$model_id[1], "ANOMALY__UCLA_Lonliness_SCORE__UTCI__all__stable_anomaly__signed_mean__W1"); expect_identical(R$transformation[1], "all:stable_anomaly:signed_mean"); expect_identical(unique(R$exposure), ".__anomaly"); expect_identical(unique(R$primary_role), "EXPLORATORY_DAILY_ANOMALY"); expect_identical(unique(R$model_tier), "M2_L")
  expect_identical(R$outcome[1:3], CFG$outcomes$outcome); expect_identical(R$window_or_lag[seq(1, 12, by = 3)], c(1L, 7L, 35L, 59L)); m <- siw_anomaly_multiplicity_verify(R); expect_true(all(m$status[m$status != "INFO"] == "PASS")); expect_equal(m$value[m$check == "q_curve families"], 120); expect_equal(m$value[m$check == "q_outcome families"], 3)
  expect_equal(names(SC$features)[1], "all|UTCI|stable_anomaly|signed_mean|1"); expect_equal(length(SC$features), 2 * 10 * 2 * length(WIN)); expect_equal(nrow(SC$diagnostics), 64L); expect_equal(sum(R$status == "COMPLETED") + sum(R$status == "NOT_ESTIMABLE"), nrow(R)); expect_equal(range(R$n), c(min(R$n), 55))
  expect_true(all(R$n <= 55)); expect_true(any(R$n == 54)); x <- SC$features[["all|IMS_rainfall_total|stable_anomaly|signed_mean|35"]]; expect_equal(sum(is.finite(x)), 55L)                                             # one missing rainfall total at lag 35 does not remove a participant from a 35-day window (80% rule)
})

test_that("equivalence of the fits with the validated engine and with independent lm / glm refits (every model), the independent anomaly and feature routes, the identities", {
  rf <- siw_anomaly_refit_verify(DAT, CFG, SC$results, SC$features, PH, every = 1L); expect_true(all(rf$summary$status == "PASS")); expect_equal(rf$models_refitted, sum(SC$results$status == "COMPLETED")); expect_equal(nrow(rf$failures), 0L)
  rf3 <- siw_anomaly_refit_verify(DAT, CFG, SC$results, SC$features, PH, every = 3L); expect_equal(rf3$models_refitted, length(seq(1L, nrow(SC$results), by = 3L)) - sum(SC$results$status[seq(1L, nrow(SC$results), by = 3L)] != "COMPLETED")); expect_equal(rf3$every, 3L)
  va <- siw_anomaly_values(DAT, CFG, PH, "all"); vl <- siw_anomaly_values(DAT, CFG, PH, "locality"); v <- rbind(siw_anomaly_values_verify(DAT, CFG, PH, va), siw_anomaly_values_verify(DAT, CFG, PH, vl)); expect_true(all(v$status == "PASS")); expect_equal(nrow(v), 16L)
  f <- siw_anomaly_features_verify(CFG, PH, va, vl, windows = WIN); expect_true(all(f$status == "PASS")); expect_equal(nrow(f), 2 * 10 * 2); expect_equal(sum(f$cells), 2 * 10 * 2 * length(WIN) * 55)
  id <- .siw_dl_identities(SC$results); expect_true(all(id$status == "PASS"))
  r <- SC$results[SC$results$status == "COMPLETED", ][1:3, ]; x <- SC$features[[paste(r$reference_definition[1], r$metric[1], r$anomaly_method[1], r$burden_direction[1], r$window_or_lag[1], sep = "|")]]; ok <- stats::complete.cases(cbind(DAT[, c(r$outcome[1], CFG$tiers$M2_L)], x)); expect_equal(r$n[1], sum(ok))
  vb <- va; vb$metrics$UTCI$stable_anomaly[3, 3] <- vb$metrics$UTCI$stable_anomaly[3, 3] + 0.01; expect_true(any(siw_anomaly_values_verify(DAT, CFG, PH, vb)$status == "FAIL"))                    # a genuine difference is detected
  ve <- va; iv <- which(ve$diagnostics$metric == "UTCI" & ve$diagnostics$method == "stable_anomaly"); ve$diagnostics$maximum[iv] <- ve$diagnostics$maximum[iv] + 0.5; expect_true(any(siw_anomaly_values_verify(DAT, CFG, PH, ve)$status == "FAIL"))
  bad <- SC$results; bad$nominal_p[7] <- bad$nominal_p[7] * 1.01; expect_true(any(siw_anomaly_multiplicity_verify(siw_anomaly_assign_q(transform(bad, q_global = bad$q_global)))$status == "PASS")); w <- SC$results; w$q_curve[2] <- w$q_curve[2] + 0.1; expect_true(any(siw_anomaly_multiplicity_verify(w)$status == "FAIL"))
  c2 <- SC$results; c2$coefficient[c2$status == "COMPLETED"][1] <- 99; rb <- siw_anomaly_refit_verify(DAT, CFG, c2, SC$features, PH, every = 1L); expect_true(any(rb$summary$status == "FAIL")); expect_gt(nrow(rb$failures), 0L)
})

test_that("missing-data and boundary cases: missing daily values, an all-missing participant, a constant feature (NOT_ESTIMABLE, never an abort), a singleton locality-day group", {
  d <- RAW; d$UTCI_L02[4] <- NA; d$UTCI_L01[9] <- NA; d[7, grep("^UTCI_L", names(d))] <- NA; dd <- siw_prepare_data(d, CFG); s <- siw_anomaly_scan(dd, CFG, PH, windows = c(1L, 7L)); r <- s$results[s$results$metric == "UTCI" & s$results$reference_definition == "all" & s$results$window_or_lag == 7L, ]
  expect_true(all(r$n <= 54)); expect_true(is.na(s$features[["all|UTCI|stable_anomaly|signed_mean|7"]][7])); expect_equal(nrow(s$results), 2 * 10 * 2 * 2 * 3); expect_true(all(is.finite(s$results$nominal_p[s$results$status == "COMPLETED"])))
  k <- RAW; k[, grep("^UTCI_L", names(k))] <- 5; ds <- siw_prepare_data(k, CFG); sk <- siw_anomaly_scan(ds, CFG, PH, windows = 1L); expect_true(any(sk$results$status == "NOT_ESTIMABLE")); ne <- sk$results[sk$results$status == "NOT_ESTIMABLE", ]; expect_true(all(is.na(ne$nominal_p))); expect_true(all(is.na(ne$q_curve)))
  e <- siw_anomaly_multiplicity_verify(sk$results); expect_true(all(e$status[e$status != "INFO"] == "PASS"))
  g <- RAW; g$Area <- rep(c(1, 2, 3), length.out = 55); g$assessment_day <- seq_len(55); gs <- siw_prepare_data(g, CFG); sg <- siw_anomaly_scan(gs, CFG, PH, windows = 1L)                                           # every participant on a different day: locality groups mostly of one
  expect_equal(nrow(sg$results), 2 * 10 * 2 * 3)
})

test_that("ALT features: hand-calculated weighted, contrast and disjoint examples with missing values", {
  H <- matrix(rep(1:59, each = 3), 3, 59); H[2, c(1:10)] <- NA; H[3, 1:11] <- NA                                                   # row 1 complete; row 2 has 10 lags missing (46 finite of 56 >= 45); row 3 has 11 missing (45 finite... see below)
  w3 <- siw_alt_weighted(H, 3, 56); wt <- 0.5^((1:56 - 1) / 3); expect_equal(w3[1], sum((1:56) * wt) / sum(wt)); expect_equal(w3[2], sum((11:56) * wt[11:56]) / sum(wt[11:56])); expect_equal(w3[3], sum((12:56) * wt[12:56]) / sum(wt[12:56]))
  H2 <- H; H2[3, 12] <- NA; expect_true(is.na(siw_alt_weighted(H2, 3, 56)[3])); expect_equal(siw_alt_weighted(matrix(7, 2, 59), Inf, 56), c(7, 7)); expect_equal(siw_alt_weighted(matrix(7, 2, 59), 14, 56), c(7, 7))
  Z <- matrix(rep(1:59, each = 2), 2, 59); ct <- siw_alt_contrasts(Z, 56); expect_equal(ct$overall_mean, c(28.5, 28.5)); expect_equal(ct$recent_minus_remote, c(7.5 - 49.5, 7.5 - 49.5)); expect_equal(ct$linear_lag_contrast, rep(14630 / 784, 2)); expect_equal(ct$curvature_contrast, c(0, 0), tolerance = 1e-12)
  M <- matrix(1, 2, 59); M[1, 1:6] <- NA; M[2, 1:7] <- NA; dj <- siw_alt_disjoint(M, "mean"); expect_equal(dj$recent_1_7, c(1, NA)); expect_equal(dj$intermediate_8_28, c(1, 1)); dj2 <- siw_alt_disjoint(M, "sum"); expect_equal(dj2$recent_1_7, c(1, NA)); expect_equal(dj2$remote_29_56, c(28, 28))           # no availability rule for the disjoint periods; sums for rainfall
  expect_equal(siw_alt_disjoint(matrix(2, 1, 59), "sum")$intermediate_8_28, 42)
})

test_that("ALT joint test, trajectory PCA and the independent routes (every single-feature model, every joint test)", {
  R <- AL$results; expect_equal(nrow(R), 312L); expect_false(anyDuplicated(do.call(paste, R[c("outcome", "metric", "method", "feature")])) > 0); expect_equal(as.vector(table(R$outcome)), c(104L, 104L, 104L)); expect_equal(sum(R$method == "three_disjoint_periods_joint"), 24L); expect_true(all(is.na(R$effect[R$method == "three_disjoint_periods_joint"])))
  expect_identical(R$feature[1:13], c("1-7|8-28|29-56", "half_life_3", "half_life_7", "half_life_14", "half_life_28", "uniform_56", "overall_mean", "recent_minus_remote", "linear_lag_contrast", "curvature_contrast", "PC1", "PC2", "PC3")); expect_identical(R$method[1:13], c("three_disjoint_periods_joint", rep("weighted_cumulative", 5), rep("trajectory_contrast", 4), rep("trajectory_PCA", 3)))
  S <- AL$features$UTCI; d <- DAT; for (nm in names(S$disjoint)) d[[nm]] <- scale(S$disjoint[[nm]])[, 1]; cv <- CFG$tiers$M2_L; jt <- siw_alt_joint(d, "UCLA_Lonliness_SCORE", "gaussian", names(S$disjoint), cv); full <- stats::lm(stats::reformulate(c(names(S$disjoint), cv), "UCLA_Lonliness_SCORE"), d); red <- stats::lm(stats::reformulate(cv, "UCLA_Lonliness_SCORE"), d)
  expect_equal(jt$p, stats::anova(red, full)[["Pr(>F)"]][2], tolerance = 1e-10); expect_equal(jt$statistic, stats::anova(red, full)[["F"]][2], tolerance = 1e-10); expect_equal(jt$n, 55L); expect_equal(jt$df1, 3L); expect_equal(AL$results$p[AL$results$metric == "UTCI" & AL$results$method == "three_disjoint_periods_joint"][1], jt$p, tolerance = 1e-10)
  fb <- suppressWarnings(stats::glm(stats::reformulate(c(names(S$disjoint), cv), "Int_dummy"), binomial(), d)); rb <- suppressWarnings(stats::glm(stats::reformulate(cv, "Int_dummy"), binomial(), d)); jb <- siw_alt_joint(d, "Int_dummy", "binomial", names(S$disjoint), cv); expect_equal(jb$p, stats::anova(rb, fb, test = "Chisq")[["Pr(>Chi)"]][2], tolerance = 1e-8); expect_true(is.na(jb$df2))
  ft <- siw_alt_features_verify(CFG, PH, AL$features); expect_true(all(ft$status == "PASS")); expect_equal(nrow(ft), 8 * 12); rr <- siw_alt_refit_verify(DAT, CFG, AL$results, AL$features, PH); expect_true(all(rr$status == "PASS")); expect_equal(nrow(rr), 312L)
  pv <- siw_alt_pca_verify(CFG, PH, AL$features, AL$pca_variance); expect_true(all(pv$status == "PASS")); expect_equal(nrow(pv), 8L); expect_true(all(abs(tapply(AL$pca_variance$variance_fraction, AL$pca_variance$metric, sum) - 1) < 1e-12)); expect_equal(AL$pca_variance$cumulative_variance[AL$pca_variance$component == max(AL$pca_variance$component)][1], 1, tolerance = 1e-12)
  m <- siw_alt_multiplicity_verify(R); expect_true(all(m$status[m$status != "INFO"] == "PASS")); expect_identical(m$value[m$check == "q_outcome family sizes"], "104")
  bad <- AL$results; bad$p[20] <- bad$p[20] + 0.05; expect_true(any(siw_alt_refit_verify(DAT, CFG, bad, AL$features, PH)$status == "FAIL")); bf <- AL$features; bf$UTCI$weighted$half_life_3[5] <- bf$UTCI$weighted$half_life_3[5] + 1; expect_true(any(siw_alt_features_verify(CFG, PH, bf)$status == "FAIL"))
  bp <- AL$pca_variance; bp$variance_fraction[2] <- bp$variance_fraction[2] + 0.01; expect_true(any(siw_alt_pca_verify(CFG, PH, AL$features, bp)$status == "FAIL"))
  few <- RAW; few[1:40, "UTCI_L10"] <- NA; fs <- siw_alt_scan(siw_prepare_data(few, CFG), CFG, PH, metrics = "UTCI"); expect_equal(sum(fs$results$method == "trajectory_PCA"), 0L); expect_equal(nrow(fs$results), 3L * 10L); expect_null(fs$pca_variance)           # fewer than 20 complete histories: no PCA, as in the original
  expect_equal(nrow(siw_alt_scan(DAT, CFG, PH, metrics = "MRT")$results), 39L)
})

test_that("sign diagnostic of the principal components separates a sign flip from other differences", {
  ref <- AL$results; new <- ref; i <- which(new$method == "trajectory_PCA" & is.finite(new$effect))[1]; new$effect[i] <- -new$effect[i]; sg <- siw_alt_pca_sign_diagnostic(new, ref, CFG$tolerances); expect_equal(sum(!sg$sign_equal), 1L); expect_true(all(sg$abs_effect_equal)); expect_true(all(sg$p_equal)); expect_equal(nrow(sg), 72L)
  expect_true(all(siw_alt_pca_sign_diagnostic(ref, ref, CFG$tolerances)$sign_equal))
})

test_that("end to end on synthetic input: success, genuine failures, missing and duplicated evidence all behave as declared (R level)", {
  td <- file.path(tempdir(), "ah_run"); unlink(td, recursive = TRUE); dir.create(td); run1 <- function(variant, protocol_hint = NULL) { b <- make_ah_synthetic(file.path(td, variant), variant, ROOT); siw_run_anomaly_history_validation(b$csv, b$ref, file.path(td, paste0("out_", variant)), CFG, protocol_path = b$protocol) }
  ok <- run1("exact"); expect_identical(ok$status, "OK"); expect_true(all(ok$statuses == "PASS")); expect_true(all(ok$coverage$cells_as_declared)); expect_true(all(ok$coverage$n_unmatched == 0L & ok$coverage$n_duplicate == 0L & ok$coverage$n_unclassified == 0L & ok$coverage$n_missing_required == 0L)); expect_true(all(ok$coverage$n_different == 0L))
  fl <- list.files(file.path(td, "out_exact")); expect_true(all(c("ANOMHIST_STATUS.txt", "RUN_SUMMARY.md", "anomhist_comparison_coverage.csv", "alt_pca_sign_diagnostic.csv", "anom_refit_coverage.csv", "alt_refit_coverage.csv", "source_manifest_anomhist.csv", "anomhist_metadata.json", "SOURCE_FINGERPRINT.txt") %in% fl)); expect_false(any(grepl("participant", fl)))
  expect_match(readLines(file.path(td, "out_exact", "RUN_SUMMARY.md")), ".*"); expect_match(paste(readLines(file.path(td, "out_exact", "anom_refit_coverage.csv")), collapse = " "), "ALL MODELS")
  a <- run1("anom_diff"); expect_identical(a$status, "VERIFICATION_FAILURE"); expect_identical(a$statuses[["ANOM_REPRODUCTION_STATUS"]], "FAIL"); expect_identical(a$statuses[["ALT_REPRODUCTION_STATUS"]], "PASS"); expect_gt(sum(a$coverage$n_different), 0L)
  l <- run1("alt_diff"); expect_identical(l$statuses[["ALT_REPRODUCTION_STATUS"]], "FAIL"); expect_identical(l$statuses[["ANOM_REPRODUCTION_STATUS"]], "PASS")
  s <- run1("pca_sign"); expect_identical(s$statuses[["ALT_REPRODUCTION_STATUS"]], "FAIL"); expect_equal(sum(!s$sign$sign_equal, na.rm = TRUE), 1L); expect_true(all(s$sign$abs_effect_equal[!s$sign$sign_equal]))                                  # a sign flip is a difference, shown separately
  m <- run1("missing_ref"); expect_identical(m$statuses[["INPUT_PROVENANCE_STATUS"]], "FAIL"); expect_true(all(m$statuses[c("ANOM_REPRODUCTION_STATUS", "ALT_VERIFICATION_STATUS")] == "NOT_TESTED")); expect_identical(m$statuses[["COVERAGE_STATUS"]], "FAIL"); expect_identical(m$status, "VERIFICATION_FAILURE")
  du <- run1("dup_key"); expect_identical(du$statuses[["INPUT_PROVENANCE_STATUS"]], "FAIL"); expect_identical(du$status, "VERIFICATION_FAILURE"); expect_true(any(grepl("keys unique", du$provenance$item[!du$provenance$ok])))
  expect_error(run1("broken_data"), "SIW_INPUT_VALIDATION_FAILED|ERROR|missing")
  b <- make_ah_synthetic(file.path(td, "ov"), "exact", ROOT); expect_error(siw_run_anomaly_history_validation(b$csv, b$ref, file.path(b$ref, "inside"), CFG, protocol_path = b$protocol), "OVERLAP")
  expect_match(readLines(file.path(td, "out_exact", "ANOMHIST_STATUS.txt")), ".*"); expect_true(any(grepl("^J02_GLM_EPSILON_ENTRY_POINTS=OPEN", readLines(file.path(td, "out_exact", "ANOMHIST_STATUS.txt"))))); expect_true(any(grepl("^SCOPE=exploratory", readLines(file.path(td, "out_exact", "ANOMHIST_STATUS.txt")))))
})

test_that("speed: the grid computations are not repeated per model (anomalies, features) and a hash check of untouched code", {
  t0 <- proc.time()[["elapsed"]]; v <- siw_anomaly_values(DAT, CFG, PH, "locality"); f <- lapply(v$metrics, function(m) siw_anomaly_features(m$stable_anomaly)); expect_lt(proc.time()[["elapsed"]] - t0, 5); expect_equal(length(f), 8L)
  expect_identical(hsh("R/ordinal.R"), "010882F8DFD0756A8577429F8A3E5423F45220F2D39AA0A4067CD2ED73704441")
})
}
