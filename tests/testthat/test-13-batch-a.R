# Batch A tests on fully synthetic data: assessment-day checks, influence diagnostics, bivariate analyses, descriptive analyses, validation coverage,
# and the end-to-end run with independently constructed references. Independent calculations use first principles or plain base-R formula interfaces.
DIAG <- NULL
get_diag <- function() { if (is.null(DIAG)) DIAG <<- siw_cumulative_diagnostics(DAT, CFG); DIAG }
spec_data <- function(outcome, expo, tier = "M2_L") {
  covs <- CFG$tiers[[tier]]; vars <- unique(c(outcome, expo, covs)); d <- DAT[stats::complete.cases(DAT[, vars, drop = FALSE]), vars, drop = FALSE]
  list(d = droplevels(d), f = stats::reformulate(c(expo, covs), outcome))
}

test_that("assessment-day validation reports completeness, integer coding and the number of groups", {
  ok <- siw_validate_assessment_day(SYN, NA); expect_equal(sum(ok$status == "FAIL"), 0L); expect_equal(ok$n_affected[ok$check_id == "BA04"], length(unique(SYN$assessment_day)))
  g <- length(unique(SYN$assessment_day)); expect_equal(siw_validate_assessment_day(SYN, g)$status[4], "PASS"); expect_equal(siw_validate_assessment_day(SYN, 33L)$status[4], "FAIL")   # study-data expectation: 33
  x <- SYN; x$assessment_day[3] <- NA; expect_equal(siw_validate_assessment_day(x, NA)$status[1], "FAIL")
  x <- SYN; x$assessment_day[3] <- 4.5; r <- siw_validate_assessment_day(x, NA); expect_equal(r$status[r$check_id == "BA02"], "FAIL")
  x <- SYN; x$assessment_day[3] <- -1; r <- siw_validate_assessment_day(x, NA); expect_equal(r$status[r$check_id == "BA03"], "FAIL")
  x <- SYN; x$assessment_day <- NULL; expect_equal(siw_validate_assessment_day(x, 33L)$status, "FAIL")
  x <- SYN; x$assessment_day <- as.character(x$assessment_day); x$assessment_day[2] <- "abc"; expect_equal(siw_validate_assessment_day(x, NA)$status[1], "FAIL")
  expect_identical(names(SYN)[names(SYN) == "assessment_day"], "assessment_day")                                    # canonical schema unchanged (no new column)
})

test_that("diagnostics follow the original definitions (independent first-principles calculation, continuous outcome)", {
  D <- get_diag(); s <- spec_data("UCLA_Lonliness_SCORE", "UTCI_AVG_14D"); X <- model.matrix(s$f, s$d); y <- s$d$UCLA_Lonliness_SCORE; n <- nrow(X); p <- ncol(X)
  XtXi <- solve(crossprod(X)); H <- X %*% XtXi %*% t(X); h <- diag(H); e <- drop(y - H %*% y); s2 <- sum(e^2) / (n - p)
  cook <- e^2 * h / (p * s2 * (1 - h)^2)
  dfb <- t(vapply(seq_len(n), function(i) { b_i <- solve(crossprod(X[-i, ]), crossprod(X[-i, ], y[-i])); e_i <- y[-i] - X[-i, ] %*% b_i; s_i <- sqrt(sum(e_i^2) / (n - 1 - p)); (drop(XtXi %*% crossprod(X, y)) - drop(b_i)) / (s_i * sqrt(diag(XtXi))) }, numeric(p)))
  r <- D[D$result_id == "CUM__UCLA__UTCI__W14__M2_L", ]; expect_equal(nrow(r), 1L)
  expect_equal(r$maximum_leverage, max(h), tolerance = 1e-10); expect_equal(r$maximum_cooks_distance, max(cook), tolerance = 1e-10); expect_equal(r$maximum_absolute_dfbeta, max(abs(dfb)), tolerance = 1e-8)
  expect_equal(r$influential_case_count, sum(cook > 4 / n | h > 2 * p / n)); expect_equal(r$maximum_coefficient_magnitude, max(abs(drop(XtXi %*% crossprod(X, y)))), tolerance = 1e-10)
  expect_equal(r$condition_number, kappa(X), tolerance = 1e-10); expect_true(abs(r$condition_number - kappa(X, exact = TRUE)) > 1e-6)           # the original uses the QR estimate, not the exact condition number
  P <- X[, -1]; vif <- vapply(seq_len(ncol(P)), function(j) 1 / (1 - summary(lm(P[, j] ~ P[, -j]))$r.squared), 1); expect_equal(r$maximum_vif, max(vif), tolerance = 1e-8)
  q <- quantile(s$d$UTCI_AVG_14D, c(.1, .25, .75, .9), names = FALSE); expect_equal(unlist(r[c("exposure_p10", "exposure_p25", "exposure_p75", "exposure_p90")], use.names = FALSE), q, tolerance = 1e-12)
  expect_true(all(is.na(r[c("calibration_intercept", "calibration_slope", "marginal_probability_difference_per_sd", "probability_at_mean_minus_half_sd", "probability_at_mean_plus_half_sd")])))   # binary-only diagnostics
  expect_equal(r$diagnostic_status, "OK"); expect_equal(r$dropped_terms, "")
})

test_that("diagnostics follow the original definitions (binary outcome)", {
  D <- get_diag(); s <- spec_data("Int_dummy", "UTCI_AVG_21D", "M3"); fit <- glm(s$f, data = s$d, family = binomial()); X <- model.matrix(fit); n <- nrow(X); p <- ncol(X)
  r <- D[D$result_id == "CUM__INT__UTCI__W21__M3", ]; w <- fit$weights; Xw <- X * sqrt(w); h <- rowSums((Xw %*% solve(crossprod(Xw))) * Xw); pr <- fitted(fit)
  pear <- (s$d$Int_dummy - pr) / sqrt(pr * (1 - pr)); cook <- (pear / (1 - h))^2 * h / p
  expect_equal(r$maximum_leverage, max(h), tolerance = 1e-9); expect_equal(r$maximum_cooks_distance, max(cook), tolerance = 1e-9)
  im <- influence.measures(fit)$infmat; expect_equal(r$maximum_absolute_dfbeta, max(abs(im[, grep("^dfb", colnames(im))])), tolerance = 1e-9)
  expect_equal(r$influential_case_count, sum(cook > 4 / n | h > 2 * p / n))
  expect_equal(r$maximum_coefficient_magnitude, max(abs(coef(fit))), tolerance = 1e-9); expect_equal(r$condition_number, kappa(X), tolerance = 1e-10)
  expect_equal(r$calibration_slope, 1, tolerance = 1e-5); expect_equal(r$calibration_intercept, 0, tolerance = 1e-5)            # in-sample ML logistic calibration
  ex <- s$d$UTCI_AVG_21D; sdx <- sd(ex); b <- coef(fit); lo <- X; hi <- X; lo[, 2] <- mean(ex) - .5 * sdx; hi[, 2] <- mean(ex) + .5 * sdx
  expect_equal(r$probability_at_mean_minus_half_sd, mean(plogis(lo %*% b)), tolerance = 1e-9); expect_equal(r$probability_at_mean_plus_half_sd, mean(plogis(hi %*% b)), tolerance = 1e-9)
  expect_equal(r$marginal_probability_difference_per_sd, r$probability_at_mean_plus_half_sd - r$probability_at_mean_minus_half_sd, tolerance = 1e-12)
})

test_that("diagnostics cover exactly the core grid, report dropped terms and undefined or singular fits explicitly", {
  D <- get_diag(); G <- siw_cumulative_grid(DAT, CFG)$results
  expect_equal(nrow(D), 1680L); expect_identical(D$result_id, G$result_id); expect_identical(D$n, as.integer(G$n)); expect_identical(D$parameter_count, as.integer(G$parameter_count)); expect_identical(D$rank, as.integer(G$rank))
  expect_true(all(D$diagnostic_status == "OK")); expect_equal(sum(D$condition_number == Inf), 0L)
  d2 <- DAT; d2$UTCI_AVG_7D[d2$Locality_3group == "Tamra and Nahariya"] <- NA                                   # empty locality level in the complete-case sample
  D2 <- siw_cumulative_diagnostics(d2, CFG, tiers = "M2_L"); r <- D2[D2$metric == "UTCI" & D2$window_or_lag == 7L & D2$outcome == "UCLA_Lonliness_SCORE", ]
  expect_equal(r$dropped_terms, "Locality_3groupTamra and Nahariya"); expect_equal(r$parameter_count, 7L); expect_equal(r$diagnostic_status, "OK")
  expect_equal(D2$dropped_terms[D2$metric == "UTCI" & D2$window_or_lag == 14L][1], "")
  X <- cbind("(Intercept)" = 1, exposure = rnorm(8), a = rnorm(8), b = rnorm(8)); X <- cbind(X, c1 = X[, 3], c2 = X[, 4], c3 = rnorm(8), c4 = rnorm(8)); y <- rnorm(8)
  r1 <- siw_model_diagnostics_one(X, y, FALSE); expect_true(grepl("^NOT_ESTIMABLE:INSUFFICIENT_RESIDUAL_DF", r1$diagnostic_status)); expect_equal(r1$condition_number, Inf); expect_equal(r1$maximum_vif, Inf)
  X2 <- cbind("(Intercept)" = 1, exposure = rnorm(30), a = rnorm(30)); X2 <- cbind(X2, dup = X2[, 3]); r2 <- siw_model_diagnostics_one(X2, rnorm(30), FALSE); expect_equal(r2$diagnostic_status, "NOT_ESTIMABLE:RANK_DEFICIENT"); expect_true(is.na(r2$maximum_cooks_distance))
  X3 <- cbind("(Intercept)" = 1, exposure = rep(1, 30), a = rnorm(30)); expect_true(grepl("^NOT_ESTIMABLE", siw_model_diagnostics_one(X3, rnorm(30), FALSE)$diagnostic_status))   # constant exposure is collinear with the intercept
  X4 <- cbind("(Intercept)" = 1, exposure = rnorm(30)); expect_equal(siw_model_diagnostics_one(X4, rep(0, 30), TRUE)$diagnostic_status, "NOT_ESTIMABLE:NO_BINARY_OUTCOME_VARIATION")
  expect_equal(siw_model_diagnostics_one(cbind("(Intercept)" = 1, exposure = rnorm(30)), rnorm(30), FALSE)$maximum_vif, 1)       # single predictor
})

test_that("bivariate analyses follow the original rules; binary correlation is the point-biserial correlation", {
  B <- siw_bivariate_grid(DAT, CFG); expect_equal(nrow(B), 240L); expect_equal(anyDuplicated(B$result_id), 0L)
  r <- B[B$outcome == "Int_dummy" & B$metric == "UTCI" & B$window_or_lag == 35L, ]; x <- DAT$UTCI_AVG_35D; y <- DAT$Int_dummy; n1 <- sum(y == 1); n0 <- sum(y == 0); n <- length(y)
  rpb <- (mean(x[y == 1]) - mean(x[y == 0])) / sqrt(sum((x - mean(x))^2) / n) * sqrt(n1 * n0 / n^2)
  expect_equal(r$pearson_or_point_biserial_r, rpb, tolerance = 1e-10); expect_equal(r$correlation_type, "point_biserial")
  expect_equal(r$pearson_p, t.test(x ~ y, var.equal = TRUE)$p.value, tolerance = 1e-8)                                      # equivalence with the pooled two-sample t test
  expect_equal(r$spearman_rho, cor(rank(x), rank(y)), tolerance = 1e-10); expect_equal(r$spearman_p, cor.test(x, y, method = "spearman", exact = FALSE)$p.value, tolerance = 1e-12)
  u <- B[B$outcome == "UCLA_Lonliness_SCORE" & B$metric == "MRT" & B$window_or_lag == 7L, ]; expect_equal(u$pearson_p, cor.test(DAT$MRT_AVG_7D, DAT$UCLA_Lonliness_SCORE)$p.value, tolerance = 1e-12); expect_equal(u$correlation_type, "pearson")
  # missing-data and testing rules
  d <- DAT; d$UTCI_AVG_7D[1:5] <- NA; b2 <- siw_bivariate_grid(d, CFG); expect_equal(b2$n[b2$result_id == "BIV__UCLA__UTCI__W7"], nrow(DAT) - 5L)
  d$UTCI_AVG_7D[] <- NA; d$UTCI_AVG_7D[1:3] <- 1:3; b3 <- siw_bivariate_grid(d, CFG); expect_equal(b3$n[b3$result_id == "BIV__UCLA__UTCI__W7"], 3L); expect_true(is.na(b3$pearson_p[b3$result_id == "BIV__UCLA__UTCI__W7"]))   # fewer than 4 pairs
  d <- DAT; d$UTCI_AVG_7D <- 5; b4 <- siw_bivariate_grid(d, CFG); expect_true(is.na(b4$pearson_p[b4$result_id == "BIV__UCLA__UTCI__W7"])); expect_equal(b4$status[b4$result_id == "BIV__UCLA__UTCI__W7"], "NOT_ESTIMABLE")   # constant exposure
  expect_equal(siw_correlation_set(c(1, 2, 3, NA), c(1, 3, 2, 5))[["n"]], 3)
  # families: global 240, within outcome 80, within outcome-metric 10; q_model from the core M0 rows
  expect_equal(B$q_pearson_global, p.adjust(B$pearson_p, "BH"), tolerance = 1e-12)
  for (o in unique(B$outcome)) { i <- B$outcome == o; expect_equal(sum(i), 80L); expect_equal(B$q_spearman_outcome[i], p.adjust(B$spearman_p[i], "BH"), tolerance = 1e-12) }
  i <- B$outcome == "Isolation_cont" & B$metric == "MRT"; expect_equal(sum(i), 10L); expect_equal(B$q_pearson_outcome_metric[i], p.adjust(B$pearson_p[i], "BH"), tolerance = 1e-12)
  G <- siw_cumulative_grid(DAT, CFG, tiers = "M0")$results; expect_equal(B$model_p, G$nominal_p[match(paste(B$outcome, B$metric, B$window_or_lag), paste(G$outcome, G$metric, G$window_or_lag))], tolerance = 1e-12)
  expect_equal(B$q_model_global, p.adjust(B$model_p, "BH"), tolerance = 1e-12)
})

test_that("descriptive analyses: original rules, coding boundaries, missingness, participant-level versus participant-day", {
  t1 <- siw_table1(DAT); expect_equal(nrow(t1), 17L); a <- t1[t1$characteristic == "Age", ]; expect_equal(a$mean, mean(DAT$Age)); expect_equal(a$sd, sd(DAT$Age)); expect_equal(a$iqr, IQR(DAT$Age)); expect_equal(a$n_or_count, nrow(DAT))
  fem <- t1[t1$characteristic == "Sex: Female", ]; expect_equal(fem$n_or_count, sum(DAT$Sex == 1)); expect_equal(fem$percent, 100 * mean(DAT$Sex == 1))
  # missingness: counts exclude missing values; categorical NA kept as its own level
  d <- DAT; d$PSQI[1:4] <- NA; tp <- siw_table1(d); expect_equal(tp$n_or_count[tp$characteristic == "PSQI"], nrow(d) - 4L); expect_equal(tp$missing[tp$characteristic == "PSQI"], 4L)
  expect_equal(siw_summary_stats(c(1, 2, NA))$n, 2L); expect_true(is.na(siw_summary_stats(5)$sd)); expect_true(is.na(siw_summary_stats(numeric())$mean))
  # coding boundaries
  d <- DAT[1:6, ]; d$Age <- c(69, 70, 79, 80, 65, 91); fa <- siw_descriptive_factors(d); expect_identical(as.character(fa$Age_group), c("<70", "70-79", "70-79", ">79", "<70", ">79"))
  d$AssessmentDay <- c(54, 55, 0, 78, 56, 54); expect_identical(as.character(siw_descriptive_factors(d)$Assessment_period), c("Early", "Late", "Early", "Late", "Late", "Early"))
  expect_identical(levels(siw_descriptive_factors(DAT)$Ethnicity_factor), c("Muslim and Christian", "Jewish")); expect_identical(levels(siw_descriptive_factors(DAT)$Econ_STS_factor), c("Above average", "Average or below"))
  nt <- siw_descriptive_coding_notes(DAT, CFG); expect_equal(as.integer(nt$value[nt$item == "participants_with_age_80"]), sum(DAT$Age == 80)); expect_true(any(nt$item == "age_group_rule")); expect_true(any(grepl("Tamra", nt$value)))
  # SI Table S2 rows and the participant-level anchors
  s2 <- siw_descriptive_s2(DAT, CFG); expect_true(all(c("UCLA loneliness", "Assessment day", "Pedestrian network length", "UTCI") %in% s2$variable)); expect_equal(sum(s2$variable == "UTCI"), 3L)
  u <- s2[s2$variable == "UTCI" & grepl("^C|^C\\.", s2$section) & grepl("21-day", s2$coding_or_unit), ]; expect_equal(u$mean, mean(DAT$UTCI_AVG_21D)); expect_equal(u$n, nrow(DAT))
  pl <- siw_exposure_participant_level(DAT, CFG); expect_equal(unique(pl$n), nrow(DAT)); expect_equal(pl$unit_of_analysis[1], "participant")
  # participant-day distributions and the supplied missing rainfall total / indicator
  w <- siw_weather_participant_day(DAT, CFG); Nd <- nrow(DAT) * 56L; val <- function(comp, cat, met = NULL) w$n[w$component == comp & w$category == cat & (if (is.null(met)) TRUE else w$metric == met)]
  expect_equal(val("participant_day_distribution", "participant_days_total", "UTCI"), Nd); expect_equal(val("participant_day_distribution", "participant_days_valid", "UTCI"), Nd)
  expect_equal(val("rainfall_validity", "participant_days_with_valid_total"), Nd - 1L); expect_equal(val("rainfall_validity", "participant_days_with_valid_indicator"), Nd)
  expect_equal(val("rainfall_validity", "total_missing_and_indicator_zero"), 1L); expect_true(unique(w$n[w$component == "participant_day_distribution" & w$category == "participant_days_valid" & w$metric == "UTCI"]) != nrow(DAT))
  expect_equal(sum(w$n[w$component == "utci_thermal_stress_class"]), Nd)
  rc <- w[w$component == "rainfall_class_by_total", ]; expect_equal(sum(rc$n), Nd - 1L)
  expect_identical(as.character(.siw_utci_classes(c(9, 0, -13, 26, 8.99, -0.01))), c("no thermal stress", "slight cold stress", "moderate cold stress", "moderate heat stress", "slight cold stress", "moderate cold stress"))   # lower bound inclusive
  # group analysis: independent test calculations
  g <- siw_group_analysis(DAT, CFG); expect_equal(nrow(g$summaries), 30L); expect_equal(nrow(g$tests), 12L)
  kt <- kruskal.test(UCLA_Lonliness_SCORE ~ Locality_3group, data = DAT); expect_equal(g$tests$p[g$tests$outcome == "UCLA_Lonliness_SCORE" & g$tests$factor == "Locality_3group"], kt$p.value, tolerance = 1e-12)
  fp <- fisher.test(table(DAT$Locality_3group, DAT$Int_dummy))$p.value; expect_equal(g$tests$p[g$tests$outcome == "Int_dummy" & g$tests$factor == "Locality_3group"], fp, tolerance = 1e-12)
  for (o in unique(g$tests$outcome)) { i <- g$tests$outcome == o; expect_equal(g$tests$q_within_outcome[i], p.adjust(g$tests$p[i], "BH"), tolerance = 1e-12) }
  bs <- g$summaries[g$summaries$outcome == "Int_dummy" & g$summaries$factor == "Sex_factor" & g$summaries$level == "Female", ]; k <- sum(DAT$Int_dummy[DAT$Sex == 1]); nn <- sum(DAT$Sex == 1)
  expect_equal(c(bs$ci_lower, bs$ci_upper), unname(binom.test(k, nn)$conf.int), tolerance = 1e-12)                          # exact intervals, as the original
})

# ---- independent construction of Batch A references (plain formula interfaces and direct calls; no package diagnostics code) ----
indep_refs <- function(dir) {
  dir.create(dir, showWarnings = FALSE); rows <- list()
  for (oi in seq_len(nrow(CFG$outcomes))) for (mi in seq_len(nrow(CFG$metrics))) for (w in CFG$windows) for (t in CFG$tier_order) {
    o <- CFG$outcomes[oi, ]; sp <- CFG$metrics[mi, ]; ex <- paste0(sp$cumulative_prefix, w, "D"); covs <- CFG$tiers[[t]]; vars <- unique(c(o$outcome, ex, covs))
    d <- droplevels(DAT[complete.cases(DAT[, vars, drop = FALSE]), vars, drop = FALSE]); f <- reformulate(c(ex, covs), o$outcome); bin <- o$family == "binomial"
    fit <- if (bin) suppressWarnings(glm(f, data = d, family = binomial())) else lm(f, data = d); X <- model.matrix(f, d); n <- nrow(X); p <- ncol(X)
    ck <- suppressWarnings(cooks.distance(fit)); hv <- suppressWarnings(hatvalues(fit)); db <- suppressWarnings(dfbetas(fit)); P <- X[, -1, drop = FALSE]
    vf <- if (ncol(P) == 1) 1 else max(diag(solve(cor(P))))
    pr <- if (bin) pmin(pmax(fitted(fit), 1e-12), 1 - 1e-12) else NULL
    cal <- if (bin) c(coef(suppressWarnings(glm(y ~ 1 + offset(l), data = data.frame(y = d[[o$outcome]], l = qlogis(pmin(pmax(pr, 1e-8), 1 - 1e-8))), family = binomial())))[[1]], coef(suppressWarnings(glm(y ~ l, data = data.frame(y = d[[o$outcome]], l = qlogis(pmin(pmax(pr, 1e-8), 1 - 1e-8))), family = binomial())))[[2]]) else c(NA, NA)
    mg <- c(NA, NA, NA); if (bin) { s <- sd(d[[ex]]); lo <- d; hi <- d; lo[[ex]] <- mean(d[[ex]]) - .5 * s; hi[[ex]] <- mean(d[[ex]]) + .5 * s; pl <- predict(fit, lo, type = "response"); ph <- predict(fit, hi, type = "response"); mg <- c(mean(ph - pl), mean(pl), mean(ph)) }
    q <- quantile(d[[ex]], c(.1, .25, .75, .9), names = FALSE)
    rows[[length(rows) + 1L]] <- data.frame(outcome = o$outcome, metric = sp$metric, window_or_lag = w, model_tier = t, condition_number = kappa(X), maximum_vif = vf, calibration_intercept = cal[1], calibration_slope = cal[2],
      maximum_coefficient_magnitude = max(abs(coef(fit))), maximum_cooks_distance = max(ck), maximum_leverage = max(hv), maximum_absolute_dfbeta = max(abs(db)), influential_case_count = sum(ck > 4 / n | hv > 2 * p / n),
      exposure_p10 = q[1], exposure_p25 = q[2], exposure_p75 = q[3], exposure_p90 = q[4], marginal_probability_difference_per_sd = mg[1], probability_at_mean_minus_half_sd = mg[2], probability_at_mean_plus_half_sd = mg[3])
  }
  write.csv(do.call(rbind, rows), file.path(dir, "batchA_cumulative_diagnostics_reference.csv"), row.names = FALSE)
  B <- list()
  for (oi in seq_len(nrow(CFG$outcomes))) for (mi in seq_len(nrow(CFG$metrics))) for (w in CFG$windows) { o <- CFG$outcomes[oi, ]; sp <- CFG$metrics[mi, ]; x <- DAT[[paste0(sp$cumulative_prefix, w, "D")]]; y <- DAT[[o$outcome]]
    m0 <- if (o$family == "binomial") summary(glm(y ~ x, family = binomial()))$coefficients[2, 4] else { Xm <- cbind(1, x); e <- y - Xm %*% solve(crossprod(Xm), crossprod(Xm, y)); h <- rowSums((Xm %*% solve(crossprod(Xm))) * Xm); V <- solve(crossprod(Xm)) %*% crossprod(Xm * drop(e / (1 - h))) %*% solve(crossprod(Xm)); 2 * pt(abs(coef(lm(y ~ x))[2] / sqrt(V[2, 2])), length(y) - 2, lower.tail = FALSE) }
    B[[length(B) + 1L]] <- data.frame(outcome = o$outcome, metric = sp$metric, window_or_lag = w, n = length(y), pearson_or_point_biserial_r = cor(x, y), pearson_p = cor.test(x, y)$p.value, spearman_rho = cor(rank(x), rank(y)),
      spearman_p = cor.test(x, y, method = "spearman", exact = FALSE)$p.value, model_p = unname(m0)) }
  B <- do.call(rbind, B)
  for (nm in c("pearson", "spearman", "model")) { p <- B[[paste0(nm, "_p")]]; B[[paste0("q_", nm, "_global")]] <- p.adjust(p, "BH"); B[[paste0("q_", nm, "_outcome")]] <- ave(p, B$outcome, FUN = function(z) p.adjust(z, "BH")); B[[paste0("q_", nm, "_outcome_metric")]] <- ave(p, B$outcome, B$metric, FUN = function(z) p.adjust(z, "BH")) }
  write.csv(B, file.path(dir, "batchA_bivariate_reference.csv"), row.names = FALSE)
  cont <- c(Age = "Age", `UCLA loneliness` = "UCLA_Lonliness_SCORE", `Objective isolation` = "Isolation_cont", PSQI = "PSQI"); N <- nrow(DAT); T1 <- list()
  for (l in names(cont)) { v <- DAT[[cont[[l]]]]; T1[[length(T1) + 1L]] <- data.frame(characteristic = l, type = "continuous", n_or_count = sum(!is.na(v)), percent = NA, mean = mean(v, na.rm = TRUE), sd = sd(v, na.rm = TRUE), median = median(v, na.rm = TRUE), iqr = IQR(v, na.rm = TRUE), minimum = min(v, na.rm = TRUE), maximum = max(v, na.rm = TRUE), missing = sum(is.na(v))) }
  catr <- function(lab, codes, labels, v) for (k in seq_along(codes)) T1[[length(T1) + 1L]] <<- data.frame(characteristic = paste0(lab, ": ", labels[k]), type = "categorical", n_or_count = sum(v == codes[k]), percent = 100 * sum(v == codes[k]) / N, mean = NA, sd = NA, median = NA, iqr = NA, minimum = NA, maximum = NA, missing = NA)
  catr("Sex", 0:1, c("Male", "Female"), DAT$Sex); catr("Ethnicity", 0:1, c("Muslim and Christian", "Jewish"), DAT$Ethnicity); catr("Locality", 1:3, c("Haifa and Krayot", "Or Akiva and surroundings", "Tamra and Nahariya"), as.integer(DAT$Locality_3group))
  catr("Economic status", 0:1, c("Above average", "Average or below"), DAT$Econ_STS); catr("High integration", 0:1, c("Lower integration", "High integration"), DAT$Int_dummy); catr("Assessment period", 0:1, c("Early", "Late"), as.integer(DAT$AssessmentDay >= 55))
  write.csv(do.call(rbind, T1), file.path(dir, "batchA_descriptive_table1_reference.csv"), row.names = FALSE)
  agegrp <- ifelse(DAT$Age < 70, "<70", ifelse(DAT$Age <= 79, "70-79", ">79")); fl <- list(Age_group = factor(agegrp, levels = c("<70", "70-79", ">79")), Sex_factor = factor(DAT$Sex, 0:1, c("Male", "Female")),
    Ethnicity_factor = factor(DAT$Ethnicity, 0:1, c("Muslim and Christian", "Jewish")), Locality_3group = DAT$Locality_3group); GS <- list(); GT <- list()
  for (o in CFG$outcomes$outcome) { y <- DAT[[o]]; bin <- o == "Int_dummy"
    for (fn in names(fl)) { g <- fl[[fn]]
      for (lv in levels(g)) { yy <- y[g == lv]; if (bin) { ci <- binom.test(sum(yy == 1), length(yy))$conf.int; GS[[length(GS) + 1L]] <- data.frame(outcome = o, factor = fn, level = lv, n = length(yy), mean_or_proportion = mean(yy), se = sqrt(mean(yy) * (1 - mean(yy)) / length(yy)), ci_lower = ci[1], ci_upper = ci[2]) }
        else { se <- sd(yy) / sqrt(length(yy)); GS[[length(GS) + 1L]] <- data.frame(outcome = o, factor = fn, level = lv, n = length(yy), mean_or_proportion = mean(yy), se = se, ci_lower = mean(yy) - 1.96 * se, ci_upper = mean(yy) + 1.96 * se) } }
      tst <- if (bin) list("Fisher exact", NA, NA, fisher.test(table(g, y))$p.value) else if (nlevels(g) == 2) { w <- wilcox.test(y ~ g, exact = FALSE); list("Mann-Whitney-Wilcoxon", unname(w$statistic), NA, w$p.value) } else { k <- kruskal.test(y ~ g); list("Kruskal-Wallis", unname(k$statistic), unname(k$parameter), k$p.value) }
      GT[[length(GT) + 1L]] <- data.frame(outcome = o, factor = fn, test = tst[[1]], statistic = tst[[2]], df = tst[[3]], p = tst[[4]]) } }
  GT <- do.call(rbind, GT); GT$q_within_outcome <- ave(GT$p, GT$outcome, FUN = function(z) p.adjust(z, "BH"))
  write.csv(do.call(rbind, GS), file.path(dir, "batchA_group_summaries_reference.csv"), row.names = FALSE); write.csv(GT, file.path(dir, "batchA_group_tests_reference.csv"), row.names = FALSE); dir
}

test_that("Batch A validation passes against independently built references and fails on defects (coverage, keys, tolerance)", {
  rd <- indep_refs(file.path(tempdir(), "bA_refs")); raw <- siw_read_study_csv(SYN_CSV, CFG)
  bt <- siw_batch_a_tables(raw, DAT, CFG, siw_cumulative_grid(DAT, CFG, tiers = "M0")$results, NA)
  v <- siw_validate_batch_a(bt, CFG, rd); expect_equal(nrow(v$summary), 5L); expect_true(all(grepl("^PASS", v$summary$status))); expect_equal(siw_overall_validation_status(v$summary), "PASS")
  expect_equal(v$summary$n_records_reference, c(1680L, 240L, 17L, 30L, 12L)); expect_true(all(v$summary$n_different == 0L & v$summary$n_unmatched == 0L & v$summary$n_missing_required == 0L & v$summary$n_duplicate == 0L))
  expect_equal(v$summary$n_exact + v$summary$n_within_tolerance, v$summary$n_compared); expect_true(v$summary$n_compared[1] > 20000)
  rd2 <- file.path(tempdir(), "bA_refs2"); dir.create(rd2); file.copy(list.files(rd, full.names = TRUE), rd2)
  unlink(file.path(rd2, "batchA_bivariate_reference.csv")); v2 <- siw_validate_batch_a(bt, CFG, rd2); expect_equal(v2$summary$status[2], "FAIL"); expect_equal(siw_overall_validation_status(v2$summary), "FAIL")     # absent required reference
  x <- read.csv(file.path(rd, "batchA_cumulative_diagnostics_reference.csv")); x <- x[, setdiff(names(x), "maximum_vif")]; write.csv(x, file.path(rd2, "batchA_cumulative_diagnostics_reference.csv"), row.names = FALSE)
  file.copy(file.path(rd, "batchA_bivariate_reference.csv"), rd2, overwrite = TRUE); v3 <- siw_validate_batch_a(bt, CFG, rd2); expect_equal(v3$summary$status[1], "FAIL"); expect_true(any(v3$detail$class == "REQUIRED_COLUMN_MISSING_REFERENCE"))   # missing required column
  x <- read.csv(file.path(rd, "batchA_cumulative_diagnostics_reference.csv")); x$maximum_cooks_distance[7] <- x$maximum_cooks_distance[7] + 1e-3; write.csv(x, file.path(rd2, "batchA_cumulative_diagnostics_reference.csv"), row.names = FALSE)
  v4 <- siw_validate_batch_a(bt, CFG, rd2); expect_equal(v4$summary$status[1], "FAIL"); expect_true(v4$summary$n_different[1] >= 1L)                                                         # out of tolerance
  x <- x[-5, ]; write.csv(x, file.path(rd2, "batchA_cumulative_diagnostics_reference.csv"), row.names = FALSE); expect_equal(siw_validate_batch_a(bt, CFG, rd2)$summary$status[1], "FAIL")           # missing expected key
  bad <- read.csv(file.path(rd, "batchA_cumulative_diagnostics_reference.csv")); bad$maximum_cooks_distance[7] <- bad$maximum_cooks_distance[7] + 1e-9; write.csv(bad, file.path(rd2, "batchA_cumulative_diagnostics_reference.csv"), row.names = FALSE)
  v5 <- siw_validate_batch_a(bt, CFG, rd2); expect_equal(v5$summary$status[1], "PASS_WITHIN_TOLERANCE"); expect_true(any(v5$detail$class == "WITHIN_TOLERANCE"))                                     # sub-tolerance difference reported
  expect_equal(length(siw_load_batch_a_plan()$references), 5L); expect_equal(siw_load_batch_a_plan()$expected_assessment_day_groups_study_data, 33L)
})

test_that("Batch A end to end: separate statuses, new run folder, metadata, manifest; core results unchanged by Batch A", {
  rd <- file.path(tempdir(), "bA_refs"); if (!dir.exists(rd)) indep_refs(rd)
  core_ref <- file.path(tempdir(), "bA_core_ref"); dir.create(core_ref, showWarnings = FALSE)
  G <- siw_cumulative_grid(DAT, CFG)$results; Dm <- siw_final_dlm(DAT, CFG, draws = 50L)$summary
  write.csv(G, file.path(core_ref, "cumulative_regression_grid.csv"), row.names = FALSE); write.csv(Dm[, setdiff(names(Dm), c("simultaneous_critical", "n_lags_simultaneous_excl_null"))], file.path(core_ref, "DLM_L01_L56_nine_representative_summary.csv"), row.names = FALSE)
  ng <- length(unique(read.csv(SYN_CSV)$assessment_day)); out <- file.path(tempdir(), "bA_run"); r <- siw_run_batch_a(SYN_CSV, out, core_ref, rd, CFG, expected_assessment_days = ng)
  expect_equal(r$status, "OK"); expect_true(all(r$statuses == "PASS")); expect_equal(names(r$statuses), c("EXECUTION_STATUS", "INPUT_CHECK_STATUS", "CORE_COMPARISON_STATUS", "BATCH_A_COMPARISON_STATUS"))
  need <- c("cumulative_results.csv", "cumulative_diagnostics.csv", "bivariate_cumulative.csv", "descriptive_table1.csv", "descriptive_si_s2.csv", "descriptive_group_summaries.csv", "descriptive_group_tests.csv", "exposure_participant_level.csv", "weather_participant_day.csv",
            "descriptive_coding_notes.csv", "batch_a_input_checks.csv", "batch_a_validation_summary.csv", "validation_summary.csv", "BATCH_A_STATUS.txt", "batch_a_metadata.json", "batch_a_self_checks.csv", "batch_a_timings.csv", "MANIFEST.csv", "run_metadata.json")
  expect_true(all(file.exists(file.path(out, need)))); m <- jsonlite::fromJSON(file.path(out, "batch_a_metadata.json")); expect_true(nzchar(m$batch_a_plan_hash) && nzchar(m$core_plan_hash)); expect_equal(m$expected_assessment_day_groups, ng)
  expect_true(length(m$batch_a_reference_files) == 5L)
  sc <- read.csv(file.path(out, "batch_a_self_checks.csv")); expect_equal(sum(sc$status == "FAIL"), 0L)
  # the core table is byte-identical to the accepted synthetic demo output from before Batch A existed (independent record), and to a fresh core grid
  demo <- file.path(ROOT, "..", "reports", "demo_final", "cumulative_results.csv"); if (file.exists(demo)) expect_identical(unname(tools::md5sum(file.path(out, "cumulative_results.csv"))), unname(tools::md5sum(demo)))
  cr <- read.csv(file.path(out, "cumulative_results.csv")); expect_equal(cr$coefficient, G$coefficient, tolerance = 1e-12); expect_equal(cr$robust_hc3_se, G$robust_hc3_se, tolerance = 1e-12); expect_equal(cr$nominal_p, G$nominal_p, tolerance = 1e-12); expect_equal(cr$q90, G$q90, tolerance = 1e-12)
  # failures are reported separately: wrong expected assessment-day groups = input check failure; a missing Batch A reference = Batch A comparison failure; core unaffected
  r2 <- siw_run_batch_a(SYN_CSV, file.path(tempdir(), "bA_run2"), core_ref, rd, CFG, expected_assessment_days = 33L)
  expect_equal(r2$statuses[["INPUT_CHECK_STATUS"]], "FAIL"); expect_equal(r2$statuses[["EXECUTION_STATUS"]], "PASS"); expect_equal(r2$statuses[["CORE_COMPARISON_STATUS"]], "PASS"); expect_equal(r2$status, "COMPARISON_OR_INPUT_FAILURE")
  rd3 <- file.path(tempdir(), "bA_refs3"); dir.create(rd3); file.copy(setdiff(list.files(rd, full.names = TRUE), file.path(rd, "batchA_group_tests_reference.csv")), rd3)
  r3 <- siw_run_batch_a(SYN_CSV, file.path(tempdir(), "bA_run3"), core_ref, rd3, CFG, expected_assessment_days = ng)
  expect_equal(r3$statuses[["BATCH_A_COMPARISON_STATUS"]], "FAIL"); expect_equal(r3$statuses[["CORE_COMPARISON_STATUS"]], "PASS"); expect_equal(r3$statuses[["EXECUTION_STATUS"]], "PASS")
  r4 <- siw_run_batch_a(SYN_CSV, file.path(tempdir(), "bA_run4"), file.path(tempdir(), "no_such_core_dir"), rd, CFG, expected_assessment_days = ng)
  expect_equal(r4$statuses[["CORE_COMPARISON_STATUS"]], "FAIL")
})
