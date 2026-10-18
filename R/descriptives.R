# Batch A: descriptive analyses (cohort, outcome, covariate and exposure summaries; group comparisons; participant-level exposure summaries versus
# participant-day weather distributions). Original definitions: WINTER_SOURCE R/050_descriptive.R (siecc_continuous_summary, siecc_table1,
# siecc_group_summary, siecc_group_test, siecc_demographic_analysis) and R/000_utils.R (siecc_se, siecc_binom_ci = exact Clopper-Pearson, siecc_bh).
# Aggregate output only. Coding discrepancies are recorded in siw_descriptive_coding_notes(), never reconciled.

#' Continuous summary exactly as the original: n, missing, mean, SD (n > 1), median, IQR (type 7), minimum, maximum.
#' @export
siw_summary_stats <- function(x) {
  x <- as.numeric(x); o <- x[is.finite(x)]
  data.frame(n = length(o), missing = length(x) - length(o), mean = if (length(o)) mean(o) else NA_real_, sd = if (length(o) > 1L) stats::sd(o) else NA_real_,
             median = if (length(o)) stats::median(o) else NA_real_, iqr = if (length(o)) stats::IQR(o) else NA_real_, minimum = if (length(o)) min(o) else NA_real_,
             maximum = if (length(o)) max(o) else NA_real_, stringsAsFactors = FALSE)
}

#' Labelled factors used by the descriptive tables (original labels and cut rules).
#' @export
siw_descriptive_factors <- function(dat) {
  list(Sex_factor = factor(dat$Sex, levels = c(0, 1), labels = c("Male", "Female")),
       Ethnicity_factor = factor(dat$Ethnicity, levels = c(0, 1), labels = c("Muslim and Christian", "Jewish")),
       Locality_3group = dat$Locality_3group,
       Econ_STS_factor = factor(dat$Econ_STS, levels = c(0, 1), labels = c("Above average", "Average or below")),
       Age_group = cut(dat$Age, breaks = c(-Inf, 69, 79, Inf), labels = c("<70", "70-79", ">79"), right = TRUE),
       High_integration = factor(dat$Int_dummy, levels = c(0, 1), labels = c("Lower integration", "High integration")),
       Assessment_period = factor(dat$AssessmentDay >= 55, levels = c(FALSE, TRUE), labels = c("Early", "Late")))
}

.siw_cat_rows <- function(label, f, n_total, section = NA_character_) {
  tab <- table(f, useNA = "ifany")
  data.frame(characteristic = paste0(label, ": ", names(tab)), type = "categorical", n_or_count = as.integer(tab), percent = 100 * as.integer(tab) / n_total, mean = NA_real_, sd = NA_real_,
             median = NA_real_, iqr = NA_real_, minimum = NA_real_, maximum = NA_real_, missing = NA_integer_, stringsAsFactors = FALSE)
}

#' Cohort table with the original 17 rows (age, UCLA, isolation, PSQI; sex, ethnicity, locality, economic status, high integration, assessment period).
#' @export
siw_table1 <- function(dat) {
  fs <- siw_descriptive_factors(dat); N <- nrow(dat); rows <- list()
  cont <- c(Age = "Age", `UCLA loneliness` = "UCLA_Lonliness_SCORE", `Objective isolation` = "Isolation_cont", PSQI = "PSQI")
  for (lab in names(cont)) { s <- siw_summary_stats(dat[[cont[[lab]]]])
    rows[[length(rows) + 1L]] <- data.frame(characteristic = lab, type = "continuous", n_or_count = s$n, percent = NA_real_, mean = s$mean, sd = s$sd, median = s$median, iqr = s$iqr, minimum = s$minimum, maximum = s$maximum, missing = s$missing, stringsAsFactors = FALSE) }
  for (lab in c("Sex", "Ethnicity", "Locality", "Economic status", "High integration", "Assessment period")) {
    f <- switch(lab, Sex = fs$Sex_factor, Ethnicity = fs$Ethnicity_factor, Locality = fs$Locality_3group, `Economic status` = fs$Econ_STS_factor, `High integration` = fs$High_integration, `Assessment period` = fs$Assessment_period)
    rows[[length(rows) + 1L]] <- .siw_cat_rows(lab, f, N) }
  do.call(rbind, rows)
}

#' SI Table S2 rows: outcomes, demographic/temporal/geographic/socioeconomic variables, health/psychosocial/mobility/built-environment variables, and the
#' participant-level cumulative exposures at the 7-, 21- and 56-day anchors (participant-level summaries; one value per participant).
#' @export
siw_descriptive_s2 <- function(dat, config, windows = c(7L, 21L, 56L)) {
  fs <- siw_descriptive_factors(dat); N <- nrow(dat); rows <- list()
  cont <- function(section, variable, unit, x) { s <- siw_summary_stats(x)
    rows[[length(rows) + 1L]] <<- data.frame(section = section, variable = variable, level = "", coding_or_unit = unit, n = s$n, count = NA_integer_, percent = NA_real_, mean = s$mean, sd = s$sd, median = s$median,
      iqr = s$iqr, minimum = s$minimum, maximum = s$maximum, missing = s$missing, stringsAsFactors = FALSE) }
  catrow <- function(section, variable, unit, f) { tab <- table(f, useNA = "ifany")
    for (lv in names(tab)) rows[[length(rows) + 1L]] <<- data.frame(section = section, variable = variable, level = lv, coding_or_unit = unit, n = sum(tab), count = as.integer(tab[[lv]]), percent = 100 * as.integer(tab[[lv]]) / N,
      mean = NA_real_, sd = NA_real_, median = NA_real_, iqr = NA_real_, minimum = NA_real_, maximum = NA_real_, missing = NA_integer_, stringsAsFactors = FALSE) }
  A <- "A. Outcomes"; B <- "B. Demographic, temporal, geographic and socioeconomic variables"; C <- "C. Health, psychosocial, mobility, built-environment and weather variables"
  cont(A, "UCLA loneliness", "score", dat$UCLA_Lonliness_SCORE); cont(A, "Objective isolation", "score", dat$Isolation_cont); cont(A, "Original integration", "ordinal score (1-4)", dat$integration_score)
  catrow(A, "High integration", "0 = score 1-3; 1 = score 4", fs$High_integration)
  cont(B, "Age", "years", dat$Age); catrow(B, "Sex", "0 = male; 1 = female", fs$Sex_factor); catrow(B, "Ethnicity", "0 = Muslim and Christian; 1 = Jewish", fs$Ethnicity_factor)
  catrow(B, "Locality", "Area 1/2/3", fs$Locality_3group); catrow(B, "Economic status", "0 = above average; 1 = average or below", fs$Econ_STS_factor)
  cont(B, "Assessment day", "days from first assessment", dat$AssessmentDay); catrow(B, "Assessment period", "assessment day < 55 early; >= 55 late", fs$Assessment_period)
  for (v in list(c("PSQI", "PSQI"), c("Subjective health", "Subjective_Health"), c("Memory", "Memory"), c("Memory change", "Memory_Changed"), c("Subjective wellbeing", "Subjective_Wellbeing"),
                 c("Memory assessment", "Memory_assess"), c("Chronic-condition domains", "chronic_domain_count"), c("Outdoor mobility", "Outdoor_Mobility"))) cont(C, v[1L], "stored score", dat[[v[2L]]])
  cont(C, "Pedestrian network length", "km", dat$PedestLength_km)
  for (i in seq_len(nrow(config$metrics))) for (w in windows) { sp <- config$metrics[i, ]
    cont(C, sp$metric, paste0(sp$unit, "; ", w, "-day cumulative exposure (participant level)"), dat[[siw_cum_name(sp$cumulative_prefix, w)]]) }
  do.call(rbind, rows)
}

siw_binom_ci <- function(events, n, conf.level = 0.95) {
  if (!is.finite(events) || !is.finite(n) || n <= 0) return(c(NA_real_, NA_real_))
  unname(stats::binom.test(events, n, conf.level = conf.level)$conf.int)       # exact (Clopper-Pearson), as the original
}
.siw_se <- function(x) { x <- x[is.finite(x)]; if (length(x) < 2L) NA_real_ else stats::sd(x) / sqrt(length(x)) }

#' Group summaries and tests of each outcome by age group, sex, ethnicity and locality, with the original tests and BH within outcome (4 tests per outcome).
#' @export
siw_group_analysis <- function(dat, config) {
  fs <- siw_descriptive_factors(dat); factors <- c("Age_group", "Sex_factor", "Ethnicity_factor", "Locality_3group"); summ <- list(); tests <- list()
  for (oi in seq_len(nrow(config$outcomes))) {
    o <- config$outcomes[oi, ]; y <- as.numeric(dat[[o$outcome]]); binary <- o$family == "binomial"
    for (fn in factors) {
      g <- fs[[fn]]
      for (lev in levels(factor(g))) {
        keep <- !is.na(g) & as.character(g) == lev; yy <- y[keep]
        if (binary) { ev <- sum(yy == 1, na.rm = TRUE); n <- sum(is.finite(yy)); ci <- siw_binom_ci(ev, n)
          summ[[length(summ) + 1L]] <- data.frame(outcome = o$outcome, factor = fn, level = lev, n = n, mean_or_proportion = if (n) ev / n else NA_real_, se = if (n) sqrt((ev / n) * (1 - ev / n) / n) else NA_real_, ci_lower = ci[[1L]], ci_upper = ci[[2L]], stringsAsFactors = FALSE)
        } else { yy <- yy[is.finite(yy)]; se <- .siw_se(yy)
          summ[[length(summ) + 1L]] <- data.frame(outcome = o$outcome, factor = fn, level = lev, n = length(yy), mean_or_proportion = mean(yy), se = se, ci_lower = mean(yy) - 1.96 * se, ci_upper = mean(yy) + 1.96 * se, stringsAsFactors = FALSE) }
      }
      cc <- is.finite(y) & !is.na(g); yc <- y[cc]; gc <- droplevels(factor(g[cc]))
      if (binary) { fit <- tryCatch(stats::fisher.test(table(gc, yc)), error = function(e) NULL)
        tests[[length(tests) + 1L]] <- data.frame(outcome = o$outcome, factor = fn, test = "Fisher exact", statistic = NA_real_, df = NA_real_, p = if (is.null(fit)) NA_real_ else fit$p.value, stringsAsFactors = FALSE)
      } else if (nlevels(gc) == 2L) { fit <- stats::wilcox.test(yc ~ gc, exact = FALSE)
        tests[[length(tests) + 1L]] <- data.frame(outcome = o$outcome, factor = fn, test = "Mann-Whitney-Wilcoxon", statistic = unname(fit$statistic), df = NA_real_, p = fit$p.value, stringsAsFactors = FALSE)
      } else { fit <- stats::kruskal.test(yc ~ gc)
        tests[[length(tests) + 1L]] <- data.frame(outcome = o$outcome, factor = fn, test = "Kruskal-Wallis", statistic = unname(fit$statistic), df = unname(fit$parameter), p = fit$p.value, stringsAsFactors = FALSE) }
    }
  }
  tt <- do.call(rbind, tests); tt$q_within_outcome <- NA_real_
  for (o in unique(tt$outcome)) { i <- which(tt$outcome == o); tt$q_within_outcome[i] <- siw_bh(tt$p[i]) }
  list(summaries = do.call(rbind, summ), tests = tt)
}

# UTCI thermal-stress categories (Brode et al. 2012), lower bound inclusive: [lo, hi)
.siw_utci_classes <- function(x) cut(x, breaks = c(-Inf, -40, -27, -13, 0, 9, 26, 32, 38, 46, Inf),
  labels = c("extreme cold stress", "very strong cold stress", "strong cold stress", "moderate cold stress", "slight cold stress", "no thermal stress", "moderate heat stress", "strong heat stress", "very strong heat stress", "extreme heat stress"), right = FALSE)

#' Participant-day distributions of the lagged daily weather (L01..L<lag_end>): validity counts, summary statistics, UTCI thermal-stress classes and rainfall classes.
#' This is NOT the participant-level exposure summary: every participant contributes lag_end days. The missing daily rainfall total and its supplied rainy-day indicator are reported as supplied.
#' @export
siw_weather_participant_day <- function(dat, config, lag_end = config$dlm$final$lag_end) {
  rows <- list(); add <- function(component, metric, category, n, percent, note = "") rows[[length(rows) + 1L]] <<- data.frame(component = component, metric = metric, category = category, n = n, percent = percent, note = note, stringsAsFactors = FALSE)
  N <- nrow(dat) * lag_end
  for (i in seq_len(nrow(config$metrics))) {
    sp <- config$metrics[i, ]; v <- as.numeric(as.matrix(dat[, siw_daily_names(sp$daily_prefix, lag_end), drop = FALSE])); o <- v[is.finite(v)]
    add("participant_day_distribution", sp$metric, "participant_days_total", N, NA_real_); add("participant_day_distribution", sp$metric, "participant_days_valid", length(o), 100 * length(o) / N)
    add("participant_day_distribution", sp$metric, "mean", length(o), mean(o)); add("participant_day_distribution", sp$metric, "sd", length(o), stats::sd(o))
    add("participant_day_distribution", sp$metric, "minimum", length(o), min(o)); add("participant_day_distribution", sp$metric, "maximum", length(o), max(o))
  }
  u <- as.numeric(as.matrix(dat[, siw_daily_names("UTCI", lag_end), drop = FALSE])); uo <- u[is.finite(u)]; cl <- table(.siw_utci_classes(uo))
  for (nm in names(cl)) add("utci_thermal_stress_class", "UTCI", nm, as.integer(cl[[nm]]), 100 * as.integer(cl[[nm]]) / length(uo), "lower bound inclusive")
  rt <- as.numeric(as.matrix(dat[, siw_daily_names("IMS_IDW_RAIN_TOTAL", lag_end), drop = FALSE])); ro <- rt[is.finite(rt)]
  rd <- as.numeric(as.matrix(dat[, siw_daily_names("IMS_IDW_RAIN_DAYS1", lag_end), drop = FALSE])); rdo <- rd[is.finite(rd)]
  add("rainfall_validity", "IMS_rainfall_total", "participant_days_with_valid_total", length(ro), 100 * length(ro) / N, "missing totals are not imputed")
  add("rainfall_validity", "IMS_rainfall_total", "participant_days_total_missing", sum(!is.finite(rt)), 100 * sum(!is.finite(rt)) / N)
  add("rainfall_validity", "IMS_rainy_day_indicator", "participant_days_with_valid_indicator", length(rdo), 100 * length(rdo) / N, "indicator as supplied")
  add("rainfall_validity", "IMS_rainfall_total_vs_indicator", "total_missing_and_indicator_zero", sum(is.na(rt) & !is.na(rd) & rd == 0), 100 * sum(is.na(rt) & !is.na(rd) & rd == 0) / N, "reported, not altered")
  add("rainfall_class_by_total", "IMS_rainfall_total", "no assigned rainfall (0 mm)", sum(ro == 0), 100 * sum(ro == 0) / length(ro), "denominator = valid totals")
  add("rainfall_class_by_total", "IMS_rainfall_total", "more than 0 and less than 1 mm", sum(ro > 0 & ro < 1), 100 * sum(ro > 0 & ro < 1) / length(ro))
  add("rainfall_class_by_total", "IMS_rainfall_total", "at least 1 mm (rainy day)", sum(ro >= 1), 100 * sum(ro >= 1) / length(ro))
  add("rainfall_class_by_indicator", "IMS_rainy_day_indicator", "rainy day (indicator = 1)", sum(rdo == 1), 100 * sum(rdo == 1) / length(rdo), "denominator = valid indicators")
  do.call(rbind, rows)
}

#' Participant-level exposure summaries at the 7/21/56-day anchors: one cumulative value per participant (see also siw_descriptive_s2).
#' @export
siw_exposure_participant_level <- function(dat, config, windows = c(7L, 21L, 56L)) {
  rows <- list()
  for (i in seq_len(nrow(config$metrics))) for (w in windows) { sp <- config$metrics[i, ]; s <- siw_summary_stats(dat[[siw_cum_name(sp$cumulative_prefix, w)]])
    rows[[length(rows) + 1L]] <- data.frame(metric = sp$metric, window_days = as.integer(w), unit = sp$unit, unit_of_analysis = "participant", s, stringsAsFactors = FALSE) }
  do.call(rbind, rows)
}

#' Coding rules and discrepancies recorded rather than reconciled (aggregate counts only).
#' @export
siw_descriptive_coding_notes <- function(dat, config) {
  rows <- list(); add <- function(item, value, note) rows[[length(rows) + 1L]] <<- data.frame(item = item, value = as.character(value), note = note, stringsAsFactors = FALSE)
  a <- dat$Age
  add("age_group_rule", "<70, 70-79, >79", "legacy rule cut(Age, c(-Inf, 69, 79, Inf), right = TRUE); the stored age-group field is not in the canonical CSV, so a mismatch with it (Phase 1B: one participant) cannot be examined here")
  for (v in c(69, 70, 79, 80)) add(paste0("participants_with_age_", v), sum(a == v, na.rm = TRUE), "boundary ages for the age-group rule")
  add("age_observed_range", paste(range(a, na.rm = TRUE), collapse = " to "), "observed in the supplied data (compare SI Table S2 at the concordance stage)")
  add("assessment_period_rule", "assessment_day >= 55 is Late", paste0("legacy rule; Early n = ", sum(dat$AssessmentDay < 55, na.rm = TRUE), ", Late n = ", sum(dat$AssessmentDay >= 55, na.rm = TRUE)))
  add("economic_status_coding", "0 = above average; 1 = average or below", "legacy labels")
  add("ethnicity_coding", "0 = Muslim and Christian; 1 = Jewish", "Ethnicity is the Jewish indicator (identical to the legacy religious_row field, Phase 1)")
  add("locality_label", "Tamra and Nahariya", "label of Area 3 as in the original; Phase 1 found no Tamra participants")
  add("binary_group_ci_method", "exact (Clopper-Pearson)", "as the original tables; the SI figure notes mention Wilson intervals (not produced here)")
  add("continuous_group_ci_method", "mean +/- 1.96 SE", "as the original")
  add("utci_class_boundaries", "lower bound inclusive", "Brode et al. 2012 categories")
  add("rainfall_total_missing_cells", sum(!is.finite(as.matrix(dat[, siw_daily_names("IMS_IDW_RAIN_TOTAL", config$dlm$final$lag_end), drop = FALSE]))), "supplied missing totals within L01-L56, not imputed")
  do.call(rbind, rows)
}

#' Assessment-day checks (Batch A input validation). Reports completeness, integer coding and the number of distinct assessment-day groups.
#' @param expected_groups expected number of distinct days (33 for the study data); NA = report only.
#' @export
siw_validate_assessment_day <- function(raw, expected_groups = NA_integer_) {
  R <- list(); add <- function(id, status, n, detail) R[[length(R) + 1L]] <<- data.frame(check_id = id, category = "assessment_day", status = status, n_affected = as.integer(n), detail = detail, stringsAsFactors = FALSE)
  if (!("assessment_day" %in% names(raw))) { add("BA01", "FAIL", NA, "assessment_day column is missing"); return(do.call(rbind, R)) }
  d <- suppressWarnings(as.numeric(raw$assessment_day)); nm <- sum(is.na(d)); ni <- sum(!is.na(d) & (d != round(d) | !is.finite(d)))
  add("BA01", if (nm) "FAIL" else "PASS", nm, paste0("missing or non-numeric assessment_day values=", nm))
  add("BA02", if (ni) "FAIL" else "PASS", ni, paste0("non-integer or non-finite assessment_day values=", ni))
  add("BA03", if (any(d < 0, na.rm = TRUE)) "FAIL" else "PASS", sum(d < 0, na.rm = TRUE), paste0("negative assessment_day values=", sum(d < 0, na.rm = TRUE), "; range ", paste(range(d, na.rm = TRUE), collapse = " to ")))
  g <- length(unique(d[!is.na(d)]))
  add("BA04", if (!is.na(expected_groups) && g != expected_groups) "FAIL" else "PASS", g, if (is.na(expected_groups)) paste0("distinct assessment_day groups=", g, " (no expected value supplied)") else paste0("distinct assessment_day groups=", g, "; expected ", expected_groups))
  do.call(rbind, R)
}
