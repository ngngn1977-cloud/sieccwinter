# Post hoc steps reproducing the archive engine (stages 13-31; accepted as the original by the author on 13 Oct 2026), on the locked canonical CSV:
#   (a) no-April restriction of the 90 primary cumulative models (stage 17), (f) ERA5-IMS source agreement (stage 28), (g) exposure residual diagnostics (stage 13),
#   (h) penalized profile-likelihood inference for the 30 representative integration models (stage 25). The stage-17 port is shared with the early/late step (e).
# Differences from the archive (another input snapshot; R 4.2) are reported by the comparison, never tuned away.

SIW_S17_COVARIATES <- c("Sex", "Age", "AssessmentDay_centered", "Locality_3group", "Econ_STS")
SIW_AGE65_MINIMUM <- 65     # PH_AGE65 (post hoc protocol v2, amendment of 14 Oct 2026): participants with Age < 65 (Age = 2026 minus birth year) are excluded

# archive src/r/17_post_dlm_sensitivity_contrasts.R fit_matrix: rank (tol 1e-10) / residual-df / outcome-variation gate; lm.fit + HC3 (pmax(1e-12, (1-h)^2)); glm.fit epsilon 1e-12 maxit 200
.siw_s17_fit <- function(x, y, family) {
  n <- nrow(x); p <- ncol(x); rk <- qr(x, tol = 1e-10)$rank
  if (n <= p || rk < p || (family == "binomial" && length(unique(y)) < 2L)) return(list(completed = FALSE, reason = "RANK_RESIDUAL_DF_OR_OUTCOME_VARIATION_GATE", coefficients = rep(NA_real_, p), covariance = matrix(NA_real_, p, p), convergence = FALSE, separation = NA))
  if (family == "gaussian") { fit <- stats::lm.fit(x, y); V <- .siw_v14_hc3(x, fit$residuals); return(list(completed = all(is.finite(fit$coefficients)) && all(is.finite(V)), reason = "", coefficients = fit$coefficients, covariance = V, convergence = TRUE, separation = FALSE)) }
  fit <- suppressWarnings(stats::glm.fit(x, y, family = stats::binomial(), control = stats::glm.control(epsilon = 1e-12, maxit = 200L))); V <- tryCatch(solve(crossprod(x, x * fit$weights)), error = function(e) matrix(NA_real_, p, p))
  sep <- !isTRUE(fit$converged) || any(!is.finite(fit$coefficients)) || max(abs(fit$coefficients), na.rm = TRUE) > 25; ok <- isTRUE(fit$converged) && all(is.finite(fit$coefficients)) && all(is.finite(V))
  list(completed = ok, reason = if (ok) "" else "BINOMIAL_CONVERGENCE_OR_FINITE_GATE", coefficients = fit$coefficients, covariance = V, convergence = fit$converged, separation = sep)
}

# calendar-spline variants (post hoc protocol v2, 14 Oct 2026): DATE_NS_DF<k> replaces the linear centred assessment-day term by splines::ns(assessment_day, df = k)
# (default knots: interior at quantiles, boundary at the range, of the model's complete-case sample); every other M2-L covariate is kept
.siw_s17_ns_df <- function(variant) if (grepl("^DATE_NS_DF[0-9]+$", variant)) as.integer(sub("^DATE_NS_DF", "", variant)) else 0L
.siw_s17_ns_frame <- function(local, outcome, exposure, k) {
  base <- setdiff(SIW_S17_COVARIATES, "AssessmentDay_centered"); local <- droplevels(local[stats::complete.cases(local[, unique(c(outcome, exposure, base, "AssessmentDay")), drop = FALSE]), , drop = FALSE])
  B <- splines::ns(as.numeric(local$AssessmentDay), df = k); for (j in seq_len(k)) local[[paste0("calendar_ns", j)]] <- B[, j]
  list(data = local, covariates = c(base, paste0("calendar_ns", seq_len(k))))
}

#' Archive stage-17 cumulative sensitivity row (variants NO_APRIL, EARLY_LATE_INTERACTION, the calendar-spline variants DATE_NS_DF2 / DATE_NS_DF3 and AGE65).
#' NO_APRIL removes participants interviewed in April (interview date = anchor + assessment_day). AGE65 removes participants with Age < 65 (SIW_AGE65_MINIMUM) and keeps every M2-L covariate. DATE_NS_DF<k> and AGE65 rows also carry parameter_count, residual_df,
#' p_t (continuous outcomes: t reference on the residual df, the primary convention; NA for logistic) and p_primary (p_t for continuous, the Wald normal p for logistic).
#' @export
siw_s17_cumulative <- function(dat, config, outcome, family, metric, window, variant = c("NO_APRIL", "EARLY_LATE_INTERACTION", "DATE_NS_DF2", "DATE_NS_DF3", "AGE65"), protocol = siw_posthoc_protocol()) {
  variant <- match.arg(variant); sp <- config$metrics[config$metrics$metric == metric, ]; exposure <- siw_cum_name(sp$cumulative_prefix, window); oc <- config$outcomes$code[config$outcomes$outcome == outcome]
  local <- dat; if (is.null(local$EarlyLate)) local$EarlyLate <- siw_early_late(local$AssessmentDay)
  if (variant == "NO_APRIL") local <- local[format(siw_interview_date(local$AssessmentDay, protocol), "%m") != "04", , drop = FALSE]
  if (variant == "AGE65") local <- local[is.finite(local$Age) & local$Age >= SIW_AGE65_MINIMUM, , drop = FALSE]
  nsk <- .siw_s17_ns_df(variant); covs <- SIW_S17_COVARIATES; if (nsk) { nf <- .siw_s17_ns_frame(local, outcome, exposure, nsk); local <- nf$data; covs <- nf$covariates }
  ec <- paste0(exposure, "_centered"); local[[ec]] <- local[[exposure]] - mean(local[[exposure]], na.rm = TRUE)
  terms <- if (variant == "EARLY_LATE_INTERACTION") c(paste0(ec, " * EarlyLate"), covs) else c(exposure, covs); fo <- stats::as.formula(paste("~", paste(terms, collapse = " + ")))
  need <- unique(c(outcome, exposure, all.vars(fo))); local <- droplevels(local[stats::complete.cases(local[, need, drop = FALSE]), , drop = FALSE])
  x <- stats::model.matrix(fo, data = local); y <- as.numeric(local[[outcome]]); fit <- .siw_s17_fit(x, y, family)
  en <- if (variant == "EARLY_LATE_INTERACTION") ec else exposure; ei <- match(en, colnames(x)); est <- if (fit$completed && !is.na(ei)) fit$coefficients[[ei]] else NA_real_; se <- if (fit$completed && !is.na(ei)) sqrt(max(0, fit$covariance[ei, ei])) else NA_real_
  ii <- if (variant == "EARLY_LATE_INTERACTION") grep(paste0("^", en, ":EarlyLate|^EarlyLate.*:", en), colnames(x)) else integer(); ok1 <- fit$completed && length(ii) == 1L
  ie <- if (ok1) fit$coefficients[[ii]] else NA_real_; ise <- if (ok1) sqrt(max(0, fit$covariance[ii, ii])) else NA_real_
  ev <- local[[exposure]][local$EarlyLate == "Early"]; lv <- local[[exposure]][local$EarlyLate == "Late"]
  ov <- if (length(ev) && length(lv)) { w <- max(local[[exposure]]) - min(local[[exposure]]); if (w > 0) max(0, min(max(ev), max(lv)) - max(min(ev), min(lv))) / w else 0 } else NA_real_
  pz <- function(b, s) if (is.finite(b) && is.finite(s) && s > 0) 2 * stats::pnorm(abs(b / s), lower.tail = FALSE) else NA_real_
  out <- data.frame(model_id = paste(siw_cum_id(oc, sp$code, window, "M2_L"), variant, sep = "__"), outcome = outcome, family = family, metric = metric, window = as.integer(window), variant = variant, n = nrow(x),
             event_count = if (family == "binomial") sum(y == 1) else NA_integer_, estimate = est, standard_error = se, ci_lower = est - 1.96 * se, ci_upper = est + 1.96 * se, p = pz(est, se),
             early_estimate = if (variant == "EARLY_LATE_INTERACTION") est else NA_real_, late_estimate = if (ok1) est + ie else NA_real_, interaction_estimate = ie, interaction_ci_lower = ie - 1.96 * ise, interaction_ci_upper = ie + 1.96 * ise,
             interaction_p = pz(ie, ise), exposure_overlap = ov, convergence = fit$convergence, separation = fit$separation, status = if (fit$completed) "COMPLETED" else "NOT_ESTIMABLE", not_estimable_reason = fit$reason, stringsAsFactors = FALSE)
  if (nsk || variant == "AGE65") { rdf <- nrow(x) - ncol(x); pt_ <- if (family == "gaussian" && is.finite(est) && is.finite(se) && se > 0) 2 * stats::pt(abs(est / se), rdf, lower.tail = FALSE) else NA_real_
    out$parameter_count <- ncol(x); out$residual_df <- rdf; out$p_t <- pt_; out$p_primary <- if (family == "gaussian") pt_ else out$p }
  out
}

#' Stage-17 variant over the 90 primary cumulative models (outcome x representative metric x window, configuration order).
#' @export
siw_s17_grid <- function(dat, config, variant, protocol = siw_posthoc_protocol()) {
  do.call(rbind, lapply(seq_len(nrow(config$outcomes)), function(oi) do.call(rbind, lapply(config$representative_metrics, function(m) do.call(rbind, lapply(config$windows, function(w) siw_s17_cumulative(dat, config, config$outcomes$outcome[oi], config$outcomes$family[oi], m, w, variant, protocol)))))))
}

# reference rows with the window recovered from source_model_id (__W<window>__)
.siw_s17_ref <- function(tab) { tab$window <- suppressWarnings(as.integer(sub(".*__W([0-9]+)__.*", "\\1", tab$source_model_id))); tab }

# independent refit (stats::lm / stats::glm on a formula; HC3 by an explicit loop over participants) of a stage-17 row
.siw_s17_independent <- function(dat, config, row, protocol) {
  sp <- config$metrics[config$metrics$metric == row$metric, ]; exposure <- siw_cum_name(sp$cumulative_prefix, row$window); local <- dat
  if (row$variant == "NO_APRIL") local <- local[format(siw_interview_date(local$AssessmentDay, protocol), "%m") != "04", , drop = FALSE]
  if (row$variant == "AGE65") local <- local[is.finite(local$Age) & local$Age >= SIW_AGE65_MINIMUM, , drop = FALSE]
  if (is.null(local$EarlyLate)) local$EarlyLate <- siw_early_late(local$AssessmentDay)
  nsk <- .siw_s17_ns_df(row$variant); covs <- SIW_S17_COVARIATES; if (nsk) { nf <- .siw_s17_ns_frame(local, row$outcome, exposure, nsk); local <- nf$data; covs <- nf$covariates }
  ec <- paste0(exposure, "_centered"); local[[ec]] <- local[[exposure]] - mean(local[[exposure]], na.rm = TRUE)
  rhs <- if (row$variant == "EARLY_LATE_INTERACTION") c(paste0(ec, " * EarlyLate"), covs) else c(exposure, covs); f <- stats::reformulate(rhs, row$outcome)
  local <- droplevels(local[stats::complete.cases(local[, unique(c(row$outcome, exposure, all.vars(f))), drop = FALSE]), , drop = FALSE]); en <- if (row$variant == "EARLY_LATE_INTERACTION") ec else exposure
  if (row$family == "gaussian") { fit <- stats::lm(f, data = local); X <- stats::model.matrix(fit); e <- stats::residuals(fit); XtXi <- solve(crossprod(X)); M <- matrix(0, ncol(X), ncol(X))
    for (i in seq_len(nrow(X))) { xi <- X[i, ]; h <- drop(t(xi) %*% XtXi %*% xi); M <- M + (e[i]^2 / max(1e-12, (1 - h)^2)) * tcrossprod(xi) }; V <- XtXi %*% M %*% XtXi }
  else { fit <- suppressWarnings(stats::glm(f, data = local, family = stats::binomial(), control = stats::glm.control(epsilon = 1e-12, maxit = 200L))); V <- stats::vcov(fit) }
  j <- match(en, names(stats::coef(fit))); c(estimate = unname(stats::coef(fit)[j]), standard_error = sqrt(V[j, j]))
}

.siw_ph_item_no_april <- function(dat, config, protocol, it, refs, ctx) {
  g <- siw_s17_grid(dat, config, "NO_APRIL", protocol); ctx$W(g, "no_april_cumulative_90.csv")
  vr <- do.call(rbind, lapply(seq_len(nrow(g)), function(i) { r <- g[i, ]; iv <- .siw_s17_independent(dat, config, r, protocol); data.frame(model_id = r$model_id, estimate_rel_diff = abs(r$estimate - iv[["estimate"]]) / max(abs(iv[["estimate"]]), 1e-12), se_rel_diff = abs(r$standard_error - iv[["standard_error"]]) / max(abs(iv[["standard_error"]]), 1e-12), stringsAsFactors = FALSE) }))
  vr$status <- ifelse(vr$estimate_rel_diff <= 1e-6 & vr$se_rel_diff <= 1e-6, "PASS", "FAIL"); ctx$W(vr, "no_april_independent_verification.csv")
  rs <- it$reference; ref <- .siw_ph_ref(ctx, refs, protocol, rs$role, rs$file, rs$row_filter); ctx$W(ref$provenance, "posthoc_reference_provenance.csv"); rstat <- .siw_ph_dep(ref)
  if (is.null(rstat)) { cmp <- siw_posthoc_compare(g, .siw_s17_ref(ref$table), unlist(rs$keys), unlist(rs$fields), config, rs$tolerance, "no_april_90", rs$file, rs$expected_compared); ctx$W(cmp$summary, "no_april_comparison_summary.csv"); ctx$W(cmp$detail, "no_april_comparison_detail.csv"); rstat <- .siw_ph_cmp_status(cmp) }
  si <- it$si_concordance; w56 <- g[g$window == 56L, ]; rows <- list(.siw_ph_si_row("no_april", "n (every model)", if (length(unique(g$n)) == 1L) unique(g$n) else NA, si$n, 0),
    .siw_ph_si_row("no_april", "high-integration events", unique(g$event_count[g$family == "binomial"])[1], si$events, 0), .siw_ph_si_row("no_april", "high-integration non-events", unique((g$n - g$event_count)[g$family == "binomial"])[1], si$non_events, 0),
    .siw_ph_si_row("no_april", "minimum nominal p across the 90 models", min(g$p, na.rm = TRUE), si$minimum_p_90, 3))
  for (t in si$w56) { r <- w56[w56$outcome == t$outcome & w56$metric == t$metric, ]; s <- stats::sd(dat[[siw_cum_name(config$metrics$cumulative_prefix[config$metrics$metric == t$metric], 56L)]], na.rm = TRUE); tr <- if (t$scale == "OR") exp else identity
    v <- tr(c(r$estimate, r$ci_lower, r$ci_upper) * s); ci <- unlist(t$ci); for (k in 1:3) rows[[length(rows) + 1L]] <- .siw_ph_si_row("no_april", paste(t$outcome, t$metric, "W56", t$scale, c("estimate", "ci_lower", "ci_upper")[k], "per full-sample SD"), v[k], c(t$estimate, ci)[k], 2) }
  sic <- do.call(rbind, rows); if (ctx$synthetic) sic$agrees <- NA; ctx$W(sic, "no_april_si_concordance.csv")
  st <- c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else ref$status, NO_APRIL_REPRODUCTION_STATUS = rstat, NO_APRIL_VERIFICATION_STATUS = if (all(vr$status == "PASS")) "PASS" else "FAIL", NO_APRIL_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(sic$agrees)) "PASS" else "REVIEW")
  info <- c(paste0("NO_APRIL_N=", paste(unique(g$n), collapse = ","), "; models completed ", sum(g$status == "COMPLETED"), " of ", nrow(g), "; minimum nominal p ", signif(min(g$p, na.rm = TRUE), 4)), "NO_APRIL_RULE=participants whose interview date (2026-01-29 + assessment_day) falls in April are removed; weather days in April within other histories are kept")
  list(statuses = st, info = info, tables = list(no_april = g, verification = vr, si = sic))
}

# ---- (f) ERA5-IMS source agreement --------------------------------------------------------------------------------------------------------------------------

#' ERA5-IMS source agreement (archive src/r/28_dlm_scenario_weather_assignment.R): temperature and RH, 10 windows, all participants and the three locality groups (80 rows).
#' @export
siw_source_agreement <- function(dat) {
  pairs <- data.frame(comparison = c("ERA5_vs_IMS_temperature", "ERA5_vs_IMS_relative_humidity"), era5 = c("AIR_TEMP_AVG_", "HUMIDITY_AVG_"), ims = c("IMS_TEMP_IDW_AVG_", "IMS_RH_IDW_AVG_"), unit = c("degrees_C", "percent_relative_humidity"), stringsAsFactors = FALSE); rows <- list()
  for (pi in seq_len(nrow(pairs))) for (w in c(1L, 3L, 7L, 14L, 21L, 28L, 35L, 42L, 49L, 56L)) { ev <- paste0(pairs$era5[pi], w, "D"); iv <- paste0(pairs$ims[pi], w, "D")
    for (grp in c("ALL", levels(dat$Locality_3group))) { s <- if (grp == "ALL") rep(TRUE, nrow(dat)) else as.character(dat$Locality_3group) == grp; x <- as.numeric(dat[[ev]][s]); y <- as.numeric(dat[[iv]][s]); k <- is.finite(x) & is.finite(y); x <- x[k]; y <- y[k]; d <- x - y; vs <- stats::var(x) + stats::var(y)
      ccc <- if (length(x) >= 3L && is.finite(vs) && vs > 0) 2 * stats::cov(x, y) / (vs + (mean(x) - mean(y))^2) else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(comparison = pairs$comparison[pi], window_days = w, stratum = "Locality_3group", group = grp, era5_variable = ev, ims_variable = iv, common_unit = pairs$unit[pi], n_pairwise = length(x),
        pearson = if (length(x) >= 3L) stats::cor(x, y) else NA_real_, spearman = if (length(x) >= 3L) stats::cor(x, y, method = "spearman") else NA_real_, mean_bias_era5_minus_ims = mean(d), median_bias_era5_minus_ims = stats::median(d), rmse = sqrt(mean(d^2)),
        lin_concordance_correlation = ccc, bland_altman_lower = mean(d) - 1.96 * stats::sd(d), bland_altman_upper = mean(d) + 1.96 * stats::sd(d), assignment_comparison_status = "COMPLETE_PAIRED_SOURCE_COMPARISON", stringsAsFactors = FALSE) } }
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

.siw_ph_item_source_agreement <- function(dat, config, protocol, it, refs, ctx) {
  a <- siw_source_agreement(dat); ctx$W(a, "weather_source_agreement_80.csv")
  vr <- do.call(rbind, lapply(seq_len(nrow(a)), function(i) { r <- a[i, ]; s <- if (r$group == "ALL") rep(TRUE, nrow(dat)) else as.character(dat$Locality_3group) == r$group; x <- as.numeric(dat[[r$era5_variable]][s]); y <- as.numeric(dat[[r$ims_variable]][s]); k <- is.finite(x) & is.finite(y); x <- x[k]; y <- y[k]; n <- length(x)
    pr <- sum((x - mean(x)) * (y - mean(y))) / sqrt(sum((x - mean(x))^2) * sum((y - mean(y))^2)); rr <- sqrt(sum((x - y)^2) / n); mb <- sum(x - y) / n
    data.frame(comparison = r$comparison, window_days = r$window_days, group = r$group, max_rel_diff = max(abs(c(pr - r$pearson, rr - r$rmse, mb - r$mean_bias_era5_minus_ims)) / pmax(abs(c(pr, rr, mb)), 1e-12)), stringsAsFactors = FALSE) }))
  vr$status <- ifelse(vr$max_rel_diff <= 1e-10, "PASS", "FAIL"); ctx$W(vr, "source_agreement_independent_verification.csv")
  rs <- it$reference; ref <- .siw_ph_ref(ctx, refs, protocol, rs$role, rs$file); ctx$W(ref$provenance, "posthoc_reference_provenance.csv"); rstat <- .siw_ph_dep(ref)
  if (is.null(rstat)) { cmp <- siw_posthoc_compare(a, ref$table, unlist(rs$keys), unlist(rs$fields), config, rs$tolerance, "weather_source_agreement_80", rs$file, rs$expected_compared); ctx$W(cmp$summary, "source_agreement_comparison_summary.csv"); ctx$W(cmp$detail, "source_agreement_comparison_detail.csv"); rstat <- .siw_ph_cmp_status(cmp) }
  rows <- list(); for (t in it$si_concordance$w56) { r <- a[a$comparison == t$comparison & a$group == t$group & a$window_days == 56L, ]
    rows <- c(rows, list(.siw_ph_si_row("source_agreement", paste(t$comparison, t$group, "W56 n"), r$n_pairwise, t$n, 0), .siw_ph_si_row("source_agreement", paste(t$comparison, t$group, "W56 pearson"), r$pearson, t$pearson, 3),
                         .siw_ph_si_row("source_agreement", paste(t$comparison, t$group, "W56 mean bias"), r$mean_bias_era5_minus_ims, t$bias, 3), .siw_ph_si_row("source_agreement", paste(t$comparison, t$group, "W56 rmse"), r$rmse, t$rmse, 3))) }
  sic <- do.call(rbind, rows); if (ctx$synthetic) sic$agrees <- NA; ctx$W(sic, "source_agreement_si_concordance.csv")
  st <- c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else ref$status, SOURCE_AGREEMENT_REPRODUCTION_STATUS = rstat, SOURCE_AGREEMENT_VERIFICATION_STATUS = if (nrow(a) == 80L && all(vr$status == "PASS")) "PASS" else "FAIL", SOURCE_AGREEMENT_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(sic$agrees)) "PASS" else "REVIEW")
  list(statuses = st, info = paste0("SOURCE_AGREEMENT_ROWS=", nrow(a), " (2 pairs x 10 windows x all + 3 localities); paired source agreement only, no model"), tables = list(agreement = a, verification = vr, si = sic))
}

# ---- (g) exposure residual diagnostics ------------------------------------------------------------------------------------------------------------------------

.siw_common_range_fraction <- function(values, groups) {
  og <- unique(as.character(groups[!is.na(groups)])); rg <- lapply(og, function(g) { x <- values[as.character(groups) == g & is.finite(values)]; if (!length(x)) c(NA_real_, NA_real_) else range(x) })
  if (!length(rg) || any(!is.finite(unlist(rg)))) return(NA_real_); lo <- max(vapply(rg, `[[`, 0, 1L)); hi <- min(vapply(rg, `[[`, 0, 2L)); fw <- diff(range(values[is.finite(values)])); if (!is.finite(fw) || fw == 0) return(1); max(0, hi - lo) / fw
}

#' Exposure residual diagnostics (archive src/r/13_descriptive_data_quality.R exposure_overlap): UTCI, IMS RH and rainy-day count at 7, 28 and 56 days regressed on assessment day, locality, or both (27 regressions).
#' @export
siw_exposure_residuals <- function(dat) {
  if (is.null(dat$EarlyLate)) dat$EarlyLate <- siw_early_late(dat$AssessmentDay)
  pp <-data.frame(metric = c("UTCI", "IMS_relative_humidity", "IMS_rainy_day_count_ge_1_mm"), prefix = c("UTCI_AVG_", "IMS_RH_IDW_AVG_", "IMS_RAIN_DAYS_GE1MM_IDW_"), stringsAsFactors = FALSE); rows <- list()
  for (mi in seq_len(nrow(pp))) for (w in c(7L, 28L, 56L)) { e <- paste0(pp$prefix[mi], w, "D"); specs <- list(AssessmentDay = paste(e, "~ AssessmentDay"), Locality = paste(e, "~ Locality_3group"), AssessmentDay_plus_Locality = paste(e, "~ AssessmentDay + Locality_3group"))
    for (mn in names(specs)) { fit <- stats::lm(stats::as.formula(specs[[mn]]), data = dat); fs <- summary(fit); esd <- stats::sd(dat[[e]], na.rm = TRUE); crl <- .siw_common_range_fraction(dat[[e]], dat$Locality_3group)
      rows[[length(rows) + 1L]] <- data.frame(metric = pp$metric[mi], exposure = e, window_days = w, adjustment_model = mn, n = stats::nobs(fit), r_squared = fs$r.squared, residual_exposure_sd = stats::sigma(fit), percent_variance_remaining = 100 * (1 - fs$r.squared),
        residual_sd_percent_of_unadjusted = if (esd > 0) 100 * stats::sigma(fit) / esd else NA_real_, early_late_common_range_fraction = .siw_common_range_fraction(dat[[e]], dat$EarlyLate), locality_common_range_fraction = crl,
        little_independent_contrast = (fs$r.squared >= 0.80 || crl < 0.10), stringsAsFactors = FALSE) } }
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

.siw_ph_item_exposure_residuals <- function(dat, config, protocol, it, refs, ctx) {
  g <- siw_exposure_residuals(dat); ctx$W(g, "exposure_residual_diagnostics_27.csv")
  vr <- do.call(rbind, lapply(seq_len(nrow(g)), function(i) { r <- g[i, ]; y <- as.numeric(dat[[r$exposure]]); X <- switch(r$adjustment_model, AssessmentDay = cbind(1, dat$AssessmentDay), Locality = stats::model.matrix(~ Locality_3group, dat), AssessmentDay_plus_Locality = stats::model.matrix(~ AssessmentDay + Locality_3group, dat))
    k <- is.finite(y) & stats::complete.cases(X); y <- y[k]; X <- X[k, , drop = FALSE]; b <- qr.coef(qr(X), y); e <- y - X %*% b; r2 <- 1 - sum(e^2) / sum((y - mean(y))^2); s <- sqrt(sum(e^2) / (length(y) - ncol(X)))
    data.frame(metric = r$metric, window_days = r$window_days, adjustment_model = r$adjustment_model, r2_rel_diff = abs(r2 - r$r_squared) / max(abs(r2), 1e-12), sigma_rel_diff = abs(s - r$residual_exposure_sd) / max(abs(s), 1e-12), stringsAsFactors = FALSE) }))
  vr$status <- ifelse(vr$r2_rel_diff <= 1e-10 & vr$sigma_rel_diff <= 1e-10, "PASS", "FAIL"); ctx$W(vr, "exposure_residuals_independent_verification.csv")
  rs <- it$reference; ref <- .siw_ph_ref(ctx, refs, protocol, rs$role, rs$file); ctx$W(ref$provenance, "posthoc_reference_provenance.csv"); rstat <- .siw_ph_dep(ref)
  if (is.null(rstat)) { cmp <- siw_posthoc_compare(g, ref$table, unlist(rs$keys), unlist(rs$fields), config, rs$tolerance, "exposure_residuals_27", rs$file, rs$expected_compared); cat_ <- .siw_ph_cat(g, ref$table, unlist(rs$keys), unlist(rs$categorical))
    ctx$W(cmp$summary, "exposure_residuals_comparison_summary.csv"); ctx$W(cmp$detail, "exposure_residuals_comparison_detail.csv"); ctx$W(cat_, "exposure_residuals_categorical_comparison.csv"); rstat <- if (.siw_ph_cmp_status(cmp) == "PASS" && all(cat_$n_different == 0L)) "PASS" else "FAIL" }
  rows <- list(); for (t in it$si_concordance$w56) { r <- g[g$metric == t$metric & g$window_days == 56L & g$adjustment_model == "AssessmentDay_plus_Locality", ]
    rows <- c(rows, list(.siw_ph_si_row("exposure_residuals", paste(t$metric, "W56 assessment day + locality R2 (%)"), 100 * r$r_squared, t$r_squared_percent, 2), .siw_ph_si_row("exposure_residuals", paste(t$metric, "W56 residual SD"), r$residual_exposure_sd, t$residual_sd, 3))) }
  sic <- do.call(rbind, rows); if (ctx$synthetic) sic$agrees <- NA; ctx$W(sic, "exposure_residuals_si_concordance.csv")
  st <- c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else ref$status, EXPOSURE_RESIDUALS_REPRODUCTION_STATUS = rstat, EXPOSURE_RESIDUALS_VERIFICATION_STATUS = if (nrow(g) == 27L && all(vr$status == "PASS")) "PASS" else "FAIL", EXPOSURE_RESIDUALS_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(sic$agrees)) "PASS" else "REVIEW")
  list(statuses = st, info = "EXPOSURE_RESIDUALS_ROWS=27 (3 metrics x 7/28/56 days x assessment day / locality / both); exposure-only regressions, not residualisation on the full M2-L set", tables = list(residuals = g, verification = vr, si = sic))
}

# ---- (h) penalized profile likelihood -----------------------------------------------------------------------------------------------------------------------

# archive src/r/model_engine.R siecc_firth_logistic (Jeffreys-adjusted score iterations with step halving), reproduced
.siw_firth_archive <- function(x, y, maximum_iterations = 100L, tolerance = 1e-8) {
  beta <- rep(0, ncol(x)); names(beta) <- colnames(x); score_norm <- Inf
  lp <- function(cand) { pr <- pmin(pmax(stats::plogis(drop(x %*% cand)), 1e-12), 1 - 1e-12); w <- pr * (1 - pr); dt <- determinant(crossprod(x, x * w), logarithm = TRUE); if (dt$sign <= 0) return(-Inf); sum(y * log(pr) + (1 - y) * log1p(-pr)) + 0.5 * as.numeric(dt$modulus) }
  prev <- lp(beta)
  for (iteration in seq_len(maximum_iterations)) { pr <- pmin(pmax(stats::plogis(drop(x %*% beta)), 1e-10), 1 - 1e-10); w <- pr * (1 - pr); inv <- tryCatch(solve(crossprod(x, x * w)), error = function(e) NULL); if (is.null(inv) || any(!is.finite(inv))) break
    h <- rowSums((x %*% inv) * x) * w; sc <- crossprod(x, y - pr + h * (0.5 - pr)); score_norm <- max(abs(sc)); step <- drop(inv %*% sc); if (any(!is.finite(step))) break
    f <- 1; cand <- beta + step; cp <- lp(cand); while ((!is.finite(cp) || cp < prev) && f > 2^-20) { f <- f / 2; cand <- beta + f * step; cp <- lp(cand) }
    ch <- max(abs(cand - beta)); beta <- cand; prev <- cp; if (is.finite(ch) && ch <= tolerance && score_norm <= sqrt(tolerance)) break }
  list(coefficients = beta)
}
.siw_firth_pll <- function(beta, x, y) { pr <- pmin(pmax(stats::plogis(drop(x %*% beta)), 1e-12), 1 - 1e-12); w <- pr * (1 - pr); dt <- determinant(crossprod(x, x * w), logarithm = TRUE); if (dt$sign <= 0 || !is.finite(as.numeric(dt$modulus))) return(-Inf); sum(y * log(pr) + (1 - y) * log1p(-pr)) + 0.5 * as.numeric(dt$modulus) }
.siw_firth_optim <- function(x, y) {
  init <- .siw_firth_archive(x, y, maximum_iterations = 500L, tolerance = 1e-10); fit <- stats::optim(init$coefficients, function(b) -.siw_firth_pll(b, x, y), method = "BFGS", control = list(maxit = 3000L, reltol = 1e-11))
  b <- fit$par; pr <- pmin(pmax(stats::plogis(drop(x %*% b)), 1e-12), 1 - 1e-12); V <- tryCatch(solve(crossprod(x, x * (pr * (1 - pr)))), error = function(e) matrix(NA_real_, ncol(x), ncol(x)))
  list(coefficients = b, covariance = V, penalized_log_likelihood = -fit$value, convergence_code = fit$convergence, converged = fit$convergence == 0L && all(is.finite(b)) && all(is.finite(V)))
}
.siw_firth_constrained <- function(theta, cv, x, y, start) {
  pv <- which.max(abs(cv)); if (!length(pv) || abs(cv[[pv]]) < 1e-12) return(list(loglik = -Inf)); ot <- setdiff(seq_len(ncol(x)), pv)
  rec <- function(g) { b <- numeric(ncol(x)); b[ot] <- g; b[[pv]] <- (theta - sum(cv[ot] * g)) / cv[[pv]]; b }
  fit <- stats::optim(start[ot], function(g) { v <- .siw_firth_pll(rec(g), x, y); if (is.finite(v)) -v else .Machine$double.xmax / 100 }, method = "BFGS", control = list(maxit = 2000L, reltol = 1e-10))
  list(loglik = -fit$value, coefficients = rec(fit$par), convergence = fit$convergence)
}
.siw_firth_profile <- function(firth, cv, x, y) {
  est <- sum(cv * firth$coefficients); va <- as.numeric(t(cv) %*% firth$covariance %*% cv); se <- if (is.finite(va) && va > 0) sqrt(va) else NA_real_; td <- stats::qchisq(0.95, df = 1L)
  obj <- function(th) 2 * (firth$penalized_log_likelihood - .siw_firth_constrained(th, cv, x, y, firth$coefficients)$loglik) - td
  side <- function(dir) { step <- if (is.finite(se) && se > 0) max(0.05, 1.25 * se) else 0.5; iv <- obj(est); if (!is.finite(iv)) return(NA_real_); out <- est + dir * step; ov <- obj(out); k <- 0L
    while ((!is.finite(ov) || ov <= 0) && k < 18L) { step <- step * 1.75; out <- est + dir * step; ov <- obj(out); k <- k + 1L }; if (!is.finite(ov) || ov <= 0) return(NA_real_)
    tryCatch(stats::uniroot(obj, interval = sort(c(est, out)), tol = 1e-7, maxiter = 100L)$root, error = function(e) NA_real_) }
  lo <- side(-1); hi <- side(1); list(estimate = est, standard_error = se, lower = lo, upper = hi, status = if (is.finite(lo) && is.finite(hi)) "COMPLETE_PENALISED_PROFILE_LIKELIHOOD" else "NOT_ESTIMABLE_PROFILE_ROOT_NOT_BRACKETED", objective = obj)
}

#' Penalized profile-likelihood inference for one cumulative integration model (archive src/r/25_binary_small_sample_completion.R, PRIMARY_CUMULATIVE branch). Base R only.
#' @export
siw_profile_likelihood_one <- function(dat, config, metric, window) {
  sp <- config$metrics[config$metrics$metric == metric, ]; e <- siw_cum_name(sp$cumulative_prefix, window); covs <- config$tiers[[config$primary_tier]]
  md <- droplevels(dat[stats::complete.cases(dat[, c("Int_dummy", e, covs), drop = FALSE]), , drop = FALSE]); x <- stats::model.matrix(stats::reformulate(c(e, covs), response = "Int_dummy"), data = md); y <- as.numeric(md$Int_dummy)
  ei <- match(e, colnames(x)); cv <- rep(0, ncol(x)); cv[[ei]] <- 1; fi <- .siw_firth_optim(x, y); pr <- .siw_firth_profile(fi, cv, x, y)
  nl <- .siw_firth_constrained(0, cv, x, y, fi$coefficients)$loglik; lrt <- 2 * (fi$penalized_log_likelihood - nl); lp <- if (is.finite(lrt) && lrt >= 0) stats::pchisq(lrt, 1L, lower.tail = FALSE) else NA_real_
  ok <- fi$converged && pr$status == "COMPLETE_PENALISED_PROFILE_LIKELIHOOD" && is.finite(lp)
  row <- data.frame(model_id = siw_cum_id("INT", sp$code, window, "M2_L"), model_class = "PRIMARY_CUMULATIVE", outcome = "Int_dummy", metric = metric, window = as.integer(window), n = nrow(x), event_count = sum(y == 1), non_event_count = sum(y == 0), parameter_count = ncol(x),
    firth_converged = fi$converged, firth_convergence_code = fi$convergence_code, coefficient = pr$estimate, odds_ratio = exp(pr$estimate), penalised_information_standard_error = pr$standard_error, profile_ci_lower = pr$lower, profile_ci_upper = pr$upper,
    profile_or_ci_lower = exp(pr$lower), profile_or_ci_upper = exp(pr$upper), profile_interval_method = "PENALISED_PROFILE_LIKELIHOOD_CHI_SQUARE_1DF", profile_status = pr$status, profile_lrt_statistic = lrt, profile_lrt_df = 1L, profile_lrt_p = lp,
    profile_lrt_method = "PENALISED_LIKELIHOOD_RATIO_EXPOSURE_COEFFICIENT", status = if (ok) "COMPLETED" else "NOT_ESTIMABLE", stringsAsFactors = FALSE)
  # verification: the interval ends solve the declared profile equation; the LRT p is the chi-square(1) tail of the statistic
  # profile duality: zero lies inside the 95% profile interval exactly when the 1-df penalized LRT at zero is below the chi-square(1) 0.95 quantile
  ver <- data.frame(model_id = row$model_id, objective_at_lower = if (is.finite(pr$lower)) pr$objective(pr$lower) else NA_real_, objective_at_upper = if (is.finite(pr$upper)) pr$objective(pr$upper) else NA_real_, interval_brackets_estimate = isTRUE(pr$lower < pr$estimate && pr$estimate < pr$upper),
                    lrt_interval_consistent = isTRUE((lrt < stats::qchisq(0.95, 1L)) == (pr$lower <= 0 && 0 <= pr$upper)), stringsAsFactors = FALSE)
  list(row = row, verification = ver)
}

.siw_ph_item_profile_likelihood <- function(dat, config, protocol, it, refs, ctx) {
  res <- lapply(config$representative_metrics, function(m) lapply(config$windows, function(w) siw_profile_likelihood_one(dat, config, m, w))); res <- unlist(res, recursive = FALSE)
  g <- do.call(rbind, lapply(res, `[[`, "row")); vr <- do.call(rbind, lapply(res, `[[`, "verification")); rownames(g) <- NULL; ctx$W(g, "profile_likelihood_integration_30.csv")
  # the reproduced (archived) optimiser can stall in flat, clamped regions near separation; an interval end that does not solve the profile equation is a limitation of the original
  # method (REVIEW, reported per model), not a reproduction failure; structural problems (wrong count, non-finite values in a COMPLETED row) are FAIL
  vr$status <- ifelse(is.finite(vr$objective_at_lower) & is.finite(vr$objective_at_upper) & abs(vr$objective_at_lower) <= 1e-3 & abs(vr$objective_at_upper) <= 1e-3 & vr$interval_brackets_estimate & vr$lrt_interval_consistent, "PASS", "REVIEW_ORIGINAL_OPTIMISER_LIMITATION"); ctx$W(vr, "profile_likelihood_independent_verification.csv")
  structural_ok <- nrow(g) == 30L && all(is.finite(as.matrix(g[g$status == "COMPLETED", c("coefficient", "profile_ci_lower", "profile_ci_upper", "profile_lrt_statistic", "profile_lrt_p")])))
  rs <- it$reference; ref <- .siw_ph_ref(ctx, refs, protocol, rs$role, rs$file, rs$row_filter); ctx$W(ref$provenance, "posthoc_reference_provenance.csv"); rstat <- .siw_ph_dep(ref)
  if (is.null(rstat)) { rt <- ref$table; rt$window <- suppressWarnings(as.integer(sub(".*cumulative window ([0-9]+) days.*", "\\1", rt$window_or_lag_specification)))
    cmp <- siw_posthoc_compare(g, rt, unlist(rs$keys), unlist(rs$fields), config, rs$tolerance, "profile_likelihood_30", rs$file, rs$expected_compared); cat_ <- .siw_ph_cat(g, rt, unlist(rs$keys), unlist(rs$categorical))
    ctx$W(cmp$summary, "profile_likelihood_comparison_summary.csv"); ctx$W(cmp$detail, "profile_likelihood_comparison_detail.csv"); ctx$W(cat_, "profile_likelihood_categorical_comparison.csv"); rstat <- if (.siw_ph_cmp_status(cmp) == "PASS" && all(cat_$n_different == 0L)) "PASS" else "FAIL" }
  si <- it$si_concordance; rows <- list(.siw_ph_si_row("profile_likelihood", "minimum profile LRT p (30 models)", min(g$profile_lrt_p, na.rm = TRUE), si$minimum_profile_p, 3), .siw_ph_si_row("profile_likelihood", "profile intervals including zero", sum(g$profile_ci_lower <= 0 & g$profile_ci_upper >= 0, na.rm = TRUE), if (isTRUE(si$all_intervals_include_zero)) 30 else NA, 0))
  for (t in si$w56) { r <- g[g$metric == t$metric & g$window == 56L, ]; ci <- unlist(t$ci); v <- c(r$coefficient, r$profile_ci_lower, r$profile_ci_upper, r$profile_lrt_p); tg <- c(t$coefficient, ci, t$p)
    for (k in 1:4) rows[[length(rows) + 1L]] <- .siw_ph_si_row("profile_likelihood", paste(t$metric, "W56", c("coefficient", "profile_ci_lower", "profile_ci_upper", "profile_lrt_p")[k]), v[k], tg[k], 3) }
  sic <- do.call(rbind, rows); if (ctx$synthetic) sic$agrees <- NA; ctx$W(sic, "profile_likelihood_si_concordance.csv")
  st <- c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else ref$status, PROFILE_LIKELIHOOD_REPRODUCTION_STATUS = rstat, PROFILE_LIKELIHOOD_VERIFICATION_STATUS = if (!structural_ok) "FAIL" else if (all(vr$status == "PASS")) "PASS" else "REVIEW", PROFILE_LIKELIHOOD_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(sic$agrees)) "PASS" else "REVIEW")
  list(statuses = st, info = c(paste0("PROFILE_LIKELIHOOD_COMPLETED=", sum(g$status == "COMPLETED"), " of ", nrow(g), "; minimum profile p ", signif(min(g$profile_lrt_p, na.rm = TRUE), 4)), "PROFILE_LIKELIHOOD_ENGINE=base R (stats::optim BFGS, stats::uniroot); logistf / brglm2 are not used",
                               paste0("PROFILE_LIKELIHOOD_EQUATION_NOT_MET=", sum(vr$status != "PASS"), " of ", nrow(vr), " models (interval end not solving the profile equation: original optimiser limitation, reproduced, not corrected; see profile_likelihood_independent_verification.csv)")), tables = list(profile = g, verification = vr, si = sic))
}
