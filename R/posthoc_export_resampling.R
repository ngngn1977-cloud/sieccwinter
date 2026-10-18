# Post hoc steps 1-3: the aggregate exposure export (POST_HOC), the selected deletion refits (c, SENSITIVITY) and the archived L01-L59 DLM bootstraps (d, SENSITIVITY).
# Originals: 14 Sep 2026 audit definitions and archive stage 13 (export); WINTER_SOURCE R/060_xy.R and R/100_resampling.R (deletion); R/100_resampling.R and R/080_dlm.R (bootstrap).

.siw_ph_hist <- function(dat, prefix, L) { H <- as.matrix(dat[, siw_daily_names(prefix, L), drop = FALSE]); storage.mode(H) <- "double"; H }

# reference read with the synthetic rule: for the shipped synthetic CSV no study reference applies
.siw_ph_ref <- function(ctx, refs, protocol, role, file, row_filter = NULL) {
  if (ctx$synthetic) return(list(table = NULL, provenance = data.frame(file = file, item = "reference", value = "not applicable: shipped synthetic input", ok = TRUE, stringsAsFactors = FALSE), ok = FALSE, status = SIW_PH_NA_SYN))
  siw_posthoc_reference(refs[[role]], protocol, file, row_filter)
}
.siw_ph_dep <- function(ref) if (identical(ref$status, "PASS")) NULL else ref$status        # status of a comparison whose reference is not usable

#' UTCI category of daily means (Broede et al. 2012 bands as declared in the post hoc protocol; 9 belongs to no thermal stress).
#' @export
siw_utci_band <- function(u) { u <- as.numeric(u); out <- ifelse(u > 26, "any_heat_stress", ifelse(u >= 9, "no_thermal_stress", ifelse(u >= 0, "slight_cold_stress", ifelse(u >= -13, "moderate_cold_stress", "strong_cold_or_colder")))); out[!is.finite(u)] <- NA_character_; out }
SIW_UTCI_BANDS <- c("no_thermal_stress", "slight_cold_stress", "moderate_cold_stress", "strong_cold_or_colder", "any_heat_stress")

#' Outcome distribution summary as in archive stage 13 (n, missing, mean, SD, median, IQR, minimum, maximum, floor and ceiling counts). Aggregate only.
#' @export
siw_outcome_distribution <- function(dat, outcomes) {
  do.call(rbind, lapply(outcomes, function(o) { v <- as.numeric(dat[[o]]); x <- v[is.finite(v)]
    data.frame(outcome = o, n = length(x), missing = length(v) - length(x), mean = mean(x), sd = stats::sd(x), median = stats::median(x), iqr = stats::IQR(x), minimum = min(x), maximum = max(x),
               floor_count = sum(v == min(v, na.rm = TRUE), na.rm = TRUE), ceiling_count = sum(v == max(v, na.rm = TRUE), na.rm = TRUE), stringsAsFactors = FALSE) }))
}

#' Aggregate exposure export (L01-L56): participant-days, UTCI categories, rainfall days, exposure date span, winter / spring UTCI, minimum-UCLA count. One row per quantity; aggregates only.
#' @export
siw_exposure_aggregates <- function(dat, config, protocol = siw_posthoc_protocol()) {
  it <- siw_posthoc_item(protocol, "exposure_export"); se <- it$definitions$season; anchor <- .siw_ph_anchor(protocol); L <- 56L; n <- nrow(dat)
  U <- .siw_ph_hist(dat, "UTCI", L); Rn <- .siw_ph_hist(dat, "IMS_IDW_RAIN_TOTAL", L); off <- siw_lag_date(dat$AssessmentDay, seq_len(L), protocol, as_offset = TRUE)
  wl <- as.integer(as.Date(se$winter_last_date) - anchor); sf <- as.integer(as.Date(se$spring_first_date) - anchor)
  fu <- is.finite(U); band <- siw_utci_band(U[fu]); nu <- sum(fu); fr <- is.finite(Rn); rv <- Rn[fr]; nr <- sum(fr)
  rows <- list(); add <- function(q, v, unit, den, def, txt = "") rows[[length(rows) + 1L]] <<- data.frame(quantity = q, value = as.numeric(v), value_text = txt, unit = unit, denominator = as.character(den), definition = def, stringsAsFactors = FALSE)
  add("participants", n, "participants", "", "rows of the canonical CSV"); add("participant_days_L01_L56", n * L, "participant-days", "", it$definitions$participant_days); add("utci_valid_days", nu, "participant-days", "participant_days_L01_L56", "finite daily UTCI")
  for (b in SIW_UTCI_BANDS) { k <- sum(band == b); add(paste0("utci_", b, "_days"), k, "participant-days", "utci_valid_days", it$definitions$utci_bands_degC[[b]]); add(paste0("utci_", b, "_percent"), 100 * k / nu, "percent", "utci_valid_days", it$definitions$utci_bands_degC[[b]]) }
  add("rain_valid_days", nr, "participant-days", "participant_days_L01_L56", it$definitions$rain_mm$valid_day); add("rain_missing_days", n * L - nr, "participant-days", "participant_days_L01_L56", "missing daily total (kept missing)")
  for (r in list(c("rainy_ge1mm", "rainy"), c("dry_0mm", "dry"), c("between_0_and_1mm", "between_0_and_1"))) { k <- switch(r[2], rainy = sum(rv >= 1), dry = sum(rv == 0), between_0_and_1 = sum(rv > 0 & rv < 1)); add(paste0("rain_", r[1], "_days"), k, "participant-days", "rain_valid_days", it$definitions$rain_mm[[r[2]]]); add(paste0("rain_", r[1], "_percent"), 100 * k / nr, "percent", "rain_valid_days", it$definitions$rain_mm[[r[2]]]) }
  add("exposure_date_span_first", min(off), "days since anchor", "", it$definitions$date_span, format(anchor + min(off))); add("exposure_date_span_last", max(off), "days since anchor", "", it$definitions$date_span, format(anchor + max(off)))
  add("exposure_date_span_days", max(off) - min(off) + 1L, "calendar days", "", "last minus first lag date, plus one")
  for (s in c("winter", "spring")) { m <- fu & if (s == "winter") off <= wl else off >= sf; uu <- U[m]
    add(paste0(s, "_participant_days"), sum(m), "participant-days", "utci_valid_days", se[[s]]); add(paste0(s, "_mean_utci"), mean(uu), "degC", paste0(s, "_participant_days"), paste0("mean daily UTCI over ", se[[s]]))
    add(paste0(s, "_no_thermal_stress_percent"), 100 * mean(siw_utci_band(uu) == "no_thermal_stress"), "percent", paste0(s, "_participant_days"), "share of the season's participant-days with no thermal stress") }
  y <- as.numeric(dat$UCLA_Lonliness_SCORE); mn <- min(y, na.rm = TRUE)
  add("ucla_minimum_score", mn, "score", "", "observed minimum UCLA score"); add("ucla_participants_at_minimum", sum(y == mn, na.rm = TRUE), "participants", "ucla_participants_nonmissing", it$definitions$minimum_ucla); add("ucla_participants_nonmissing", sum(is.finite(y)), "participants", "", "participants with a UCLA score")
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

# independent recomputation of the export (calendar dates as Date objects, cut() for the bands, table() counts)
.siw_ph_export_verify <- function(dat, config, protocol, ex) {
  it <- siw_posthoc_item(protocol, "exposure_export"); anchor <- .siw_ph_anchor(protocol); L <- 56L; U <- .siw_ph_hist(dat, "UTCI", L); Rn <- .siw_ph_hist(dat, "IMS_IDW_RAIN_TOTAL", L)
  dates <- matrix(anchor + rep(as.integer(dat$AssessmentDay), L) - rep(seq_len(L), each = nrow(dat)), nrow(dat)); dstr <- format(as.Date(dates, origin = "1970-01-01"))
  u <- as.vector(U); ok <- is.finite(u); cb <- cut(u[ok], breaks = c(-Inf, -13, 0, 9, Inf), right = FALSE, labels = c("strong_cold_or_colder", "moderate_cold_stress", "slight_cold_stress", "upper")); lab <- as.character(cb); lab[lab == "upper"] <- ifelse(u[ok][lab == "upper"] > 26, "any_heat_stress", "no_thermal_stress")
  tb <- table(factor(lab, levels = SIW_UTCI_BANDS)); r <- as.vector(Rn); r <- r[is.finite(r)]; win <- dstr <= it$definitions$season$winter_last_date; spr <- dstr >= it$definitions$season$spring_first_date
  ind <- c(participant_days_L01_L56 = length(u), utci_valid_days = sum(ok), stats::setNames(as.numeric(tb), paste0("utci_", names(tb), "_days")), rain_valid_days = length(r), rain_rainy_ge1mm_days = sum(r >= 1), rain_dry_0mm_days = sum(r == 0), rain_between_0_and_1mm_days = length(r) - sum(r >= 1) - sum(r == 0),
           winter_mean_utci = mean(u[ok & as.vector(win)]), spring_mean_utci = mean(u[ok & as.vector(spr)]), winter_participant_days = sum(ok & as.vector(win)), spring_participant_days = sum(ok & as.vector(spr)),
           ucla_participants_at_minimum = as.numeric(table(dat$UCLA_Lonliness_SCORE)[1L]), exposure_date_span_days = as.numeric(diff(range(as.Date(dstr)))) + 1)
  pv <- ex$value[match(names(ind), ex$quantity)]; d <- abs(pv - ind)
  data.frame(quantity = names(ind), package_value = pv, independent_value = unname(ind), abs_diff = d, status = ifelse(is.finite(d) & d <= 1e-12 * pmax(1, abs(ind)), "PASS", "FAIL"), stringsAsFactors = FALSE)
}

.siw_ph_item_exposure_export <- function(dat, config, protocol, it, refs, ctx) {
  ex <- siw_exposure_aggregates(dat, config, protocol); ctx$W(ex, "exposure_aggregate_export.csv")
  ver <- .siw_ph_export_verify(dat, config, protocol, ex); ctx$W(ver, "exposure_export_independent_verification.csv")
  acc <- do.call(rbind, lapply(it$acceptance, function(a) .siw_ph_si_row("exposure_export", a$quantity, ex$value[match(a$quantity, ex$quantity)], a$value, a$decimals, "manuscript value")))
  if (ctx$synthetic) acc$agrees <- NA; ctx$W(acc, "exposure_export_acceptance.csv")
  ac <- it$archive_comparison; od <- siw_outcome_distribution(dat, unlist(ac$outcomes)); ctx$W(od, "outcome_distribution_summary.csv")
  ref <- .siw_ph_ref(ctx, refs, protocol, ac$role, ac$file); ctx$W(ref$provenance, "posthoc_reference_provenance.csv")
  mstat <- .siw_ph_dep(ref)
  if (is.null(mstat)) { rr <- ref$table[ref$table$outcome %in% unlist(ac$outcomes), , drop = FALSE]; cmp <- siw_posthoc_compare(od, rr, unlist(ac$keys), unlist(ac$fields), config, ac$tolerance, "outcome_distribution", ac$file, length(unlist(ac$outcomes))); ctx$W(cmp$summary, "outcome_distribution_comparison_summary.csv"); ctx$W(cmp$detail, "outcome_distribution_comparison_detail.csv"); mstat <- .siw_ph_cmp_status(cmp) }
  privacy <- !any(c("participant_code", "assessment_day") %in% names(ex)) && nrow(ex) < 100L
  st <- c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (ref$status == "NOT_TESTED") "NOT_TESTED" else ref$status, EXPORT_STATUS = if (privacy) "PASS" else "FAIL",
          EXPORT_VERIFICATION_STATUS = if (all(ver$status == "PASS")) "PASS" else "FAIL", EXPORT_ACCEPTANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(acc$agrees)) "PASS" else "FAIL", MIN_UCLA_ARCHIVE_REPRODUCTION_STATUS = mstat)
  g <- function(q) ex$value[ex$quantity == q]; gt <- function(q) ex$value_text[ex$quantity == q]
  info <- c(paste0("EXPORT_PARTICIPANT_DAYS=", g("participant_days_L01_L56"), "; UTCI no/slight/moderate/strong/heat=", paste(vapply(SIW_UTCI_BANDS, function(b) g(paste0("utci_", b, "_days")), 0), collapse = "/")),
            paste0("EXPORT_RAIN_VALID_DAYS=", g("rain_valid_days"), "; rainy/dry/0-1mm %=", paste(sprintf("%.1f", c(g("rain_rainy_ge1mm_percent"), g("rain_dry_0mm_percent"), g("rain_between_0_and_1mm_percent"))), collapse = "/")),
            paste0("EXPORT_DATE_SPAN=", gt("exposure_date_span_first"), " to ", gt("exposure_date_span_last")), paste0("EXPORT_WINTER_SPRING_MEAN_UTCI=", sprintf("%.2f/%.2f", g("winter_mean_utci"), g("spring_mean_utci")), "; % no thermal stress=", sprintf("%.1f/%.1f", g("winter_no_thermal_stress_percent"), g("spring_no_thermal_stress_percent"))),
            paste0("EXPORT_UCLA_AT_MINIMUM=", g("ucla_participants_at_minimum"), " of ", g("ucla_participants_nonmissing")), paste0("EXPORT_ACCEPTANCE_AGREEING=", if (ctx$synthetic) "not applicable (synthetic)" else paste0(sum(acc$agrees), " of ", nrow(acc))),
            "EXPORT_DEFINITIONS=declared post hoc (14 Sep 2026 audit definitions; author decision 13 Oct 2026); calendar anchor assessment_day 0 = 2026-01-29")
  list(statuses = st, info = info, tables = list(export = ex, verification = ver, acceptance = acc, outcome_distribution = od))
}

# ---- (c) selected deletion refits -------------------------------------------------------------------------------------------------------------------------

#' The six previously selected cumulative models (WINTER_SOURCE R/060_xy.R siecc_select_strong_xy): per outcome and domain, the maximum |Spearman rho| (continuous outcomes)
#' or |point-biserial r| (binary outcome) over the 8 metrics x 10 windows of that domain; the first maximum in outcome-metric-window order.
#' @export
siw_selected_xy <- function(dat, config, protocol = siw_posthoc_protocol()) {
  dom <- unlist(siw_posthoc_item(protocol, "deletion")$metric_domain); rows <- list()
  for (oi in seq_len(nrow(config$outcomes))) { o <- config$outcomes[oi, ]; y <- as.numeric(dat[[o$outcome]])
    xy <- do.call(rbind, lapply(seq_len(nrow(config$metrics)), function(mi) { sp <- config$metrics[mi, ]; do.call(rbind, lapply(config$windows, function(w) { f <- siw_cum_name(sp$cumulative_prefix, w); cs <- siw_correlation_set(as.numeric(dat[[f]]), y)
      data.frame(metric = sp$metric, window = as.integer(w), exposure = f, pearson = cs[["pearson_r"]], spearman = cs[["spearman_rho"]], stringsAsFactors = FALSE) })) }))
    for (d in c("thermal", "moisture")) { s <- xy[dom[xy$metric] == d, , drop = FALSE]; rk <- if (o$family == "binomial") abs(s$pearson) else abs(s$spearman); rk[!is.finite(rk)] <- -Inf; k <- which.max(rk)
      rows[[length(rows) + 1L]] <- data.frame(outcome = o$outcome, outcome_family = o$family, domain = d, metric = s$metric[k], window = s$window[k], exposure = s$exposure[k], selection_statistic = if (o$family == "binomial") "abs_point_biserial_r" else "abs_spearman_rho", selection_value = rk[k], candidates = nrow(s), stringsAsFactors = FALSE) } }
  do.call(rbind, rows)
}

# WINTER_SOURCE R/100_resampling.R siecc_simple_model_coefficient, reproduced: complete cases of outcome, exposure and covariates, droplevels, stats::lm or stats::glm (binomial, default control)
.siw_v14_simple_coef <- function(d0, outcome, family, exposure, covs) {
  v <- unique(c(outcome, exposure, covs)); cc <- stats::complete.cases(d0[, v, drop = FALSE]); d <- droplevels(d0[cc, v, drop = FALSE]); f <- stats::reformulate(c(exposure, covs), outcome)
  if (nrow(d) <= length(covs) + 3L) return(list(coef = NA_real_, d = d, f = f))
  fit <- if (family == "gaussian") tryCatch(stats::lm(f, data = d), error = function(e) NULL) else tryCatch(suppressWarnings(stats::glm(f, data = d, family = stats::binomial())), error = function(e) NULL)
  list(coef = if (is.null(fit) || !(exposure %in% names(stats::coef(fit)))) NA_real_ else as.numeric(stats::coef(fit)[[exposure]]), d = d, f = f)
}

#' The 528 selected deletion refits: 6 selected models x (each participant deleted; each assessment date deleted). Coefficients by the original route; n, SE, 95% interval and convergence
#' reconstructed with the final cumulative estimator (HC3 with residual-df t; model-based Wald for the binary outcome). Deletions are keyed by position (P<row>) or assessment day (D<day>).
#' @export
siw_deletion_refits <- function(dat, config, selected) {
  covs <- config$tiers[[config$primary_tier]]; rows <- list()
  for (i in seq_len(nrow(selected))) { s <- selected[i, ]
    for (mode in c("participant", "date")) {
      levs <- if (mode == "participant") seq_len(nrow(dat)) else unique(as.integer(dat$AssessmentDay))
      for (lev in levs) { keep <- if (mode == "participant") seq_len(nrow(dat)) != lev else as.integer(dat$AssessmentDay) != lev
        r <- .siw_v14_simple_coef(dat[keep, , drop = FALSE], s$outcome, s$outcome_family, s$exposure, covs); X <- stats::model.matrix(r$f, r$d); y <- as.numeric(r$d[[s$outcome]])
        st <- if (s$outcome_family == "gaussian") siw_fit_gaussian(X, y, 2L) else siw_fit_logistic(X, y, 2L, epsilon = config$glm$cumulative_epsilon, maxit = config$glm$cumulative_maxit)
        g <- function(nm) { v <- st[[nm]]; if (is.null(v)) NA_real_ else as.numeric(v) }; se <- if (s$outcome_family == "gaussian") g("robust_hc3_se") else g("model_based_se")
        rows[[length(rows) + 1L]] <- data.frame(outcome = s$outcome, outcome_family = s$outcome_family, metric = s$metric, window = as.integer(s$window), mode = mode, deletion_key = paste0(if (mode == "participant") "P" else "D", lev),
          n = nrow(r$d), coefficient = r$coef, final_coefficient = g("coefficient"), standard_error = se, ci_lower = g("ci_lower"), ci_upper = g("ci_upper"), nominal_p = g("nominal_p"),
          convergence = isTRUE(st$convergence), separation_status = st$separation_status %||% "", status = if (isTRUE(st$estimable)) "COMPLETED" else "NOT_ESTIMABLE", stringsAsFactors = FALSE) } } }
  out <- do.call(rbind, rows); out$interval_includes_zero <- out$ci_lower <= 0 & out$ci_upper >= 0; rownames(out) <- NULL; out
}

.siw_ph_item_deletion <- function(dat, config, protocol, it, refs, ctx) {
  sel <- siw_selected_xy(dat, config, protocol); ctx$W(sel, "selected_models.csv"); exp_sel <- do.call(rbind, lapply(it$expected_selection, as.data.frame, stringsAsFactors = FALSE))
  ks <- function(d) sort(paste(d$outcome, d$metric, d$window)); sel_ok <- identical(ks(sel), ks(exp_sel))
  del <- siw_deletion_refits(dat, config, sel); ctx$W(del, "selected_deletion_refits_with_intervals.csv")
  tol <- config$tolerances; dv <- abs(del$coefficient - del$final_coefficient); ver_ok <- all(is.finite(dv) & dv <= tol$cumulative_abs + tol$cumulative_rel * abs(del$coefficient))
  ctx$W(data.frame(check = "original-route coefficient equals the final-estimator coefficient (528 fits)", max_abs_diff = max(dv), status = if (ver_ok) "PASS" else "FAIL", stringsAsFactors = FALSE), "deletion_independent_verification.csv")
  rs <- it$reference; ref <- .siw_ph_ref(ctx, refs, protocol, rs$role, rs$file); ctx$W(ref$provenance, "posthoc_reference_provenance.csv"); rstat <- .siw_ph_dep(ref)
  if (is.null(rstat)) { r <- ref$table; anchor <- .siw_ph_anchor(protocol); key <- character(nrow(r))
    for (j in seq_len(nrow(r))) { om <- trimws(as.character(r$omitted[j]))
      if (identical(r$mode[j], "participant")) { k <- match(om, trimws(ctx$raw_participant_code)); key[j] <- if (is.na(k)) paste0("P_UNMATCHED_", j) else paste0("P", k) }
      else { d <- suppressWarnings(as.integer(as.Date(om, format = "%Y-%m-%d") - anchor)); key[j] <- if (is.na(d)) paste0("D_UNPARSEABLE_", j) else paste0("D", d) } }
    rr <- data.frame(outcome = r$outcome, metric = r$metric, window = r$window, mode = r$mode, deletion_key = key, coefficient = r$coefficient, stringsAsFactors = FALSE)       # identifiers dropped here; never written
    cmp <- siw_posthoc_compare(del, rr, c("outcome", "metric", "window", "mode", "deletion_key"), "coefficient", config, rs$tolerance, "selected_deletion_528", rs$file, rs$expected_compared)
    ctx$W(cmp$summary, "deletion_comparison_summary.csv"); ctx$W(cmp$detail, "deletion_comparison_detail.csv"); rstat <- .siw_ph_cmp_status(cmp) }
  si <- it$si_concordance; srows <- list()
  for (rg in si$ranges) { m <- del$outcome == rg$outcome & del$metric == rg$metric & del$window == rg$window
    for (md in c("participant", "date")) { v <- del$coefficient[m & del$mode == md]; tgt <- unlist(rg[[md]]); srows[[length(srows) + 1L]] <- .siw_ph_si_row("deletion", paste(rg$outcome, rg$metric, rg$window, md, "minimum"), if (length(v)) min(v) else NA, tgt[1], 3); srows[[length(srows) + 1L]] <- .siw_ph_si_row("deletion", paste(rg$outcome, rg$metric, rg$window, md, "maximum"), if (length(v)) max(v) else NA, tgt[2], 3) } }
  sm <- si$statements; nd <- del$n[del$mode == "date"]
  srows[[length(srows) + 1L]] <- .siw_ph_si_row("deletion", "participant deletion n (all equal to)", if (length(unique(del$n[del$mode == "participant"])) == 1L) unique(del$n[del$mode == "participant"]) else NA, sm$participant_n, 0)
  srows[[length(srows) + 1L]] <- .siw_ph_si_row("deletion", "date deletion n minimum", min(nd), unlist(sm$date_n_range)[1], 0); srows[[length(srows) + 1L]] <- .siw_ph_si_row("deletion", "date deletion n maximum", max(nd), unlist(sm$date_n_range)[2], 0)
  srows[[length(srows) + 1L]] <- .siw_ph_si_row("deletion", "refits converged", sum(del$convergence & del$status == "COMPLETED"), sm$refits_converged, 0)
  srows[[length(srows) + 1L]] <- .siw_ph_si_row("deletion", "refits whose 95% interval includes zero", sum(del$interval_includes_zero, na.rm = TRUE), if (isTRUE(sm$all_intervals_include_zero)) nrow(del) else NA, 0)
  sic <- do.call(rbind, srows); if (ctx$synthetic) sic$agrees <- NA; ctx$W(sic, "deletion_si_concordance.csv")
  st <- c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else ref$status, DELETION_SELECTION_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (sel_ok) "PASS" else "FAIL", DELETION_REPRODUCTION_STATUS = rstat,
          DELETION_VERIFICATION_STATUS = if (ver_ok && nrow(del) == nrow(sel) * (nrow(dat) + length(unique(dat$AssessmentDay)))) "PASS" else "FAIL", DELETION_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(sic$agrees)) "PASS" else "REVIEW")
  info <- c(paste0("DELETION_SELECTED=", paste(paste0(sel$outcome, "/", sel$metric, "/W", sel$window), collapse = "; ")), paste0("DELETION_REFITS=", nrow(del), " (", sum(del$mode == "participant"), " participant, ", sum(del$mode == "date"), " date); converged ", sum(del$convergence), "; intervals including zero ", sum(del$interval_includes_zero, na.rm = TRUE)),
            paste0("DELETION_SI_AGREEING=", if (ctx$synthetic) "not applicable (synthetic)" else paste0(sum(sic$agrees), " of ", nrow(sic))))
  list(statuses = st, info = info, tables = list(selected = sel, deletion = del, si = sic))
}

# ---- (d) archived L01-L59 DLM bootstrap ---------------------------------------------------------------------------------------------------------------------

# WINTER_SOURCE R/080_dlm.R siecc_fit_dlm (L01-L59, df 3, no simultaneous draws), reproduced for the cumulative effect only: complete cases, droplevels, rank / n / outcome-variation gates,
# lm.fit with the V1.4 HC3 (stops on rank deficiency) or glm.fit (epsilon 1e-10, maxit 200) with model-based covariance; NA unless the basis coefficients and covariance are finite.
.siw_v14_hc3 <- function(x, e) { if (qr(x)$rank != ncol(x)) stop("HC3_RANK_DEFICIENT"); xi <- solve(crossprod(x)); h <- rowSums((x %*% xi) * x); xi %*% crossprod(x, x * (e^2 / pmax(1e-12, (1 - h)^2))) %*% xi }
.siw_v14_dlm_cum <- function(d0, outcome, family, lag_names, covs, B, lag_df = 3L) {
  v <- unique(c(outcome, covs, lag_names)); cc <- stats::complete.cases(d0[, v, drop = FALSE]); d <- droplevels(d0[cc, v, drop = FALSE]); H <- as.matrix(d[, lag_names, drop = FALSE]); storage.mode(H) <- "double"
  if (nrow(d) <= length(covs) + lag_df + 2L || stats::sd(as.numeric(H)) == 0) return(NA_real_)
  cx <- if (length(covs)) stats::model.matrix(stats::reformulate(covs), data = d) else matrix(1, nrow(d), 1L); x <- cbind(cx, H %*% B); y <- as.numeric(d[[outcome]]); n <- nrow(x); p <- ncol(x); bi <- (ncol(cx) + 1L):p
  if (qr(x)$rank < p || n <= p || (family == "binomial" && length(unique(y)) < 2L)) return(NA_real_)
  if (family == "gaussian") { fit <- stats::lm.fit(x, y); b <- fit$coefficients; V <- .siw_v14_hc3(x, fit$residuals) }
  else { fit <- suppressWarnings(stats::glm.fit(x, y, family = stats::binomial(), control = stats::glm.control(epsilon = 1e-10, maxit = 200L))); b <- fit$coefficients; V <- tryCatch(solve(crossprod(x, x * fit$weights)), error = function(e) matrix(NA_real_, p, p)) }
  bc <- b[bi]; Vb <- V[bi, bi, drop = FALSE]; if (any(!is.finite(bc)) || any(!is.finite(Vb))) return(NA_real_)
  sum(drop(B %*% bc))
}

#' One archived DLM bootstrap target (WINTER_SOURCE R/100_resampling.R siecc_bootstrap_dlm_one): set.seed(seed) once; participant resampling sample.int(n, n, TRUE), or assessment-date
#' clusters (ISO dates from the declared anchor, sorted as split() sorts them) sampled with replacement and expanded to all their rows; percentile interval type 8 when at least 20 replicates succeed.
#' @export
siw_v14_dlm_bootstrap <- function(dat, config, outcome, family, metric, replicates, seed, cluster_labels = NULL, lag_end = 59L, lag_df = 3L) {
  sp <- config$metrics[config$metrics$metric == metric, ]; ln <- siw_daily_names(sp$daily_prefix, lag_end); covs <- config$tiers[[config$primary_tier]]; B <- siw_lag_basis(lag_end, lag_df)
  d0 <- dat[, unique(c(outcome, covs, ln)), drop = FALSE]; n <- nrow(d0); gr <- if (!is.null(cluster_labels)) split(seq_len(n), as.character(cluster_labels)) else NULL; gn <- names(gr)
  est <- siw_with_seed(seed, { e <- rep(NA_real_, replicates)
    for (b in seq_len(replicates)) { idx <- if (is.null(gr)) sample.int(n, n, replace = TRUE) else unlist(lapply(sample(gn, length(gn), replace = TRUE), function(g) gr[[g]]), use.names = FALSE)
      e[[b]] <- tryCatch(.siw_v14_dlm_cum(d0[idx, , drop = FALSE], outcome, family, ln, covs, B, lag_df), error = function(err) NA_real_) }; e })
  ok <- is.finite(est); ci <- if (sum(ok) >= 20L) stats::quantile(est[ok], c(0.025, 0.975), names = FALSE, type = 8) else c(NA_real_, NA_real_)
  data.frame(replicates_requested = as.integer(replicates), replicates_successful = sum(ok), estimate_median = if (any(ok)) stats::median(est[ok]) else NA_real_, ci_lower = ci[[1L]], ci_upper = ci[[2L]],
             positive_fraction = if (any(ok)) mean(est[ok] > 0) else NA_real_, negative_fraction = if (any(ok)) mean(est[ok] < 0) else NA_real_, cluster_count = if (is.null(gr)) NA_integer_ else length(gn), stringsAsFactors = FALSE)
}

#' The 18 archived DLM bootstrap rows (9 representative M2-L DLMs x participant / assessment-date). seed = config seed + 100 x target_index (+1 for the date scheme), target_index = 11 x block.
#' @export
siw_l59_bootstrap <- function(dat, config, protocol = siw_posthoc_protocol(), replicates = NULL) {
  it <- siw_posthoc_item(protocol, "l59_bootstrap"); R <- replicates %||% it$replicates; iso <- format(siw_interview_date(dat$AssessmentDay, protocol)); rows <- list(); blk <- 0L
  for (oi in seq_len(nrow(config$outcomes))) for (m in config$representative_metrics) { blk <- blk + 1L; ti <- 11L * blk; o <- config$outcomes[oi, ]
    for (type in c("participant", "assessment_date")) { seed <- as.integer(config$seed + 100L * ti + if (type == "assessment_date") 1L else 0L)
      r <- siw_v14_dlm_bootstrap(dat, config, o$outcome, o$family, m, as.integer(if (type == "participant") R$participant else R$assessment_date), seed, if (type == "assessment_date") iso else NULL)
      rows[[length(rows) + 1L]] <- cbind(r, target_type = "primary_DLM_cumulative_effect", resampling_type = type, outcome = o$outcome, metric = m, window_or_lag = 59L, target_index = ti, seed = seed, stringsAsFactors = FALSE) } }
  out <- do.call(rbind, rows); out$interval_excludes_null <- is.finite(out$ci_lower) & is.finite(out$ci_upper) & (out$ci_lower > 0 | out$ci_upper < 0); rownames(out) <- NULL; out
}

.siw_ph_item_l59_bootstrap <- function(dat, config, protocol, it, refs, ctx) {
  R <- ctx$options$replicates; bs <- siw_l59_bootstrap(dat, config, protocol, R); ctx$W(bs, "l59_dlm_bootstrap_summary.csv")
  acfg <- .siw_archived_cfg(config); ver <- do.call(rbind, lapply(seq_len(nrow(config$outcomes)), function(oi) do.call(rbind, lapply(config$representative_metrics, function(m) { o <- config$outcomes[oi, ]; sp <- config$metrics[config$metrics$metric == m, ]
    a <- .siw_v14_dlm_cum(dat, o$outcome, o$family, siw_daily_names(sp$daily_prefix, 59L), config$tiers[[config$primary_tier]], siw_lag_basis(59L, 3L)); b <- siw_fit_dlm(dat, acfg, o$outcome, m, 59L, 3L, config$primary_tier, draws = 0L, return_curves = FALSE)$summary$cumulative_effect %||% NA_real_
    d <- abs(a - b); data.frame(outcome = o$outcome, metric = m, original_route = a, package_fit = b, abs_diff = d, status = if (is.finite(d) && d <= config$tolerances$dlm_abs + config$tolerances$dlm_rel * abs(b)) "PASS" else "FAIL", stringsAsFactors = FALSE) }))))
  ctx$W(ver, "l59_bootstrap_full_sample_verification.csv")
  req_ok <- is.null(R) && all(bs$replicates_requested == ifelse(bs$resampling_type == "participant", it$replicates$participant, it$replicates$assessment_date))
  rs <- it$reference; ref <- .siw_ph_ref(ctx, refs, protocol, rs$role, rs$file, rs$row_filter); ctx$W(ref$provenance, "posthoc_reference_provenance.csv"); rstat <- .siw_ph_dep(ref)
  if (is.null(rstat)) { cmp <- siw_posthoc_compare(bs, ref$table, unlist(rs$keys), unlist(rs$fields), config, rs$tolerance, "l59_dlm_bootstrap_18", rs$file, rs$expected_compared); ctx$W(cmp$summary, "l59_bootstrap_comparison_summary.csv"); ctx$W(cmp$detail, "l59_bootstrap_comparison_detail.csv"); rstat <- .siw_ph_cmp_status(cmp) }
  si <- it$si_concordance; x <- si$isolation_ims_rh; ip <- bs[bs$outcome == "Isolation_cont" & bs$metric == "IMS_relative_humidity" & bs$resampling_type == "participant", ]; id <- bs[bs$outcome == "Isolation_cont" & bs$metric == "IMS_relative_humidity" & bs$resampling_type == "assessment_date", ]
  sp_ <- bs$replicates_successful[bs$resampling_type == "participant"]; sd_ <- bs$replicates_successful[bs$resampling_type == "assessment_date"]
  sic <- rbind(.siw_ph_si_row("l59_bootstrap", "isolation x IMS RH participant ci_lower", ip$ci_lower, unlist(x$participant_ci)[1], 3), .siw_ph_si_row("l59_bootstrap", "isolation x IMS RH participant ci_upper", ip$ci_upper, unlist(x$participant_ci)[2], 3),
               .siw_ph_si_row("l59_bootstrap", "isolation x IMS RH participant successful", ip$replicates_successful, x$participant_successful, 0), .siw_ph_si_row("l59_bootstrap", "isolation x IMS RH date ci_lower", id$ci_lower, unlist(x$date_ci)[1], 3),
               .siw_ph_si_row("l59_bootstrap", "isolation x IMS RH date ci_upper", id$ci_upper, unlist(x$date_ci)[2], 3), .siw_ph_si_row("l59_bootstrap", "isolation x IMS RH date successful", id$replicates_successful, x$date_successful, 0),
               .siw_ph_si_row("l59_bootstrap", "participant successful minimum (nine DLMs)", min(sp_), unlist(si$participant_success_range)[1], 0), .siw_ph_si_row("l59_bootstrap", "participant successful maximum (nine DLMs)", max(sp_), unlist(si$participant_success_range)[2], 0),
               .siw_ph_si_row("l59_bootstrap", "date successful minimum (nine DLMs)", min(sd_), unlist(si$date_success_range)[1], 0), .siw_ph_si_row("l59_bootstrap", "date successful maximum (nine DLMs)", max(sd_), unlist(si$date_success_range)[2], 0))
  if (ctx$synthetic || !is.null(R)) sic$agrees <- NA; ctx$W(sic, "l59_bootstrap_si_concordance.csv")
  st <- c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else ref$status, L59_BOOTSTRAP_REPRODUCTION_STATUS = rstat, L59_BOOTSTRAP_VERIFICATION_STATUS = if (all(ver$status == "PASS") && (req_ok || !is.null(R))) "PASS" else "FAIL",
          L59_BOOTSTRAP_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (!is.null(R)) "NOT_TESTED" else if (all(sic$agrees)) "PASS" else "REVIEW")
  info <- c(paste0("L59_BOOTSTRAP_REPLICATES=participant ", paste(unique(bs$replicates_requested[bs$resampling_type == "participant"]), collapse = ","), "; date ", paste(unique(bs$replicates_requested[bs$resampling_type == "assessment_date"]), collapse = ","), if (!is.null(R)) " (TEST OVERRIDE: not the declared counts)" else ""),
            paste0("L59_BOOTSTRAP_SUCCESS=participant ", min(sp_), "-", max(sp_), "; date ", min(sd_), "-", max(sd_)), paste0("L59_BOOTSTRAP_INTERVALS_EXCLUDING_NULL=", sum(bs$interval_excludes_null), " of ", nrow(bs), " (unadjusted; archived L01-L59 horizon, not the final L01-L56 models)"))
  list(statuses = st, info = info, tables = list(bootstrap = bs, verification = ver, si = sic))
}
