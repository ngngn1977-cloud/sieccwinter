# Post hoc steps 8-10: early/late seasonal analyses (e, RECONSTRUCTION), the H3 nested constant-versus-varying lag-weight test (D, POST_HOC) and the nearest-station check (b, POST_HOC).

.siw_ph_fit_cum <- function(sub, config, outcome, family, exposure) {
  covs <- config$tiers[[config$primary_tier]]; Xc <- siw_tier_matrix(sub, covs); y <- as.numeric(sub[[outcome]]); x <- as.numeric(sub[[exposure]]); m <- stats::complete.cases(Xc) & is.finite(y) & is.finite(x)
  Xs <- Xc[m, , drop = FALSE]; dr <- grepl("^Locality_3group", colnames(Xs)) & (colSums(Xs != 0) == 0L); Xs <- Xs[, !dr, drop = FALSE]; X <- cbind(Xs[, 1L, drop = FALSE], exposure = x[m], Xs[, -1L, drop = FALSE]); colnames(X)[1L] <- "(Intercept)"
  st <- if (family == "gaussian") siw_fit_gaussian(X, y[m], 2L) else siw_fit_logistic(X, y[m], 2L, epsilon = config$glm$cumulative_epsilon, maxit = config$glm$cumulative_maxit); list(st = st, X = X, y = y[m])
}

#' Early / late M2-L subgroup fits (RECONSTRUCTION; no original exists): every representative outcome x metric x window within each group, with the final cumulative estimator.
#' q180_subgroup: BH over all 180 tests (the governing family stated in the SI Table S11 note; author decision 13 Oct 2026); q18_subgroup: BH over the 18 56-day tests (informational only). Per-SD values use the full-cohort SD.
#' @export
siw_early_late_subgroups <- function(dat, config) {
  if (is.null(dat$EarlyLate)) dat$EarlyLate <- siw_early_late(dat$AssessmentDay); rows <- list()
  for (oi in seq_len(nrow(config$outcomes))) { o <- config$outcomes[oi, ]
    for (m in config$representative_metrics) { sp <- config$metrics[config$metrics$metric == m, ]
      for (w in config$windows) { e <- siw_cum_name(sp$cumulative_prefix, w); s <- stats::sd(dat[[e]], na.rm = TRUE)
        for (g in c("Early", "Late")) { f <- .siw_ph_fit_cum(dat[dat$EarlyLate == g, , drop = FALSE], config, o$outcome, o$family, e); st <- f$st; gv <- function(nm) { v <- st[[nm]]; if (is.null(v)) NA_real_ else as.numeric(v) }
          b <- gv("coefficient"); lo <- gv("ci_lower"); hi <- gv("ci_upper"); tr <- if (o$family == "binomial") exp else identity
          rows[[length(rows) + 1L]] <- data.frame(outcome = o$outcome, family = o$family, metric = m, window = as.integer(w), group = g, n = st$n, event_count = if (o$family == "binomial") sum(f$y == 1) else NA_integer_,
            coefficient = b, standard_error = if (o$family == "gaussian") gv("robust_hc3_se") else gv("model_based_se"), ci_lower = lo, ci_upper = hi, nominal_p = gv("nominal_p"), full_cohort_sd = s, scale = if (o$family == "binomial") "OR" else "B",
            per_sd_estimate = tr(b * s), per_sd_ci_lower = tr(lo * s), per_sd_ci_upper = tr(hi * s), convergence = isTRUE(st$convergence), separation_status = st$separation_status %||% "", status = if (isTRUE(st$estimable)) "COMPLETED" else "NOT_ESTIMABLE", stringsAsFactors = FALSE) } } } }
  out <- do.call(rbind, rows); out$q180_subgroup <- siw_bh(out$nominal_p); out$q18_subgroup <- NA_real_; i <- which(out$window == 56L); out$q18_subgroup[i] <- siw_bh(out$nominal_p[i]); rownames(out) <- NULL; out
}

#' SI Table S11 Panel A: per group the participant-level mean (SD) of the 56-day exposures and the UTCI category counts over L01-L56 participant-days.
#' @export
siw_early_late_panel_a <- function(dat, config) {
  if (is.null(dat$EarlyLate)) dat$EarlyLate <- siw_early_late(dat$AssessmentDay); U <- .siw_ph_hist(dat, "UTCI", 56L); rows <- list()
  for (g in c("Early", "Late")) { k <- dat$EarlyLate == g
    rows[[length(rows) + 1L]] <- data.frame(group = g, quantity = c("participants", "high_integration_events"), metric = "", value = c(sum(k), sum(dat$Int_dummy[k] == 1, na.rm = TRUE)), stringsAsFactors = FALSE)
    for (m in config$representative_metrics) { v <- dat[[siw_cum_name(config$metrics$cumulative_prefix[config$metrics$metric == m], 56L)]][k]; rows[[length(rows) + 1L]] <- data.frame(group = g, quantity = c("mean_56_day", "sd_56_day"), metric = m, value = c(mean(v, na.rm = TRUE), stats::sd(v, na.rm = TRUE)), stringsAsFactors = FALSE) }
    b <- siw_utci_band(U[k, , drop = FALSE]); b <- b[!is.na(b)]; rows[[length(rows) + 1L]] <- data.frame(group = g, quantity = c("utci_valid_participant_days", paste0("utci_", SIW_UTCI_BANDS, "_days")), metric = "UTCI", value = c(length(b), vapply(SIW_UTCI_BANDS, function(z) sum(b == z), 0)), stringsAsFactors = FALSE) }
  do.call(rbind, rows)
}

.siw_ph_dec <- function(v) ifelse(abs(v) < 0.01 & v != 0, 4L, 2L)

.siw_ph_item_early_late <- function(dat, config, protocol, it, refs, ctx) {
  ia <- siw_s17_grid(dat, config, "EARLY_LATE_INTERACTION", protocol); ia$q90_interaction <- siw_bh(ia$interaction_p); ia$q9_interaction <- NA_real_; i56 <- which(ia$window == 56L); ia$q9_interaction[i56] <- siw_bh(ia$interaction_p[i56]); ctx$W(ia, "early_late_interaction_90.csv")
  sg <- siw_early_late_subgroups(dat, config); ctx$W(sg, "early_late_subgroups_180.csv"); pa <- siw_early_late_panel_a(dat, config); ctx$W(pa, "early_late_panel_a.csv")
  vr <- do.call(rbind, lapply(seq_len(nrow(sg)), function(i) { r <- sg[i, ]; sub <- dat[dat$EarlyLate == r$group, , drop = FALSE]; e <- siw_cum_name(config$metrics$cumulative_prefix[config$metrics$metric == r$metric], r$window); f <- stats::reformulate(c(e, config$tiers[[config$primary_tier]]), r$outcome)
    fit <- if (r$family == "gaussian") stats::lm(f, data = sub) else suppressWarnings(stats::glm(f, data = sub, family = stats::binomial(), control = stats::glm.control(epsilon = config$glm$cumulative_epsilon, maxit = config$glm$cumulative_maxit)))
    b <- unname(stats::coef(fit)[e]); data.frame(outcome = r$outcome, metric = r$metric, window = r$window, group = r$group, coefficient_rel_diff = abs(b - r$coefficient) / max(abs(b), 1e-12), stringsAsFactors = FALSE) }))
  vr$status <- ifelse(is.finite(vr$coefficient_rel_diff) & vr$coefficient_rel_diff <= 1e-6, "PASS", "FAIL"); ctx$W(vr, "early_late_subgroup_independent_verification.csv")
  rp <- it$part_reproduction$reference; ref <- .siw_ph_ref(ctx, refs, protocol, rp$role, rp$file, rp$row_filter); ctx$W(ref$provenance, "posthoc_reference_provenance.csv"); rstat <- .siw_ph_dep(ref)
  if (is.null(rstat)) { cmp <- siw_posthoc_compare(ia, .siw_s17_ref(ref$table), unlist(rp$keys), unlist(rp$fields), config, rp$tolerance, "early_late_interaction_90", rp$file, rp$expected_compared); ctx$W(cmp$summary, "early_late_interaction_comparison_summary.csv"); ctx$W(cmp$detail, "early_late_interaction_comparison_detail.csv"); rstat <- .siw_ph_cmp_status(cmp) }
  si <- it$si_concordance; A <- si$panel_a; rows <- list(); s56 <- sg[sg$window == 56L, ]; i56d <- ia[ia$window == 56L, ]; gv <- function(g, q, m = "") pa$value[pa$group == g & pa$quantity == q & pa$metric == m]
  for (t in A$means) for (g in c("early", "late")) { G <- if (g == "early") "Early" else "Late"; rows <- c(rows, list(.siw_ph_si_row("early_late", paste("Panel A", t$metric, g, "mean"), gv(G, "mean_56_day", t$metric), t[[paste0(g, "_mean")]], 2), .siw_ph_si_row("early_late", paste("Panel A", t$metric, g, "SD"), gv(G, "sd_56_day", t$metric), t[[paste0(g, "_sd")]], 2))) }
  for (g in c("early", "late")) { G <- if (g == "early") "Early" else "Late"; for (b in names(A$utci_categories[[g]])) rows[[length(rows) + 1L]] <- .siw_ph_si_row("early_late", paste("Panel A UTCI", g, b), gv(G, paste0("utci_", b, "_days"), "UTCI"), A$utci_categories[[g]][[b]], 0)
    rows[[length(rows) + 1L]] <- .siw_ph_si_row("early_late", paste("group n", g), gv(G, "participants"), A$group_n[[g]], 0); rows[[length(rows) + 1L]] <- .siw_ph_si_row("early_late", paste("group events", g), gv(G, "high_integration_events"), A$group_events[[g]], 0) }
  for (m in names(A$common_sd)) rows[[length(rows) + 1L]] <- .siw_ph_si_row("early_late", paste("common full-cohort SD", m), unique(s56$full_cohort_sd[s56$metric == m])[1], A$common_sd[[m]], 2)
  for (t in si$panel_b) { for (g in c("early", "late")) { r <- s56[s56$outcome == t$outcome & s56$metric == t$metric & s56$group == (if (g == "early") "Early" else "Late"), ]; tv <- unlist(t[[g]]); v <- c(r$per_sd_estimate, r$per_sd_ci_lower, r$per_sd_ci_upper)
      for (k in 1:3) rows[[length(rows) + 1L]] <- .siw_ph_si_row("early_late", paste("Panel B", t$metric, t$outcome, g, c("estimate", "ci_lower", "ci_upper")[k], "per common SD"), v[k], tv[k], .siw_ph_dec(tv[k]))
      rows <- c(rows, list(.siw_ph_si_row("early_late", paste("Panel B", t$metric, t$outcome, g, "p"), r$nominal_p, t[[paste0(g, "_p")]], 3), .siw_ph_si_row("early_late", paste("Panel B", t$metric, t$outcome, g, "q (180-test family, SI note)"), r$q180_subgroup, t[[paste0(g, "_q")]], 3))) }
    r <- i56d[i56d$outcome == t$outcome & i56d$metric == t$metric, ]; rows <- c(rows, list(.siw_ph_si_row("early_late", paste("Panel B", t$metric, t$outcome, "interaction p"), r$interaction_p, t$interaction_p, 3), .siw_ph_si_row("early_late", paste("Panel B", t$metric, t$outcome, "interaction q (90-test family, SI note)"), r$q90_interaction, t$interaction_q, 3))) }
  rows <- c(rows, list(.siw_ph_si_row("early_late", "minimum subgroup q (180-test family, SI note)", min(s56$q180_subgroup, na.rm = TRUE), si$minimum_subgroup_q, 3), .siw_ph_si_row("early_late", "minimum interaction q (90-test family, SI note)", min(i56d$q90_interaction, na.rm = TRUE), si$minimum_interaction_q, 3)))
  sic <- do.call(rbind, rows); if (ctx$synthetic) sic$agrees <- NA; ctx$W(sic, "early_late_si_concordance.csv")
  qf <- data.frame(family = c("q18_subgroup_informational", "q180_subgroup_governing", "q9_interaction_informational", "q90_interaction_governing"), tests = c(sum(is.finite(s56$q18_subgroup)), sum(is.finite(sg$q180_subgroup)), sum(is.finite(i56d$q9_interaction)), sum(is.finite(ia$q90_interaction))),
                   minimum_q_56_day = c(min(s56$q18_subgroup, na.rm = TRUE), min(s56$q180_subgroup, na.rm = TRUE), min(i56d$q9_interaction, na.rm = TRUE), min(i56d$q90_interaction, na.rm = TRUE)), si_minimum = c(si$minimum_subgroup_q, si$minimum_subgroup_q, si$minimum_interaction_q, si$minimum_interaction_q), stringsAsFactors = FALSE)
  qf$rounds_to_si <- .siw_ph_rounds_to(qf$minimum_q_56_day, qf$si_minimum, 3); if (ctx$synthetic) qf$rounds_to_si <- NA; ctx$W(qf, "early_late_q_family_comparison.csv")
  st <- c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else ref$status, EARLY_LATE_INTERACTION_REPRODUCTION_STATUS = rstat, EARLY_LATE_SUBGROUP_VERIFICATION_STATUS = if (nrow(sg) == 180L && all(vr$status == "PASS")) "PASS" else "FAIL",
          EARLY_LATE_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(sic$agrees)) "PASS" else "REVIEW")
  info <- c("EARLY_LATE_LABEL=RECONSTRUCTION: the subgroup fits and q values have no original; the interaction rows reproduce archive stage 17", "EARLY_LATE_GROUPS=Early assessment_day <= 54 (interview 29 Jan-24 Mar 2026); Late >= 55",
            paste0("EARLY_LATE_Q_FAMILIES=governing (SI-declared, author decision 13 Oct 2026): q180 subgroup min ", sprintf("%.3f", qf$minimum_q_56_day[2]), ", q90 interaction min ", sprintf("%.3f", qf$minimum_q_56_day[4]),
                   "; informational only: q18 subgroup min ", sprintf("%.3f", qf$minimum_q_56_day[1]), ", q9 interaction min ", sprintf("%.3f", qf$minimum_q_56_day[3])))
  list(statuses = st, info = info, tables = list(interaction = ia, subgroups = sg, panel_a = pa, si = sic, q_families = qf))
}

# ---- D: H3 nested test --------------------------------------------------------------------------------------------------------------------------------------

#' H3 nested test for one representative pair: reduced = W56 cumulative M2-L (constant lag weights); full = final L01-L56 DLM (natural spline df 3 over lag). Nesting is verified first.
#' @return list(row, nesting, acceptance values)
#' @export
siw_h3_nested_one <- function(dat, config, outcome, metric, lag_end = 56L, lag_df = 3L) {
  o <- config$outcomes[config$outcomes$outcome == outcome, ]; sp <- config$metrics[config$metrics$metric == metric, ]; fam <- o$family; covs <- config$tiers[[config$primary_tier]]
  Xc <- siw_tier_matrix(dat, covs); y <- as.numeric(dat[[outcome]]); H <- .siw_ph_hist(dat, sp$daily_prefix, lag_end); x56 <- as.numeric(dat[[siw_cum_name(sp$cumulative_prefix, lag_end)]])
  md <- stats::complete.cases(Xc) & is.finite(y) & (rowSums(!is.finite(H)) == 0L); mc <- stats::complete.cases(Xc) & is.finite(y) & is.finite(x56)
  Xs <- Xc[md, , drop = FALSE]; dr <- grepl("^Locality_3group", colnames(Xs)) & (colSums(Xs != 0) == 0L); Xs <- Xs[, !dr, drop = FALSE]; ys <- y[md]; B <- siw_lag_basis(lag_end, lag_df); cb <- siw_crossbasis(H[md, , drop = FALSE], B)
  Xf <- cbind(Xs, cb); Xr <- cbind(Xs[, 1L, drop = FALSE], exposure = x56[md], Xs[, -1L, drop = FALSE]); n <- nrow(Xf); pf <- ncol(Xf); pr <- ncol(Xr)
  a <- qr.solve(B, rep(1, lag_end)); a_res <- max(abs(B %*% a - 1)); qf <- qr(Xf); rk_f <- qf$rank; rk_j <- qr(cbind(Xf, Xr))$rank; ex <- qr.resid(qf, x56[md]); cs_rel <- max(abs(ex)) / max(abs(x56[md]), 1e-300)
  Xk <- cbind(Xs, constrained = drop(cb %*% a))
  if (fam == "gaussian") { fr <- stats::lm.fit(Xr, ys); fk <- stats::lm.fit(Xk, ys); ff <- stats::lm.fit(Xf, ys); fit_d <- max(abs(fr$fitted.values - fk$fitted.values)) / max(1, max(abs(fr$fitted.values))); rssR <- sum(fr$residuals^2); rssK <- sum(fk$residuals^2); rssF <- sum(ff$residuals^2); obj_d <- abs(rssR - rssK) / max(rssR, 1e-300) }
  else { ctl <- stats::glm.control(epsilon = config$dlm$final$glm_epsilon, maxit = config$dlm$final$glm_maxit); fr <- suppressWarnings(stats::glm.fit(Xr, ys, family = stats::binomial(), control = ctl)); fk <- suppressWarnings(stats::glm.fit(Xk, ys, family = stats::binomial(), control = ctl)); ff <- suppressWarnings(stats::glm.fit(Xf, ys, family = stats::binomial(), control = ctl))
    fit_d <- max(abs(fr$fitted.values - fk$fitted.values)); obj_d <- abs(fr$deviance - fk$deviance) / max(fr$deviance, 1e-300) }
  nest <- data.frame(outcome = outcome, metric = metric, n_dlm_sample = sum(md), n_reduced_sample = sum(mc), same_sample = identical(md, mc), constant_vector_residual = a_res, rank_full = rk_f, rank_joint = rk_j, columns_full = pf, columns_reduced = pr,
                     df_difference = pf - pr, exposure_column_residual_rel = cs_rel, constrained_fit_fitted_diff = fit_d, constrained_fit_objective_rel_diff = obj_d, stringsAsFactors = FALSE)
  nest$status <- if (nest$same_sample && a_res <= 1e-10 && rk_f == pf && rk_j == rk_f && qr(Xr)$rank == pr && nest$df_difference == 2L && cs_rel <= 1e-8 && fit_d <= 1e-7 && obj_d <= 1e-8) "PASS" else "FAIL"
  row <- data.frame(outcome = outcome, metric = metric, n = n, df_full = pf, df_reduced = pr, stat_type = if (fam == "gaussian") "F" else "LR", statistic = NA_real_, df_num = pf - pr, df_den_or_NA = if (fam == "gaussian") n - pf else NA_real_,
                    p_model_based = NA_real_, p_HC3_wald = NA_real_, reduced_model_code = siw_cum_id(o$code, sp$code, lag_end, config$primary_tier), dlm_model_code = siw_dlm_id(o$code, sp$code, lag_end, lag_df, config$primary_tier), stringsAsFactors = FALSE)
  if (fam == "gaussian") { Fst <- ((rssR - rssF) / (pf - pr)) / (rssF / (n - pf)); row$statistic <- Fst; row$p_model_based <- stats::pf(Fst, pf - pr, n - pf, lower.tail = FALSE)
    bi <- (ncol(Xs) + 1L):pf; V <- .siw_hc3(Xf, ff$residuals)$cov[bi, bi, drop = FALSE]; Q <- qr.Q(qr(matrix(a, ncol = 1L)), complete = TRUE); C <- t(Q[, 2:3, drop = FALSE]); cb_ <- drop(C %*% ff$coefficients[bi])
    Wd <- drop(t(cb_) %*% solve(C %*% V %*% t(C)) %*% cb_); row$p_HC3_wald <- stats::pf(Wd / 2, 2, n - pf, lower.tail = FALSE); nest$hc3_wald_statistic <- Wd }
  else { LR <- fr$deviance - ff$deviance; row$statistic <- LR; row$p_model_based <- stats::pchisq(LR, pf - pr, lower.tail = FALSE); nest$full_converged <- isTRUE(ff$converged); nest$reduced_converged <- isTRUE(fr$converged) }
  list(row = row, nesting = nest)
}

#' The nine H3 nested tests with q9-nested (BH across the nine model-based p values; distinct from the existing global q9).
#' @export
siw_h3_nested <- function(dat, config, run_id = "", source_fingerprint = "") {
  res <- unlist(lapply(config$outcomes$outcome, function(o) lapply(config$representative_metrics, function(m) siw_h3_nested_one(dat, config, o, m))), recursive = FALSE)
  rows <- do.call(rbind, lapply(res, `[[`, "row")); nest <- siw_bind_rows(lapply(res, `[[`, "nesting")); rows$q9_nested <- siw_bh(rows$p_model_based); rows$run_id <- run_id; rows$source_fingerprint <- source_fingerprint
  cols <- unlist(siw_posthoc_item(siw_posthoc_protocol(), "h3_nested")$output_columns); rownames(rows) <- NULL; list(table = rows[, cols], nesting = nest)
}

.siw_ph_item_h3_nested <- function(dat, config, protocol, it, refs, ctx) {
  h <- siw_h3_nested(dat, config, ctx$run_id, ctx$fingerprint); ctx$W(h$nesting, "h3_nesting_checks.csv"); nest_ok <- all(h$nesting$status == "PASS")
  rec <- list(protocol = list(name = protocol$protocol_name, version = protocol$protocol_version, status = protocol$status_of_this_document, sha256 = protocol$protocol_hash, item_declared = it$declared, family = it$multiplicity$family),
              timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), input_sha256 = ctx$input_sha256, run_id = ctx$run_id, source_fingerprint = ctx$fingerprint, nesting_status = if (nest_ok) "PASS" else "FAIL", nesting_checks = h$nesting)
  if (!nest_ok) { jsonlite::write_json(rec, file.path(ctx$out_dir, "H3_RUN_RECORD.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null")
    return(list(statuses = c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else "NOT_TESTED", H3_NESTING_STATUS = "FAIL", H3_TEST_STATUS = "NOT_TESTED", H3_ACCEPTANCE_STATUS = "NOT_TESTED"), info = "H3_STOPPED=the nesting check failed (see h3_nesting_checks.csv); no test is reported", tables = list(nesting = h$nesting))) }
  tab <- h$table; ctx$W(tab, "H3_NESTED_TEST.csv"); fin <- all(is.finite(as.matrix(tab[, c("statistic", "p_model_based", "q9_nested")]))) && all(is.finite(tab$p_HC3_wald[tab$stat_type == "F"]))
  acc <- it$acceptance; arows <- list(); covs <- config$tiers[[config$primary_tier]]
  red <- do.call(rbind, lapply(seq_len(nrow(tab)), function(i) { r <- tab[i, ]; o <- config$outcomes[config$outcomes$outcome == r$outcome, ]; e <- siw_cum_name(config$metrics$cumulative_prefix[config$metrics$metric == r$metric], 56L)
    f <- .siw_ph_fit_cum(dat, config, r$outcome, o$family, e); g <- siw_fit_dlm(dat, config, r$outcome, r$metric, 56L, 3L, config$primary_tier, draws = 0L, return_curves = FALSE)$summary
    data.frame(outcome = r$outcome, metric = r$metric, window_or_lag = 56L, model_tier = config$primary_tier, coefficient = f$st$coefficient %||% NA_real_, global_p = g$global_p %||% NA_real_, stringsAsFactors = FALSE) }))
  ctx$W(red, "h3_acceptance_values.csv"); tbl <- unlist(acc$full_global_p$table3_p); ord <- unlist(acc$full_global_p$table3_order); kp <- paste(red$outcome, red$metric, sep = "|")
  t3 <- do.call(rbind, lapply(seq_along(ord), function(i) .siw_ph_si_row("h3_nested", paste("Table 3 global p", ord[i]), red$global_p[match(ord[i], kp)], tbl[i], 3))); if (ctx$synthetic) t3$agrees <- NA; ctx$W(t3, "h3_table3_concordance.csv")
  cr <- .siw_ph_ref(ctx, refs, protocol, acc$reduced_coefficients$role, acc$reduced_coefficients$file); dr <- .siw_ph_ref(ctx, refs, protocol, acc$full_global_p$role, acc$full_global_p$file)
  ctx$W(rbind(cr$provenance, dr$provenance), "posthoc_reference_provenance.csv")
  s1 <- .siw_ph_dep(cr); if (is.null(s1)) { c1 <- siw_posthoc_compare(red[, c("outcome", "metric", "window_or_lag", "model_tier", "coefficient")], cr$table[cr$table$model_tier == config$primary_tier & cr$table$window_or_lag == 56 & cr$table$metric %in% config$representative_metrics, ], c("outcome", "metric", "window_or_lag", "model_tier"), "coefficient", config, "cumulative", "h3_reduced_W56", acc$reduced_coefficients$file, 9L); ctx$W(c1$summary, "h3_reduced_coefficient_comparison.csv"); ctx$W(c1$detail, "h3_reduced_coefficient_comparison_detail.csv"); s1 <- .siw_ph_cmp_status(c1) }
  s2 <- .siw_ph_dep(dr); if (is.null(s2)) { c2 <- siw_posthoc_compare(red[, c("outcome", "metric", "model_tier", "global_p")], dr$table, c("outcome", "metric", "model_tier"), "global_p", config, "dlm", "h3_full_global_p", acc$full_global_p$file, 9L); ctx$W(c2$summary, "h3_full_global_p_comparison.csv"); ctx$W(c2$detail, "h3_full_global_p_comparison_detail.csv"); s2 <- .siw_ph_cmp_status(c2) }
  s3 <- if (ctx$synthetic) SIW_PH_NA_SYN else if (all(t3$agrees)) "PASS" else "FAIL"
  rec$output_file <- "H3_NESTED_TEST.csv"; rec$rows <- nrow(tab); rec$acceptance <- list(reduced_coefficients = s1, full_global_p_archived = s2, full_global_p_table3 = s3, finite = fin)
  jsonlite::write_json(rec, file.path(ctx$out_dir, "H3_RUN_RECORD.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null")
  st <- c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else .siw_ph_combine(c(cr$status, dr$status)), H3_NESTING_STATUS = "PASS", H3_TEST_STATUS = if (fin && nrow(tab) == 9L) "PASS" else "FAIL",
          H3_REDUCED_COEFFICIENT_STATUS = s1, H3_FULL_GLOBAL_P_STATUS = s2, H3_TABLE3_STATUS = s3)
  st[["H3_ACCEPTANCE_STATUS"]] <- .siw_ph_combine(st[c("H3_REDUCED_COEFFICIENT_STATUS", "H3_FULL_GLOBAL_P_STATUS", "H3_TABLE3_STATUS", "H3_TEST_STATUS")])
  info <- c("H3_FAMILY=q9-nested (BH across the nine model-based p values; distinct from the existing global q9)", paste0("H3_MIN_P_MODEL_BASED=", signif(min(tab$p_model_based), 4), "; MIN_Q9_NESTED=", signif(min(tab$q9_nested), 4)),
            "H3_DECLARED=2026-10-13 post hoc (after the 30 July 2026 family lock); the manuscript cites only H3_NESTED_TEST.csv")
  list(statuses = st, info = info, tables = list(h3 = tab, nesting = h$nesting, acceptance = red, table3 = t3))
}

# ---- (b) nearest station ------------------------------------------------------------------------------------------------------------------------------------

SIW_NEAREST_MAP <- c(IMS_temperature = "TD_MEAN", IMS_relative_humidity = "RH_MEAN", IMS_rainfall_total = "RAIN_TOTAL", IMS_rainy_day_count_ge_1_mm = "RAIN_DAYS1")

SIW_NEAREST_ALIASES <- list(participant_code = c("participant_code", "FEATURE_ID"), date = c("LOCAL_DATE", "JERUSALEM_LOCAL_DATE"))

# declared column aliases of the nearest-station input (protocol item nearest_station, input$column_aliases)
.siw_nearest_aliases <- function(protocol) { a <- tryCatch(siw_posthoc_item(protocol, "nearest_station")$input$column_aliases, error = function(e) NULL); if (is.null(a)) SIW_NEAREST_ALIASES else lapply(a, unlist) }

#' Substitute nearest-station daily histories for the IMS IDW histories. `nearest` is a long table keyed by participant_code and a local calendar date with columns
#' TD_MEAN, RH_MEAN, RAIN_TOTAL, RAIN_DAYS1; declared aliases: FEATURE_ID for participant_code (the raw IMS exports), LOCAL_DATE or JERUSALEM_LOCAL_DATE for the date
#' (ISO; the first 10 characters are used). Other columns are ignored. Lag k of participant i is the row dated 2026-01-29 + assessment_day_i - k (declared anchor).
#' Cumulative IMS fields are then rebuilt with the declared rule. UTCI and every ERA5 field are unchanged. No address or coordinate is read or written.
#' Attributes: nearest_coverage (share of lag cells present per metric) and nearest_participants_matched (count only).
#' @export
siw_nearest_substitute <- function(dat, config, nearest, protocol = siw_posthoc_protocol(), lag_end = 56L) {
  al <- .siw_nearest_aliases(protocol); ic <- intersect(al$participant_code, names(nearest))[1L]; dc <- intersect(al$date, names(nearest))[1L]
  if (is.na(ic) || is.na(dc) || length(setdiff(unname(SIW_NEAREST_MAP), names(nearest)))) stop("SIW_NEAREST_SCHEMA: participant_code (alias FEATURE_ID), LOCAL_DATE (or JERUSALEM_LOCAL_DATE), TD_MEAN, RH_MEAN, RAIN_TOTAL and RAIN_DAYS1 are required", call. = FALSE)
  d10 <- substr(trimws(as.character(nearest[[dc]])), 1L, 10L); nb <- !is.na(d10) & nzchar(d10)
  if (any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", d10[nb]))) stop("SIW_NEAREST_DATE_NOT_ISO: ", sum(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", d10[nb])), " row(s) of ", dc, " do not start with an ISO date", call. = FALSE)
  ids <- trimws(as.character(nearest[[ic]])); key <- paste(ids, d10); if (anyDuplicated(key[nb])) stop("SIW_NEAREST_DUPLICATE_PARTICIPANT_DATE", call. = FALSE)
  out <- dat; ld <- siw_lag_date(dat$AssessmentDay, seq_len(lag_end), protocol); pc <- trimws(as.character(dat$participant_code))
  for (m in names(SIW_NEAREST_MAP)) { sp <- config$metrics[config$metrics$metric == m, ]; v <- suppressWarnings(as.numeric(nearest[[SIW_NEAREST_MAP[[m]]]]))
    for (k in seq_len(lag_end)) out[[sprintf("%s_L%02d", sp$daily_prefix, k)]] <- v[match(paste(pc, ld[, k]), key)] }
  for (nm in grep("^IMS_", siw_cumulative_columns(config), value = TRUE)) out[[nm]] <- NULL
  rec <- siw_reconstruct_cumulative(out, config); for (nm in grep("^IMS_", names(rec), value = TRUE)) out[[nm]] <- rec[[nm]]
  attr(out, "nearest_coverage") <- vapply(names(SIW_NEAREST_MAP), function(m) mean(is.finite(as.matrix(out[, siw_daily_names(config$metrics$daily_prefix[config$metrics$metric == m], lag_end), drop = FALSE]))), 0)
  attr(out, "nearest_participants_matched") <- sum(pc %in% ids); attr(out, "nearest_id_column") <- ic; attr(out, "nearest_date_column") <- dc; out
}

#' The nearest-station check: 120 IMS cumulative M2-L fits and the six L01-L56 M2-L DLMs (IMS RH and rainy-day indicator) on substituted histories.
#' @export
siw_nearest_station_check <- function(dat_nearest, config) {
  cfg <- config; cfg$metrics <- config$metrics[grepl("^IMS_", config$metrics$metric), ]; cfg$representative_metrics <- intersect(config$representative_metrics, cfg$metrics$metric)
  cum <- siw_cumulative_grid(dat_nearest, cfg, tiers = config$primary_tier)$results; dl <- list()
  for (o in config$outcomes$outcome) for (m in c("IMS_relative_humidity", "IMS_rainy_day_count_ge_1_mm")) dl[[length(dl) + 1L]] <- siw_fit_dlm(dat_nearest, config, o, m, 56L, 3L, config$primary_tier, draws = 0L, return_curves = FALSE)$summary
  list(cumulative = cum, dlm = siw_bind_rows(dl))
}

# the nearest-station input files: one CSV, several separated by "|", or the CSVs of a folder
.siw_nearest_files <- function(nd) {
  if (is.null(nd) || !nzchar(nd)) return(character()); p <- unlist(strsplit(nd, "|", fixed = TRUE)); p <- p[nzchar(p)]
  unlist(lapply(p, function(x) if (dir.exists(x)) list.files(x, pattern = "[.]csv$", full.names = TRUE) else x))
}

# declared variant of one input file (by SHA-256), or an undeclared input identified by its header only
.siw_nearest_variant <- function(f, it, k) {
  h <- toupper(siw_file_hash(f)$value); v <- Filter(function(z) identical(toupper(z$sha256), h), it$variants)
  if (length(v)) list(id = v[[1L]]$id, label = v[[1L]]$label, declared = TRUE, sha256 = h) else list(id = paste0("input", k), label = "not a declared variant (identified by its header only)", declared = FALSE, sha256 = h)
}

.siw_ph_item_nearest_station <- function(dat, config, protocol, it, refs, ctx) {
  f <- .siw_nearest_files(refs$nearest_station_histories)
  if (ctx$synthetic) { vars <- list(list(id = "synthetic", label = "synthetic nearest histories derived from the shipped synthetic CSV (demo)", declared = TRUE, sha256 = "", data = .siw_nearest_synthetic(dat, protocol))) }
  else if (!length(f)) return(list(statuses = c(INPUT_PROVENANCE_STATUS = "NOT_TESTED", NEAREST_CHECK_STATUS = "NOT_TESTED", NEAREST_SI_CONCORDANCE_STATUS = "NOT_TESTED"),
                                    info = "NEAREST_NOT_TESTED=no nearest-station history input (a declared export variant or one CSV keyed by participant_code and date) was supplied", tables = list()))
  else { missing <- f[!file.exists(f)]; if (length(missing)) stop("SIW_NEAREST_INPUT_NOT_FOUND: ", length(missing), " file(s)", call. = FALSE)
    vars <- lapply(seq_along(f), function(k) .siw_nearest_variant(f[k], it, k)); ids <- vapply(vars, `[[`, "", "id"); if (anyDuplicated(ids)) stop("SIW_NEAREST_DUPLICATE_VARIANT: the same declared variant was given twice", call. = FALSE)
    for (k in seq_along(f)) vars[[k]]$path <- f[k] }
  multi <- length(vars) > 1L; sfx <- function(v, name) if (multi) sub("[.]csv$", paste0("_", v$id, ".csv"), name) else name
  res <- lapply(vars, function(v) { nr <- if (is.null(v$data)) utils::read.csv(v$path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"), colClasses = "character") else v$data
    for (m in unname(SIW_NEAREST_MAP)) if (m %in% names(nr)) nr[[m]] <- suppressWarnings(as.numeric(nr[[m]]))
    r <- tryCatch(.siw_nearest_one(dat, config, protocol, it, nr, ctx, function(name) sfx(v, name)), error = function(e) list(check = list(cumulative = data.frame(), dlm = data.frame()), si = .siw_ph_si_row("nearest_station", "x", 0, 0, 0)[0, ],
      check_status = "FAIL", coverage = NA_real_, rows = nrow(nr), matched = NA_integer_, id_column = "", date_column = "", error = conditionMessage(e)))
    r$variant <- v; r })
  sm <- do.call(rbind, lapply(res, function(r) data.frame(variant = r$variant$id, label = r$variant$label, declared_variant = r$variant$declared, input_sha256 = r$variant$sha256, input_rows = r$rows, participants_matched = r$matched, participants = nrow(dat),
    id_column = r$id_column, date_column = r$date_column, min_lag_coverage = min(r$coverage), cumulative_fits = nrow(r$check$cumulative), dlm_fits = nrow(r$check$dlm), check_status = r$check_status,
    si_values = nrow(r$si), si_values_agreeing = sum(r$si$agrees %in% TRUE), max_abs_difference = suppressWarnings(max(abs(r$si$package_value - r$si$si_value), na.rm = TRUE)),
    reproduces_si = !ctx$synthetic && nrow(r$si) > 0L && all(r$si$agrees %in% TRUE), error = r$error %||% "", stringsAsFactors = FALSE)))
  sm$max_abs_difference[!is.finite(sm$max_abs_difference)] <- NA_real_; ctx$W(sm, "nearest_station_variant_summary.csv")
  dif <- siw_bind_rows(lapply(res, function(r) if (nrow(r$si)) data.frame(variant = r$variant$id, r$si, difference = r$si$package_value - r$si$si_value, stringsAsFactors = FALSE) else NULL))
  if (is.null(dif) || !nrow(dif)) dif <- data.frame(variant = character(), quantity = character(), package_value = numeric(), si_value = numeric(), agrees = logical(), difference = numeric()); ctx$W(dif, "nearest_station_variant_differences.csv")
  rep_ids <- sm$variant[sm$reproduces_si]
  prov <- if (ctx$synthetic) SIW_PH_NA_SYN else if (all(sm$declared_variant)) "PASS" else "REVIEW"
  st <- c(INPUT_PROVENANCE_STATUS = prov, NEAREST_CHECK_STATUS = if (all(sm$check_status == "PASS")) "PASS" else "FAIL", NEAREST_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (length(rep_ids)) "PASS" else "REVIEW")
  info <- c(paste0("NEAREST_VARIANTS_RUN=", nrow(sm), " (", paste(sm$variant, collapse = ", "), ")"),
            paste0("NEAREST_VARIANT_", toupper(sm$variant), "=", ifelse(sm$declared_variant, "declared variant", "undeclared input (header only)"), "; participants matched ", sm$participants_matched, "/", sm$participants,
                   "; minimum lag coverage ", sprintf("%.3f", sm$min_lag_coverage), "; fits ", sm$check_status, "; SI values agreeing ", sm$si_values_agreeing, "/", sm$si_values),
            paste0("NEAREST_REPRODUCING_VARIANT=", if (ctx$synthetic) "not applicable (synthetic input)" else if (length(rep_ids)) paste(rep_ids, collapse = ", ") else "NONE (differences in nearest_station_variant_differences.csv)"),
            "NEAREST_SCOPE=the single nearest eligible station replaces IDW for the same addresses; UTCI and ERA5 unchanged; 6 Oct 2026 analysis reimplemented in R (POST_HOC); input values are never written")
  list(statuses = st, info = info, tables = list(summary = sm, differences = dif, variants = res))
}

# one nearest-station input: substitution, the 120 + 6 fits and the SI S9/S10 concordance (aggregate outputs only; W(name) adds the variant suffix)
.siw_nearest_one <- function(dat, config, protocol, it, nr, ctx, nm) {
  dn <- siw_nearest_substitute(dat, config, nr, protocol); cov <- attr(dn, "nearest_coverage"); ctx$W(data.frame(metric = names(cov), share_of_lag_cells_present = unname(cov), stringsAsFactors = FALSE), nm("nearest_history_coverage.csv"))
  ch <- siw_nearest_station_check(dn, config); ctx$W(ch$cumulative, nm("nearest_station_cumulative_120.csv")); ctx$W(ch$dlm, nm("nearest_station_dlm_6.csv"))
  si <- it$si_concordance; rows <- list(); w56 <- ch$cumulative[ch$cumulative$window_or_lag == 56L, ]
  for (t in si$w56) { r <- w56[w56$outcome == t$outcome & w56$metric == t$metric, ]; s <- stats::sd(dat[[siw_cum_name(config$metrics$cumulative_prefix[config$metrics$metric == t$metric], 56L)]], na.rm = TRUE); tr <- if (t$scale == "OR") exp else identity; v <- tr(c(r$coefficient, r$ci_lower, r$ci_upper) * s); ci <- unlist(t$ci)
    for (k in 1:3) rows[[length(rows) + 1L]] <- .siw_ph_si_row("nearest_station", paste(t$outcome, t$metric, "W56", c("estimate", "ci_lower", "ci_upper")[k], "per primary SD"), v[k], c(t$estimate, ci)[k], 2) }
  for (t in si$dlm) { r <- ch$dlm[ch$dlm$outcome == t$outcome & ch$dlm$metric == t$metric, ]; ci <- unlist(t$ci); v <- c(r$cumulative_effect, r$cumulative_ci_lower, r$cumulative_ci_upper, r$global_p)
    for (k in 1:4) rows[[length(rows) + 1L]] <- .siw_ph_si_row("nearest_station", paste(t$outcome, t$metric, "DLM", c("summed coefficient", "ci_lower", "ci_upper", "global p")[k]), v[k], c(t$sum, ci, t$global_p)[k], 3) }
  sm <- si$statements; rows <- c(rows, list(.siw_ph_si_row("nearest_station", "cumulative fits converged", sum(ch$cumulative$status == "COMPLETED" & ch$cumulative$convergence %in% TRUE), sm$cumulative_converged, 0),
                                            .siw_ph_si_row("nearest_station", "integration fits with the heuristic flag", sum(ch$cumulative$outcome == "Int_dummy" & ch$cumulative$separation_status == "SUSPECTED"), sm$integration_heuristic_flags, 0)))
  sic <- do.call(rbind, rows); if (ctx$synthetic) sic$agrees <- NA; ctx$W(sic, nm("nearest_station_si_concordance.csv"))
  ok <- nrow(ch$cumulative) == 120L && nrow(ch$dlm) == 6L && all(ch$dlm$status == "COMPLETED")
  list(check = ch, si = sic, check_status = if (ok) "PASS" else "FAIL", coverage = cov, rows = nrow(nr), matched = attr(dn, "nearest_participants_matched"), id_column = attr(dn, "nearest_id_column"), date_column = attr(dn, "nearest_date_column"))
}

# synthetic nearest histories for the demo: the shipped synthetic IDW histories with a fixed artificial perturbation, in the declared long layout (no real data)
.siw_nearest_synthetic <- function(dat, protocol) {
  ld <- siw_lag_date(dat$AssessmentDay, seq_len(56L), protocol); rows <- list()
  for (i in seq_len(nrow(dat))) { g <- function(p) as.numeric(dat[i, siw_daily_names(p, 56L)]); rows[[i]] <- data.frame(participant_code = dat$participant_code[i], LOCAL_DATE = ld[i, ], TD_MEAN = g("IMS_IDW_TD_MEAN") + 0.3, RH_MEAN = pmin(100, g("IMS_IDW_RH_MEAN") + 1),
    RAIN_TOTAL = g("IMS_IDW_RAIN_TOTAL"), RAIN_DAYS1 = g("IMS_IDW_RAIN_DAYS1"), stringsAsFactors = FALSE) }
  do.call(rbind, rows)
}
