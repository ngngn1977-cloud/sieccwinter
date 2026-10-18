# Post hoc step 30 of config/posthoc_sensitivity_protocol_v2.json (amendment of 14 Oct 2026, after the 30 July 2026 family lock):
#   PH_AGE65 (SENSITIVITY) the 90 representative M2-L cumulative models refitted without participants with Age < 65 (Age = 2026 minus birth year; variant AGE65 of siw_s17_cumulative).
# Same estimators and inference as the primary models: HC3 with p from t on the residual df for continuous outcomes, maximum-likelihood logistic with the model-based Wald (normal) p for high
# integration (glm epsilon 1e-12, maxit 200); Benjamini-Hochberg across the 90 tests (label q90-age65). Read only for q90, q9, q9-nested and every accepted status.
# Outputs are aggregate; no participant code, date or row value is written. There is no reference file: the comparison is with the values declared in the protocol item (SI concordance).

.siw_ph_item_age65 <- function(dat, config, protocol, it, refs, ctx) {
  g <- siw_s17_grid(dat, config, "AGE65", protocol); g$q90_age65 <- siw_bh(g$p_primary); g$family_label <- "q90-age65"; ctx$W(g, "age65_cumulative_90.csv")
  vr <- do.call(rbind, lapply(seq_len(nrow(g)), function(i) { r <- g[i, ]; iv <- .siw_s17_independent(dat, config, r, protocol)
    data.frame(model_id = r$model_id, estimate_rel_diff = abs(r$estimate - iv[["estimate"]]) / max(abs(iv[["estimate"]]), 1e-12), se_rel_diff = abs(r$standard_error - iv[["standard_error"]]) / max(abs(iv[["standard_error"]]), 1e-12), stringsAsFactors = FALSE) }))
  vr$status <- ifelse(vr$estimate_rel_diff <= 1e-6 & vr$se_rel_diff <= 1e-6, "PASS", "FAIL"); ctx$W(vr, "age65_independent_verification.csv")
  age <- as.numeric(dat$Age); smp <- data.frame(quantity = c("participants in the canonical CSV", "participants with Age < minimum age (excluded)", "participants with Age >= minimum age", "minimum age (years)"),
                                                 value = c(nrow(dat), sum(is.finite(age) & age < SIW_AGE65_MINIMUM), sum(is.finite(age) & age >= SIW_AGE65_MINIMUM), SIW_AGE65_MINIMUM), stringsAsFactors = FALSE); ctx$W(smp, "age65_sample.csv")
  e <- it$expected; I <- "age65"; S <- .siw_ph_si_row; im <- which.min(g$p_primary); one <- function(v) { u <- unique(v[!is.na(v)]); if (length(u) == 1L) u else NA_real_ }
  bn <- g[g$family == "binomial", ]
  rows <- list(S(I, "n (every model)", one(g$n), e$n, 0), S(I, "high-integration events", one(bn$event_count), e$events, 0), S(I, "nominal p < 0.05 (t-based for continuous outcomes)", sum(g$p_primary < 0.05, na.rm = TRUE), e$nominal_p_lt_05, 0),
               S(I, "smallest p", g$p_primary[im], e$smallest_p, 3),
               .siw_ph_flag(I, paste0("smallest p is ", e$smallest_p_model$outcome, ", ", e$smallest_p_model$metric, ", ", e$smallest_p_model$window, " days (1 = yes)"), g$outcome[im] == e$smallest_p_model$outcome && g$metric[im] == e$smallest_p_model$metric && g$window[im] == e$smallest_p_model$window),
               S(I, "minimum q90-age65", min(g$q90_age65, na.rm = TRUE), e$minimum_q90, 3))
  sic <- .siw_ph_ms(do.call(rbind, rows), ctx$synthetic); ctx$W(sic, "age65_si_concordance.csv")
  st <- c(AGE65_COMPUTATION_STATUS = if (nrow(g) == 90L && all(g$status == "COMPLETED") && all(is.finite(g$p_primary))) "PASS" else "FAIL", AGE65_VERIFICATION_STATUS = if (all(vr$status == "PASS")) "PASS" else "FAIL",
          AGE65_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(sic$agrees %in% TRUE)) "PASS" else "REVIEW")
  info <- c(paste0("AGE65_N=", paste(unique(g$n), collapse = ","), "; participants excluded (Age < ", SIW_AGE65_MINIMUM, ") ", smp$value[2L], " of ", smp$value[1L], "; models completed ", sum(g$status == "COMPLETED"), " of ", nrow(g)),
            paste0("AGE65_EVENTS=", paste(unique(bn$event_count), collapse = ","), "; nominal p < 0.05: ", sum(g$p_primary < 0.05, na.rm = TRUE), "; smallest p ", sprintf("%.4f", g$p_primary[im]), " (", g$outcome[im], ", ", g$metric[im], ", ", g$window[im], " days); minimum q90-age65 ", sprintf("%.4f", min(g$q90_age65, na.rm = TRUE))),
            "AGE65_FAMILY=q90-age65 (BH across the 90 tests; p = t on the residual df for continuous outcomes, model-based Wald for high integration); distinct from q90, which is unchanged")
  list(statuses = st, info = info, tables = list(grid = g, verification = vr, sample = smp, si = sic))
}
