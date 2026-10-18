# Archived L01-L59 programme (validation only). Kept separate from the final L01-L56 analysis and never mixed with it numerically.

# V1.4 R/080_dlm.R siecc_fit_dlm used glm.control(epsilon = 1e-10, maxit = 200L); see config/archived_dlm_validation_protocol.json specification_trace.binary_fit_control.
.SIW_ARCHIVED_GLM_CONTROL <- list(glm_epsilon = 1e-10, glm_maxit = 200L)

#' Archived binary-fit control (glm epsilon 1e-10, maxit 200 as in the V1.4 archived analysis); config$dlm$archived$glm_epsilon / glm_maxit override it when present.
#' @export
siw_archived_glm_control <- function(config) {
  a <- config$dlm$archived
  list(glm_epsilon = as.numeric(a$glm_epsilon %||% .SIW_ARCHIVED_GLM_CONTROL$glm_epsilon), glm_maxit = as.integer(a$glm_maxit %||% .SIW_ARCHIVED_GLM_CONTROL$glm_maxit))
}

# Configuration copy whose final-branch glm control is the archived one; the final L01-L56 configuration (epsilon 1e-12) is never modified.
.siw_archived_cfg <- function(config) { g <- siw_archived_glm_control(config); cfg <- config; cfg$dlm$final$glm_epsilon <- g$glm_epsilon; cfg$dlm$final$glm_maxit <- g$glm_maxit; cfg }

#' Archived fixed DLM grid: 3 outcomes x 8 metrics x tiers at L01-L59 (168 models for seven tiers), with q24 (M2-L, 24 tests) and q9.
#' Binary models use the archived glm control (siw_archived_glm_control: epsilon 1e-10, maxit 200).
#' @export
siw_archived_dlm_grid <- function(dat, config, tiers = config$tier_order, draws = config$dlm$archived$draws) {
  config <- .siw_archived_cfg(config)
  a <- config$dlm$archived; cache <- .siw_new_cache(); xc <- lapply(stats::setNames(tiers, tiers), function(t) siw_tier_matrix(dat, config$tiers[[t]]))
  S <- list(); LC <- list()
  for (oi in seq_len(nrow(config$outcomes))) for (m in config$metrics$metric) for (t in tiers) {
    r <- siw_fit_dlm(dat, config, config$outcomes$outcome[oi], m, a$lag_end, a$lag_df, t, draws = if (t == a$tier) draws else draws, cache = cache, return_curves = FALSE, x_cov = xc[[t]])
    S[[length(S) + 1L]] <- r$summary
  }
  s <- siw_bind_rows(S); s$q24 <- NA_real_; s$q9 <- NA_real_
  i24 <- which(s$model_tier == a$tier & is.finite(s$global_p)); s$q24[i24] <- siw_bh(s$global_p[i24])
  i9 <- which(s$model_tier == a$tier & s$metric %in% config$representative_metrics & is.finite(s$global_p)); s$q9[i9] <- siw_bh(s$global_p[i9])
  s$multiplicity_family_q24 <- ifelse(is.na(s$q24), "", "ARCHIVED_L01_L59_M2_L_q24"); s$multiplicity_family_q9 <- ifelse(is.na(s$q9), "", "ARCHIVED_L01_L59_M2_L_q9")
  s
}

#' Archived lag-length x lag-df sensitivity: 24 outcome-metric pairs x 3 lag ends x 3 lag dfs (216 fits, no bands), with q216.
#' Binary models use the archived glm control (siw_archived_glm_control: epsilon 1e-10, maxit 200).
#' @export
siw_archived_dlm_sensitivity <- function(dat, config) {
  config <- .siw_archived_cfg(config)
  a <- config$dlm$archived; cache <- .siw_new_cache(); xc <- siw_tier_matrix(dat, config$tiers[[a$tier]]); S <- list()
  for (oi in seq_len(nrow(config$outcomes))) for (m in config$metrics$metric) for (L in a$sensitivity_lag_ends) for (d in a$sensitivity_lag_dfs) {
    r <- siw_fit_dlm(dat, config, config$outcomes$outcome[oi], m, L, d, a$tier, draws = 0L, cache = cache, return_curves = FALSE, x_cov = xc)
    s <- r$summary; s$sensitivity_specification <- paste0("L", L, "_DF", d); S[[length(S) + 1L]] <- s
  }
  s <- siw_bind_rows(S); s$q216 <- siw_bh(s$global_p); s$multiplicity_family_q216 <- "ARCHIVED_DLM_LAG_FORM_q216"
  s
}
