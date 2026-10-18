# Cumulative-window regression grid: 3 outcomes x 8 metrics x 10 windows x 7 tiers = 1,680 specifications.

.siw_cum_stats <- c("n", "event_count", "non_event_count", "parameter_count", "residual_df", "rank", "coefficient", "model_based_se", "robust_hc3_se", "statistic", "robust_statistic",
  "ci_lower", "ci_upper", "nominal_p", "standardised_effect", "iqr_effect", "odds_ratio_raw", "odds_ratio_sd", "odds_ratio_iqr", "lr_statistic", "lr_df", "lr_p", "model_f", "model_f_p",
  "r_squared", "adjusted_r_squared", "partial_r_squared_weather", "nagelkerke_r_squared", "aic", "aicc", "residual_rmse", "brier_score", "log_loss", "auc", "exposure_sd", "exposure_iqr")

#' Covariate design matrix of a tier (intercept + covariates), built once with missing values kept in place.
#' Factor levels follow the configuration; the reference level is the first level of Locality_3group.
#' @export
siw_tier_matrix <- function(dat, covariates) {
  n <- nrow(dat)
  if (!length(covariates)) return(matrix(1, n, 1L, dimnames = list(NULL, "(Intercept)")))
  mf <- stats::model.frame(stats::reformulate(covariates), dat, na.action = stats::na.pass)
  X <- stats::model.matrix(attr(mf, "terms"), mf)
  X
}

#' Fit the cumulative grid. Rows are in deterministic order: outcome, metric, window, tier (configuration order).
#' @param tiers Tiers to fit (default all seven).
#' @export
siw_cumulative_grid <- function(dat, config, tiers = config$tier_order) {
  n <- nrow(dat)
  tier_x <- lapply(stats::setNames(tiers, tiers), function(t) siw_tier_matrix(dat, config$tiers[[t]]))
  tier_ok <- lapply(tier_x, function(X) stats::complete.cases(X))
  cache <- .siw_new_cache()
  meta <- list(); stats_list <- list(); acc <- list(); k <- 0L
  for (oi in seq_len(nrow(config$outcomes))) {
    o <- config$outcomes[oi, ]; y <- as.numeric(dat[[o$outcome]]); y_ok <- is.finite(y)
    for (mi in seq_len(nrow(config$metrics))) {
      sp <- config$metrics[mi, ]
      for (w in config$windows) {
        x <- as.numeric(dat[[siw_cum_name(sp$cumulative_prefix, w)]]); x_ok <- is.finite(x)
        for (t in tiers) {
          Xc <- tier_x[[t]]; mask <- tier_ok[[t]] & y_ok & x_ok
          Xs <- Xc[mask, , drop = FALSE]
          drop <- grepl("^Locality_3group", colnames(Xs)) & (colSums(Xs != 0) == 0L)   # a locality level absent from the complete-case sample
          Xs <- Xs[, !drop, drop = FALSE]
          X <- cbind(Xs[, 1L, drop = FALSE], exposure = x[mask], Xs[, -1L, drop = FALSE]); colnames(X)[1L] <- "(Intercept)"
          key <- paste(o$outcome, t, paste(which(!mask), collapse = ","), sep = "|")
          st <- if (o$family == "gaussian") siw_fit_gaussian(X, y[mask], 2L, cache, key)
                else siw_fit_logistic(X, y[mask], 2L, cache, key, epsilon = config$glm$cumulative_epsilon, maxit = config$glm$cumulative_maxit)
          k <- k + 1L
          st$parameter_count <- st$p; st$n <- st$n
          meta[[k]] <- data.frame(result_id = siw_cum_id(o$code, sp$code, w, t), outcome = o$outcome, outcome_family = o$family, metric = sp$metric, exposure = siw_cum_name(sp$cumulative_prefix, w),
            exposure_unit = sp$unit, window_or_lag = as.integer(w), transformation = "raw", model_tier = t,
            primary_role = if (t == config$primary_tier) "PRIMARY_ADJUSTED" else "TIER_SENSITIVITY",
            status = if (isTRUE(st$estimable)) "COMPLETED" else "NOT_ESTIMABLE", not_estimable_reason = st$reason %||% "",
            separation_status = st$separation_status %||% "", convergence = if (is.null(st$convergence)) NA else st$convergence, stringsAsFactors = FALSE)
          stats_list[[k]] <- vapply(.siw_cum_stats, function(s) { v <- st[[s]]; if (is.null(v)) NA_real_ else as.numeric(v) }, numeric(1))
          miss_cov <- if (length(config$tiers[[t]])) vapply(config$tiers[[t]], function(cv) sum(is.na(dat[[cv]])), numeric(1)) else numeric()
          acc[[k]] <- data.frame(result_id = meta[[k]]$result_id, n_input = n, n_complete = sum(mask), n_excluded = n - sum(mask),
            excluded_missing_outcome = sum(!y_ok), excluded_missing_exposure = sum(!x_ok), excluded_missing_covariate = sum(!tier_ok[[t]]),
            covariates_with_missing = paste(names(miss_cov)[miss_cov > 0], collapse = "|"), stringsAsFactors = FALSE)
        }
      }
    }
  }
  res <- cbind(do.call(rbind, meta), as.data.frame(do.call(rbind, stats_list), stringsAsFactors = FALSE))
  rownames(res) <- NULL
  res <- siw_assign_cumulative_q(res, config)
  list(results = res, accounting = do.call(rbind, acc), designs = siw_design_audit(dat, config, tiers, tier_x))
}

#' Benjamini-Hochberg q10, q80 and q90, each only within its declared family (M2-L, estimable, finite p).
#' @export
siw_assign_cumulative_q <- function(res, config) {
  res$q10 <- res$q80 <- res$q90 <- NA_real_
  res$multiplicity_family_q10 <- res$multiplicity_family_q80 <- res$multiplicity_family_q90 <- ""
  el <- res$model_tier == config$primary_tier & res$status == "COMPLETED" & is.finite(res$nominal_p)
  for (o in unique(res$outcome)) {
    for (m in unique(res$metric[el & res$outcome == o])) {
      i <- which(el & res$outcome == o & res$metric == m)
      res$q10[i] <- siw_bh(res$nominal_p[i]); res$multiplicity_family_q10[i] <- paste0("RAW_CUMULATIVE_q10_", o, "_", m)
    }
    i <- which(el & res$outcome == o)
    res$q80[i] <- siw_bh(res$nominal_p[i]); res$multiplicity_family_q80[i] <- paste0("RAW_CUMULATIVE_q80_", o)
  }
  i <- which(el & res$metric %in% config$representative_metrics)
  res$q90[i] <- siw_bh(res$nominal_p[i]); res$multiplicity_family_q90[i] <- "RAW_CUMULATIVE_q90_all_primary_pairs"
  res
}

siw_design_audit <- function(dat, config, tiers, tier_x) {
  do.call(rbind, lapply(tiers, function(t) {
    X <- tier_x[[t]]
    data.frame(model_tier = t, covariates = paste(config$tiers[[t]], collapse = "|"), design_columns = paste(colnames(X), collapse = "|"),
               n_columns_including_exposure = ncol(X) + 1L, factor_reference_levels = if ("Locality_3group" %in% config$tiers[[t]]) paste0("Locality_3group=", config$coding$locality_levels[1L]) else "",
               n_rows_complete_covariates = sum(stats::complete.cases(X)), stringsAsFactors = FALSE)
  }))
}
