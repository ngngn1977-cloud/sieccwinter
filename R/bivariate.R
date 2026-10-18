# Batch A: crude (bivariate) weather-outcome associations across the cumulative windows: 3 outcomes x 8 metrics x 10 windows = 240 tests.
# Original rules (WINTER_SOURCE R/060_xy.R siecc_correlation_set, siecc_xy_scan_cumulative):
#   * only participants with finite outcome AND exposure enter a test; fewer than 4 pairs, or a constant variable, gives NA;
#   * Pearson correlation (point-biserial for the binary outcome: Pearson on the 0/1 code), p from stats::cor.test(method = "pearson");
#   * Spearman rho with stats::cor.test(method = "spearman", exact = FALSE);
#   * q_model_* use the M0 (exposure-only) regression p-value, which is the M0 row of the validated cumulative grid (HC3 t for continuous outcomes, Wald z for binary);
#   * Benjamini-Hochberg families: global (240), within outcome (80), within outcome-metric (10), separately for Pearson, Spearman and model p.
# The legacy output also carries Kendall tau and the M0 regression columns; Kendall is not documented in the SI (Table S6 lists Pearson/point-biserial and Spearman) and is not produced here.

#' One crude association (n, Pearson, Spearman) under the original missing-data and testing rules.
#' @export
siw_correlation_set <- function(x, y) {
  keep <- is.finite(x) & is.finite(y); x <- as.numeric(x[keep]); y <- as.numeric(y[keep])
  if (length(x) < 4L || length(unique(x)) < 2L || length(unique(y)) < 2L) return(c(n = length(x), pearson_r = NA_real_, pearson_p = NA_real_, spearman_rho = NA_real_, spearman_p = NA_real_))
  p <- suppressWarnings(stats::cor.test(x, y, method = "pearson")); s <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
  c(n = length(x), pearson_r = unname(p$estimate), pearson_p = p$p.value, spearman_rho = unname(s$estimate), spearman_p = s$p.value)
}

#' The 240 cumulative bivariate tests with the documented descriptive multiplicity families.
#' @param m0 M0 rows of the cumulative grid results (data frame with outcome, metric, window_or_lag, model_tier, nominal_p). Computed when NULL.
#' @export
siw_bivariate_grid <- function(dat, config, m0 = NULL) {
  if (is.null(m0)) { m0 <- siw_cumulative_grid(dat, config, tiers = "M0")$results }
  m0 <- m0[m0$model_tier == "M0", ]; mk <- paste(m0$outcome, m0$metric, m0$window_or_lag)
  rows <- list()
  for (oi in seq_len(nrow(config$outcomes))) {
    o <- config$outcomes[oi, ]; y <- as.numeric(dat[[o$outcome]])
    for (mi in seq_len(nrow(config$metrics))) {
      sp <- config$metrics[mi, ]
      for (w in config$windows) {
        f <- siw_cum_name(sp$cumulative_prefix, w); cs <- siw_correlation_set(as.numeric(dat[[f]]), y)
        rows[[length(rows) + 1L]] <- data.frame(result_id = paste("BIV", o$code, sp$code, paste0("W", w), sep = "__"), outcome = o$outcome, outcome_family = o$family, metric = sp$metric,
          exposure_field = f, window_or_lag = as.integer(w), correlation_type = if (o$family == "binomial") "point_biserial" else "pearson", n = as.integer(cs[["n"]]),
          pearson_or_point_biserial_r = cs[["pearson_r"]], pearson_p = cs[["pearson_p"]], spearman_rho = cs[["spearman_rho"]], spearman_p = cs[["spearman_p"]],
          model_p = m0$nominal_p[match(paste(o$outcome, sp$metric, w), mk)], status = if (is.finite(cs[["pearson_p"]])) "COMPLETED" else "NOT_ESTIMABLE", stringsAsFactors = FALSE)
      }
    }
  }
  out <- do.call(rbind, rows)
  for (nm in c("pearson", "spearman", "model")) {
    p <- out[[paste0(nm, "_p")]]
    out[[paste0("q_", nm, "_global")]] <- siw_bh(p)
    out[[paste0("q_", nm, "_outcome")]] <- NA_real_; out[[paste0("q_", nm, "_outcome_metric")]] <- NA_real_
    for (o in unique(out$outcome)) { i <- which(out$outcome == o); out[[paste0("q_", nm, "_outcome")]][i] <- siw_bh(p[i]) }
    for (o in unique(out$outcome)) for (m in unique(out$metric)) { i <- which(out$outcome == o & out$metric == m); out[[paste0("q_", nm, "_outcome_metric")]][i] <- siw_bh(p[i]) }
  }
  rownames(out) <- NULL; out
}
