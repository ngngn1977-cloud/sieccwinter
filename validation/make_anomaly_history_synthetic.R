# Builds FULLY SYNTHETIC input for the tests of the anomaly / alternative-history run (Phase 3I): a data CSV, a frozen-format reference folder (four tables) and a matching protocol copy.
# Usage: Rscript validation/make_anomaly_history_synthetic.R <out_dir> <variant> [<repo_root>]
#   variants: exact | anom_diff (one ANOM p value x 1.001) | alt_diff (one ALT p value x 1.001) | pca_sign (the sign of one PC effect flipped) | missing_ref (alternative_PCA_variance.csv deleted) | dup_key (one ANOM key duplicated) | broken_data
# The reference is built from the package's own output (circular by construction): it tests the run, the comparison and the status handling, not the statistics. The function make_ah_synthetic(out, variant, root) can also be sourced by the tests.
# synthetic only: every day has wet and dry participants, so no rainfall feature is constant, and every weather value gets participant-level noise so that the same-day residuals are not rounding noise (the original aborts on a rank-deficient model; the package records NOT_ESTIMABLE); one missing rainfall total at lag 35 as in the study data
ah_rich_rain <- function(raw, seed = 5L) { set.seed(seed); n <- nrow(raw); for (l in 1:59) { wet <- stats::rbinom(n, 1L, 0.45); raw[[sprintf("IMS_IDW_RAIN_DAYS1_L%02d", l)]] <- wet; tot <- wet * stats::rexp(n, 0.25); if (l == 35L) tot[3L] <- NA_real_; raw[[sprintf("IMS_IDW_RAIN_TOTAL_L%02d", l)]] <- tot; for (pf in c("UTCI", "MRT", "T2M", "RH", "IMS_IDW_TD_MEAN", "IMS_IDW_RH_MEAN")) { cn <- sprintf("%s_L%02d", pf, l); raw[[cn]] <- raw[[cn]] + stats::rnorm(n, 0, 0.7) } }; raw }
make_ah_synthetic <- function(out, variant = "exact", root, windows = c(1L, 7L, 35L, 59L), seed = 21L, n = 55L) {
  source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root); CFG <- siw_load_config(); PD <- siw_ah_protocol(); unlink(out, recursive = TRUE); dir.create(out, recursive = TRUE)
  raw <- ah_rich_rain(siw_make_synthetic(n = n, seed = seed, config = CFG)); csv <- file.path(out, "syn.csv"); utils::write.csv(raw, csv, row.names = FALSE, na = ""); dat <- siw_prepare_data(raw, CFG)
  sc <- siw_anomaly_scan(dat, CFG, PD, windows = windows); al <- siw_alt_scan(dat, CFG, PD); A <- sc$results; L <- al$results
  if (variant == "anom_diff") A$nominal_p[5] <- A$nominal_p[5] * 1.001; if (variant == "alt_diff") L$p[3] <- L$p[3] * 1.001
  if (variant == "pca_sign") { i <- which(L$method == "trajectory_PCA" & is.finite(L$effect))[1L]; L$effect[i] <- -L$effect[i] }
  refdir <- file.path(out, "ref"); dir.create(refdir); Wr <- function(x, f) utils::write.csv(x, file.path(refdir, f), row.names = FALSE, na = "")
  Wr(A, "daily_anomaly_results.csv"); Wr(L, "alternative_method_results.csv"); Wr(sc$diagnostics, "daily_anomaly_diagnostics.csv"); Wr(al$pca_variance, "alternative_PCA_variance.csv")
  p <- PD; tabs <- list()
  for (i in seq_along(p$reference_files)) { f <- file.path(refdir, p$reference_files[[i]]$file); p$reference_files[[i]]$sha256 <- siw_file_hash(f)$value; p$reference_files[[i]]$bytes <- file.info(f)$size; tabs[[p$reference_files[[i]]$component]] <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")); p$reference_files[[i]]$expected_records <- nrow(tabs[[p$reference_files[[i]]$component]])
    if (!is.null(p$reference_files[[i]]$expected_grid)) p$reference_files[[i]]$expected_grid$per_outcome <- nrow(tabs[[p$reference_files[[i]]$component]]) / 3L }
  for (i in seq_along(p$comparisons)) { cc <- p$comparisons[[i]]; d <- tabs[[cc$reference]]; p$comparisons[[i]]$expected_records <- nrow(d); p$comparisons[[i]]$expected_compared_cells <- sum(vapply(unlist(cc$fields), function(f) sum(is.finite(suppressWarnings(as.numeric(d[[f]])))), 0L)) }
  p$parameters$windows_subset <- as.list(windows); p$parameters$independent_refit_every_anom <- 1L; p$protocol_path <- NULL; p$protocol_hash <- NULL; jsonlite::write_json(p, file.path(out, "protocol_syn.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  if (variant == "missing_ref") unlink(file.path(refdir, "alternative_PCA_variance.csv"))
  if (variant == "dup_key") { f <- file.path(refdir, "daily_anomaly_results.csv"); d <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")); d <- rbind(d, d[1L, ]); utils::write.csv(d, f, row.names = FALSE, na = ""); q <- jsonlite::fromJSON(file.path(out, "protocol_syn.json"), simplifyVector = FALSE)
    q$reference_files[[1L]]$sha256 <- siw_file_hash(f)$value; q$reference_files[[1L]]$bytes <- file.info(f)$size; q$reference_files[[1L]]$expected_records <- nrow(d); q$reference_files[[1L]]$expected_grid$per_outcome <- NULL; q$reference_files[[1L]]$expected_grid <- NULL; jsonlite::write_json(q, file.path(out, "protocol_syn.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA) }
  if (variant == "broken_data") writeLines(c("not,a,study,file", "1,2"), csv)
  invisible(list(csv = csv, ref = refdir, protocol = file.path(out, "protocol_syn.json"), windows = windows))
}
if (!interactive() && length(grep("^--file=.*make_anomaly_history_synthetic[.]R$", commandArgs(FALSE)))) {
  args <- commandArgs(trailingOnly = TRUE); fa <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]); root <- if (length(args) >= 3L) args[3L] else normalizePath(file.path(dirname(fa), ".."), winslash = "/")
  r <- make_ah_synthetic(args[1L], args[2L], root); cat("built", args[2L], "in", args[1L], "\n")
}
