# Phase 3C.2: DIAGNOSTIC-ONLY audit of the saved Batch C bootstrap estimates (declared in config/bootstrap_diagnostics_protocol.json).
# Reads: checkpoints/CKPT__<target_id>.rds and the run's aggregate tables (bootstrap_summary.csv, bootstrap_validity.csv, batch_c_metadata.json, RUN_MANIFEST.csv, source_manifest.csv).
# Never reads participant data, never reconstructs a sample, never refits a model, never writes into the audited run folder. Exports aggregate tables only (no replicate-level estimates).
# Coefficient-based flags are EXTREMENESS DIAGNOSTICS, not confirmed separation. Filtered limits are diagnostic only and never replace the original intervals.

.siw_ba_fields <- c("estimates", "reason", "glm_not_converged", "level_absent", "other_aliased", "n_complete", "seconds", "summary")
.siw_ba_summary_cols <- c("replicates_requested", "replicates_attempted", "replicates_successful", "replicates_failed", "estimate_median", "ci_lower", "ci_upper", "positive_fraction", "negative_fraction", "interval_excludes_null")
.siw_ba_defs <- c("X1_abs_gt_1e6", "X2_gt_100_se", "X3_gt_10_se", "X4_gt_10_mad", "X5_gt_1000_se")
.siw_ba_qprobs <- c(0.001, 0.01, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99, 0.999)

#' Load the declared diagnostic protocol (config/bootstrap_diagnostics_protocol.json).
#' @export
siw_bootstrap_audit_protocol <- function(path = NULL) {
  if (is.null(path)) { root <- siw_root(); cand <- c(file.path(root, "config", "bootstrap_diagnostics_protocol.json"), file.path(root, "extdata", "bootstrap_diagnostics_protocol.json"), file.path(root, "inst", "extdata", "bootstrap_diagnostics_protocol.json")); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_BOOTSTRAP_DIAGNOSTICS_PROTOCOL_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value; p
}

#' Extremeness flags of a vector of finite estimates (definitions X1-X5 of the declared protocol). Coefficient-based extremeness diagnostics, NOT confirmed separation.
#' @param e finite estimates; p primary coefficient; s reference SE
#' @return logical matrix (one column per definition); X4 is NA for every estimate when the scaled MAD is 0 (not evaluable)
#' @export
siw_bootstrap_extreme_flags <- function(e, p, s) {
  m <- stats::median(e); mad <- 1.4826 * stats::median(abs(e - m)); ok_s <- is.finite(p) && is.finite(s) && s > 0
  out <- cbind(X1_abs_gt_1e6 = abs(e) > 1e6, X2_gt_100_se = if (ok_s) abs(e - p) > 100 * s else rep(NA, length(e)), X3_gt_10_se = if (ok_s) abs(e - p) > 10 * s else rep(NA, length(e)),
               X4_gt_10_mad = if (is.finite(mad) && mad > 0) abs(e - m) > 10 * mad else rep(NA, length(e)), X5_gt_1000_se = if (ok_s) abs(e - p) > 1000 * s else rep(NA, length(e)))
  out
}

# Type-8 order-statistic position (same rule as the independent quantile): j = floor(n p + (p + 1)/3 + 4 eps)
.siw_ba_j8 <- function(n, p) as.integer(floor(n * p + (p + 1) / 3 + 4 * .Machine$double.eps))

#' Read and validate one saved checkpoint. Returns list(ok, issues, value). Schema whitelist: only the declared fields; the only data frame allowed is the one-row summary; lengths and classes must be consistent. Never fits or reads data.
#' @export
siw_bootstrap_audit_read_checkpoint <- function(path, target_id, signature, replicates) {
  bad <- function(msg, sig = FALSE) list(ok = FALSE, issues = msg, value = NULL, signature_ok = sig)
  if (!file.exists(path)) return(bad("missing"))
  obj <- tryCatch(readRDS(path), error = function(e) NULL); if (is.null(obj)) return(bad("unreadable"))
  if (!is.list(obj) || is.data.frame(obj) || !setequal(names(obj), c("signature", "target_id", "value"))) return(bad("unexpected top-level structure"))
  iss <- character(); sig_ok <- identical(obj$signature, signature)
  if (!identical(obj$target_id, target_id)) iss <- c(iss, "target id differs from file name")
  if (!identical(obj$signature, signature)) iss <- c(iss, "signature does not match the recomputed signature")
  v <- obj$value; if (!is.list(v) || is.data.frame(v)) return(bad(paste(c(iss, "value is not a list"), collapse = "; "), sig_ok))
  extra <- setdiff(names(v), .siw_ba_fields); if (length(extra)) iss <- c(iss, paste0("undeclared fields: ", paste(extra, collapse = ",")))
  miss <- setdiff(.siw_ba_fields, names(v)); if (length(miss)) return(bad(paste(c(iss, paste0("missing fields: ", paste(miss, collapse = ","))), collapse = "; "), sig_ok))
  dfs <- names(v)[vapply(v, is.data.frame, NA)]; if (!identical(dfs, "summary")) iss <- c(iss, "a data frame other than the summary is present")
  n <- length(v$estimates); if (!is.numeric(v$estimates)) iss <- c(iss, "estimates not numeric")
  if (n != replicates) iss <- c(iss, sprintf("length %d differs from the declared replicate count %d", n, replicates))
  if (!is.character(v$reason) || length(v$reason) != n) iss <- c(iss, "reason has the wrong class or length")
  for (k in c("glm_not_converged", "level_absent", "other_aliased")) if (!is.logical(v[[k]]) || length(v[[k]]) != n || anyNA(v[[k]])) iss <- c(iss, paste0(k, " has the wrong class, length or missing values"))
  if (!is.numeric(v$n_complete) || length(v$n_complete) != n) iss <- c(iss, "n_complete has the wrong class or length")
  if (!is.data.frame(v$summary) || nrow(v$summary) != 1L || !all(.siw_ba_summary_cols %in% names(v$summary))) iss <- c(iss, "summary is not a one-row data frame with the declared columns")
  if (!length(iss) && is.character(v$reason) && anyNA(v$reason)) iss <- c(iss, "reason has missing values")
  if (!length(iss) && any(is.finite(v$estimates) != !nzchar(v$reason))) iss <- c(iss, "estimate classes inconsistent with the failure reasons")
  list(ok = !length(iss), issues = paste(iss, collapse = "; "), value = if (!length(iss)) v else NULL, signature_ok = sig_ok)
}

#' Class counts of saved estimates: finite, NA, NaN, +Inf, -Inf (mutually exclusive; add up to the number attempted).
#' @export
siw_bootstrap_estimate_classes <- function(est) {
  nan <- is.nan(est); na <- is.na(est) & !nan
  c(n_attempted = length(est), n_finite = sum(is.finite(est)), n_na = sum(na), n_nan = sum(nan), n_pos_inf = sum(est == Inf, na.rm = TRUE), n_neg_inf = sum(est == -Inf, na.rm = TRUE))
}

.siw_ba_limits8 <- function(x) if (length(x) >= 20L) stats::quantile(x, c(0.025, 0.975), names = FALSE, type = 8) else c(NA_real_, NA_real_)

#' Field completeness of the audit's distribution and reconstruction tables. A missing value is EXPECTED only where the quantity is undefined by an explicit rule (no finite estimates, fewer than 2 for an SD, fewer than 20 for a percentile limit, no usable reference SE);
#' any other missing value is UNEXPECTED and fails verification (this is the check that would have caught the empty IQR columns of the first audit export).
#' @param target the distribution table; reconstruction the reconstruction table
#' @return data frame: table, column, n_rows, n_missing, n_expected_missing, n_unexpected_missing, rule
#' @export
siw_bootstrap_audit_field_completeness <- function(target, reconstruction) {
  one <- function(tab, nm, d, expected, rule) { v <- d[[nm]]; miss <- is.na(v); data.frame(table = tab, column = nm, n_rows = nrow(d), n_missing = sum(miss), n_expected_missing = sum(miss & expected), n_unexpected_missing = sum(miss & !expected), rule = rule, stringsAsFactors = FALSE) }
  n <- target$n_finite; badse <- !is.finite(target$reference_se) | target$reference_se <= 0; none <- n == 0L; rows <- list()
  stat <- c("median", "iqr_type7", "mad_raw", "mad_scaled", "minimum", "maximum", grep("^q0[.]", names(target), value = TRUE), "range_q975_minus_median", "range_median_minus_q025", "range_max_minus_q975", "range_q025_minus_min")
  for (nm in stat) rows[[length(rows) + 1L]] <- one("target_distribution", nm, target, none, "undefined only when no estimate is finite")
  rows[[length(rows) + 1L]] <- one("target_distribution", "run_sd_ratio", target, n < 2L, "the run's SD ratio is undefined only with fewer than 2 finite estimates")
  for (nm in c("sd", "sd_without_X2")) rows[[length(rows) + 1L]] <- one("target_distribution", nm, target, n < 2L, "undefined only with fewer than 2 finite estimates")
  for (nm in c("ratio_sd_to_se", "ratio_iqr_scaled_to_se", "ratio_mad_scaled_to_se", "ratio_q_width_to_wald_width")) rows[[length(rows) + 1L]] <- one("target_distribution", nm, target, none | badse | (nm == "ratio_sd_to_se" & n < 2L), "undefined only when no finite estimate, or the reference SE is missing or not positive (SD ratio: or fewer than 2 finite estimates)")
  for (nm in c("primary_coefficient", "reference_se", "n_attempted", "n_finite", "n_na", "n_nan", "n_pos_inf", "n_neg_inf")) rows[[length(rows) + 1L]] <- one("target_distribution", nm, target, rep(FALSE, nrow(target)), "never undefined: taken from the run's tables or counted")
  few <- reconstruction$n_finite < 20L
  for (nm in c("lower_reconstructed", "upper_reconstructed", "lower_independent", "upper_independent", "lower_checkpoint", "upper_checkpoint", "lower_run_summary", "upper_run_summary")) rows[[length(rows) + 1L]] <- one("quantile_reconstruction", nm, reconstruction, few, "undefined only with fewer than 20 finite estimates (original rule)")
  for (nm in c("median_reconstructed", "median_checkpoint", "median_run_summary")) rows[[length(rows) + 1L]] <- one("quantile_reconstruction", nm, reconstruction, reconstruction$n_finite == 0L, "undefined only when no estimate is finite")
  do.call(rbind, rows)
}

# one target: returns list(target, extreme, reconstruction, ranks, omission)
.siw_ba_target <- function(v, meta, run_row) {
  est <- v$estimates; cls <- siw_bootstrap_estimate_classes(est); e <- est[is.finite(est)]; n <- length(e); p <- meta$primary; s <- meta$reference_se
  q7 <- if (n) stats::quantile(e, .siw_ba_qprobs, names = FALSE, type = 7) else rep(NA_real_, length(.siw_ba_qprobs)); names(q7) <- paste0("q", format(.siw_ba_qprobs, scientific = FALSE, trim = TRUE))
  qq <- function(p) { i <- match(p, .siw_ba_qprobs); if (anyNA(i)) stop("SIW_AUDIT_UNDECLARED_QUANTILE_PROBABILITY", call. = FALSE); unname(q7[i]) }   # lookup by declared probability, never by name (exported column names keep the common-width form q0.250, q0.500, q0.750, ...)
  med <- if (n) stats::median(e) else NA_real_; madraw <- if (n) stats::median(abs(e - med)) else NA_real_; madsc <- 1.4826 * madraw; iqr <- if (n) unname(qq(0.75) - qq(0.25)) else NA_real_
  sdv <- if (n > 1L) stats::sd(e) else NA_real_; fl <- if (n) siw_bootstrap_extreme_flags(e, p, s) else matrix(NA, 0L, 5L, dimnames = list(NULL, .siw_ba_defs))
  x2 <- fl[, "X2_gt_100_se"]; sd_wo <- if (n && any(x2 %in% TRUE) && sum(!(x2 %in% TRUE)) > 1L) stats::sd(e[!(x2 %in% TRUE)]) else if (n && !any(x2 %in% TRUE)) sdv else NA_real_
  rr <- function(a, b) if (is.finite(a) && is.finite(b) && b > 0) a / b else NA_real_
  tg <- data.frame(result_id = meta$result_id, outcome_family = meta$family, binary_target = meta$family == "binomial", previously_flagged_review = meta$flagged, as.list(cls), median = med, iqr_type7 = iqr, mad_raw = madraw, mad_scaled = madsc, sd = sdv, sd_without_X2 = sd_wo,
    minimum = if (n) min(e) else NA_real_, maximum = if (n) max(e) else NA_real_, as.list(q7), range_q975_minus_median = unname(qq(0.975) - qq(0.5)), range_median_minus_q025 = unname(qq(0.5) - qq(0.025)), range_max_minus_q975 = if (n) max(e) - unname(qq(0.975)) else NA_real_,
    range_q025_minus_min = if (n) unname(qq(0.025)) - min(e) else NA_real_, primary_coefficient = p, reference_se = s, ratio_sd_to_se = rr(sdv, s), ratio_iqr_scaled_to_se = rr(iqr / 1.349, s), ratio_mad_scaled_to_se = rr(madsc, s),
    ratio_q_width_to_wald_width = rr(unname(qq(0.975) - qq(0.025)), 2 * 1.96 * s), run_sd_ratio = meta$run_sd_ratio, recomputed_sd_ratio_matches_run = is.finite(meta$run_sd_ratio) && is.finite(rr(sdv, s)) && abs(rr(sdv, s) - meta$run_sd_ratio) <= 1e-9 * max(1, abs(meta$run_sd_ratio)), stringsAsFactors = FALSE, check.names = FALSE)
  # extremeness
  ov <- function(flag, vec) sum(flag %in% TRUE & vec[is.finite(est)])
  ex <- do.call(rbind, lapply(.siw_ba_defs, function(d) { f <- fl[, d]; ev <- !all(is.na(f)) || n == 0L
    data.frame(result_id = meta$result_id, definition = d, evaluable = ev, n_finite = n, n_extreme = if (ev) sum(f %in% TRUE) else NA_integer_, proportion_extreme = if (ev && n) sum(f %in% TRUE) / n else NA_real_, n_extreme_low = if (ev) sum(f %in% TRUE & e < p, na.rm = TRUE) else NA_integer_, n_extreme_high = if (ev) sum(f %in% TRUE & e >= p, na.rm = TRUE) else NA_integer_,
      overlap_glm_not_converged = if (ev) ov(f, v$glm_not_converged) else NA_integer_, overlap_level_absent = if (ev) ov(f, v$level_absent) else NA_integer_, overlap_other_aliased = if (ev) ov(f, v$other_aliased) else NA_integer_, stringsAsFactors = FALSE) }))
  # percentile reconstruction (type 8)
  st <- v$summary; lim <- .siw_ba_limits8(e); ind <- if (n >= 20L) siw_quantile8(e, c(0.025, 0.975)) else c(NA_real_, NA_real_); tol <- 1e-12
  agree <- function(a, b) if (is.na(a) && is.na(b)) TRUE else if (is.na(a) || is.na(b)) FALSE else abs(a - b) <= tol * max(abs(a), abs(b), 1e-300) || abs(a - b) <= tol
  bit <- function(a, b) identical(as.numeric(a), as.numeric(b))
  rec <- data.frame(result_id = meta$result_id, n_finite = n, lower_reconstructed = lim[1L], upper_reconstructed = lim[2L], lower_independent = ind[1L], upper_independent = ind[2L], median_reconstructed = med, lower_checkpoint = st$ci_lower, upper_checkpoint = st$ci_upper, median_checkpoint = st$estimate_median,
    lower_run_summary = run_row$ci_lower, upper_run_summary = run_row$ci_upper, median_run_summary = run_row$estimate_median,
    agree_checkpoint = agree(lim[1L], st$ci_lower) && agree(lim[2L], st$ci_upper) && agree(med, st$estimate_median), agree_run_summary = agree(lim[1L], run_row$ci_lower) && agree(lim[2L], run_row$ci_upper) && agree(med, run_row$estimate_median), agree_independent = agree(lim[1L], ind[1L]) && agree(lim[2L], ind[2L]),
    bitwise_checkpoint = bit(lim[1L], st$ci_lower) && bit(lim[2L], st$ci_upper) && bit(med, st$estimate_median), bitwise_run_summary = bit(lim[1L], run_row$ci_lower) && bit(lim[2L], run_row$ci_upper) && bit(med, run_row$estimate_median), stringsAsFactors = FALSE)
  # ranks and omission (per definition)
  ord <- order(e); es <- e[ord]; jl <- .siw_ba_j8(n, 0.025); ju <- .siw_ba_j8(n, 0.975); ranks <- list(); om <- list()
  for (d in .siw_ba_defs) {
    f <- fl[, d]; evaluable <- !all(is.na(f)) && n > 0L; fs <- f[ord]
    if (!evaluable) { ranks[[d]] <- data.frame(result_id = meta$result_id, definition = d, evaluable = FALSE, n_finite = n, j_lower = jl, j_upper = ju, stringsAsFactors = FALSE); om[[d]] <- data.frame(result_id = meta$result_id, definition = d, evaluable = FALSE, n_omitted = NA_integer_, stringsAsFactors = FALSE); next }
    pos <- function(i) if (n >= 20L && i >= 1L && i <= n) fs[i] %in% TRUE else NA
    lowset <- seq_len(min(n, jl + 1L)); upset <- if (ju <= n) ju:n else integer()
    ranks[[d]] <- data.frame(result_id = meta$result_id, definition = d, evaluable = TRUE, n_finite = n, j_lower = jl, j_upper = ju, n_flagged = sum(fs %in% TRUE), n_flagged_below = sum(fs[es < p] %in% TRUE), n_flagged_above = sum(fs[es >= p] %in% TRUE), n_in_lower_tail_set = length(lowset), n_flagged_in_lower_tail_set = sum(fs[lowset] %in% TRUE),
      n_in_upper_tail_set = length(upset), n_flagged_in_upper_tail_set = sum(fs[upset] %in% TRUE), lower_defining_flagged = pos(jl) %in% TRUE || pos(jl + 1L) %in% TRUE, upper_defining_flagged = pos(ju) %in% TRUE || pos(ju + 1L) %in% TRUE,
      share_flagged_in_tail_sets = if (length(lowset) + length(upset)) (sum(fs[lowset] %in% TRUE) + sum(fs[upset] %in% TRUE)) / (length(lowset) + length(upset)) else NA_real_, stringsAsFactors = FALSE)
    keep <- e[!(f %in% TRUE)]; k <- sum(f %in% TRUE); fl8 <- .siw_ba_limits8(keep); w <- lim[2L] - lim[1L]; ev_ok <- is.finite(fl8[1L]) && is.finite(lim[1L])
    om[[d]] <- data.frame(result_id = meta$result_id, definition = d, evaluable = TRUE, n_omitted = k, n_remaining = length(keep), original_lower = lim[1L], original_upper = lim[2L], filtered_lower = fl8[1L], filtered_upper = fl8[2L],
      change_lower = if (ev_ok) fl8[1L] - lim[1L] else NA_real_, change_upper = if (ev_ok) fl8[2L] - lim[2L] else NA_real_, relative_change_lower = if (ev_ok && is.finite(w) && w > 0) (fl8[1L] - lim[1L]) / w else NA_real_, relative_change_upper = if (ev_ok && is.finite(w) && w > 0) (fl8[2L] - lim[2L]) / w else NA_real_,
      change_lower_in_se_units = if (ev_ok && is.finite(s) && s > 0) (fl8[1L] - lim[1L]) / s else NA_real_, change_upper_in_se_units = if (ev_ok && is.finite(s) && s > 0) (fl8[2L] - lim[2L]) / s else NA_real_, width_ratio_filtered_to_original = if (ev_ok && is.finite(w) && w > 0) (fl8[2L] - fl8[1L]) / w else NA_real_,
      original_excludes_zero = is.finite(lim[1L]) && (lim[1L] > 0 || lim[2L] < 0), filtered_excludes_zero = is.finite(fl8[1L]) && (fl8[1L] > 0 || fl8[2L] < 0), stringsAsFactors = FALSE)
    om[[d]]$zero_inclusion_changes <- om[[d]]$original_excludes_zero != om[[d]]$filtered_excludes_zero
  }
  list(target = tg, extreme = ex, reconstruction = rec, ranks = siw_bind_rows(ranks), omission = siw_bind_rows(om))
}

#' Audit the saved bootstrap estimates of a Batch C run (diagnostic only; reads checkpoints and aggregate tables, writes only to a NEW output folder).
#' @param run_dir the audited Batch C run folder (read only)
#' @param out_dir new output folder (must not exist or be empty)
#' @return list(out_dir, statuses, tables, status)
#' @export
siw_run_bootstrap_audit <- function(run_dir, out_dir, protocol = siw_bootstrap_audit_protocol()) {
  t0 <- proc.time()[["elapsed"]]; run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE); ck_dir <- file.path(run_dir, "checkpoints")
  if (!dir.exists(ck_dir)) stop("SIW_AUDIT_NO_CHECKPOINT_FOLDER", call. = FALSE)
  if (dir.exists(out_dir) && length(list.files(out_dir, all.files = TRUE, no.. = TRUE))) stop("SIW_AUDIT_OUTPUT_FOLDER_NOT_EMPTY", call. = FALSE)
  if (startsWith(normalizePath(out_dir, winslash = "/", mustWork = FALSE), paste0(run_dir, "/"))) stop("SIW_AUDIT_OUTPUT_INSIDE_AUDITED_RUN", call. = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); out_dir <- normalizePath(out_dir, winslash = "/")
  rd <- function(f) { p <- file.path(run_dir, f); if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) else NULL }
  S <- rd("bootstrap_summary.csv"); Y <- rd("bootstrap_validity.csv"); mp <- file.path(run_dir, "batch_c_metadata.json"); meta <- if (file.exists(mp)) jsonlite::fromJSON(mp, simplifyVector = FALSE) else NULL
  if (is.null(S) || is.null(Y) || is.null(meta)) stop("SIW_AUDIT_RUN_TABLES_MISSING", call. = FALSE)
  sm0 <- siw_source_manifest(); files <- sort(list.files(ck_dir, full.names = FALSE)); ck_files <- paste0("CKPT__", S$result_id, ".rds"); expected_n <- as.integer(protocol$scope$targets)
  siw_log("bootstrap audit: ", nrow(S), " targets; checkpoint files found ", length(files)); hash_before <- if (length(files)) .siw_hash_many(file.path(ck_dir, files)) else character(); bytes_before <- as.numeric(file.info(file.path(ck_dir, files))$size)
  R <- as.integer(meta$replicates_requested); parts <- c(meta$input_hash, meta$config_hash, meta$protocol_hash, meta$source_fingerprint_sha256)
  yi <- match(S$result_id, Y$result_id); T <- list(); X <- list(); Q <- list(); K <- list(); O <- list(); prov <- list()
  for (i in seq_len(nrow(S))) {
    id <- S$result_id[i]; sig <- .siw_ckpt_signature(c(parts, id, S$seed[i], R)); chk <- siw_bootstrap_audit_read_checkpoint(file.path(ck_dir, paste0("CKPT__", id, ".rds")), id, sig, R)
    prov[[i]] <- data.frame(result_id = id, checkpoint_file = paste0("CKPT__", id, ".rds"), present = file.exists(file.path(ck_dir, paste0("CKPT__", id, ".rds"))), schema_and_content_valid = chk$ok, signature_matches = isTRUE(chk$signature_ok), issues = chk$issues, accounting_ok = FALSE, stringsAsFactors = FALSE)
    if (!chk$ok) next
    j <- yi[i]; m <- list(result_id = id, family = S$outcome_family[i], flagged = !is.na(j) && identical(Y$validity_status[j], "REVIEW"), primary = if (is.na(j)) NA_real_ else Y$primary_coefficient[j], reference_se = if (is.na(j)) NA_real_ else Y$reference_se[j], run_sd_ratio = if (is.na(j)) NA_real_ else Y$bootstrap_sd_to_reference_se[j])
    r <- .siw_ba_target(chk$value, m, S[i, ]); T[[id]] <- r$target; X[[id]] <- r$extreme; Q[[id]] <- r$reconstruction; K[[id]] <- r$ranks; O[[id]] <- r$omission
    cls <- siw_bootstrap_estimate_classes(chk$value$estimates); prov[[i]]$accounting_ok <- sum(cls[-1L]) == cls[["n_attempted"]] && cls[["n_finite"]] == S$replicates_successful[i] && cls[["n_attempted"]] - cls[["n_finite"]] == S$replicates_failed[i] && cls[["n_attempted"]] == S$replicates_attempted[i]
  }
  PV <- do.call(rbind, prov)
  hash_after <- if (length(files)) .siw_hash_many(file.path(ck_dir, files)) else character(); bytes_after <- as.numeric(file.info(file.path(ck_dir, files))$size)
  FH <- data.frame(checkpoint_file = files, bytes_before = bytes_before, sha256_before = hash_before, bytes_after = bytes_after, sha256_after = hash_after, unchanged = hash_before == hash_after & bytes_before == bytes_after, listed_in_run_summary = files %in% ck_files, stringsAsFactors = FALSE)
  # original run provenance
  mf <- rd("RUN_MANIFEST.csv"); man_bad <- if (is.null(mf)) "RUN_MANIFEST.csv missing" else { b <- character(); for (k in seq_len(nrow(mf))) { pth <- file.path(run_dir, mf$file[k]); if (!file.exists(pth)) b <- c(b, paste0(mf$file[k], ": missing")) else if (file.info(pth)$size != mf$bytes[k] || !identical(toupper(siw_file_hash(pth)$value), toupper(mf$hash[k]))) b <- c(b, paste0(mf$file[k], ": differs")) }; b }
  need <- c("input_hash", "config_hash", "protocol_hash", "source_fingerprint_sha256", "replicates_requested", "seed", "rng"); meta_missing <- need[!vapply(need, function(k) !is.null(meta[[k]]) && length(meta[[k]]) > 0L, NA)]
  sm1 <- siw_source_manifest(); tool_diff <- siw_source_manifest_diff(sm0, sm1); om_run <- rd("source_manifest.csv"); orig_diff <- if (is.null(om_run)) NULL else siw_source_manifest_diff(list(manifest = om_run, fingerprint = meta$source_fingerprint_sha256), sm0)
  # tables
  tt <- siw_bind_rows(T); xt <- siw_bind_rows(X); qt <- siw_bind_rows(Q); kt <- siw_bind_rows(K); ot <- siw_bind_rows(O)
  scope <- data.frame(result_id = S$result_id, outcome_family = S$outcome_family, binary_target = S$outcome_family == "binomial", previously_flagged_review = !is.na(yi) & Y$validity_status[yi] %in% "REVIEW", resampling_type = S$resampling_type, stringsAsFactors = FALSE)
  nb <- sum(scope$binary_target); nf <- sum(scope$previously_flagged_review); scope_notes <- c(if (nrow(S) != expected_n) sprintf("targets: %d (expected %d)", nrow(S), expected_n), if (nb != protocol$scope$binary_targets_expected) sprintf("binary targets: %d (expected %d)", nb, protocol$scope$binary_targets_expected), if (nf != protocol$scope$previously_flagged_targets_expected) sprintf("previously flagged targets: %d (expected %d)", nf, protocol$scope$previously_flagged_targets_expected))
  unexpected <- setdiff(files, ck_files); integ_ok <- nrow(S) == expected_n && all(PV$present) && all(PV$schema_and_content_valid) && all(PV$signature_matches) && !length(unexpected) && !anyDuplicated(S$result_id)
  fc <- if (nrow(tt) && nrow(qt)) siw_bootstrap_audit_field_completeness(tt, qt) else data.frame(n_unexpected_missing = 1L)
  st <- c(EXECUTION_STATUS = "PASS", CHECKPOINT_INTEGRITY_STATUS = if (integ_ok) "PASS" else "FAIL", CHECKPOINTS_UNCHANGED_STATUS = if (length(files) && all(FH$unchanged)) "PASS" else "FAIL",
    ACCOUNTING_STATUS = if (nrow(PV) == expected_n && all(PV$accounting_ok)) "PASS" else "FAIL", QUANTILE_RECONSTRUCTION_STATUS = if (nrow(qt) == expected_n && all(qt$agree_checkpoint) && all(qt$agree_run_summary) && all(qt$agree_independent)) "PASS" else "FAIL",
    ORIGINAL_RUN_PROVENANCE_STATUS = if (!length(man_bad) && !length(meta_missing)) "PASS" else "FAIL", SOURCE_INTEGRITY_STATUS = if (tool_diff$identical_fingerprint) "PASS" else "FAIL", OUTPUT_COMPLETENESS_STATUS = if (nrow(tt) == expected_n && nrow(qt) == expected_n && !any(fc$n_unexpected_missing > 0L)) "PASS" else "FAIL")
  xs <- if (nrow(xt)) do.call(rbind, lapply(.siw_ba_defs, function(d) { z <- xt[xt$definition == d, ]; data.frame(definition = d, targets_evaluable = sum(z$evaluable), targets_with_extreme = sum(z$n_extreme > 0, na.rm = TRUE), binary_targets_with_extreme = sum(z$n_extreme > 0 & z$result_id %in% scope$result_id[scope$binary_target], na.rm = TRUE),
      flagged_targets_with_extreme = sum(z$n_extreme > 0 & z$result_id %in% scope$result_id[scope$previously_flagged_review], na.rm = TRUE), total_extreme_estimates = sum(z$n_extreme, na.rm = TRUE), max_proportion_extreme = suppressWarnings(max(z$proportion_extreme, na.rm = TRUE)), stringsAsFactors = FALSE) })) else data.frame()
  defs <- data.frame(id = vapply(protocol$extremeness_diagnostics$definitions, function(z) z$id, ""), rule = vapply(protocol$extremeness_diagnostics$definitions, function(z) z$rule, ""), note = vapply(protocol$extremeness_diagnostics$definitions, function(z) z$note, ""), stringsAsFactors = FALSE)
  W <- function(x, f) siw_write_csv(x, file.path(out_dir, f)); W(scope, "audit_scope.csv"); W(tt, "audit_target_distribution.csv"); W(xt, "audit_extremeness_by_target.csv"); W(xs, "audit_extremeness_by_definition.csv"); W(qt, "audit_quantile_reconstruction.csv"); W(kt, "audit_tail_ranks.csv"); W(ot, "audit_omission_sensitivity_DIAGNOSTIC_ONLY.csv")
  W(fc, "audit_field_completeness.csv"); W(PV, "audit_checkpoint_checks.csv"); W(FH, "audit_checkpoint_hashes.csv"); W(defs, "audit_definitions.csv"); W(sm0$manifest, "audit_tool_source_manifest.csv")
  if (!is.null(orig_diff)) W(data.frame(kind = c(rep("changed", length(orig_diff$changed)), rep("added_in_tool", length(orig_diff$added)), rep("removed_from_tool", length(orig_diff$removed))), file = c(orig_diff$changed, orig_diff$added, orig_diff$removed), stringsAsFactors = FALSE), "audit_source_difference_original_vs_tool.csv")
  gaps <- unlist(protocol$provenance$known_unavailable_evidence)
  pj <- list(tool = "bootstrap checkpoint audit (diagnostic only)", package_version = siw_pkg_version(), r_version = R.version.string, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), diagnostics_protocol_hash = protocol$protocol_hash, audited_run = basename(run_dir), checkpoints = length(files), original_run = list(input_hash = meta$input_hash, config_hash = meta$config_hash, protocol_hash = meta$protocol_hash, source_fingerprint_sha256 = meta$source_fingerprint_sha256, source_files = meta$source_files, replicates_requested = meta$replicates_requested, seed = meta$seed, rng = meta$rng,
      package_version = meta$package_version, created_utc = meta$created_utc, manifest_problems = man_bad, metadata_fields_missing = meta_missing, original_source_manifest_available = !is.null(om_run), files_differing_from_audit_tool_source = if (is.null(orig_diff)) NA else length(orig_diff$changed) + length(orig_diff$added) + length(orig_diff$removed)),
    tool_source = list(fingerprint_start = sm0$fingerprint, fingerprint_end = sm1$fingerprint, files = sm0$n_files, changed_during_audit = !tool_diff$identical_fingerprint), statuses = as.list(st), scope_notes = scope_notes, provenance_evidence_unavailable = gaps)
  writeLines(jsonlite::toJSON(pj, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"), file.path(out_dir, "audit_provenance.json"))
  writeLines(c(paste0(names(st), "=", st), paste0("TARGETS=", nrow(S)), paste0("BINARY_TARGETS=", nb), paste0("PREVIOUSLY_FLAGGED_TARGETS=", nf), paste0("TARGETS_WITH_X2_EXTREME=", if (nrow(xs)) xs$targets_with_extreme[xs$definition == "X2_gt_100_se"] else NA)), file.path(out_dir, "BOOTSTRAP_DIAGNOSTICS_STATUS.txt"))
  gp <- function(d) if (nrow(xs)) xs[xs$definition == d, ] else NULL; x2 <- gp("X2_gt_100_se")
  L <- c("# Bootstrap checkpoint audit (diagnostic only)", "", paste0("* Audited run: `", basename(run_dir), "`; checkpoints: ", length(files), "; targets: ", nrow(S), " (binary ", nb, "; previously flagged ", nf, ")"), paste0("* Diagnostic protocol hash: `", protocol$protocol_hash, "`; tool source fingerprint: `", sm0$fingerprint, "` (", sm0$n_files, " files)"),
    paste0("* Original run: input hash `", meta$input_hash, "`; source fingerprint `", meta$source_fingerprint_sha256, "`"), "", "## Statuses (separate)", "", paste0("* ", names(st), ": ", st), "", "## Extremeness diagnostics (coefficient-based; NOT confirmed separation)", "",
    if (nrow(xs)) c("| definition | targets with extreme | binary | previously flagged | extreme estimates |", "|---|---|---|---|---|", paste0("| ", xs$definition, " | ", xs$targets_with_extreme, " | ", xs$binary_targets_with_extreme, " | ", xs$flagged_targets_with_extreme, " | ", xs$total_extreme_estimates, " |")) else "_none_", "",
    if (length(scope_notes)) c("## Scope notes", "", paste0("* ", scope_notes), "") else NULL, "## Provenance evidence that is unavailable", "", paste0("* ", gaps), "",
    "## Notes", "", "* The audit reads saved estimates only; no participant data were read, no sample reconstructed, no model refitted. Filtered limits are diagnostic only and never replace the original intervals; no flag, interval or acceptance criterion of the original run was changed.", "* Aggregate outputs only: no replicate-level estimate is exported.")
  writeLines(L, file.path(out_dir, "RUN_SUMMARY.md"))
  list(out_dir = out_dir, statuses = as.list(st), tables = list(target = tt, extreme = xt, reconstruction = qt, ranks = kt, omission = ot, scope = scope, checks = PV, hashes = FH, completeness = fc), status = if (all(st == "PASS")) "OK" else "CHECKS_FAILED", seconds = proc.time()[["elapsed"]] - t0)
}
