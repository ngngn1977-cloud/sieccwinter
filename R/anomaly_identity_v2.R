# ANOM identity, protocol v2 (post hoc; declared in config/anomaly_history_validation_protocol_v2.json before any study run under it).
# Each COMPLETED gaussian row's implied SD (coefficient x exposure_sd / standardised_effect) is compared with the directly measured SD of the outcome over that
# row's own complete-case sample (the traced mask of validation/anom_sample_sd_diag.R), at the unchanged 1e-9 relative tolerance. The v1 by-n row is still
# computed elsewhere (unchanged code) and reported as superseded. This file adds code only; no existing function is changed.

SIW_AH_V1_IDENTITY_CHECK <- "gaussian standardised_effect: implied SD(outcome) constant within each outcome"
SIW_AH_V2_KEYS <- c("outcome", "metric", "reference_definition", "anomaly_method", "burden_direction", "window_or_lag")
SIW_AH_V2_TOL <- 1e-9

#' Load the ANOM protocol v2. With strict = TRUE it fails closed unless the v1 protocol it extends is byte-identical to the one in use.
#' @param v1_hash SHA-256 of the v1 protocol in use (siw_ah_protocol()$protocol_hash)
#' @param strict FALSE only for synthetic runs that use a protocol copy; the result then records that the check was not applicable
#' @export
siw_ah_protocol_v2 <- function(path = NULL, v1_hash = siw_ah_protocol()$protocol_hash, strict = TRUE) {
  if (is.null(path)) { root <- siw_root(); cand <- file.path(root, c("config", "extdata", "inst/extdata"), "anomaly_history_validation_protocol_v2.json"); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_AH_PROTOCOL_V2_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value
  same <- identical(toupper(as.character(p$extends_protocol$sha256)), toupper(as.character(v1_hash)))
  pub <- identical(as.character(p$extends_protocol$sha256), SIW_REDACTED)            # public build: the declared hashes are held in the private validation kit
  if (strict && !same && !pub) stop("SIW_AH_PROTOCOL_V2_EXTENDS_A_DIFFERENT_V1: v2 declares ", p$extends_protocol$sha256, "; v1 in use ", v1_hash, call. = FALSE)
  if (!identical(as.numeric(p$rule_v2$tolerance), SIW_AH_V2_TOL)) stop("SIW_AH_PROTOCOL_V2_TOLERANCE_NOT_AS_IMPLEMENTED", call. = FALSE)
  p$extends_check <- if (same) "PASS" else if (pub) "NOT_APPLICABLE_PUBLIC_BUILD" else "NOT_APPLICABLE_SYNTHETIC_PROTOCOL_COPY"
  p
}

#' Traced complete-case masks of the gaussian ANOM rows (rule of asd_masks in validation/anom_sample_sd_diag.R): mask = tier_ok & finite outcome & finite feature.
#' @param keys data frame with the key columns (outcome, metric, reference_definition, anomaly_method, burden_direction, window_or_lag); only gaussian outcomes are traced
#' @return one row per distinct gaussian key: the keys, n_mask and direct_sd (aggregate only; no mask or membership is returned)
#' @export
siw_anom_traced_sd <- function(dat, config, protocol, keys) {
  P <- protocol$parameters; tier_ok <- stats::complete.cases(siw_tier_matrix(dat, config$tiers[[P$tier]]))
  gau <- config$outcomes$outcome[config$outcomes$family == "gaussian"]
  k <- unique(keys[as.character(keys$outcome) %in% gau, SIW_AH_V2_KEYS, drop = FALSE]); k$window_or_lag <- as.integer(k$window_or_lag)
  out <- list()
  for (ref in unique(as.character(k$reference_definition))) {
    vals <- siw_anomaly_values(dat, config, protocol, ref); kr <- k[k$reference_definition == ref, , drop = FALSE]
    for (metric in unique(as.character(kr$metric))) for (method in unique(as.character(kr$anomaly_method[kr$metric == metric]))) {
      A <- vals$metrics[[metric]][[method]]; if (is.null(A)) next
      Fe <- siw_anomaly_features(A, P$min_available_fraction); km <- kr[kr$metric == metric & kr$anomaly_method == method, , drop = FALSE]
      nm <- integer(nrow(km)); sdv <- numeric(nrow(km))
      for (i in seq_len(nrow(km))) {
        x <- Fe[[as.character(km$burden_direction[i])]][, km$window_or_lag[i]]; y <- as.numeric(dat[[as.character(km$outcome[i])]]); m <- tier_ok & is.finite(y) & is.finite(x)
        nm[i] <- sum(m); sdv[i] <- if (nm[i] >= 2L) stats::sd(y[m]) else NA_real_
      }
      km$n_mask <- nm; km$direct_sd <- sdv; out[[length(out) + 1L]] <- km
    }
  }
  r <- if (length(out)) do.call(rbind, out) else cbind(k[0, ], n_mask = integer(), direct_sd = numeric()); rownames(r) <- NULL; r
}

#' Rule v2 on one ANOM results table (package or frozen): every COMPLETED gaussian row's implied SD against the traced direct SD of its own complete-case sample.
#' @param traced output of siw_anom_traced_sd
#' @return list(rows = per-row table, checks = three check rows in the identity format: mask present, mask size = recorded n, implied SD = direct SD)
#' @export
siw_anom_identity_v2 <- function(res, traced, tol = SIW_AH_V2_TOL) {
  e <- res[as.character(res$status) == "COMPLETED" & as.character(res$outcome_family) == "gaussian", , drop = FALSE]
  key <- function(d) do.call(paste, c(lapply(d[SIW_AH_V2_KEYS], function(z) as.character(if (is.numeric(z)) as.integer(z) else z)), sep = "|"))
  j <- if (nrow(e)) match(key(e), key(traced)) else integer()
  implied <- as.numeric(e$coefficient) * as.numeric(e$exposure_sd) / as.numeric(e$standardised_effect); direct <- traced$direct_sd[j]; nm <- traced$n_mask[j]
  d <- abs(implied - direct) / pmax(abs(direct), 1e-300); z <- is.finite(implied) & is.finite(direct) & abs(implied - direct) <= 1e-300; d[z] <- 0
  found <- !is.na(j); size_ok <- found & !is.na(nm) & nm == as.integer(e$n); sd_ok <- found & is.finite(d) & d <= tol
  rows <- data.frame(e[SIW_AH_V2_KEYS], n = as.integer(e$n), n_mask = nm, implied_sd = implied, direct_sd = direct, rel_diff = d, mask_found = found, status = ifelse(found & size_ok & sd_ok, "PASS", "FAIL"), stringsAsFactors = FALSE)
  st <- function(ok) if (!nrow(e)) "NOT_TESTED" else if (all(ok)) "PASS" else "FAIL"
  mx <- if (any(found & is.finite(d))) max(d[found & is.finite(d)]) else NA_real_
  checks <- data.frame(check = c("V2 identity: every gaussian row has a traced complete-case mask", "V2 identity: traced mask size equals the recorded n",
                                 "V2 identity: gaussian standardised_effect: implied SD(outcome) equals the directly measured SD of the row's own complete-case sample"),
                       n_models = nrow(e), max_rel_diff = c(sum(!found), sum(found & !size_ok), mx), tolerance = c(0, 0, tol),
                       status = c(st(found), st(size_ok | !found), if (!nrow(e)) "NOT_TESTED" else if (all(sd_ok | !found) && any(found)) "PASS" else "FAIL"), stringsAsFactors = FALSE)
  list(rows = rows, checks = checks)
}

#' The identity table of a run under protocol v2: every v1 identity row (the v1 by-n implied-SD row relabelled 'V1 identity (superseded, kept for the record)') and the v2 rows.
#' @param v1 identity rows of .siw_dl_identities (with a 'source' column); v2 list of check tables (named by source)
#' @export
siw_anom_identities_v2_table <- function(v1, v2) {
  a <- v1; sup <- a$check == SIW_AH_V1_IDENTITY_CHECK
  a$check[sup] <- paste0("V1 identity (superseded, kept for the record): ", SIW_AH_V1_IDENTITY_CHECK); a$counts_in_v2 <- !sup; a$protocol <- ifelse(sup, "v1 (superseded)", "v1 (unchanged; counts in V2)")
  b <- do.call(rbind, lapply(names(v2), function(s) { x <- v2[[s]]; x$source <- s; x$counts_in_v2 <- TRUE; x$protocol <- "v2"; x }))
  rbind(a[, c("source", "check", "n_models", "max_rel_diff", "tolerance", "status", "counts_in_v2", "protocol")], b[, c("source", "check", "n_models", "max_rel_diff", "tolerance", "status", "counts_in_v2", "protocol")])
}
