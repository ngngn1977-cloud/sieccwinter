# Batch C: primary cumulative bootstrap (participant and assessment-date-cluster), 90 M2-L models x 2 schemes = 180 target-scheme combinations, 2,000 resamples each.
# Reproduces the original procedure (WINTER_SOURCE/R/100_resampling.R) - RNG, per-target seeds, sampling order, fit controls, failure rules, coefficient scale, percentile calculation -
# with the repeated data preparation removed (the design matrix of each resample is built directly and fitted with lm.fit / glm.fit). The protocol is declared in
# config/bootstrap_protocol.json. Failed refits are counted and classified, never redrawn; no estimator is replaced.

#' Load the declared bootstrap protocol (config/bootstrap_protocol.json).
#' @export
siw_bootstrap_protocol <- function(path = NULL) {
  if (is.null(path)) {
    root <- siw_root(); cand <- c(file.path(root, "config", "bootstrap_protocol.json"), file.path(root, "extdata", "bootstrap_protocol.json"), file.path(root, "inst", "extdata", "bootstrap_protocol.json"))
    path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_BOOTSTRAP_PROTOCOL_NOT_FOUND", call. = FALSE)
  }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value; p
}

#' The 180 target-scheme combinations with the original target indices and seeds.
#' Order: outcome, representative metric (configuration order), window, scheme (participant, then assessment_date). target_index increments per (outcome, metric, window) and once more after the
#' windows of every outcome-metric block (the legacy DLM target); seed = config seed + 100 * target_index + (1 for the assessment-date scheme).
#' @export
siw_bootstrap_plan <- function(config, replicates = 2000L) {
  rows <- list(); idx <- 0L; metrics <- config$metrics[config$metrics$representative, , drop = FALSE]
  for (oi in seq_len(nrow(config$outcomes))) {
    o <- config$outcomes[oi, ]
    for (mi in seq_len(nrow(metrics))) {
      sp <- metrics[mi, ]
      for (w in config$windows) {
        idx <- idx + 1L
        for (sch in c("participant", "assessment_date")) {
          rows[[length(rows) + 1L]] <- data.frame(target_id = paste("BS", o$code, sp$code, paste0("W", w), sch, paste0("R", replicates), sep = "__"), outcome = o$outcome, outcome_family = o$family, metric = sp$metric,
            exposure = siw_cum_name(sp$cumulative_prefix, w), window_or_lag = as.integer(w), resampling_type = sch, target_index = idx, seed = as.integer(config$seed + 100L * idx + if (sch == "assessment_date") 1L else 0L),
            replicates_requested = as.integer(replicates), stringsAsFactors = FALSE)
        }
      }
      idx <- idx + 1L                                     # the legacy DLM target consumed one index after each outcome-metric block
    }
  }
  do.call(rbind, rows)
}

#' Resampling units: row groups of the assessment dates (label = assessment_day) in order of first appearance.
#' @export
siw_bootstrap_groups <- function(dat) {
  lab <- as.character(dat$assessment_day); list(n = nrow(dat), groups = unique(lab), group_rows = split(seq_len(nrow(dat)), lab), label = lab)
}

#' All resample index vectors of one target and scheme, drawn in the original order (one draw per replicate, nothing else consumes random numbers).
#' participant: sample.int(n, n, replace = TRUE); assessment_date: sampled <- sample(groups, length(groups), replace = TRUE), rows = concatenation of the rows of each sampled date.
#' @export
siw_bootstrap_indices <- function(gr, scheme, replicates, seed) {
  siw_with_seed(seed, {
    if (identical(scheme, "participant")) lapply(seq_len(replicates), function(b) sample.int(gr$n, gr$n, replace = TRUE))
    else lapply(seq_len(replicates), function(b) { sampled <- sample(gr$groups, length(gr$groups), replace = TRUE); unlist(lapply(sampled, function(g) gr$group_rows[[g]]), use.names = FALSE) })
  })
}

#' Design specification of one target: outcome, exposure, covariates in formula order (numeric columns and factor codes) and the complete-case indicator of the full data.
#' @export
siw_bootstrap_design_spec <- function(dat, config, outcome, family, exposure, tier = config$primary_tier) {
  covs <- config$tiers[[tier]]; vars <- c(outcome, exposure, covs)
  cov <- lapply(covs, function(cn) { v <- dat[[cn]]; if (is.factor(v)) list(name = cn, factor = TRUE, v = as.integer(v), nlev = nlevels(v)) else list(name = cn, factor = FALSE, v = as.numeric(v)) })
  list(outcome = outcome, family = family, y = as.numeric(dat[[outcome]]), x = as.numeric(dat[[exposure]]), cov = cov, n_cov = length(covs), cc = stats::complete.cases(dat[, vars, drop = FALSE]))
}

# One refit on resample indices `idx`. Same sample, column space and column order as lm()/glm() on droplevels(data[idx, ][complete cases]); lm.fit / glm.fit are the functions lm() / glm() call.
.siw_boot_fit <- function(idx, spec, fam) {
  kept <- idx[spec$cc[idx]]; m <- length(kept)
  if (m <= spec$n_cov + 3L) return(list(est = NA_real_, reason = "TOO_FEW_COMPLETE_CASES", conv = NA, level_absent = NA, other_aliased = NA, n = m))
  cols <- list(rep(1, m), spec$x[kept]); level_absent <- FALSE
  for (cv in spec$cov) {
    if (!cv$factor) cols[[length(cols) + 1L]] <- cv$v[kept]
    else { codes <- cv$v[kept]; pres <- which(tabulate(codes, cv$nlev) > 0L); if (length(pres) < cv$nlev) level_absent <- TRUE
      # original rule: after droplevels a factor with a single level makes model.matrix() / lm() / glm() stop ("contrasts can be applied only to factors with 2 or more levels") = a failed refit
      if (length(pres) < 2L) return(list(est = NA_real_, reason = "FIT_ERROR: contrasts can be applied only to factors with 2 or more levels", conv = NA, level_absent = TRUE, other_aliased = NA, n = m))
      for (l in pres[-1L]) cols[[length(cols) + 1L]] <- as.numeric(codes == l) }
  }
  X <- matrix(unlist(cols, use.names = FALSE), nrow = m); yk <- spec$y[kept]; conv <- NA
  fit <- tryCatch(if (identical(spec$family, "gaussian")) stats::lm.fit(X, yk) else suppressWarnings(stats::glm.fit(X, yk, family = fam)), error = function(e) structure(list(message = conditionMessage(e)), class = "siw_fit_error"))
  if (inherits(fit, "siw_fit_error")) return(list(est = NA_real_, reason = paste0("FIT_ERROR: ", substr(gsub("[\r\n,\"]", " ", fit$message), 1L, 60L)), conv = NA, level_absent = level_absent, other_aliased = NA, n = m))
  b <- fit$coefficients; if (!identical(spec$family, "gaussian")) conv <- isTRUE(fit$converged)
  est <- as.numeric(b[2L]); other_aliased <- anyNA(b[-2L])
  reason <- if (is.na(est)) "EXPOSURE_ALIASED" else if (!is.finite(est)) "NON_FINITE_ESTIMATE" else ""
  list(est = est, reason = reason, conv = conv, level_absent = level_absent, other_aliased = other_aliased, n = m)
}

#' Summary of the successful estimates exactly as the original: median, type-8 percentile interval (at least 20 successes), sign fractions.
#' @export
siw_bootstrap_summarise <- function(est, replicates = length(est)) {
  ok <- is.finite(est); ci <- if (sum(ok) >= 20L) stats::quantile(est[ok], c(0.025, 0.975), names = FALSE, type = 8) else c(NA_real_, NA_real_)
  data.frame(replicates_requested = as.integer(replicates), replicates_attempted = as.integer(length(est)), replicates_successful = as.integer(sum(ok)), replicates_failed = as.integer(sum(!ok)),
             estimate_median = if (any(ok)) stats::median(est[ok]) else NA_real_, ci_lower = ci[[1L]], ci_upper = ci[[2L]], positive_fraction = if (any(ok)) mean(est[ok] > 0) else NA_real_,
             negative_fraction = if (any(ok)) mean(est[ok] < 0) else NA_real_, interval_excludes_null = is.finite(ci[[1L]]) && is.finite(ci[[2L]]) && (ci[[1L]] > 0 || ci[[2L]] < 0), stringsAsFactors = FALSE)
}

#' Run one target-scheme combination: draw all resamples, refit, classify failures. Failed refits are not redrawn.
#' @return list(estimates, reason, glm_not_converged, level_absent, other_aliased, n_complete, seconds)
#' @export
siw_bootstrap_target <- function(gr, spec, scheme, replicates, seed) {
  t0 <- proc.time()[["elapsed"]]; ix <- siw_bootstrap_indices(gr, scheme, replicates, seed); fam <- stats::binomial()
  est <- rep(NA_real_, replicates); reason <- character(replicates); conv <- rep(NA, replicates); labs <- rep(NA, replicates); oal <- rep(NA, replicates); nn <- integer(replicates)
  for (b in seq_len(replicates)) { r <- .siw_boot_fit(ix[[b]], spec, fam); est[b] <- r$est; reason[b] <- r$reason; conv[b] <- r$conv; labs[b] <- r$level_absent; oal[b] <- r$other_aliased; nn[b] <- r$n }
  list(estimates = est, reason = reason, glm_not_converged = conv %in% FALSE, level_absent = labs %in% TRUE, other_aliased = oal %in% TRUE, n_complete = nn, seconds = proc.time()[["elapsed"]] - t0)
}

# ------------------------------------------------------------------ independent checks (separate code paths) ------------------------------------------------------------------

#' Independent Hyndman-Fan type-8 quantile (written from the definition; not stats::quantile): m = (p + 1)/3, j = floor(n p + m), g = n p + m - j, Q = (1 - g) x_j + g x_(j+1).
#' @export
siw_quantile8 <- function(x, p) {
  x <- sort(x); n <- length(x); vapply(p, function(pp) { m <- (pp + 1) / 3; j <- floor(n * pp + m + 4 * .Machine$double.eps); g <- n * pp + m - j; xj <- if (j < 1L) x[1L] else if (j > n) x[n] else x[j]; xj1 <- if (j + 1L < 1L) x[1L] else if (j + 1L > n) x[n] else x[j + 1L]; (1 - g) * xj + g * xj1 }, numeric(1))
}

#' Independent fit the way the original does it: complete cases, droplevels, lm()/glm() through the formula interface on the resampled data frame (legacy siecc_simple_model_coefficient).
#' @export
siw_bootstrap_legacy_style_fit <- function(data, idx, outcome, family, exposure, covariates) {
  d0 <- data[idx, , drop = FALSE]; variables <- unique(c(outcome, exposure, covariates)); complete <- stats::complete.cases(d0[, variables, drop = FALSE]); d <- droplevels(d0[complete, variables, drop = FALSE])
  formula <- stats::reformulate(c(exposure, covariates), outcome); if (nrow(d) <= length(covariates) + 3L) return(NA_real_)
  fit <- if (family == "gaussian") tryCatch(stats::lm(formula, data = d), error = function(e) NULL) else tryCatch(suppressWarnings(stats::glm(formula, data = d, family = stats::binomial())), error = function(e) NULL)
  if (is.null(fit) || !(exposure %in% names(stats::coef(fit)))) return(NA_real_); as.numeric(stats::coef(fit)[[exposure]])
}

#' Check the sampling of one target: invariants on EVERY replicate, and uniformity of selection. Independent of the draw code (replays the generator and verifies the structure).
#' participant: each vector has length n with indices in 1..n. assessment_date: each vector equals the concatenation, in sampled order, of the full row sets of the sampled dates, so every participant of
#' every sampled occurrence is retained (a date sampled twice repeats all its participants), and the number of occurrences of a row equals the number of times its date was sampled.
#' @return data frame with one row: replicates checked, violations, maximum |z| of the selection counts
#' @export
siw_bootstrap_check_sampling <- function(gr, scheme, replicates, seed, indices = NULL) {
  ix <- if (is.null(indices)) siw_bootstrap_indices(gr, scheme, replicates, seed) else indices; viol <- 0L; n <- gr$n; ng <- length(gr$groups); sizes <- lengths(gr$group_rows[gr$groups]); counts <- numeric(if (scheme == "participant") n else ng)
  replay <- siw_with_seed(seed, lapply(seq_len(replicates), function(b) if (scheme == "participant") sample.int(n, n, replace = TRUE) else sample.int(ng, ng, replace = TRUE)))
  for (b in seq_len(replicates)) {
    v <- ix[[b]]
    if (scheme == "participant") { ok <- length(v) == n && all(v >= 1L & v <= n) && identical(as.integer(v), as.integer(replay[[b]])); counts <- counts + tabulate(v, n) }
    else { s <- replay[[b]]; exp <- unlist(lapply(s, function(k) gr$group_rows[[gr$groups[k]]]), use.names = FALSE); ok <- identical(as.integer(v), as.integer(exp)) && length(v) == sum(sizes[s]) &&
      all(tabulate(match(gr$label[v], gr$groups), ng) == tabulate(s, ng) * sizes); counts <- counts + tabulate(s, ng) }
    if (!isTRUE(ok)) viol <- viol + 1L
  }
  p <- 1 / length(counts); z <- (counts - replicates * (if (scheme == "participant") n else ng) * p) / sqrt(replicates * (if (scheme == "participant") n else ng) * p * (1 - p))
  data.frame(replicates_checked = replicates, violations = viol, uniformity_max_abs_z = max(abs(z)), stringsAsFactors = FALSE)
}

#' Independent target-index and seed rule: index = 33 * outcome + 11 * metric + window position (1-based within the block, windows 1..10, then one skipped index); seed = base + 100 * index + (scheme == date).
#' @export
siw_bootstrap_expected_seed <- function(config, outcome_pos, metric_pos, window_pos, scheme) {
  nw <- length(config$windows); nm <- sum(config$metrics$representative); index <- (outcome_pos - 1L) * nm * (nw + 1L) + (metric_pos - 1L) * (nw + 1L) + window_pos
  c(index = index, seed = config$seed + 100L * index + if (scheme == "assessment_date") 1L else 0L)
}
