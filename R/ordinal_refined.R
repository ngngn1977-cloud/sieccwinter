# Ordinal refinement (Phase 3B.2): the same 80 proportional-odds specifications as the legacy reproduction (R/ordinal.R, unchanged), fitted with ONE documented protocol
# (config/ordinal_refinement_plan.json): predictors centred and scaled internally, MASS::polr with tight optimiser control and a finite-difference Hessian with a small step,
# restarts from its own solution while the Newton decrement is large, and an exact back-transformation of coefficients, thresholds and the covariance matrix to the original
# parameterisation. Every refined model is then checked against pre-declared numerical acceptance criteria (never the optimiser flag alone) using an independent
# maximum-likelihood fit with analytic derivatives. The legacy table is not modified; refined results, their BH q80 over the 80 tests and their verification are separate tables.

#' Load the ordinal refinement plan (config/ordinal_refinement_plan.json).
#' @export
siw_ordinal_refinement_plan <- function(path = NULL) {
  if (is.null(path)) {
    root <- siw_root(); cand <- c(file.path(root, "config", "ordinal_refinement_plan.json"), file.path(root, "extdata", "ordinal_refinement_plan.json"), file.path(root, "inst", "extdata", "ordinal_refinement_plan.json"))
    path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_ORDINAL_REFINEMENT_PLAN_NOT_FOUND", call. = FALSE)
  }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$plan_path <- normalizePath(path, winslash = "/"); p$plan_hash <- siw_file_hash(path)$value; p
}

#' Linear map from the parameters of the scaled fit, theta_s = (gamma, zeta_s), to the original parameters (beta, zeta).
#' beta = gamma / s;  zeta = zeta_s + sum(beta * mu)   (P(Y <= k) = F(zeta_k - eta), eta = sum(gamma * (x - mu) / s)).
#' @export
siw_polr_backtransform_matrix <- function(mu, s, q) {
  p <- length(mu); T <- diag(p + q)
  T[seq_len(p), seq_len(p)] <- diag(1 / s, p)
  T[p + seq_len(q), seq_len(p)] <- matrix(rep(mu / s, each = q), q, p)
  T
}

#' Fit one specification with the refined protocol. Failures are returned as reasons, never thrown.
#' @param d complete-case data frame with the ordered outcome, the exposure and the covariates (levels already dropped)
#' @return list(ok, reason, coefficients, zeta, vcov, se, loglik, newton_decrement, restarts, code, ...) on the ORIGINAL parameterisation
#' @export
siw_polr_refined_one <- function(d, outcome, exposure, covariates, plan = siw_ordinal_refinement_plan()) {
  pr <- plan$protocol; ctl <- list(reltol = pr$optim_control$reltol, maxit = as.integer(pr$optim_control$maxit)); restart_above <- pr$restarts$restart_if_newton_decrement_above; max_restart <- as.integer(pr$restarts$max)
  X <- stats::model.matrix(stats::reformulate(c(exposure, covariates)), d)[, -1L, drop = FALSE]; nm <- colnames(X); p <- ncol(X)
  mu <- colMeans(X); s <- apply(X, 2L, stats::sd)
  if (any(!is.finite(s) | s <= 0)) return(list(ok = FALSE, reason = paste0("ZERO_VARIANCE_COLUMN: ", paste(nm[!is.finite(s) | s <= 0], collapse = "|")), warnings = ""))
  Z <- sweep(sweep(X, 2L, mu, "-"), 2L, s, "/"); vn <- paste0("V", seq_len(p)); colnames(Z) <- vn
  dd <- data.frame(y = d[[outcome]], Z, check.names = FALSE); f <- stats::reformulate(vn, "y"); y <- as.integer(dd$y); q <- nlevels(dd$y) - 1L
  ctl$ndeps <- rep(pr$optim_control$ndeps, p + q); warn <- character()
  fit_once <- function(start = NULL) tryCatch(withCallingHandlers({
      if (is.null(start)) MASS::polr(f, data = dd, Hess = TRUE, method = "logistic", control = ctl) else MASS::polr(f, data = dd, Hess = TRUE, method = "logistic", start = start, control = ctl)
    }, warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }), error = function(e) structure(list(message = conditionMessage(e)), class = "siw_fit_error"))
  nd_of <- function(fit) { dv <- siw_po_derivatives(c(fit$zeta, fit$coefficients), y, Z); tryCatch(sqrt(max(0, sum(dv$gradient * solve(-dv$hessian, dv$gradient)))), error = function(e) NA_real_) }
  fit <- fit_once(); if (inherits(fit, "siw_fit_error")) return(list(ok = FALSE, reason = paste0("POLR_ERROR: ", substr(fit$message, 1L, 80)), warnings = paste(unique(warn), collapse = " | ")))
  nd <- nd_of(fit); restarts <- 0L
  while (is.finite(nd) && nd > restart_above && restarts < max_restart) {
    nf <- fit_once(start = c(fit$coefficients, fit$zeta)); if (inherits(nf, "siw_fit_error")) break
    fit <- nf; nd <- nd_of(fit); restarts <- restarts + 1L
  }
  H <- fit$Hessian; ev <- tryCatch(eigen((H + t(H)) / 2, symmetric = TRUE, only.values = TRUE)$values, error = function(e) rep(NA_real_, nrow(H)))
  Vs <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(Vs) || any(!is.finite(Vs))) return(list(ok = FALSE, reason = "VCOV_UNAVAILABLE_OR_NON_FINITE", warnings = paste(unique(warn), collapse = " | "), code = fit$convergence))
  T <- siw_polr_backtransform_matrix(mu, s, q); V <- T %*% Vs %*% t(T)
  theta <- as.numeric(T %*% c(fit$coefficients, fit$zeta)); beta <- stats::setNames(theta[seq_len(p)], nm); zeta <- stats::setNames(theta[p + seq_len(q)], names(fit$zeta)); dimnames(V) <- list(c(nm, names(zeta)), c(nm, names(zeta)))
  list(ok = TRUE, reason = "", coefficients = beta, zeta = zeta, vcov = V, se = sqrt(pmax(diag(V), 0)), loglik = -fit$deviance / 2, newton_decrement = nd, restarts = restarts, code = fit$convergence,
       f_evals = unname(fit$niter[1L]), g_evals = unname(fit$niter[2L]), hessian_min_eigenvalue = min(ev), hessian_condition = if (all(is.finite(ev)) && min(ev) > 0) max(ev) / min(ev) else Inf,
       warnings = paste(unique(warn), collapse = " | "), scaling_mu = mu, scaling_sd = s)
}

#' The 80 refined ordinal specifications (M2-L, 8 metrics x 10 windows) with their own BH family q80_ordinal_refined.
#' @return list(results = data frame, fits = named list of fit objects, data = named list of the complete-case data frames)
#' @export
siw_ordinal_refined_grid <- function(dat, config, plan = siw_ordinal_refinement_plan(), tier = config$primary_tier) {
  dat$integration_ordinal <- siw_ordinal_outcome(dat); covs <- config$tiers[[tier]]; rows <- list(); fits <- list(); dds <- list()
  for (mi in seq_len(nrow(config$metrics))) {
    sp <- config$metrics[mi, ]
    for (w in config$windows) {
      ex <- siw_cum_name(sp$cumulative_prefix, w); vars <- unique(c("integration_ordinal", ex, covs)); keep <- stats::complete.cases(dat[, vars, drop = FALSE]); d <- droplevels(dat[keep, vars, drop = FALSE])
      rid <- paste("ORDR", "INT", sp$code, paste0("W", w), tier, sep = "__"); r <- siw_polr_refined_one(d, "integration_ordinal", ex, covs, plan); fits[[rid]] <- r; dds[[rid]] <- d
      loc_all <- if ("Locality_3group" %in% covs) levels(dat$Locality_3group) else character(); loc_used <- if ("Locality_3group" %in% covs) levels(d$Locality_3group) else character()
      base <- data.frame(result_id = rid, legacy_result_id = paste("ORD", "INT", sp$code, paste0("W", w), tier, sep = "__"), outcome = "integration_score", outcome_family = "ordinal", metric = sp$metric, exposure = ex, window_or_lag = as.integer(w), model_tier = tier,
                         n = nrow(d), categories_used = length(levels(d$integration_ordinal)), dropped_terms = paste(setdiff(loc_all, loc_used), collapse = "|"), protocol_id = plan$protocol$id, stringsAsFactors = FALSE)
      if (isTRUE(r$ok)) {
        b <- r$coefficients[[ex]]; se <- r$se[[ex]]; sdx <- stats::sd(d[[ex]])
        rows[[length(rows) + 1L]] <- cbind(base, data.frame(status = "COMPLETED", not_estimable_reason = "", optimiser_code = as.integer(r$code), restarts = r$restarts, f_evals = r$f_evals, g_evals = r$g_evals, coefficient = b, se = se, odds_ratio = exp(b * sdx),
          p = 2 * stats::pnorm(abs(b / se), lower.tail = FALSE), log_likelihood = r$loglik, parameter_count = length(r$coefficients) + length(r$zeta), exposure_sd = sdx, newton_decrement = r$newton_decrement, fit_warnings = r$warnings, stringsAsFactors = FALSE))
      } else rows[[length(rows) + 1L]] <- cbind(base, data.frame(status = "NOT_ESTIMABLE", not_estimable_reason = r$reason, optimiser_code = NA_integer_, restarts = NA_integer_, f_evals = NA_integer_, g_evals = NA_integer_, coefficient = NA_real_, se = NA_real_, odds_ratio = NA_real_,
          p = NA_real_, log_likelihood = NA_real_, parameter_count = NA_integer_, exposure_sd = NA_real_, newton_decrement = NA_real_, fit_warnings = r$warnings, stringsAsFactors = FALSE))
    }
  }
  out <- do.call(rbind, rows); out$q80_ordinal_refined <- siw_bh(out$p); out$multiplicity_family_q80_ordinal_refined <- ifelse(is.finite(out$q80_ordinal_refined), "ORDINAL_REFINED_q80_8_metrics_x_10_windows", "")
  list(results = out, fits = fits, data = dds)
}

#' Evaluate the pre-declared acceptance criteria for every refined model against an independent maximum-likelihood fit (own analytic derivatives, original parameterisation).
#' @return data frame, one row per specification: the measured quantities, one logical column per criterion and status ACCEPTED / REJECTED / NOT_VERIFIED
#' @export
siw_ordinal_refined_verify <- function(grid, config, plan = siw_ordinal_refinement_plan(), tier = config$primary_tier) {
  ac <- plan$acceptance_criteria; covs <- config$tiers[[tier]]; rows <- list()
  for (i in seq_len(nrow(grid$results))) {
    g <- grid$results[i, ]; rid <- g$result_id; r <- grid$fits[[rid]]; d <- grid$data[[rid]]
    if (!isTRUE(r$ok)) { rows[[i]] <- data.frame(result_id = rid, status = "NOT_VERIFIED_NOT_ESTIMABLE", failed_criteria = "model not estimable", stringsAsFactors = FALSE); next }
    X <- stats::model.matrix(stats::reformulate(c(g$exposure, covs)), d)[, -1L, drop = FALSE]; y <- as.integer(d$integration_ordinal); q <- length(r$zeta)
    ind <- tryCatch(siw_polr_independent(y, X), error = function(e) NULL)
    if (is.null(ind) || !isTRUE(ind$converged)) { rows[[i]] <- data.frame(result_id = rid, status = "REJECTED", failed_criteria = "independent_fit_converged", criterion_independent_fit_pass = FALSE, all_criteria_pass = FALSE, stringsAsFactors = FALSE); next }
    th_ind <- c(ind$coefficients, ind$zeta); dv <- siw_po_derivatives(c(ind$zeta, ind$coefficients), y, X)             # theta order of siw_po_derivatives: zeta then beta
    Vi <- tryCatch(solve(-dv$hessian), error = function(e) NULL); evi <- tryCatch(min(eigen(-dv$hessian, symmetric = TRUE, only.values = TRUE)$values), error = function(e) NA_real_)
    nd_ind <- tryCatch(sqrt(max(0, sum(dv$gradient * solve(-dv$hessian, dv$gradient)))), error = function(e) NA_real_)
    se_ind <- if (is.null(Vi)) rep(NA_real_, q + ncol(X)) else sqrt(pmax(diag(Vi), 0)); se_ind <- c(se_ind[q + seq_len(ncol(X))], se_ind[seq_len(q)])    # reorder to (beta, zeta)
    th_ref <- c(r$coefficients, r$zeta); se_ref <- r$se
    rel <- function(a, b) abs(a - b) / pmax(1, abs(b))
    dll <- r$loglik - ind$log_likelihood; coef_diff <- max(rel(th_ref, th_ind)); ex <- g$exposure
    dex <- abs(r$coefficients[[ex]] - ind$coefficients[[ex]]); sex <- abs(r$se[[ex]] - ind$se[[ex]]) / ind$se[[ex]]; se_all <- max(abs(se_ref - se_ind) / se_ind)
    ordered_ok <- all(is.finite(r$zeta)) && all(diff(r$zeta) > 0)
    crit <- c(
      optimiser_code_zero = identical(as.integer(r$code), 0L),
      newton_decrement = is.finite(r$newton_decrement) && r$newton_decrement <= ac$newton_decrement_max,
      loglik_agreement = is.finite(dll) && abs(dll) <= ac$loglik_abs_diff_vs_independent_max && dll <= ac$loglik_not_above_independent_by_more_than,
      thresholds_ordered = ordered_ok,
      information_matrix = is.finite(r$hessian_min_eigenvalue) && r$hessian_min_eigenvalue > ac$information_matrix$polr_hessian_min_eigenvalue_above && is.finite(evi) && evi > ac$information_matrix$analytic_observed_information_min_eigenvalue_above &&
        r$hessian_condition <= ac$information_matrix$polr_hessian_condition_number_max && is.finite(se_all) && se_all <= ac$information_matrix$all_parameter_se_relative_diff_vs_analytic_max,
      coefficients_thresholds_agree = is.finite(coef_diff) && coef_diff <= ac$coefficients_and_thresholds_abs_diff_vs_independent_max,
      exposure_coefficient_agrees = is.finite(dex) && dex <= ac$exposure_coefficient_abs_diff_vs_independent_max,
      exposure_se_agrees = is.finite(sex) && sex <= ac$exposure_se_relative_diff_vs_independent_max,
      independent_fit_converged = is.finite(nd_ind) && ind$gradient_max_abs <= ac$independent_fit$gradient_max_abs_at_most && nd_ind <= ac$independent_fit$newton_decrement_at_most,
      all_estimates_finite = all(is.finite(c(th_ref, se_ref, r$loglik))))
    inh <- ac$inherited_batch_b_criteria_still_reported; inh_ok <- abs(dll) <= inh$loglik && dex <= inh$coefficient && abs(r$se[[ex]] - ind$se[[ex]]) <= inh$se
    # measurements and logical criteria carry distinct names (criterion_<name>_pass); the status is computed from the named criterion vector itself
    crit_cols <- stats::setNames(as.list(unname(crit)), paste0("criterion_", names(crit), "_pass"))
    rows[[i]] <- data.frame(result_id = rid, status = if (all(crit)) "ACCEPTED" else "REJECTED", failed_criteria = paste(names(crit)[!crit], collapse = "|"), optimiser_code = as.integer(r$code), restarts = r$restarts,
      newton_decrement_value = r$newton_decrement, loglik_difference = dll, max_coefficient_threshold_difference = coef_diff, exposure_coefficient_difference = dex, exposure_se_relative_difference = sex, max_se_relative_difference_all_parameters = se_all,
      min_threshold_gap = min(diff(r$zeta)), polr_hessian_min_eigenvalue = r$hessian_min_eigenvalue, polr_hessian_condition = r$hessian_condition, analytic_information_min_eigenvalue = evi,
      independent_newton_decrement = nd_ind, independent_gradient_max_abs = ind$gradient_max_abs, inherited_batch_b_criteria_met = inh_ok, all_criteria_pass = all(crit), as.data.frame(crit_cols, check.names = FALSE), stringsAsFactors = FALSE)
  }
  out <- siw_bind_rows(rows); if (anyDuplicated(names(out))) stop("SIW_DUPLICATE_COLUMN_NAMES_IN_VERIFICATION_TABLE: ", paste(names(out)[duplicated(names(out))], collapse = ", "), call. = FALSE)
  out
}

#' Descriptive comparison of the refined table with the preserved legacy table (joined on metric and window). Not a pass/fail comparison.
#' @export
siw_ordinal_refined_vs_legacy <- function(refined, legacy, legacy_verification = NULL) {
  key <- function(d) paste(d$metric, d$window_or_lag); i <- match(key(refined), key(legacy)); stopifnot(!anyNA(i)); L <- legacy[i, ]
  out <- data.frame(metric = refined$metric, window_or_lag = refined$window_or_lag, refined_result_id = refined$result_id, legacy_result_id = L$result_id, n_refined = refined$n, n_legacy = L$n,
    coefficient_legacy = L$coefficient, coefficient_refined = refined$coefficient, coefficient_difference = refined$coefficient - L$coefficient,
    se_legacy = L$se, se_refined = refined$se, se_difference = refined$se - L$se, p_legacy = L$p, p_refined = refined$p, p_difference = refined$p - L$p,
    odds_ratio_legacy = L$odds_ratio, odds_ratio_refined = refined$odds_ratio, odds_ratio_relative_change = refined$odds_ratio / L$odds_ratio - 1,
    q80_legacy = L$q80_ordinal, q80_refined = refined$q80_ordinal_refined, q80_difference = refined$q80_ordinal_refined - L$q80_ordinal,
    loglik_legacy = L$log_likelihood, loglik_refined = refined$log_likelihood, loglik_gain = refined$log_likelihood - L$log_likelihood,
    newton_decrement_legacy = L$newton_decrement, newton_decrement_refined = refined$newton_decrement,
    sign_change = sign(refined$coefficient) != sign(L$coefficient), p_crosses_0.05 = (refined$p < 0.05) != (L$p < 0.05), q80_crosses_0.10 = (refined$q80_ordinal_refined < 0.10) != (L$q80_ordinal < 0.10), stringsAsFactors = FALSE)
  out$legacy_numerical_status <- if (is.null(legacy_verification)) NA_character_ else legacy_verification$status[match(L$result_id, legacy_verification$result_id)]
  out
}

#' Table schemas of the ordinal refinement step.
#' @export
siw_ordinal_refinement_output_columns <- function() {
  crit <- paste0("criterion_", c("optimiser_code_zero", "newton_decrement", "loglik_agreement", "thresholds_ordered", "information_matrix", "coefficients_thresholds_agree", "exposure_coefficient_agrees", "exposure_se_agrees", "independent_fit_converged", "all_estimates_finite"), "_pass")
  list(
    ordinal_refined = c("result_id", "legacy_result_id", "outcome", "outcome_family", "metric", "exposure", "window_or_lag", "model_tier", "n", "categories_used", "dropped_terms", "protocol_id", "status", "not_estimable_reason", "optimiser_code", "restarts", "f_evals", "g_evals",
      "coefficient", "se", "odds_ratio", "p", "log_likelihood", "parameter_count", "exposure_sd", "newton_decrement", "fit_warnings", "q80_ordinal_refined", "multiplicity_family_q80_ordinal_refined"),
    ordinal_refined_verification = c("result_id", "status", "failed_criteria", "optimiser_code", "restarts", "newton_decrement_value", "loglik_difference", "max_coefficient_threshold_difference", "exposure_coefficient_difference", "exposure_se_relative_difference",
      "max_se_relative_difference_all_parameters", "min_threshold_gap", "polr_hessian_min_eigenvalue", "polr_hessian_condition", "analytic_information_min_eigenvalue", "independent_newton_decrement", "independent_gradient_max_abs", "inherited_batch_b_criteria_met", "all_criteria_pass", crit),
    ordinal_refined_vs_legacy = c("metric", "window_or_lag", "refined_result_id", "legacy_result_id", "n_refined", "n_legacy", "coefficient_legacy", "coefficient_refined", "coefficient_difference", "se_legacy", "se_refined", "se_difference", "p_legacy", "p_refined", "p_difference",
      "odds_ratio_legacy", "odds_ratio_refined", "odds_ratio_relative_change", "q80_legacy", "q80_refined", "q80_difference", "loglik_legacy", "loglik_refined", "loglik_gain", "newton_decrement_legacy", "newton_decrement_refined", "sign_change", "p_crosses_0.05", "q80_crosses_0.10", "legacy_numerical_status"),
    ordinal_legacy_reproduction = c("result_id", "outcome", "outcome_family", "metric", "exposure", "window_or_lag", "model_tier", "n", "categories_used", "dropped_terms", "status", "not_estimable_reason", "convergence", "coefficient", "se", "odds_ratio", "p", "log_likelihood", "parameter_count",
      "exposure_sd", "newton_decrement", "fit_warnings", "q80_ordinal", "multiplicity_family_q80_ordinal"),
    ordinal_refinement_summary = c("item", "value", "note"),
    ordinal_refinement_validation_summary = siw_batch_b_output_columns()$batch_b_validation_summary, ordinal_refinement_validation_detail = siw_batch_b_output_columns()$batch_b_validation_detail)
}

#' Run the ordinal refinement step on one canonical CSV: legacy reproduction (recomputed and compared with the Batch B frozen reference and optionally with an accepted run folder),
#' refined fits, pre-declared acceptance criteria, comparison. Writes only to out_dir; nothing existing is modified.
#' @param batch_b_reference_dir Batch B reference bundle (ordinal reference); legacy_run_dir optional accepted Batch B run folder holding the legacy ordinal_integration.csv
#' @export
siw_run_ordinal_refinement <- function(data_csv, out_dir, batch_b_reference_dir, config = siw_load_config(), legacy_run_dir = NULL, label = NULL) {
  t_all <- proc.time()[["elapsed"]]; tm <- list(); tick <- function(name, expr) { t0 <- proc.time()[["elapsed"]]; v <- force(expr); tm[[name]] <<- proc.time()[["elapsed"]] - t0; v }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); options(sieccwinter.log_file = file.path(out_dir, "run.log"))
  plan <- siw_ordinal_refinement_plan(); siw_log("ordinal refinement; protocol ", plan$protocol$id, "; plan hash ", plan$plan_hash)
  ih <- siw_file_hash(data_csv); raw <- siw_read_study_csv(data_csv, config); rep <- siw_validate_input(raw, config, lag_end = config$dlm$final$lag_end); siw_write_table(rep, "input_validation_report", out_dir)
  if (any(rep$status == "FAIL")) stop("SIW_INPUT_VALIDATION_FAILED: see input_validation_report.csv", call. = FALSE)
  dat <- siw_attach_cumulative(siw_prepare_data(raw, config), config)$data
  legacy <- tick("legacy_reproduction", siw_ordinal_grid(dat, config))
  lv <- tick("legacy_verification", siw_ordinal_verify(dat, config, legacy))
  grid <- tick("refined_fits", siw_ordinal_refined_grid(dat, config, plan))
  ver <- tick("refined_verification", siw_ordinal_refined_verify(grid, config, plan))
  cmp <- siw_ordinal_refined_vs_legacy(grid$results, legacy, lv)
  # legacy reproduction: (a) strict validator against the Batch B frozen ordinal reference; (b) exact equality with the accepted run's table when supplied
  bplan <- siw_load_batch_b_plan(); bplan$references <- Filter(function(r) identical(r$name, "batchB_ordinal"), bplan$references)
  lvv <- siw_validate_batch_b(list(ordinal_integration = legacy), config, batch_b_reference_dir, bplan)
  repro_ok <- all(grepl("^PASS", lvv$summary$status)); same_run <- NA
  if (!is.null(legacy_run_dir)) {
    f <- file.path(legacy_run_dir, "ordinal_integration.csv")
    if (file.exists(f)) { o <- utils::read.csv(f, stringsAsFactors = FALSE); same_run <- identical(nrow(o), nrow(legacy)) && all(o$result_id == legacy$result_id) && all(vapply(c("n", "coefficient", "se", "p", "q80_ordinal"), function(cn) isTRUE(all(abs(o[[cn]] - legacy[[cn]]) <= 1e-12 * pmax(1, abs(legacy[[cn]])))), NA)) } else same_run <- FALSE
  }
  repro_status <- if (repro_ok && (is.na(same_run) || isTRUE(same_run))) "PASS" else "FAIL"
  n_acc <- sum(ver$status == "ACCEPTED"); n_est <- sum(grid$results$status == "COMPLETED")
  ref_status <- if (nrow(ver) == 80L && n_acc == 80L) "PASS" else "FAIL"
  lg_ok <- sum(lv$status == "PASS", na.rm = TRUE); lg_lim <- sum(lv$status == "POLR_NOT_AT_MAXIMUM", na.rm = TRUE)
  summ <- data.frame(item = c("protocol_id", "plan_hash", "refined_models_completed", "refined_models_accepted", "refined_models_rejected", "legacy_polr_at_maximum_by_batch_b_rule", "legacy_polr_not_at_maximum_by_batch_b_rule", "refined_max_newton_decrement",
      "refined_max_restarts", "legacy_max_newton_decrement", "max_abs_coefficient_change_vs_legacy", "max_abs_p_change_vs_legacy", "max_abs_q80_change_vs_legacy", "sign_changes", "p_0.05_crossings", "q80_0.10_crossings", "min_q80_legacy", "min_q80_refined", "legacy_equals_accepted_run_table"),
    value = c(plan$protocol$id, plan$plan_hash, n_est, n_acc, 80L - n_acc, lg_ok, lg_lim, signif(max(grid$results$newton_decrement, na.rm = TRUE), 4), max(grid$results$restarts, na.rm = TRUE), signif(max(legacy$newton_decrement, na.rm = TRUE), 4), signif(max(abs(cmp$coefficient_difference), na.rm = TRUE), 4),
      signif(max(abs(cmp$p_difference), na.rm = TRUE), 4), signif(max(abs(cmp$q80_difference), na.rm = TRUE), 4), sum(cmp$sign_change, na.rm = TRUE), sum(cmp$`p_crosses_0.05`, na.rm = TRUE), sum(cmp$`q80_crosses_0.10`, na.rm = TRUE),
      signif(min(legacy$q80_ordinal, na.rm = TRUE), 4), signif(min(grid$results$q80_ordinal_refined, na.rm = TRUE), 4), if (is.na(same_run)) "not requested" else as.character(same_run)),
    note = "", stringsAsFactors = FALSE)
  siw_write_table(legacy, "ordinal_legacy_reproduction", out_dir); siw_write_table(grid$results, "ordinal_refined", out_dir); siw_write_table(ver, "ordinal_refined_verification", out_dir); siw_write_table(cmp, "ordinal_refined_vs_legacy", out_dir)
  siw_write_table(summ, "ordinal_refinement_summary", out_dir); siw_write_table(lvv$summary, "ordinal_refinement_validation_summary", out_dir); siw_write_table(lvv$detail, "ordinal_refinement_validation_detail", out_dir)
  siw_write_csv(lv, file.path(out_dir, "ordinal_legacy_verification.csv"))
  siw_write_csv(data.frame(stage = names(tm), seconds = round(unlist(tm), 3)), file.path(out_dir, "ordinal_refinement_timings.csv"))
  st <- c(EXECUTION_STATUS = "PASS", INPUT_CHECK_STATUS = if (any(rep$status == "FAIL")) "FAIL" else "PASS", LEGACY_REPRODUCTION_STATUS = repro_status,
          LEGACY_NUMERICAL_VERIFICATION_STATUS = if (lg_lim == 0L) "PASS" else "DOCUMENTED_LIMITATION", REFINED_NUMERICAL_VERIFICATION_STATUS = ref_status)
  writeLines(c(paste0(names(st), "=", st), paste0("REFINED_MODELS_ACCEPTED=", n_acc, "_OF_80"), paste0("LEGACY_POLR_NOT_AT_MAXIMUM=", lg_lim, "_OF_80"), "REFINED_VS_LEGACY=DESCRIPTIVE_COMPARISON_SEE_ordinal_refined_vs_legacy.csv",
               "The legacy table was not modified; refined results are separate; nothing was adjusted to fit any reference."), file.path(out_dir, "ORDINAL_REFINEMENT_STATUS.txt"))
  ref_file <- file.path(batch_b_reference_dir, "batchB_ordinal_reference.csv")
  meta <- list(package = "sieccwinter", package_version = siw_pkg_version(), step = "ordinal_refinement", protocol = plan$protocol$id, plan_hash = plan$plan_hash, batch_b_plan_hash = bplan$plan_hash, config_hash = config$config_hash, tolerances = config$tolerances,
               acceptance_criteria = plan$acceptance_criteria, input_label = label %||% basename(data_csv), input_hash_algorithm = ih$algorithm, input_hash = ih$value, n_participants = nrow(raw), statuses = as.list(st),
               batch_b_ordinal_reference_hash = if (file.exists(ref_file)) siw_file_hash(ref_file)$value else NA_character_, legacy_run_dir_supplied = !is.null(legacy_run_dir), MASS_version = as.character(utils::packageVersion("MASS")),
               numDeriv_available = requireNamespace("numDeriv", quietly = TRUE), r_version = R.version.string, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), seconds = round(proc.time()[["elapsed"]] - t_all, 2))
  jsonlite::write_json(meta, file.path(out_dir, "ordinal_refinement_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  writeLines(utils::capture.output(print(utils::sessionInfo())), file.path(out_dir, "session_info.txt"))
  siw_write_manifest(out_dir); siw_log("ordinal refinement done: ", paste(names(st), st, sep = "=", collapse = "; ")); options(sieccwinter.log_file = NULL)
  list(out_dir = out_dir, statuses = st, legacy = legacy, legacy_verification = lv, grid = grid, verification = ver, comparison = cmp, summary = summ, legacy_validation = lvv,
       status = if (all(st[c("LEGACY_REPRODUCTION_STATUS", "REFINED_NUMERICAL_VERIFICATION_STATUS")] == "PASS")) "OK" else "VERIFICATION_OR_REPRODUCTION_FAILURE")
}
