# Batch C equivalence with the original (legacy) SIECC bootstrap on FULLY SYNTHETIC data only (no real data). The legacy code is sourced read-only; its own function siecc_bootstrap_one()
# (and siecc_simple_model_coefficient()) produces the comparators: the same data, the same per-target seeds and the same replicate counts.
#   * all 180 target-scheme combinations at a reduced replicate count (default 100), compared cell by cell (replicates_successful, median, 95% interval, sign fractions);
#   * a few targets at the full 2,000 replicates (default targets 1,2,141,142 = continuous and binary, participant and date schemes), with the legacy run time (benchmark);
#   * the date grouping uses assessment_day (as.character) in the legacy cluster argument; the legacy code groups by first appearance, so any one-to-one label gives the same draws.
#   Rscript validation/legacy_equivalence_batch_c_synthetic.R <legacy_package_root> [<library_with_dlnm>] [--reps N] [--full 1,2,141,142] [--out DIR]
args <- commandArgs(trailingOnly = TRUE); legacy_root <- args[1L]; lib <- if (length(args) > 1L && !startsWith(args[2L], "--")) args[2L] else NULL
opt <- function(f, d) { i <- match(f, args); if (is.na(i)) d else args[i + 1L] }; reps <- as.integer(opt("--reps", "100")); full <- as.integer(strsplit(opt("--full", "1,2,141,142"), ",")[[1L]]); out <- opt("--out", NULL)
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); root <- normalizePath(file.path(dirname(script_arg[1L]), ".."), winslash = "/")
if (!is.null(lib)) .libPaths(c(lib, .libPaths())); source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root); cfg <- siw_load_config()
lenv <- new.env(parent = globalenv()); for (f in sort(list.files(file.path(legacy_root, "R"), pattern = "[.]R$", full.names = TRUE))) sys.source(f, envir = lenv)
raw <- siw_make_synthetic(config = cfg); dat <- siw_attach_cumulative(siw_prepare_data(raw, cfg), cfg)$data
ldat <- dat; ldat$Int_dummy <- as.integer(ldat$Int_dummy); ldat$assessment_date_iso <- as.character(ldat$assessment_day)
covs <- cfg$tiers[[cfg$primary_tier]]; gr <- siw_bootstrap_groups(dat); plan <- siw_bootstrap_plan(cfg, reps); fullplan <- siw_bootstrap_plan(cfg, 2000L)
cmp_cols <- c("replicates_requested", "replicates_successful", "estimate_median", "ci_lower", "ci_upper", "positive_fraction", "negative_fraction")
one <- function(p, R) {
  t0 <- proc.time()[["elapsed"]]; l <- lenv$siecc_bootstrap_one(ldat, p$outcome, p$outcome_family, p$exposure, covs, replicates = R, cluster = if (p$resampling_type == "assessment_date") "assessment_date_iso" else NULL, seed = p$seed); tl <- proc.time()[["elapsed"]] - t0
  t1 <- proc.time()[["elapsed"]]; spec <- siw_bootstrap_design_spec(dat, cfg, p$outcome, p$outcome_family, p$exposure); v <- siw_bootstrap_target(gr, spec, p$resampling_type, R, p$seed); s <- siw_bootstrap_summarise(v$estimates, R); tp <- proc.time()[["elapsed"]] - t1
  list(legacy = l, package = s, t_legacy = tl, t_package = tp, v = v) }
cat("== reduced replicate count:", reps, "x 180 targets ==\n"); L <- list(); rows <- list()
for (i in seq_len(nrow(plan))) { p <- plan[i, ]; r <- one(p, reps); a <- as.numeric(r$legacy[, cmp_cols]); b <- as.numeric(r$package[, cmp_cols]); d <- ifelse(is.na(a) & is.na(b), 0, abs(a - b))
  rows[[i]] <- data.frame(target_id = p$target_id, outcome_family = p$outcome_family, resampling_type = p$resampling_type, max_abs_diff = max(d), identical = all(d == 0), n_failed_package = r$package$replicates_failed, t_legacy = r$t_legacy, t_package = r$t_package, stringsAsFactors = FALSE) }
R <- do.call(rbind, rows); cat("targets:", nrow(R), " identical in all 7 reported statistics:", sum(R$identical), " max abs diff:", signif(max(R$max_abs_diff), 3), " failed refits (package):", sum(R$n_failed_package), "\n")
cat(sprintf("time at %d replicates: legacy %.1f s, package %.1f s (x%.1f)\n", reps, sum(R$t_legacy), sum(R$t_package), sum(R$t_legacy) / sum(R$t_package)))
cat("\n== full 2,000 replicates, targets", paste(full, collapse = ","), "==\n"); F <- list()
for (i in full) { p <- fullplan[i, ]; r <- one(p, 2000L); a <- as.numeric(r$legacy[, cmp_cols]); b <- as.numeric(r$package[, cmp_cols]); F[[length(F) + 1L]] <- data.frame(target_id = p$target_id, max_abs_diff = max(abs(a - b)), identical = all(a == b), successful = r$package$replicates_successful, t_legacy = r$t_legacy, t_package = r$t_package, speedup = r$t_legacy / r$t_package, stringsAsFactors = FALSE) }
F <- do.call(rbind, F); print(F, digits = 4, row.names = FALSE)
# estimates on identical resamples, replicate by replicate (legacy-style fit vs engine) for the full targets
cat("\nlegacy-style fits on identical resamples (all 2,000 replicates of target", full[1L], "):\n"); p <- fullplan[full[1L], ]; ix <- siw_bootstrap_indices(gr, p$resampling_type, 2000L, p$seed); spec <- siw_bootstrap_design_spec(dat, cfg, p$outcome, p$outcome_family, p$exposure); fam <- stats::binomial()
mine <- vapply(ix, function(i) .siw_boot_fit(i, spec, fam)$est, 0); ref <- vapply(ix, function(i) lenv$siecc_simple_model_coefficient(ldat[i, , drop = FALSE], p$outcome, p$outcome_family, p$exposure, covs), 0); cat("max abs difference:", max(abs(mine - ref)), " bitwise identical:", identical(mine, ref), "\n")
if (!is.null(out)) { dir.create(out, recursive = TRUE, showWarnings = FALSE); siw_write_csv(R, file.path(out, "legacy_equivalence_batch_c_reduced_replicates.csv")); siw_write_csv(F, file.path(out, "legacy_equivalence_batch_c_full_replicates.csv")) }
quit(status = as.integer(!all(R$identical) || !all(F$identical)))
