# Batch C stress equivalence with the original (legacy) bootstrap on FULLY SYNTHETIC, deliberately difficult data: failing refits, vanishing locality levels, heavy missingness, separation and
# collinearity. The legacy functions are sourced read-only. For every stress data set, outcome (continuous and binary) and scheme the package and the legacy function are run with the same seed and the
# replicate counts of the legacy summary are compared, together with the failure classification of the package (attempted, successful, failed by reason).
#   Rscript validation/legacy_equivalence_batch_c_stress_synthetic.R <legacy_package_root> [<library_with_dlnm>] [--reps N] [--out DIR]
args <- commandArgs(trailingOnly = TRUE); legacy_root <- args[1L]; lib <- if (length(args) > 1L && !startsWith(args[2L], "--")) args[2L] else NULL
opt <- function(f, d) { i <- match(f, args); if (is.na(i)) d else args[i + 1L] }; reps <- as.integer(opt("--reps", "400")); out <- opt("--out", NULL)
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); root <- normalizePath(file.path(dirname(script_arg[1L]), ".."), winslash = "/")
if (!is.null(lib)) .libPaths(c(lib, .libPaths())); source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root); cfg <- siw_load_config()
lenv <- new.env(parent = globalenv()); for (f in sort(list.files(file.path(legacy_root, "R"), pattern = "[.]R$", full.names = TRUE))) sys.source(f, envir = lenv)
raw <- siw_make_synthetic(config = cfg); base <- siw_attach_cumulative(siw_prepare_data(raw, cfg), cfg)$data; base$Int_dummy <- as.integer(base$Int_dummy); covs <- cfg$tiers[[cfg$primary_tier]]
set.seed(77)
S <- list()
S$small_n <- base[sample(nrow(base), 14), ]                                                                                       # few participants: locality levels vanish, near-singular fits
S$heavy_missing <- { d <- base[sample(nrow(base), 40), ]; d$Age[sample(40, 24)] <- NA; d }                                          # complete cases often <= 8: TOO_FEW_COMPLETE_CASES
S$rare_locality <- { d <- base; lv <- levels(d$Locality_3group); d$Locality_3group[seq_len(nrow(d))] <- lv[1L]; d$Locality_3group[1:2] <- lv[2L]; d$Locality_3group[3] <- lv[3L]; d }   # two levels held by 1-2 participants
S$sparse_events <- { d <- base; d$Int_dummy <- 0L; d$Int_dummy[sample(nrow(d), 3)] <- 1L; d }                                       # 3 events: separation and non-convergence in resamples
S$collinear <- { d <- base; d$Econ_STS <- d$Sex; d }                                                                                 # a covariate aliased with another in every sample
S$constant_exposure_part <- { d <- base; d$UTCI_AVG_7D[-(1:6)] <- 5; d }                                                             # exposure nearly constant: aliased / unstable in many resamples
rows <- list()
for (nm in names(S)) {
  d <- S[[nm]]; d$assessment_date_iso <- as.character(d$assessment_day); gr <- siw_bootstrap_groups(d)
  for (oc in cfg$outcomes$outcome) for (sch in c("participant", "assessment_date")) {
    fam <- cfg$outcomes$family[cfg$outcomes$outcome == oc]; ex <- "UTCI_AVG_7D"; seed <- 424242L
    l <- tryCatch(lenv$siecc_bootstrap_one(d, oc, fam, ex, covs, replicates = reps, cluster = if (sch == "assessment_date") "assessment_date_iso" else NULL, seed = seed), error = function(e) NULL)
    spec <- siw_bootstrap_design_spec(d, cfg, oc, fam, ex); v <- siw_bootstrap_target(gr, spec, sch, reps, seed); s <- siw_bootstrap_summarise(v$estimates, reps)
    cols <- c("replicates_successful", "estimate_median", "ci_lower", "ci_upper", "positive_fraction", "negative_fraction"); a <- as.numeric(l[, cols]); b <- as.numeric(s[, cols]); dd <- ifelse(is.na(a) & is.na(b), 0, abs(a - b))
    tb <- table(v$reason[nzchar(v$reason)]); rows[[length(rows) + 1L]] <- data.frame(dataset = nm, outcome = oc, scheme = sch, successful_legacy = l$replicates_successful, successful_package = s$replicates_successful, failed = s$replicates_failed,
      reasons = paste(names(tb), tb, sep = "=", collapse = "; "), glm_not_converged = sum(v$glm_not_converged), level_absent = sum(v$level_absent), max_abs_diff = max(dd), identical = all(dd == 0), stringsAsFactors = FALSE) } }
R <- do.call(rbind, rows); options(width = 220); print(R[, c("dataset", "outcome", "scheme", "successful_legacy", "successful_package", "failed", "glm_not_converged", "level_absent", "max_abs_diff", "identical", "reasons")], right = FALSE, row.names = FALSE)
cat("\ncombinations:", nrow(R), " identical:", sum(R$identical), " with failed refits:", sum(R$failed > 0), " total failed:", sum(R$failed), " with glm non-convergence:", sum(R$glm_not_converged > 0), " with absent locality levels:", sum(R$level_absent > 0), "\n")
if (!is.null(out)) { dir.create(out, recursive = TRUE, showWarnings = FALSE); siw_write_csv(R, file.path(out, "legacy_equivalence_batch_c_stress.csv")) }
quit(status = as.integer(!all(R$identical)))
