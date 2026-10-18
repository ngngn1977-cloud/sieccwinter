# Builds FULLY SYNTHETIC input for the PowerShell wrapper tests of the daily layer (Phase 3H.2): a data CSV, a frozen-format reference folder and a matching protocol copy.
# Usage: Rscript validation/make_daily_layer_synthetic.R <out_dir> <variant>      variants: exact | xyd_diff | missing_ref | broken_data
# The reference is built from the package's own output (circular by construction): it tests the wrapper's status handling, not the statistics.
args <- commandArgs(trailingOnly = TRUE); out <- args[1L]; variant <- args[2L]
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); root <- normalizePath(file.path(dirname(script_arg[1L]), ".."), winslash = "/")
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root); CFG <- siw_load_config(); PD <- siw_daily_protocol(); lags <- c(1L, 35L, 59L); B <- 300L
unlink(out, recursive = TRUE); dir.create(out, recursive = TRUE); raw <- siw_make_synthetic(n = 55L, seed = 21L, config = CFG); csv <- file.path(out, "syn.csv"); utils::write.csv(raw, csv, row.names = FALSE, na = "")
dat <- siw_attach_cumulative(siw_prepare_data(raw, CFG), CFG)$data; XD <- siw_xy_daily_grid(dat, CFG, PD, lags = lags); DY <- siw_single_day_grid(dat, CFG, PD, lags = lags); mx <- siw_xy_maxt_grid(dat, CFG, PD, replicates = B)
if (variant == "xyd_diff") XD$kendall_p[5] <- XD$kendall_p[5] * 1.001
refdir <- file.path(out, "ref"); dir.create(refdir); W <- function(x, f) utils::write.csv(x, file.path(refdir, f), row.names = FALSE, na = ""); W(XD, "XY_daily_all_1416.csv"); W(DY, "single_day_adjusted_regression_grid.csv"); W(mx$table, "selected_xy_maxT.csv")
p <- siw_daily_protocol(); tabs <- list()
for (i in seq_along(p$reference_files)) { f <- file.path(refdir, p$reference_files[[i]]$file); p$reference_files[[i]]$sha256 <- siw_file_hash(f)$value; p$reference_files[[i]]$bytes <- file.info(f)$size; tabs[[p$reference_files[[i]]$component]] <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")); p$reference_files[[i]]$expected_records <- nrow(tabs[[p$reference_files[[i]]$component]]) }
for (i in seq_along(p$comparisons)) { cc <- p$comparisons[[i]]; d <- tabs[[cc$reference]]; p$comparisons[[i]]$expected_records <- nrow(d); p$comparisons[[i]]$expected_compared_cells <- sum(vapply(unlist(cc$fields), function(f) sum(is.finite(suppressWarnings(as.numeric(d[[f]])))), 0L)) }
p$parameters$maxt_replicates <- B; p$parameters$lags_subset <- as.list(lags); p$parameters$independent_refit_every <- 8L; p$protocol_path <- NULL; p$protocol_hash <- NULL; jsonlite::write_json(p, file.path(out, "protocol_syn.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
if (variant == "missing_ref") unlink(file.path(refdir, "selected_xy_maxT.csv")); if (variant == "broken_data") writeLines(c("not,a,study,file", "1,2"), csv)
cat("built", variant, "in", out, "\n")
