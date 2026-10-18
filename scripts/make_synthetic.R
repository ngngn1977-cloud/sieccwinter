# Regenerate the fully synthetic demo data and refresh the installed copy of the configuration.
#   Rscript scripts/make_synthetic.R
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- normalizePath(file.path(dirname(script_arg[1L]), ".."), winslash = "/")
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
cfg <- siw_load_config()
d <- siw_make_synthetic(n = 60L, seed = 1234L, edge_cases = TRUE, config = cfg)
out <- file.path(root, "inst", "extdata", "synthetic_siecc_winter.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
siw_write_csv(d, out)
file.copy(file.path(root, "config", "sieccwinter_config.json"), file.path(root, "inst", "extdata", "sieccwinter_config.json"), overwrite = TRUE)
cat("wrote", out, "with", nrow(d), "rows and", ncol(d), "columns\n")
