# Regenerates output_schema/*.schema.json from a synthetic demo run (archived branch included so all tables are described).
root <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), ".."), winslash = "/")
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
out <- file.path(tempdir(), "schema_run"); r <- siw_run_demo(out, siw_load_config(), resume = FALSE, archived = TRUE)
tabs <- r$tables
siw_write_schemas(tabs, file.path(root, "output_schema"))
cat("schemas written:", paste(names(tabs), collapse = ", "), "\n")
