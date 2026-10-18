# Writes a file inventory of the package (path, role, bytes, lines) to the given CSV (default: PACKAGE_INVENTORY.csv beside the package).
root <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), ".."), winslash = "/")
args <- commandArgs(TRUE); out <- if (length(args)) args[1] else file.path(dirname(root), "reports", "PACKAGE_INVENTORY.csv")
f <- list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = FALSE); f <- f[!grepl("^(output|checkpoints)/", f)]
role <- function(p) if (grepl("^R/", p)) "package code" else if (grepl("^tests/", p)) "tests" else if (grepl("^scripts/", p)) "script" else if (grepl("^validation/", p)) "validation" else if (grepl("^output_schema/", p)) "output schema" else if (grepl("^inst/extdata", p)) "synthetic data / config" else if (grepl("^config/", p)) "configuration" else "top-level"
inv <- data.frame(path = f, role = vapply(f, role, ""), bytes = file.info(file.path(root, f))$size,
  lines = vapply(f, function(p) tryCatch(length(readLines(file.path(root, p), warn = FALSE)), error = function(e) NA_integer_), 1L), stringsAsFactors = FALSE)
inv <- inv[order(inv$role, inv$path), ]; utils::write.csv(inv, out, row.names = FALSE); cat("files:", nrow(inv), "; code lines in R/:", sum(inv$lines[inv$role == "package code"]), "\n")
