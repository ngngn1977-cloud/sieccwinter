# Test entry point. With testthat installed this is the standard test_check(); otherwise a minimal base-R shim runs the same test files.
# Usage from a source checkout (no installation): Rscript tests/testthat.R [report_csv_path]
args <- commandArgs(TRUE)
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat); library(sieccwinter); test_check("sieccwinter"); quit(status = 0L)
}
fa <- grep("--file=", commandArgs(FALSE), value = TRUE)
tdir <- if (length(fa)) normalizePath(dirname(sub("--file=", "", fa[1])), winslash = "/") else normalizePath(getwd(), winslash = "/")
root <- dirname(tdir); src <- dir.exists(file.path(root, "R")) && file.exists(file.path(root, "scripts", "load_package.R"))
if (src) { source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root); parent <- globalenv() } else { library(sieccwinter); parent <- asNamespace("sieccwinter") }
.res <- new.env(); .res$rows <- list(); .res$cur <- NULL; .res$file <- ""
.rec <- function(ok, msg) { r <- .res$cur; r$n <- r$n + 1L; if (!ok) { r$fail <- r$fail + 1L; r$msgs <- c(r$msgs, msg) }; .res$cur <- r; invisible(ok) }
shim <- new.env(parent = parent)
shim$test_that <- function(desc, code) {
  .res$cur <- list(n = 0L, fail = 0L, msgs = character(), err = "")
  t0 <- Sys.time()
  tryCatch(eval(substitute(code), parent.frame()), error = function(e) .res$cur$err <- conditionMessage(e))
  .res$rows[[length(.res$rows) + 1L]] <- data.frame(file = .res$file, test = desc, expectations = .res$cur$n, failed = .res$cur$fail, error = .res$cur$err,
    seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2), detail = paste(.res$cur$msgs, collapse = " || "), stringsAsFactors = FALSE)
}
shim$expect_true <- function(x) .rec(length(x) > 0 && isTRUE(all(x)), paste("not TRUE:", deparse(substitute(x))[1]))
shim$expect_false <- function(x) .rec(length(x) > 0 && isTRUE(all(!x)), paste("not FALSE:", deparse(substitute(x))[1]))
shim$expect_identical <- function(a, b) .rec(identical(a, b), paste("not identical:", deparse(substitute(a))[1]))
shim$expect_equal <- function(a, b, tolerance = 1.5e-8) { r <- all.equal(a, b, tolerance = tolerance, check.attributes = FALSE); .rec(isTRUE(r), paste("not equal:", deparse(substitute(a))[1], "-", paste(r, collapse = "; "))) }
shim$expect_error <- function(x, regexp = NULL) { m <- tryCatch({ x; NULL }, error = function(e) conditionMessage(e)); .rec(!is.null(m) && (is.null(regexp) || grepl(regexp, m)), paste("expected error:", deparse(substitute(x))[1])) }
td <- file.path(tdir, "testthat"); old <- setwd(td); on.exit(setwd(old))
for (h in sort(list.files(".", pattern = "^helper-.*[.]R$"))) source(h, local = shim)
for (f in sort(list.files(".", pattern = Sys.getenv("SIW_TEST_PATTERN", "^test-.*[.]R$")))) {     # SIW_TEST_PATTERN limits the run to matching test files (default: all)
  .res$file <- f
  tryCatch(source(f, local = new.env(parent = shim)), error = function(e) .res$rows[[length(.res$rows) + 1L]] <- data.frame(file = f, test = "(file)", expectations = 0L, failed = 0L, error = conditionMessage(e), seconds = 0, detail = "", stringsAsFactors = FALSE))
}
R <- do.call(rbind, .res$rows)
print(R[, c("file", "test", "expectations", "failed", "error", "seconds")], right = FALSE)
cat(sprintf("\nTESTS: %d; EXPECTATIONS: %d; FAILED EXPECTATIONS: %d; ERRORED TESTS: %d\n", nrow(R), sum(R$expectations), sum(R$failed), sum(nzchar(R$error))))
sk <- if (exists("SIW_SOURCE_ONLY_SKIPS", envir = shim, inherits = FALSE)) shim$SIW_SOURCE_ONLY_SKIPS$items else character()
cat(sprintf("SKIPPED (source checkout only; not counted as passed): %d%s\n", length(sk), if (length(sk)) paste0("\n  ", paste(sk, collapse = "\n  ")) else ""))
bad <- which(R$failed > 0 | nzchar(R$error))
if (length(bad)) { cat("\nFAILURE DETAIL\n"); for (i in bad) cat(R$file[i], "|", R$test[i], "|", R$error[i], R$detail[i], "\n") }
if (length(args)) utils::write.csv(R, args[1], row.names = FALSE)
setwd(old); quit(status = as.integer(length(bad) > 0))
