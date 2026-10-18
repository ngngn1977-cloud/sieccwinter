# Regression tests for scripts/finalise_run.R (manual-run hand-off files), on temporary synthetic fixtures only.
FIN <- file.path(ROOT, "scripts", "finalise_run.R")
RSC <- file.path(R.home("bin"), "Rscript")
run_fin <- function(args) suppressWarnings(system2(RSC, c(shQuote(FIN), shQuote(args)), stdout = TRUE, stderr = TRUE))
status_of <- function(out) { s <- attr(out, "status"); if (is.null(s)) 0L else s }
make_fixture <- function(session_lines = NULL) {
  d <- tempfile("finfix_"); dir.create(d); dir.create(file.path(d, "checkpoints"))
  if (is.null(session_lines)) { invisible(loadNamespace("jsonlite")); session_lines <- utils::capture.output(print(utils::sessionInfo())) }
  writeLines(session_lines, file.path(d, "session_info.txt"))
  utils::write.csv(data.frame(check_id = "V00", category = "structure", status = "PASS", n_affected = 1L, detail = "fixture"), file.path(d, "input_validation_report.csv"), row.names = FALSE)
  utils::write.csv(data.frame(check = "c", status = "PASS", detail = "x"), file.path(d, "self_checks.csv"), row.names = FALSE)
  utils::write.csv(data.frame(stage = "total", seconds = 1, status = "-"), file.path(d, "timings.csv"), row.names = FALSE)
  writeLines("synthetic", file.path(d, "checkpoints", "a.rds")); writeLines("t", file.path(d, "work.tmp")); writeLines("l", file.path(d, "x.lock"))
  writeLines("open log", file.path(d, "finalise_console.log")); writeLines("e", file.path(d, "finalise_err.tmp"))
  d
}

test_that("the case-insensitive session-file problem cannot empty the delivered file", {
  if (file.exists(FIN)) {
    d <- make_fixture(); orig <- readLines(file.path(d, "session_info.txt"))
    out <- run_fin(c(d, "demo", "0", "1"))
    nm <- list.files(d, pattern = "^session_info[.]txt$", ignore.case = TRUE)
    expect_identical(nm, "SESSION_INFO.txt")                                 # one canonical name, exact case
    p <- file.path(d, "SESSION_INFO.txt")
    expect_true(file.info(p)$size > 0)
    got <- readLines(p); expect_identical(got, orig)                          # complete content preserved
    expect_true(any(grepl("^R version ", got))); expect_true(any(grepl("attached base packages", got))); expect_true(any(grepl("loaded via a namespace", got)))
    expect_false(file.exists(file.path(d, "SESSION_INFO.txt.tmp")))
    out2 <- run_fin(c(d, "demo", "0", "1"))                                   # finalising twice is safe
    expect_identical(readLines(p), orig)
  }
})

test_that("an empty or incomplete session source makes finalisation fail", {
  if (file.exists(FIN)) {
    d <- make_fixture(character()); writeLines("", file.path(d, "session_info.txt"))
    expect_true(status_of(run_fin(c(d, "demo", "0", "1"))) != 0L)
    d2 <- make_fixture("R version 4.4.3 only"); expect_true(status_of(run_fin(c(d2, "demo", "0", "1"))) != 0L)
  }
})

test_that("manifest excludes transient files and the open finalisation log; every row verifies", {
  if (file.exists(FIN)) {
    d <- make_fixture(); run_fin(c(d, "demo", "0", "1"))
    m <- read.csv(file.path(d, "RUN_MANIFEST.csv"))
    expect_false(any(m$file %in% c("RUN_MANIFEST.csv", "finalise_console.log", "finalise_err.tmp", "work.tmp", "x.lock")))
    expect_false(any(grepl("^checkpoints/", m$file)))
    expect_true(all(c("SESSION_INFO.txt", "RUN_SUMMARY.md", "INPUT_VALIDATION.csv", "VALIDATION_STATUS.csv", "RUNTIME.csv", "self_checks.csv") %in% m$file))
    expect_true(all(file.exists(file.path(d, m$file))))
    expect_equal(m$bytes, unname(file.info(file.path(d, m$file))$size))
    expect_true(all(!grepl("[/\\\\]{2}|^[A-Za-z]:|^/", m$file)))               # relative paths
    expect_true(all(m$bytes > 0))
    expect_equal(status_of(run_fin(c("--verify", d))), 0L)
    writeLines("appended after the manifest", file.path(d, "finalise_console.log"))   # a log appended later does not matter
    expect_equal(status_of(run_fin(c("--verify", d))), 0L)
  }
})

test_that("modification, removal or size change of a listed file is detected", {
  if (file.exists(FIN)) {
    d <- make_fixture(); run_fin(c(d, "demo", "0", "1"))
    cat("tamper\n", file = file.path(d, "timings.csv"), append = TRUE)
    expect_equal(status_of(run_fin(c("--verify", d))), 1L)
    d2 <- make_fixture(); run_fin(c(d2, "demo", "0", "1"))
    p <- file.path(d2, "self_checks.csv"); x <- readLines(p); x[2] <- sub("PASS", "FAIL", x[2]); writeLines(x, p)      # same size, different content
    expect_equal(status_of(run_fin(c("--verify", d2))), 1L)
    d3 <- make_fixture(); run_fin(c(d3, "demo", "0", "1")); unlink(file.path(d3, "RUNTIME.csv"))
    expect_equal(status_of(run_fin(c("--verify", d3))), 1L)
  }
})
