# Package hygiene: the installed package reads inst/extdata, so every config/*.json must equal its inst/extdata copy; NAMESPACE exports and man pages agree;
# private folders and data files are excluded from the build; the full-run preflight requires what DESCRIPTION declares.

test_that("every config/*.json is byte-identical to its inst/extdata copy (source checkout)", {
  cd <- file.path(ROOT, "config"); xd <- file.path(ROOT, "inst", "extdata")
  if (dir.exists(cd) && dir.exists(xd)) {
    f <- setdiff(list.files(cd, pattern = "[.]json$"), c("private_local.json", "private_local.template.json")); expect_true(length(f) >= 17L)   # the local configuration is never shipped in inst/extdata
    for (x in f) expect_identical(unname(tools::md5sum(file.path(cd, x))), unname(tools::md5sum(file.path(xd, x))))
  }
})

test_that("every exported function exists and has a man page; every man page documents an export", {
  ns <- file.path(ROOT, "NAMESPACE"); md <- file.path(ROOT, "man")
  if (file.exists(ns) && dir.exists(md)) {
    ex <- unlist(lapply(regmatches(readLines(ns), gregexpr("(?<=export\\().*?(?=\\))", readLines(ns), perl = TRUE)), function(z) trimws(unlist(strsplit(z, ",")))))
    ex <- ex[nzchar(ex)]; rd <- sub("[.]Rd$", "", list.files(md, pattern = "[.]Rd$"))
    expect_identical(sort(setdiff(ex, rd)), character()); expect_identical(sort(setdiff(rd, ex)), character())
    expect_true(all(vapply(ex, exists, logical(1), mode = "function")))
  }
})

test_that(".Rbuildignore and .gitignore exclude private folders, run outputs, data and the filled paths file (source checkout)", {
  rb <- file.path(ROOT, ".Rbuildignore"); gi <- file.path(ROOT, ".gitignore")
  if (file.exists(rb)) {
    pats <- readLines(rb)
    for (p in c("private_validation", "runs", "output", "manual_run_paths.json", "fullrun_x", "siecc_winter_canonical.csv", "x.sav", "config", "output_schema", "DEVELOPMENT_LOG_2026-10-06.md"))
      expect_true(any(vapply(pats, function(re) grepl(re, p), logical(1))))
    expect_false(any(vapply(pats, function(re) grepl(re, "inst"), logical(1)))); expect_false(any(vapply(pats, function(re) grepl(re, "R"), logical(1))))
  }
  if (file.exists(gi)) { g <- readLines(gi); for (x in c("private_validation/", "runs/", "manual_run_paths.json", "fullrun_*/", "*.sav", ".Renviron", "config/private_local.json", "tests/testthat/private_constants.json", "config/project_inputs_declaration_v1.json")) expect_true(x %in% g) }
})

test_that("the full-run preflight requires the versions DESCRIPTION declares (source checkout)", {
  pf <- file.path(ROOT, "scripts", "fullrun_env_check.R"); de <- file.path(ROOT, "DESCRIPTION")
  if (file.exists(pf) && file.exists(de)) {
    txt <- paste(readLines(pf), collapse = "\n"); d <- read.dcf(de)
    expect_true(grepl("MASS = \"7.3-64\"", txt, fixed = TRUE) && grepl("MASS (>= 7.3-64)", d[, "Imports"], fixed = TRUE))
    expect_true(grepl("jsonlite = \"2.0.0\"", txt, fixed = TRUE) && grepl("jsonlite (>= 2.0.0)", d[, "Imports"], fixed = TRUE))
    expect_true(grepl("EXPECTED_CONFIG_SHA <- \"5A8C605469AE0AA8133215E8E1666182BEAE17F92F01A00CD0BDE0612E3DC5DE\"", txt, fixed = TRUE))
    expect_identical(toupper(CFG$config_hash), "5A8C605469AE0AA8133215E8E1666182BEAE17F92F01A00CD0BDE0612E3DC5DE")
  }
})
