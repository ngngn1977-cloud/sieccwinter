# Tests of the private SAV -> canonical CSV workflow (scripts/prepare_private_csv.R, scripts/private_conversion_lib.R).
# All inputs are fully synthetic .sav files written by tests/testthat/helper-sav.R; the confidential SAV is never involved.
PRIV_LIB <- file.path(ROOT, "scripts", "private_conversion_lib.R")
PRIV_CLI <- file.path(ROOT, "scripts", "prepare_private_csv.R")
HAVE_PRIV <- file.exists(PRIV_LIB) && file.exists(PRIV_CLI) && requireNamespace("foreign", quietly = TRUE)
if (HAVE_PRIV) source(PRIV_LIB)
RSC <- file.path(R.home("bin"), "Rscript")
prep_cli <- function(args) { o <- suppressWarnings(system2(RSC, c(shQuote(PRIV_CLI), shQuote(args)), stdout = TRUE, stderr = TRUE)); st <- attr(o, "status"); list(out = o, status = if (is.null(st)) 0L else st) }
new_dir <- function(tag) { d <- file.path(tempdir(), paste0(tag, "_", sample.int(1e6, 1))); dir.create(d); d }
SAVDIR <- if (HAVE_PRIV) new_dir("sav") else tempdir()
audit_status <- function(d, id) { a <- read.csv(file.path(d, "CONVERSION_AUDIT.csv"), stringsAsFactors = FALSE); a$status[a$check_id == id] }
convert_inproc <- function(sav, exp_rows = NA_integer_) { r1 <- siw_priv_read_sav(sav, TRUE); r2 <- siw_priv_read_sav(sav, FALSE); siw_priv_convert(r1$data, CFG, r2$data, exp_rows) }

test_that("mapping covers exactly the canonical schema and the audited SAV aliases", {
  if (HAVE_PRIV) {
    map <- siw_priv_mapping(CFG)
    hdr <- names(read.csv(SYN_CSV, nrows = 1, check.names = FALSE))
    expect_identical(siw_priv_canonical_columns(CFG)[1:22], hdr[1:22]); expect_identical(sort(siw_priv_canonical_columns(CFG)), sort(hdr))   # same schema; scalar order fixed
    expect_identical(map$canonical, siw_priv_canonical_columns(CFG))
    expect_equal(nrow(map), 574L); expect_equal(sum(map$role == "daily_exposure"), 472L); expect_equal(sum(map$role == "cumulative_exposure"), 80L)
    al <- setNames(map$source, map$canonical)
    expect_identical(unname(al[c("Econ_STS", "Subjective_Health", "Memory", "Memory_Changed", "Subjective_Wellbeing", "Outdoor_Mobility", "PedestLength_m")]),
      c("economic_vulnerabilityref.elsepoorfairavaerage1", "Subjective_helth", "Howwouldyourateyourmemory", "Comparedwiththepreviousseasonsummerwinterisyourmemor", "Subjective_wellbieng", "outdoor_mobility", "PedestLengthm"))
    expect_identical(unname(al["Memory_assess"]), "Memory_assess")            # no fallback to the memory rating
    expect_identical(unname(al[c("Jewish", "Muslim", "Christian")]), c("Jewish", "Muslim", "Christian"))
    expect_false(any(grepl("date|address|latitude|longitude|coord", map$canonical, ignore.case = TRUE)))
  }
})

test_that("legacy read semantics: labelled numerics stay numeric, user-defined and system missing become NA, strings untrimmed, order kept", {
  if (HAVE_PRIV) {
    vars <- list(list(name = "participant_code", type = "string", width = 8L, values = c("SYN001", "SYN002", "SYN003", "SYN004")),
      list(name = "A_long.variable_name_beyond_eight", type = "numeric", values = c(0.1 + 0.2, 1 / 3, NA, 99), user_missing = 99, value_labels = list(low = 1, high = 2)),
      list(name = "plain", type = "numeric", values = c(99, 5, 1e-17, 123456789.123456789)))
    f <- tempfile(fileext = ".sav"); write_min_sav(f, vars, 4L)
    r <- siw_priv_read_sav(f, TRUE); k <- siw_priv_read_sav(f, FALSE)
    expect_identical(names(r$data), c("participant_code", "A_long.variable_name_beyond_eight", "plain"))       # long names and column order kept
    x <- r$data[[2]]; expect_true(is.numeric(x) && !is.factor(x))                                              # value labels not applied
    expect_true(!is.null(attr(x, "value.labels")))
    expect_identical(as.numeric(x)[1:2], c(0.1 + 0.2, 1 / 3))                                                   # numeric precision preserved
    expect_true(is.na(x[3])); expect_true(is.na(x[4]))                                                          # system missing and user-defined missing (99) -> NA
    expect_equal(as.numeric(k$data[[2]])[4], 99)                                                                # ... but the code is visible without use.missings
    expect_equal(as.numeric(r$data$plain)[1], 99)                                                               # 99 is NOT missing where it was not declared
    expect_identical(as.numeric(r$data$plain)[3:4], c(1e-17, 123456789.123456789))
    expect_identical(r$data$participant_code, c("SYN001  ", "SYN002  ", "SYN003  ", "SYN004  "))                # trailing blanks kept by read.spss (legacy code trims)
    cv <- siw_priv_convert(r$data[0, ], CFG); expect_true(any(cv$audit$status == "FAIL"))                       # not a study file: required variables missing
  }
})

test_that("number formatting round-trips exactly and missing values are empty", {
  if (HAVE_PRIV) {
    x <- c(0.1 + 0.2, 1 / 3, 1e-17, 123456789.123456789, 5, -2.5, 1e15 + 0.5, 22 / 7, NA)
    s <- siw_priv_fmt_num(x); expect_identical(s[9], ""); expect_identical(as.numeric(s[1:8]), x[1:8]); expect_identical(s[5], "5")
    expect_error(siw_priv_fmt_num(c(1, Inf)), "NONFINITE")
  }
})

test_that("end-to-end conversion of a synthetic SAV reproduces the canonical data exactly and exports only required variables", {
  if (HAVE_PRIV) {
    sav <- file.path(SAVDIR, "synthetic_study.sav"); make_synthetic_sav(sav)
    d <- new_dir("conv"); r <- prep_cli(c("--sav", sav, "--out-dir", d, "--expected-rows", "60"))
    expect_equal(r$status, 0L); expect_true(any(grepl("CONVERSION_STATUS=PASS", r$out)))
    expect_true(all(file.exists(file.path(d, siw_priv_output_names()))))
    csv <- file.path(d, "siecc_winter_canonical.csv"); back <- siw_read_study_csv(csv, CFG)
    expect_identical(names(back), siw_priv_canonical_columns(CFG))                                   # column order
    expect_identical(back$participant_code, SYN$participant_code)                                     # row order and identifiers
    expect_true(all(vapply(setdiff(names(SYN), "participant_code"), function(n) identical(as.numeric(back[[n]]), as.numeric(SYN[[n]])), logical(1))))   # exact numerics incl. NA
    expect_false(any(c("assessment_date_iso", "home_address_raw", "latitude", "longitude") %in% names(back)))
    expect_equal(sum(siw_validate_input(back, CFG, lag_end = 59L)$status == "FAIL"), 0L)
    expect_equal(audit_status(d, "C14"), "PASS"); expect_equal(audit_status(d, "C13"), "PASS"); expect_equal(audit_status(d, "C17"), "PASS")
    m <- read.csv(file.path(d, "CONVERSION_MANIFEST.csv")); expect_true("synthetic_study.sav" %in% m$file[m$role == "input"])
    expect_true(all(m$bytes > 0)); expect_true(all(!grepl("[/\\\\]", m$file)))
    for (i in which(m$role == "output")) expect_identical(toupper(siw_file_hash(file.path(d, m$file[i]))$value), toupper(m$sha256_or_hash[i]))
    dict <- jsonlite::fromJSON(file.path(d, "canonical_data_dictionary.json"), simplifyVector = FALSE); expect_equal(dict$n_columns, 574L); expect_equal(length(dict$columns), 574L)
    expect_equal(sum(nchar(list.files(d)) > 0), 6L)                                                    # exactly the six declared files, no stray or partial files
    expect_false(any(grepl("[.]partial$", list.files(d))))
    # existing paper-mode interface accepts the converted CSV (synthetic data; R function behind run_paper.ps1)
    ext_ok <- tolower(tools::file_ext(csv)) == "csv"; expect_true(ext_ok)
    p <- siw_run_paper(csv, file.path(d, "paper_check"), CFG, resume = FALSE); expect_equal(p$status, "OK"); expect_equal(nrow(p$tables$cumulative_results), 1680L); expect_equal(nrow(p$tables$dlm_final_summary), 9L)
    refd <- new_dir("emptyref"); v <- siw_run_validate(csv, refd, file.path(d, "val_check"), CFG, resume = FALSE)
    expect_equal(v$overall, "FAIL")          # validate interface accepts the CSV; with no reference files the required references are absent, which now FAILS (Phase 2D)
  }
})

test_that("user-defined missing codes follow the legacy rule and are counted in aggregate", {
  if (HAVE_PRIV) {
    sav <- file.path(SAVDIR, "usermiss.sav"); ov <- list(PSQI = replace(SYN$PSQI, c(3, 4), 99))
    make_synthetic_sav(sav, override = ov, extra_missing = list(PSQI = list(user_missing = 99)))
    cv <- convert_inproc(sav); expect_true(is.na(cv$data$PSQI[3]) && is.na(cv$data$PSQI[4]))
    c9 <- cv$audit[cv$audit$check_id == "C09", ]; expect_equal(c9$observed, "2"); expect_true(grepl("1 canonical source", c9$detail))
    sav2 <- file.path(SAVDIR, "undeclared.sav"); make_synthetic_sav(sav2, override = ov)                # 99 not declared missing: kept as a value
    expect_equal(convert_inproc(sav2)$data$PSQI[3], 99)
  }
})

test_that("invalid source data are rejected (never corrected)", {
  if (HAVE_PRIV) {
    chk <- function(id, ...) { f <- tempfile(fileext = ".sav"); make_synthetic_sav(f, ...); cv <- convert_inproc(f); expect_true(any(cv$audit$status == "FAIL" & cv$audit$check_id == id)); cv }
    chk("C04", drop = "Age")                                                                            # missing required variable
    chk("C04", drop = "Memory_assess")                                                                  # no silent fallback to the memory rating
    x <- SYN$participant_code; x[2] <- x[1]; chk("C06", override = list(participant_code = x))          # duplicate identifiers
    x <- SYN$participant_code; x[5] <- "   "; chk("C06", override = list(participant_code = x))         # blank identifier
    chk("VAL_V04_Sex", override = list(Sex = replace(SYN$Sex, 4, 2)))                                    # out-of-range coding
    chk("VAL_V04_Area", override = list(Area = replace(SYN$Area, 7, 4)))                                 # unexpected factor level
    chk("VAL_V10", override = list(int_dummy = 1 - SYN$int_dummy))                                      # coding inconsistency
    f <- tempfile(fileext = ".sav"); make_synthetic_sav(f); cv <- convert_inproc(f, 59L)                # expected row count mismatch
    expect_true(any(cv$audit$check_id == "C02" & cv$audit$status == "FAIL"))
    # numeric field stored as text in the source
    vars_bad <- tempfile(fileext = ".sav")
    map <- siw_priv_mapping(CFG); vars <- lapply(seq_len(nrow(map)), function(i) { cn <- map$canonical[i]
      if (cn == "participant_code") list(name = map$source[i], type = "string", width = 8L, values = SYN[[cn]])
      else if (cn == "Sex") list(name = map$source[i], type = "string", width = 8L, values = as.character(SYN[[cn]]))
      else list(name = map$source[i], type = "numeric", values = as.numeric(SYN[[cn]])) })
    write_min_sav(vars_bad, vars, nrow(SYN)); cv <- convert_inproc(vars_bad); expect_true(any(cv$audit$check_id == "C07" & cv$audit$status == "FAIL"))
  }
})

test_that("a failed conversion writes no canonical CSV and the CLI returns 1", {
  if (HAVE_PRIV) {
    sav <- file.path(SAVDIR, "bad_sex.sav"); make_synthetic_sav(sav, override = list(Sex = replace(SYN$Sex, 4, 2)))
    d <- new_dir("fail"); r <- prep_cli(c("--sav", sav, "--out-dir", d)); expect_equal(r$status, 1L)
    expect_false(file.exists(file.path(d, "siecc_winter_canonical.csv"))); expect_true(file.exists(file.path(d, "CONVERSION_AUDIT.csv")))
    expect_true(grepl("Conversion summary: FAIL", readLines(file.path(d, "CONVERSION_SUMMARY.md"))[1]))
    d2 <- new_dir("hash"); sav2 <- file.path(SAVDIR, "ok_hash.sav"); make_synthetic_sav(sav2)
    r2 <- prep_cli(c("--sav", sav2, "--out-dir", d2, "--expected-sav-sha256", strrep("0", 64))); expect_equal(r2$status, 1L); expect_false(file.exists(file.path(d2, "siecc_winter_canonical.csv")))
    d3 <- new_dir("rows"); r3 <- prep_cli(c("--sav", sav2, "--out-dir", d3, "--expected-rows", "59")); expect_equal(r3$status, 1L)
    d4 <- new_dir("hashok"); r4 <- prep_cli(c("--sav", sav2, "--out-dir", d4, "--expected-sav-sha256", siw_file_hash(sav2)$value)); expect_equal(r4$status, 0L)
  }
})

test_that("existing conversions are never overwritten without --overwrite", {
  if (HAVE_PRIV) {
    sav <- file.path(SAVDIR, "ow.sav"); make_synthetic_sav(sav); d <- new_dir("ow")
    expect_equal(prep_cli(c("--sav", sav, "--out-dir", d))$status, 0L)
    before <- unname(tools::md5sum(file.path(d, siw_priv_output_names())))
    r <- prep_cli(c("--sav", sav, "--out-dir", d)); expect_equal(r$status, 2L); expect_true(any(grepl("--overwrite", r$out)))
    expect_identical(unname(tools::md5sum(file.path(d, siw_priv_output_names()))), before)
    expect_equal(prep_cli(c("--sav", sav, "--out-dir", d, "--overwrite"))$status, 0L)
    expect_false(any(grepl("[.]partial$", list.files(d))))
  }
})

test_that("invalid arguments and unsafe output locations are refused with exit code 2 and nothing is written", {
  if (HAVE_PRIV) {
    sav <- file.path(SAVDIR, "args.sav"); make_synthetic_sav(sav); d <- new_dir("args")
    expect_equal(prep_cli(c("--sav", sav))$status, 2L); expect_equal(prep_cli(c("--out-dir", d))$status, 2L)
    expect_equal(prep_cli(c("--sav", sav, "--out-dir", d, "--bogus"))$status, 2L)
    expect_equal(prep_cli(c("--sav", file.path(SAVDIR, "nope.sav"), "--out-dir", d))$status, 2L)
    csv <- file.path(tempdir(), "x.csv"); writeLines("a", csv); expect_equal(prep_cli(c("--sav", csv, "--out-dir", d))$status, 2L)
    expect_equal(prep_cli(c("--sav", sav, "--out-dir", file.path(ROOT, "private_out_test")))$status, 2L)          # inside the repository
    expect_false(dir.exists(file.path(ROOT, "private_out_test")))
    expect_equal(prep_cli(c("--sav", sav, "--out-dir", file.path(dirname(sav), "sub_out")))$status, 2L)          # inside the SAV folder
    expect_false(dir.exists(file.path(dirname(sav), "sub_out")))
    expect_equal(length(list.files(d)), 0L)
    # only the chosen directory is touched
    par <- new_dir("parent"); before <- list.files(par, recursive = TRUE); out <- file.path(par, "priv"); expect_equal(prep_cli(c("--sav", sav, "--out-dir", out))$status, 0L)
    expect_true(all(startsWith(list.files(par, recursive = TRUE), "priv/")))
    expect_true(siw_priv_inside(file.path(out, "a"), out)); expect_false(siw_priv_inside(paste0(out, "_x"), out)); expect_false(siw_priv_inside(dirname(out), out))
  }
})

test_that("logs, reports, dictionary, manifest and console output contain no participant-level or confidential content", {
  if (HAVE_PRIV) {
    sav <- file.path(SAVDIR, "priv_check.sav"); make_synthetic_sav(sav); d <- new_dir("privacy")
    r <- prep_cli(c("--sav", sav, "--out-dir", d)); expect_equal(r$status, 0L)
    files <- setdiff(siw_priv_output_names(), "siecc_winter_canonical.csv")
    txt <- paste(c(r$out, unlist(lapply(file.path(d, files), readLines, warn = FALSE))), collapse = "\n")
    expect_false(grepl("SYN0[0-9][0-9]", txt)); expect_false(grepl("STREET|DECOY", txt))
    expect_false(grepl(gsub("\\\\", "/", tempdir()), gsub("\\\\", "/", txt), fixed = TRUE))                    # no absolute private paths
    expect_false(grepl("[A-Za-z]:[/\\\\]Users", txt))
    expect_false(grepl(sprintf("%.6f", 32.001), txt, fixed = TRUE))
    # the scripts themselves carry no absolute development path or credentials
    src <- paste(c(readLines(PRIV_LIB), readLines(PRIV_CLI)), collapse = "\n"); expect_false(grepl("[A-Za-z]:[/\\\\](Users|Program)", src)); expect_false(grepl("password|api[_-]?key|secret", src, ignore.case = TRUE))
  }
})
