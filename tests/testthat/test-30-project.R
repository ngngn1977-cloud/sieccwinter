# No-code interface from R (synthetic only): the empty-project helper and siw_run_project() through the shared launcher (plan only; nothing is run).

test_that("siw_create_project makes data, references, outputs and README_FIRST.txt", {
  p <- file.path(tempdir(), paste0("siw proj ", sample.int(1e6, 1))); siw_create_project(p)
  expect_true(all(dir.exists(file.path(p, c("data", "references", "outputs"))))); expect_true(file.exists(file.path(p, "README_FIRST.txt")))
  expect_true(any(grepl("identifies|recognises every file by its", readLines(file.path(p, "README_FIRST.txt")))))
  expect_error(siw_create_project(""), "SIW_PROJECT_PATH_REQUIRED")
})

test_that("siw_run_project shows the launcher's plan: synthetic data outside demo mode is NOT_THE_ACCEPTED_INPUT; nothing is run", {
  if (siw_skip_source_only("test-30-project: siw_run_project needs the launcher of the package folder")) return(invisible(NULL))
  if (.Platform$OS.type != "windows") return(invisible(NULL))
  p <- file.path(tempdir(), paste0("siwrp", sample.int(1e6, 1))); siw_create_project(p); file.copy(SYN_CSV, file.path(p, "data", "renamed.csv"))
  out <- utils::capture.output(rc <- siw_run_project(p, plan_only = TRUE))
  expect_equal(rc, 0L); expect_true(any(grepl("NOT_THE_ACCEPTED_INPUT", out))); expect_true(any(grepl("MODE: ANALYSIS", out, fixed = TRUE)))
  expect_true(any(grepl("PLAN_ONLY: the run can start.", out, fixed = TRUE))); expect_equal(length(list.dirs(file.path(p, "outputs"), recursive = FALSE)), 0L)
})

test_that("siw_run_project with no folder chosen runs nothing", {
  expect_equal(suppressMessages(siw_run_project(NA_character_)), 2L)
})
