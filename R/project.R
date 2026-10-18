# No-code interface from R: the same launcher as RUN_SIECC.cmd (scripts/siecc_launcher.ps1), which identifies the inputs of a project folder by content
# and runs run_all.ps1 in project mode. Needs Windows and the package source folder (the PowerShell wrappers are not part of an installed package).

#' Create an empty project folder for the no-code interface
#' @param path folder to create (or reuse); data, references and outputs subfolders and README_FIRST.txt are added
#' @export
siw_create_project <- function(path) {
  if (missing(path) || !nzchar(path)) stop("SIW_PROJECT_PATH_REQUIRED", call. = FALSE)
  for (d in c("data", "references", "outputs")) dir.create(file.path(path, d), recursive = TRUE, showWarnings = FALSE)
  rf <- file.path(path, "README_FIRST.txt"); tpl <- .siw_extdata_file("project_README_FIRST.txt")
  if (!file.exists(rf) && nzchar(tpl)) file.copy(tpl, rf)
  invisible(normalizePath(path, winslash = "/"))
}

.siw_extdata_file <- function(name) { root <- siw_root(); cand <- file.path(root, c("inst/extdata", "extdata"), name); cand <- cand[file.exists(cand)]; if (length(cand)) cand[1L] else "" }

.siw_pick_folder <- function(caption = "Choose the SIECC project folder") {
  if (.Platform$OS.type == "windows") return(utils::choose.dir(caption = caption))
  if (requireNamespace("tcltk", quietly = TRUE) && isTRUE(capabilities("tcltk"))) { p <- tcltk::tclvalue(tcltk::tkchooseDirectory(title = caption)); if (nzchar(p)) return(p) }
  if (interactive()) { p <- trimws(readline("Paste the project folder path: ")); if (nzchar(p)) return(p) }
  NA_character_
}

#' Run the package on a project folder (no-code interface)
#' @param path project folder; NULL opens a folder chooser (Windows: utils::choose.dir; elsewhere tcltk, else a typed path)
#' @param yes start without asking (otherwise the plan is shown and "Start?" is asked)
#' @param open open the run folder in Explorer at the end
#' @param plan_only show the plan and stop
#' @return invisibly, the launcher exit code (0 all PASS; 3 REVIEW/NOT_TESTED/DIAGNOSTIC; 4 a FAIL; 1 incomplete; 2 refused)
#' @export
siw_run_project <- function(path = NULL, yes = FALSE, open = TRUE, plan_only = FALSE) {
  if (is.null(path)) path <- .siw_pick_folder()
  if (length(path) != 1L || is.na(path) || !nzchar(path)) { message("No project folder was chosen. Nothing was run."); return(invisible(2L)) }
  launcher <- file.path(siw_root(), "scripts", "siecc_launcher.ps1")
  if (.Platform$OS.type != "windows" || !file.exists(launcher))
    stop("SIW_PROJECT_NEEDS_WINDOWS_AND_THE_PACKAGE_FOLDER: the full run uses the PowerShell wrappers of the package folder. On Windows, open the package folder and use RUN_SIECC.cmd (or source scripts/load_package.R there and call siw_run_project()).", call. = FALSE)
  # always Windows PowerShell 5.1 (never PowerShell 7) with the 5.1 default PSModulePath, whatever shell R was started from
  sr <- Sys.getenv("SystemRoot"); ps <- file.path(sr, "System32", "WindowsPowerShell", "v1.0", "powershell.exe"); if (!file.exists(ps)) ps <- Sys.which("powershell"); if (!nzchar(ps)) stop("SIW_PROJECT_POWERSHELL_NOT_FOUND", call. = FALSE)
  pf <- Sys.getenv("ProgramFiles"); docs <- file.path(Sys.getenv("USERPROFILE"), "Documents", "WindowsPowerShell", "Modules")
  psmod <- paste(c(docs, file.path(pf, "WindowsPowerShell", "Modules"), file.path(sr, "System32", "WindowsPowerShell", "v1.0", "Modules")), collapse = ";"); old_psmod <- Sys.getenv("PSModulePath", NA_character_); Sys.setenv(PSModulePath = psmod)
  on.exit(if (is.na(old_psmod)) Sys.unsetenv("PSModulePath") else Sys.setenv(PSModulePath = old_psmod), add = TRUE)
  base <- c("-NoProfile", "-ExecutionPolicy", "Bypass", "-STA", "-File", shQuote(launcher, type = "cmd"), "-ProjectDir", shQuote(normalizePath(path, winslash = "\\", mustWork = FALSE), type = "cmd"))
  plan <- suppressWarnings(system2(ps, c(base, "-PlanOnly"), stdout = TRUE, stderr = TRUE)); st <- attr(plan, "status"); st <- if (is.null(st)) 0L else as.integer(st)
  cat(plan, sep = "\n")
  if (plan_only || st != 0L) return(invisible(st))
  if (!yes) { ok <- if (interactive()) utils::askYesNo("Start?", default = FALSE) else FALSE; if (!isTRUE(ok)) { message("Nothing was run."); return(invisible(0L)) } }
  rc <- system2(ps, c(base, "-Yes", if (!open) "-NoOpen"))
  invisible(as.integer(rc))
}
