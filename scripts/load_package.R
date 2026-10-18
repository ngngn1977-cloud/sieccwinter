# Load the package sources without installing anything: sources R/*.R into an environment attached as "package:sieccwinter_dev".
# Used by run.R and the tests when the package is not installed.
siw_dev_load <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  options(sieccwinter.root = root)
  env <- new.env(parent = globalenv())
  for (f in sort(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))) sys.source(f, envir = env)
  # make internal helpers visible to each other and to the caller
  if ("package:sieccwinter_dev" %in% search()) detach("package:sieccwinter_dev", character.only = TRUE)
  attach(env, name = "package:sieccwinter_dev", warn.conflicts = FALSE)
  invisible(env)
}
