\name{siw_source_manifest}
\alias{siw_source_manifest}
\title{Package-source manifest and SHA-256 fingerprint}
\usage{
siw_source_manifest(root = siw_root())
}
\arguments{
\item{root}{See the package README and configuration file.}
}
\value{A list with manifest, fingerprint, scope and n_files.}
\description{Relative paths, SHA-256 and sizes of the package source files in an explicit scope (R, config, inst/extdata, scripts, run files, DESCRIPTION, NAMESPACE; private data, runs, outputs, checkpoints, reports, tests and documentation excluded) and the SHA-256 of their canonical listing.}
