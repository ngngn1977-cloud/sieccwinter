\name{siw_run_bootstrap_audit}
\alias{siw_run_bootstrap_audit}
\title{Diagnostic audit of saved Batch C bootstrap estimates}
\usage{
siw_run_bootstrap_audit(run_dir, out_dir, protocol = siw_bootstrap_audit_protocol())
}
\arguments{
\item{run_dir}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
}
\value{A list with out_dir, statuses, tables and status.}
\description{Reads only the saved target checkpoints and aggregate tables of a Batch C run (no participant data, no refitting), reports extremeness diagnostics, reconstructs the original type-8 limits, relates flagged estimates to the quantile ranks, gives diagnostic-only omission sensitivity and records checkpoint hashes and provenance. Writes aggregate tables to a new folder; never changes a result.}
