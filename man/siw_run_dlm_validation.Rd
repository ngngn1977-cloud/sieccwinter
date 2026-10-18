\name{siw_run_dlm_validation}
\alias{siw_run_dlm_validation}
\title{Validate the nine final L01-L56 DLM outputs}
\usage{
siw_run_dlm_validation(data_csv, out_dir, dlm_reference_dir, config = siw_load_config(),
                       accepted_run_dir = NULL, label = NULL, mc = TRUE,
                       manuscript_concordance = TRUE)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{dlm_reference_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{accepted_run_dir}{See the package README and configuration file.}
\item{label}{See the package README and configuration file.}
\item{mc}{See the package README and configuration file.}
\item{manuscript_concordance}{See the package README and configuration file.}
}
\value{A list with out_dir, statuses, checks and status.}
\description{Reproduces the accepted fits, compares them with the audited L56 reconstruction, verifies lag basis, lag-specific estimates and SEs, cumulative curves, pointwise and simultaneous bands, critical values and descriptive peaks with an independent route, separates exact seeded reproduction from Monte Carlo stability and reports NOT_TESTED for what has no reference; writes only to a new folder.}
