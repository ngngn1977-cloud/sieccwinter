\name{siw_run_dlm_covariance_v21}
\alias{siw_run_dlm_covariance_v21}
\title{V2.1 covariance rule check (candidate)}
\usage{
siw_run_dlm_covariance_v21(v2_run_dir, out_dir, config = siw_load_config())
}
\arguments{
\item{v2_run_dir}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
}
\value{A list with out_dir, statuses, checks and status.}
\description{Reads the aggregate 3 x 3 cross-basis covariance matrices saved in a V2 run folder (no data, no refit), verifies their provenance, computes the exact worst-case relative standard-error difference over all linear contrasts for the nine models and compares it with the existing relative SE tolerances; carries forward earlier statuses as labelled previously reviewed evidence.}
