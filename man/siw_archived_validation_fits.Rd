\name{siw_archived_validation_fits}
\alias{siw_archived_validation_fits}
\title{Archived DLM fits for validation}
\usage{
siw_archived_validation_fits(dat, config, protocol = siw_archived_validation_protocol())
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
}
\value{A list of summaries and curve tables.}
\description{Runs siw_archived_dlm_grid, siw_archived_dlm_sensitivity and the curve-producing fits with the archived glm epsilon on a copy of the configuration; no simulated draws.}
