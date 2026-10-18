\name{siw_dlm_sim_capture}
\alias{siw_dlm_sim_capture}
\title{Capture the production covariance and draws}
\usage{
siw_dlm_sim_capture(dat, config, outcome, metric, draws, x_cov = NULL)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{metric}{See the package README and configuration file.}
\item{draws}{See the package README and configuration file.}
\item{x_cov}{See the package README and configuration file.}
}
\value{A list with the production fit, covariance, draws and lag basis.}
\description{Runs the unchanged siw_fit_dlm through a closure copy whose siw_mvn_draws records and returns the same values.}
