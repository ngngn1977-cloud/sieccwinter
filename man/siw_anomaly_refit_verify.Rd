\name{siw_anomaly_refit_verify}
\alias{siw_anomaly_refit_verify}
\title{Independent refit of anomaly models}
\usage{
siw_anomaly_refit_verify(dat, config, res, features, protocol, every = 1L,
                         tol = config$tolerances)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{res}{See the package README and configuration file.}
\item{features}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{every}{See the package README and configuration file.}
\item{tol}{See the package README and configuration file.}
}
\value{A list with a summary, the failures and the coverage.}
\description{Refits models with lm / glm and compares them at the cumulative tolerance class; the coverage is stated.}
