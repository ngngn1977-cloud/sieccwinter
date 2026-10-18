\name{siw_alt_refit_verify}
\alias{siw_alt_refit_verify}
\title{Independent refit of alternative-history models}
\usage{
siw_alt_refit_verify(dat, config, res, features, protocol, tol = config$tolerances)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{res}{See the package README and configuration file.}
\item{features}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{tol}{See the package README and configuration file.}
}
\value{A data frame with a status per test.}
\description{Refits every single-feature model and recomputes every joint test independently.}
