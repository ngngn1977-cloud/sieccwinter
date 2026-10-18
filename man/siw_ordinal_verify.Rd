\name{siw_ordinal_verify}
\alias{siw_ordinal_verify}
\title{Verify the ordinal fits}
\usage{
siw_ordinal_verify(dat, config, grid, tier = config$primary_tier,
                   tol = list(loglik = 1e-06, coefficient = 1e-04, se = 1e-04))
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{grid}{See the package README and configuration file.}
\item{tier}{See the package README and configuration file.}
\item{tol}{See the package README and configuration file.}
}
\value{A data frame.}
\description{Compares every polr fit with the independent maximum-likelihood fit: PASS, POLR_NOT_AT_MAXIMUM (documented polr stopping limitation) or FAIL.}
