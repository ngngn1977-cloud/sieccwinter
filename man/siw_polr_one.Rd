\name{siw_polr_one}
\alias{siw_polr_one}
\title{One polr fit}
\usage{
siw_polr_one(d, outcome, exposure, covariates)
}
\arguments{
\item{d}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{exposure}{See the package README and configuration file.}
\item{covariates}{See the package README and configuration file.}
}
\value{A list.}
\description{Fits MASS::polr and returns the exposure statistics; failures, warnings and non-finite standard errors are returned as reasons, never thrown.}
