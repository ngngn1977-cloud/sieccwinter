\name{siw_transformation_verify}
\alias{siw_transformation_verify}
\title{Verify the transformation fits}
\usage{
siw_transformation_verify(dat, config, grid, tier = config$primary_tier, tol = 1e-08)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{grid}{See the package README and configuration file.}
\item{tier}{See the package README and configuration file.}
\item{tol}{See the package README and configuration file.}
}
\value{A data frame.}
\description{Independent recalculation with stats::lm / stats::glm and a hand-written HC3 or information matrix.}
