\name{siw_nonlinear_verify}
\alias{siw_nonlinear_verify}
\title{Verify the nonlinear fits}
\usage{
siw_nonlinear_verify(dat, config, grid, tier = config$primary_tier, tol = 1e-08)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{grid}{See the package README and configuration file.}
\item{tier}{See the package README and configuration file.}
\item{tol}{See the package README and configuration file.}
}
\value{A data frame.}
\description{Independent recalculation with stats::lm / stats::glm, splines::ns and a direct Wald quadratic form.}
