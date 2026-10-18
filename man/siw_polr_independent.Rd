\name{siw_polr_independent}
\alias{siw_polr_independent}
\title{Independent proportional-odds maximum likelihood}
\usage{
siw_polr_independent(y, X)
}
\arguments{
\item{y}{See the package README and configuration file.}
\item{X}{See the package README and configuration file.}
}
\value{A list with coefficients, zeta, log_likelihood, se and convergence.}
\description{Own likelihood with analytic derivatives, a robust first stage and exact-Hessian Newton iterations; does not call MASS::polr.}
