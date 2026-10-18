\name{siw_polr_backtransform_matrix}
\alias{siw_polr_backtransform_matrix}
\title{Back-transformation of a scaled polr fit}
\usage{
siw_polr_backtransform_matrix(mu, s, q)
}
\arguments{
\item{mu}{See the package README and configuration file.}
\item{s}{See the package README and configuration file.}
\item{q}{See the package README and configuration file.}
}
\value{A matrix.}
\description{Linear map from (gamma, zeta_s) of a fit on centred and scaled predictors to the original (beta, zeta): beta = gamma / s, zeta = zeta_s + sum(beta * mu).}
