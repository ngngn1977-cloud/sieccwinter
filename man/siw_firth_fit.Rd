\name{siw_firth_fit}
\alias{siw_firth_fit}
\title{Firth logistic regression}
\usage{
siw_firth_fit(X, y, maximum_iterations = 200L, tolerance = 1e-10)
}
\arguments{
\item{X}{See the package README and configuration file.}
\item{y}{See the package README and configuration file.}
\item{maximum_iterations}{See the package README and configuration file.}
\item{tolerance}{See the package README and configuration file.}
}
\value{A list with coefficients, standard_error, covariance, converged, status and convergence diagnostics.}
\description{Maximises l(b) + 0.5 log|I(b)| by Fisher scoring on the Jeffreys-adjusted score, with the SE from the expected information at the penalised estimate; convergence is judged at the returned estimate.}
