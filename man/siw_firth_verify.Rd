\name{siw_firth_verify}
\alias{siw_firth_verify}
\title{Mathematical verification of a Firth solution}
\usage{
siw_firth_verify(X, y, fit = siw_firth_fit(X, y),
    tol = list(score = 1e-08, gradient = 1e-06, coefficient = 1e-07, se = 1e-07))
}
\arguments{
\item{X}{See the package README and configuration file.}
\item{y}{See the package README and configuration file.}
\item{fit}{See the package README and configuration file.}
\item{tol}{See the package README and configuration file.}
}
\value{A one-row data frame of checks.}
\description{Scaled score, analytic versus numerical gradient, numerical Hessian, hand-evaluated penalty, an independent augmentation estimator and a local random search.}
