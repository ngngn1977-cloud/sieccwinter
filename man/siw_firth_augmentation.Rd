\name{siw_firth_augmentation}
\alias{siw_firth_augmentation}
\title{Independent Firth estimator by pseudo-data augmentation}
\usage{
siw_firth_augmentation(X, y, maximum_iterations = 500L, tolerance = 1e-12)
}
\arguments{
\item{X}{See the package README and configuration file.}
\item{y}{See the package README and configuration file.}
\item{maximum_iterations}{See the package README and configuration file.}
\item{tolerance}{See the package README and configuration file.}
}
\value{A list with coefficients, converged and iterations.}
\description{Iterated weighted logistic fits of the original and half-weighted pseudo observations (Firth 1993; Heinze and Schemper 2002), using stats::glm.fit.}
