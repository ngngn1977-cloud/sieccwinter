\name{siw_mvn_draws}
\alias{siw_mvn_draws}
\title{Multivariate-normal draws}
\usage{
siw_mvn_draws(n, Sigma, method = "MASS_mvrnorm")
}
\arguments{
\item{n}{See the package README and configuration file.}
\item{Sigma}{See the package README and configuration file.}
\item{method}{See the package README and configuration file.}
}
\value{An n x p matrix.}
\description{method MASS_mvrnorm (default, legacy-comparable) or eigen_sign_normalised (cross-platform sensitivity).}
