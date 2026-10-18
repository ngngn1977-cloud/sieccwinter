\name{siw_po_derivatives}
\alias{siw_po_derivatives}
\title{Proportional-odds log-likelihood derivatives}
\usage{
siw_po_derivatives(theta, y, X)
}
\arguments{
\item{theta}{See the package README and configuration file.}
\item{y}{See the package README and configuration file.}
\item{X}{See the package README and configuration file.}
}
\value{A list with loglik, gradient and hessian.}
\description{Log-likelihood, analytic gradient and Hessian of P(Y <= k) = plogis(zeta_k - x'b).}
