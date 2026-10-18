\name{siw_dlm_delta_se}
\alias{siw_dlm_delta_se}
\title{Worst-case relative standard-error difference}
\usage{
siw_dlm_delta_se(V_candidate, V_reference)
}
\arguments{
\item{V_candidate}{See the package README and configuration file.}
\item{V_reference}{See the package README and configuration file.}
}
\value{A list with ok, reason, delta_se, the eigenvalues and asymmetry measures.}
\description{max |sqrt(lambda) - 1| over the generalized eigenvalues of V_candidate v = lambda V_reference v (Cholesky reduction, base R); invalid matrices fail without repair.}
