\name{siw_dlm_sim_factor}
\alias{siw_dlm_sim_factor}
\title{Covariance factor of a covariance matrix}
\usage{
siw_dlm_sim_factor(Sigma, B,
                   method = c("production_eigen", "aligned_eigen", "symmetric_sqrt"),
                   signs = NULL)
}
\arguments{
\item{Sigma}{See the package README and configuration file.}
\item{B}{See the package README and configuration file.}
\item{method}{See the package README and configuration file.}
\item{signs}{See the package README and configuration file.}
}
\value{A list with F, V and the eigenvalues.}
\description{Production eigen factor, sign-aligned (lag space) eigen factor or symmetric square root; optional sign vector for sign-pattern search.}
