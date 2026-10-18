\name{siw_dlm_verify_model}
\alias{siw_dlm_verify_model}
\title{Verify one DLM against the independent route}
\usage{
siw_dlm_verify_model(dat, config, pkg, pkg10k_summary = NULL, outcome, metric,
                     protocol = siw_dlm_validation_protocol(), mc = TRUE, x_cov = NULL)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{pkg}{See the package README and configuration file.}
\item{pkg10k_summary}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{metric}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{mc}{See the package README and configuration file.}
\item{x_cov}{See the package README and configuration file.}
}
\value{A list of check tables.}
\description{Lag effects, SEs, cumulative curves, bands, peak and critical values for one package fit.}
