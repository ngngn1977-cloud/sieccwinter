\name{siw_bootstrap_design_spec}
\alias{siw_bootstrap_design_spec}
\title{Design specification of one bootstrap target}
\usage{
siw_bootstrap_design_spec(dat, config, outcome, family, exposure,
                          tier = config$primary_tier)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{family}{See the package README and configuration file.}
\item{exposure}{See the package README and configuration file.}
\item{tier}{See the package README and configuration file.}
}
\value{A list.}
\description{Outcome, exposure, covariates in formula order and the complete-case indicator of the full data.}
