\name{siw_bootstrap_legacy_style_fit}
\alias{siw_bootstrap_legacy_style_fit}
\title{Original-style bootstrap refit}
\usage{
siw_bootstrap_legacy_style_fit(data, idx, outcome, family, exposure, covariates)
}
\arguments{
\item{data}{See the package README and configuration file.}
\item{idx}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{family}{See the package README and configuration file.}
\item{exposure}{See the package README and configuration file.}
\item{covariates}{See the package README and configuration file.}
}
\value{The exposure coefficient or NA.}
\description{Complete cases, droplevels and lm() or glm() through the formula interface on the resampled data frame, as the original code does.}
