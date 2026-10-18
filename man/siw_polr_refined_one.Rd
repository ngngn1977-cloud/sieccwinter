\name{siw_polr_refined_one}
\alias{siw_polr_refined_one}
\title{One refined polr fit}
\usage{
siw_polr_refined_one(d, outcome, exposure, covariates,
                     plan = siw_ordinal_refinement_plan())
}
\arguments{
\item{d}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{exposure}{See the package README and configuration file.}
\item{covariates}{See the package README and configuration file.}
\item{plan}{See the package README and configuration file.}
}
\value{A list.}
\description{Fits MASS::polr on scaled predictors with tight optimiser control, restarts and a fine finite-difference Hessian, and returns coefficients, thresholds and covariance on the original parameterisation; failures are returned as reasons.}
