\name{siw_ordinal_refined_verify}
\alias{siw_ordinal_refined_verify}
\title{Verify the refined ordinal fits}
\usage{
siw_ordinal_refined_verify(grid, config, plan = siw_ordinal_refinement_plan(),
                           tier = config$primary_tier)
}
\arguments{
\item{grid}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{plan}{See the package README and configuration file.}
\item{tier}{See the package README and configuration file.}
}
\value{A data frame.}
\description{Evaluates the declared acceptance criteria (optimiser code, Newton decrement, log-likelihood, ordered thresholds, information matrix, coefficient and SE agreement with an independent maximum-likelihood fit) for every model.}
