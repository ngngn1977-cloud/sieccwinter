\name{siw_vif_summary}
\alias{siw_vif_summary}
\title{VIF summary of the M2-L cumulative models}
\usage{
siw_vif_summary(dat, config, tier = config$primary_tier)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{tier}{See the package README and configuration file.}
}
\value{A list with the per-model and per-column tables.}
\description{Post hoc step 28 (RECONSTRUCTION): for every M2-L cumulative model (Batch A design and samples) the maximum VIF, the column and term attaining it, and every per-column VIF.}
