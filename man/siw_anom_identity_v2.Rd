\name{siw_anom_identity_v2}
\alias{siw_anom_identity_v2}
\title{ANOM identity, protocol v2}
\usage{
siw_anom_identity_v2(res, traced, tol = SIW_AH_V2_TOL)
}
\arguments{
\item{res}{See the package README and configuration file.}
\item{traced}{See the package README and configuration file.}
\item{tol}{See the package README and configuration file.}
}
\value{A list with the per-row table and three check rows.}
\description{Each COMPLETED gaussian row's implied SD (coefficient x exposure_sd / standardised_effect) against the directly measured SD of its own complete-case sample, relative difference |a - b| / |b|, tolerance 1e-9; a missing mask or a mask size different from the recorded n fails.}
