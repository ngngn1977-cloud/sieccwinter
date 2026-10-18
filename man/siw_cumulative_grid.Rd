\name{siw_cumulative_grid}
\alias{siw_cumulative_grid}
\title{Fit the cumulative-window grid}
\usage{
siw_cumulative_grid(dat, config, tiers = config$tier_order)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{tiers}{See the package README and configuration file.}
}
\value{A list with results, accounting and designs.}
\description{3 outcomes x 8 metrics x 10 windows x 7 tiers (1,680 specifications), in deterministic order, with BH q10, q80 and q90 within M2-L families.}
