\name{siw_xy_daily_grid}
\alias{siw_xy_daily_grid}
\title{Crude daily-lag scan}
\usage{
siw_xy_daily_grid(dat, config, protocol = siw_daily_protocol(), lags = NULL)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{lags}{See the package README and configuration file.}
}
\value{A data frame of 1,416 rows.}
\description{3 outcomes x 8 metrics x lags 1-59 with correlations, the M0 regression and Benjamini-Hochberg q within the declared families.}
