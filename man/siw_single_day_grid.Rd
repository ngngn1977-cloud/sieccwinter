\name{siw_single_day_grid}
\alias{siw_single_day_grid}
\title{Adjusted single-day grid}
\usage{
siw_single_day_grid(dat, config, protocol = siw_daily_protocol(), lags = NULL,
                    diagnostics = TRUE)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{lags}{See the package README and configuration file.}
\item{diagnostics}{See the package README and configuration file.}
}
\value{A data frame of 1,416 rows.}
\description{3 outcomes x 8 metrics x lags 1-59 in tier M2_L with the validated engine and diagnostics; q59, q472 and q_global.}
