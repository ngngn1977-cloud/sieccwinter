\name{siw_validate_input}
\alias{siw_validate_input}
\title{Validate a study data frame}
\usage{
siw_validate_input(raw, config, lag_end = 56L)
}
\arguments{
\item{raw}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{lag_end}{See the package README and configuration file.}
}
\value{A data frame with check_id, category, status (PASS, WARN, FAIL), n_affected and detail.}
\description{Checks required variables, types, allowed codes, lag fields, duplicate codes, coding consistency, supplied cumulative fields and rainfall anomalies. Reports only; never corrects and never returns row values.}
