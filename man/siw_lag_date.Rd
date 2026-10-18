\name{siw_lag_date}
\alias{siw_lag_date}
\title{Lag dates from the calendar anchor}
\usage{
siw_lag_date(assessment_day, lag, protocol = siw_posthoc_protocol(), as_offset = FALSE)
}
\arguments{
\item{assessment_day}{See the package README and configuration file.}
\item{lag}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{as_offset}{See the package README and configuration file.}
}
\value{A Date vector, a character matrix of ISO dates, or a matrix of day offsets.}
\description{Lag k of participant i falls on 2026-01-29 + assessment_day_i - k.}
