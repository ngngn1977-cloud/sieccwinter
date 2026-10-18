\name{siw_weather_participant_day}
\alias{siw_weather_participant_day}
\title{Participant-day weather distributions}
\usage{
siw_weather_participant_day(dat, config, lag_end = config$dlm$final$lag_end)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{lag_end}{See the package README and configuration file.}
}
\value{A long data frame.}
\description{Distributions over participant-days (not participant-level), UTCI thermal-stress classes and rainfall classes; the supplied missing rainfall total and its indicator are reported as supplied.}
