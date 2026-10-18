\name{siw_raw_station_day_counts}
\alias{siw_raw_station_day_counts}
\title{Station-day value counts of a raw 10-minute IMS file}
\usage{
siw_raw_station_day_counts(f)
}
\arguments{
\item{f}{See the package README and configuration file.}
}
\value{A data frame station_id, date, n_td, n_rh, n_rain.}
\description{Reads only station_id, JERUSALEM_LOCAL_DATE, TD_C, RH_PCT and RAIN_MM of one raw file (CSV or gzip) and counts the non-missing values per station and Jerusalem local date.}
