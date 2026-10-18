\name{siw_read_station_stages}
\alias{siw_read_station_stages}
\title{Station counts from the two session records of the analysed IMS run}
\usage{
siw_read_station_stages(stage_a, stage_b)
}
\arguments{
\item{stage_a}{See the package README and configuration file.}
\item{stage_b}{See the package README and configuration file.}
}
\value{A data frame quantity, value.}
\description{Reads the stage A coordinate metadata and the stage B observation-retrieval record (station-level JSON): coordinate-valid, requested and returned stations, and the stations in the fetch status. Stops if either file holds a participant-level field.}
