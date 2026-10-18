\name{siw_read_gis_inputs}
\alias{siw_read_gis_inputs}
\title{Read the GIS inputs of the analysed IMS run}
\usage{
siw_read_gis_inputs(paths, protocol, role = SIW_GIS_ROLE)
}
\arguments{
\item{paths}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{role}{See the package README and configuration file.}
}
\value{A list of minimal tables (daily, audit, plan, coverage, counts).}
\description{Reads the declared files of role gis_ims_v8_1_reference, found by their content tags in the post hoc protocol (daily, audit, plan, period_coverage, raw_10min), keeping only the declared columns.}
