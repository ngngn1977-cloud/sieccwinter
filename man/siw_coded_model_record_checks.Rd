\name{siw_coded_model_record_checks}
\alias{siw_coded_model_record_checks}
\title{Checks of the coded model record}
\usage{
siw_coded_model_record_checks(x, config = siw_load_config())
}
\arguments{
\item{x}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
}
\value{A data frame of checks (PASS or FAIL).}
\description{Counts per scope, unique codes, disjoint final and archived codes, declared family sizes (q10, q80, q90, q9, q24, archived q9, q216) and agreement of in-run families with the declared ones.}
