\name{siw_run_demo}
\alias{siw_run_demo}
\title{Run the synthetic demonstration workflow}
\usage{
siw_run_demo(out_dir, config = siw_load_config(), resume = FALSE, archived = FALSE)
}
\arguments{
\item{out_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{resume}{See the package README and configuration file.}
\item{archived}{See the package README and configuration file.}
}
\value{A list with out_dir, tables, self_checks, timings, input_validation and status ("OK" or "SELF_CHECK_FAILURE").}
\description{Runs the full paper workflow on the shipped fully synthetic CSV and writes aggregate outputs to out_dir. Contains no study data.}
