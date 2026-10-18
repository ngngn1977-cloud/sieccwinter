\name{siw_run_public_demo}
\alias{siw_run_public_demo}
\title{End-to-end synthetic demo of the public package}
\usage{
siw_run_public_demo(out_dir, config = siw_load_config(), steps = SIW_PH_ITEMS)
}
\arguments{
\item{out_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{steps}{See the package README and configuration file.}
}
\value{A list with the output folder, the step statuses and the overall class.}
\description{Runs the paper workflow and the ten post hoc steps on the shipped fully synthetic CSV and writes DEMO_STATUS.txt and DEMO_SUMMARY.csv. Comparisons with study values are not applicable; synthetic numbers have no scientific meaning.}
