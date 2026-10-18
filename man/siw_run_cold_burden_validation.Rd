\name{siw_run_cold_burden_validation}
\alias{siw_run_cold_burden_validation}
\title{Cold-burden scan: reproduction and mathematical verification}
\usage{
siw_run_cold_burden_validation(data_csv, reference_dir, out_dir,
                               config = siw_load_config(), protocol_path = NULL)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{reference_dir}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{protocol_path}{See the package README and configuration file.}
}
\value{A list with the output folder, the statuses and the overall status.}
\description{Runs the exploratory cold-burden scan (14 fitted thresholds x 59 windows x 9 variants x 3 outcomes = 22,302 models) with the validated engine, compares it with the frozen V1.4 output under a declared plan (numerical reproduction), verifies burden features, thresholds, coefficient scales, inference, multiplicity and an independent refit sample (mathematical verification), and reports without interpreting any model.}
