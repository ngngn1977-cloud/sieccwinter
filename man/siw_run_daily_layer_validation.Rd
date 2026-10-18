\name{siw_run_daily_layer_validation}
\alias{siw_run_daily_layer_validation}
\title{Daily-lag layer: reproduction and verification}
\usage{
siw_run_daily_layer_validation(data_csv, reference_dir, out_dir,
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
\description{Runs the crude daily-lag scan (1,416), the adjusted single-day grid (1,416) and the max-|r| permutation test (240) with the original specifications and sampling sequence, compares them with the frozen V1.4 output under a declared plan, verifies them independently and assesses the permutation assumptions separately.}
