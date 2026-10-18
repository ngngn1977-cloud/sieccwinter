\name{siw_run_anomaly_history_validation}
\alias{siw_run_anomaly_history_validation}
\title{Anomaly and alternative-history validation run}
\usage{
siw_run_anomaly_history_validation(data_csv, reference_dir, out_dir,
                                   config = siw_load_config(), protocol_path = NULL)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{reference_dir}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{protocol_path}{See the package README and configuration file.}
}
\value{A list with the output folder, statuses and overall status.}
\description{Runs ANOM (7,080) and ALT (312) against the frozen V1.4 files and verifies them independently; aggregate output only.}
