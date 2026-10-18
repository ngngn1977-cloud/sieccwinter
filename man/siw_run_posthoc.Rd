\name{siw_run_posthoc}
\alias{siw_run_posthoc}
\title{Run one post hoc step}
\usage{
siw_run_posthoc(item, data_csv, out_dir, config = siw_load_config(), refs = list(),
                synthetic_demo = FALSE, protocol_path = NULL, options = list())
}
\arguments{
\item{item}{See the package README and configuration file.}
\item{data_csv}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{refs}{See the package README and configuration file.}
\item{synthetic_demo}{See the package README and configuration file.}
\item{protocol_path}{See the package README and configuration file.}
\item{options}{See the package README and configuration file.}
}
\value{A list with the output folder, statuses and the overall class (PASS, REVIEW or FAIL).}
\description{Runs one step of config/posthoc_sensitivity_protocol_v2.json (steps 17-26 declared 13 Oct 2026, steps 27-30 on 14 Oct 2026; labelled SENSITIVITY, POST_HOC or RECONSTRUCTION) on a canonical CSV, compares with the declared references at the existing tolerance classes and writes aggregate files and POSTHOC_STATUS.txt. It cannot change q90, q9, q9-nested or any accepted status.}
