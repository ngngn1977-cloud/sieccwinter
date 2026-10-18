\name{siw_run_archived_dlm_validation}
\alias{siw_run_archived_dlm_validation}
\title{Archived L01-L59 grid and lag-form sensitivity validation}
\usage{
siw_run_archived_dlm_validation(data_csv, reference_dir, out_dir,
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
\description{Fits the 168 archived grid models and the 216 lag-form sensitivity models with the unchanged package functions (no simulated draws) and compares deterministic quantities (sample sizes, coefficients, standard errors, intervals, global tests, q24, q9, q216, lag-specific and cumulative curves) with the frozen V1.4 outputs under a declared plan; simultaneous critical values and bands are not compared and are reported as requiring a separate validation. Separate from the nine final L01-L56 models.}
