\name{siw_run_validate}
\alias{siw_run_validate}
\title{Run the paper workflow and compare with reference outputs}
\usage{
siw_run_validate(data_csv, reference_dir, out_dir, config = siw_load_config(),
                 resume = TRUE)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{reference_dir}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{resume}{See the package README and configuration file.}
}
\value{As siw_run_paper, plus validation, validation_detail and overall (PASS, FAIL or NOT_TESTED).}
\description{Runs siw_run_paper and compares the aggregate results with frozen reference tables in reference_dir. Every discrepancy is reported; results are never altered to fit a reference.}
