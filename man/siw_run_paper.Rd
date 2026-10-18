\name{siw_run_paper}
\alias{siw_run_paper}
\title{Run the paper workflow on a study CSV}
\usage{
siw_run_paper(data_csv, out_dir, config = siw_load_config(), resume = TRUE,
              archived = FALSE, rainfall_sensitivity = FALSE, label = NULL)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{resume}{See the package README and configuration file.}
\item{archived}{See the package README and configuration file.}
\item{rainfall_sensitivity}{See the package README and configuration file.}
\item{label}{See the package README and configuration file.}
}
\value{A list as for siw_run_demo.}
\description{Reads and validates the canonical study CSV once, then runs the 1,680-specification cumulative grid and the nine final L01-L56 distributed-lag models, with checkpointed stages. Optional archived L01-L59 and rainfall-sensitivity branches. No participant-level value is written to any output or log.}
