\name{siw_write_coded_model_record}
\alias{siw_write_coded_model_record}
\title{Write the coded model record, version 2}
\usage{
siw_write_coded_model_record(out_dir, config = siw_load_config(), paper_dir = NULL,
                             archived_dir = NULL, run_label = "")
}
\arguments{
\item{out_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{paper_dir}{See the package README and configuration file.}
\item{archived_dir}{See the package README and configuration file.}
\item{run_label}{See the package README and configuration file.}
}
\value{Invisibly, a list with the tables, the checks and the status.}
\description{Writes the record, the family table, the checks, a metadata file, a status file and a manifest to a new or empty folder. Refuses a non-empty folder and never writes the legacy record file name.}
