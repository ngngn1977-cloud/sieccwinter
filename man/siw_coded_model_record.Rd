\name{siw_coded_model_record}
\alias{siw_coded_model_record}
\title{Coded model record, version 2}
\usage{
siw_coded_model_record(config = siw_load_config(), paper_dir = NULL, archived_dir = NULL)
}
\arguments{
\item{config}{See the package README and configuration file.}
\item{paper_dir}{See the package README and configuration file.}
\item{archived_dir}{See the package README and configuration file.}
}
\value{A list with the record and the family table.}
\description{Stable model codes and declared multiplicity families of the cumulative grid (1,680), the nine final L01-L56 DLMs and the archived L01-L59 grid (168) and lag-form sensitivity (216), derived from the configuration; statuses of a paper run and the archived validation run are attached when their folders are given. Separate from, and never replacing, the legacy SIECC_All_Models_with_Codes.csv.}
