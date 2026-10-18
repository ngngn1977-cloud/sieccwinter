\name{siw_archived_glm_control}
\alias{siw_archived_glm_control}
\title{Archived binary-fit glm control}
\usage{
siw_archived_glm_control(config)
}
\arguments{
\item{config}{See the package README and configuration file.}
}
\value{A list with glm_epsilon and glm_maxit.}
\description{The glm control of the archived L01-L59 analysis (epsilon 1e-10, maxit 200, traced to the V1.4 source). siw_archived_dlm_grid and siw_archived_dlm_sensitivity apply it to a copy of the configuration; the final L01-L56 models keep epsilon 1e-12.}
