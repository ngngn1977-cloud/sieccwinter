\name{siw_load_config}
\alias{siw_load_config}
\title{Load the analysis configuration}
\usage{
siw_load_config(path = NULL)
}
\arguments{
\item{path}{See the package README and configuration file.}
}
\value{A configuration list including config_path and config_hash.}
\description{Reads the JSON configuration (design, seed, tolerances, defaults) from the repository or installed package, or from path.}
