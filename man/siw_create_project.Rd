\name{siw_create_project}
\alias{siw_create_project}
\title{Create an empty project folder}
\usage{
siw_create_project(path)
}
\arguments{
\item{path}{See the package README and configuration file.}
}
\value{Invisibly, the normalised folder path.}
\description{Creates data, references and outputs subfolders and README_FIRST.txt for the no-code interface. Subfolders are a convenience only: inputs are identified by content (SHA-256).}
