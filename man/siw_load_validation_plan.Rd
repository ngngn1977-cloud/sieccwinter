\name{siw_load_validation_plan}
\alias{siw_load_validation_plan}
\title{Load the declared validation plan}
\usage{
siw_load_validation_plan(path = NULL)
}
\arguments{
\item{path}{See the package README and configuration file.}
}
\value{A list (plan) with references and a plan hash.}
\description{Reads config/validation_plan.json: which reference tables are required, their expected record counts and required columns. Material declared out of scope is reported NOT_TESTED, never PASS.}
