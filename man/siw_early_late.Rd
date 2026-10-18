\name{siw_early_late}
\alias{siw_early_late}
\title{Early and late assessment groups}
\usage{
siw_early_late(assessment_day)
}
\arguments{
\item{assessment_day}{See the package README and configuration file.}
}
\value{A factor with levels Early, Late.}
\description{Early: assessment_day <= 54; Late: >= 55.}
