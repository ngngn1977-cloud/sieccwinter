\name{siw_interview_date}
\alias{siw_interview_date}
\title{Interview date from the calendar anchor}
\usage{
siw_interview_date(assessment_day, protocol = siw_posthoc_protocol())
}
\arguments{
\item{assessment_day}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
}
\value{A Date vector.}
\description{assessment_day 0 = 2026-01-29 (declared constant); the interview (L00) date is the anchor plus assessment_day. No date file is read.}
