\name{siw_run_project}
\alias{siw_run_project}
\title{Run the package on a project folder (no-code interface)}
\usage{
siw_run_project(path = NULL, yes = FALSE, open = TRUE, plan_only = FALSE)
}
\arguments{
\item{path}{See the package README and configuration file.}
\item{yes}{See the package README and configuration file.}
\item{open}{See the package README and configuration file.}
\item{plan_only}{See the package README and configuration file.}
}
\value{Invisibly, the launcher exit code.}
\description{Opens a folder chooser when path is NULL (Windows: utils::choose.dir; elsewhere tcltk or a typed path), shows the plan produced by the launcher used by RUN_SIECC.cmd, asks Start?, and runs run_all.ps1 in project mode. Inputs are identified by content; absent inputs make steps NOT_TESTED. Needs Windows and the package source folder.}
