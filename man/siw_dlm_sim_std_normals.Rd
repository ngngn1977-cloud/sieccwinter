\name{siw_dlm_sim_std_normals}
\alias{siw_dlm_sim_std_normals}
\title{Seeded standard normals as mvrnorm draws them}
\usage{
siw_dlm_sim_std_normals(n, p, seed)
}
\arguments{
\item{n}{See the package README and configuration file.}
\item{p}{See the package README and configuration file.}
\item{seed}{See the package README and configuration file.}
}
\value{An n x p matrix.}
\description{matrix(rnorm(p * n), n) under the seeded Mersenne-Twister / Inversion stream.}
