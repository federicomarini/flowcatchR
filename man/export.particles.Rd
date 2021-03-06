% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary.R
\name{export.particles}
\alias{export.particles}
\title{Exports a \code{ParticleSet} object}
\usage{
export.particles(
  particleset,
  dir = tempdir(),
  nameStub = "testExport_particles"
)
}
\arguments{
\item{particleset}{A \code{ParticleSet} object}

\item{dir}{The path of the folder where the particle sets should be written}

\item{nameStub}{The stub for the file name, that will be used as a prefix for the exported particle sets}
}
\value{
Particle sets files are written in the desired location
}
\description{
Writes the particles contained in the \code{particles} data frame slot of the \code{ParticleSet} object elements.
A track of the provenience of the particles is stored as a comment line above the header
}
\examples{
data("candidate.platelets")
\dontrun{export.particles(candidate.platelets)}

}
\author{
Federico Marini, \email{marinif@uni-mainz.de}, 2014
}
