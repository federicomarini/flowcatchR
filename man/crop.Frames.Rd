% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{crop.Frames}
\alias{crop.Frames}
\title{Cut borders of a \code{Frames} object}
\usage{
crop.Frames(
  frames,
  cutLeft = 5,
  cutRight = 5,
  cutUp = 5,
  cutDown = 5,
  cutAll = 0,
  testing = FALSE,
  ...
)
}
\arguments{
\item{frames}{An input \code{Frames} object}

\item{cutLeft}{Amount of pixels to be cut at the side}

\item{cutRight}{Amount of pixels to be cut at the side}

\item{cutUp}{Amount of pixels to be cut at the side}

\item{cutDown}{Amount of pixels to be cut at the side}

\item{cutAll}{Amount of pixels to be cut at all sides. Overrides the single side values}

\item{testing}{Logical, whether to just test the cropping or to actually perform it. Default set to \code{FALSE}}

\item{...}{Arguments to be passed to \code{\link{display}} (e.g. setting the \code{method} argument)}
}
\value{
A \code{Frames} object, with cropped frames in the \code{image} slot
}
\description{
Performs cropping on the \code{Frames} object, selecting how many pixels should be cut on each side
}
\details{
Cropping can be performed with careful choice of all cutting sides, or cropping a single value from
all sides
}
\examples{
data("MesenteriumSubset")
crop.Frames(MesenteriumSubset)

}
\author{
Federico Marini, \email{marinif@uni-mainz.de}, 2014
}
