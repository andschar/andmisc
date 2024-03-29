% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freadwrite2.R
\name{fread2}
\alias{fread2}
\title{Convenience data.table::fread() wrapper.}
\usage{
fread2(..., na.strings = "")
}
\arguments{
\item{...}{Any parameters for fread().}
}
\description{
Convenience data.table::fread() wrapper.
}
\examples{
fl = file.path(tempdir(), 'iris.csv')
write.csv(iris, fl)
fread2(fl)

}
\author{
Andreas Scharmueller, \email{andschar@proton.me}
}
