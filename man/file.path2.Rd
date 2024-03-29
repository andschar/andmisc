% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filepath2.R
\name{file.path2}
\alias{file.path2}
\title{Function to Construct Path to Files}
\usage{
file.path2(..., recursive = TRUE, mode = "0777", fsep = .Platform$file.sep)
}
\arguments{
\item{...}{Character vectors as in \code{file.path()}.}

\item{recursive}{As in \code{dir.create()}.}

\item{mode}{As in \code{dir.create()}.}

\item{fsep}{Path separator as in \code{file.path()}.}
}
\description{
Works as \code{file.path()} but automatically creates
a directory when missing.
}
\examples{
file.path2(tempdir(), 'test', 'blub')

}
\author{
Andreas Scharmueller, \email{andschar@proton.me}
}
