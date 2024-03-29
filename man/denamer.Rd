% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/namer.R
\name{denamer}
\alias{denamer}
\title{Function to extract arguments from an object created by \code{namer()}.}
\usage{
denamer(string = NULL, arg = NULL, sep = getOption("sep"))
}
\arguments{
\item{string}{A string created by \code{namer()}.}

\item{arg}{Which argument should be extracted?}

\item{sep}{Separator used.}
}
\description{
Function to extract arguments from an object created by \code{namer()}.
}
\examples{
sep = '_'
denamer(
 namer(dir = tempdir(),
        prequel = 'test-prequel',
        arg = list(weight = 12,
                   color = 'white'),
        sep = sep,
        ext = '.png'),
  arg = c('color', 'weight'),
  sep = sep
)

}
\author{
Andreas Scharmueller, \email{andschar@proton.me}
}
