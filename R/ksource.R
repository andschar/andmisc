# https://stackoverflow.com/questions/10966109/how-to-source-r-markdown-file-like-sourcemyfile-r
# doesn't load eval=FALSE

#' A function to source .Rmd files.
#' @param x Path to a .Rmd file.
#' @param ... Arguments to pass on.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#'
ksource = function(x, ...) {
  source(knitr::purl(x, output = tempfile()),
         ...)
}
