#' Convenience data.table::fread() wrapper.
#'  
#' @param ... Any parameters for fread().
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#'
#' @examples 
#' fl = file.path(tempdir(), 'iris.csv')
#' write.csv(iris, fl)
#' fread2(fl)
#'
fread2 = function(..., na.strings = '') {
  fread(..., na.strings = na.strings)
}

#' Convenience data.table::fwrite() wrapper.
#' 
#' @param ... Any parameters for fwrite().
#' @param sep Separator as in fwrite().
#' @param quote Quote as in fwrite().
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#'
#' @examples 
#' fwrite2(iris, file.path(tempdir(), 'iris.tsv'),
#'         sep = '|')
#'
fwrite2 = function(..., sep = '\t', quote = FALSE) {
  fwrite(..., sep = sep, quote = quote)
}
