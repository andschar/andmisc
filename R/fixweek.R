#' Convenience function to turn week 1-9 into 01-09.
#' 
#' @description This is important in as.Date() parsing.
#'
#' @param x a week number (as string or character).
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples 
#' fixweek(9)
#' 
fixweek = function(x) {
  x = as.character(x)
  data.table::fifelse(nchar(x) == 1, paste0('0', x), x)
}
