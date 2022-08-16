#' Function to return every nth element of a vector
#' 
#' @description Useful fo reducing entry overload for discrete axis on a plot.
#' Helper function.
#' 
#' @param v A character vector.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples 
#' x = row.names(mtcars)
#' nth_element(x, 3)
#' 
nth_element = function(x, n = 2) {
  x[seq(1, length(x), by = n)]
}
