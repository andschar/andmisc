#' Function to print R object pretty size
#'
#' @param obj An R object.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#'
#' @examples 
#' size(iris)
#' 
size = function(obj) {
  format(utils::object.size(obj), unit = 'auto')
}
