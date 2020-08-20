#' Function to upper-case 1st letter
#' 
#' @param x character; String for which the first letter should be set to upper
#' case
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#'
#' @examples 
#' firstup('andreas')
#' 
#'
firstup = function(x) {
  substr(x, 1, 1) = toupper(substr(x, 1, 1))
  
  x
}
