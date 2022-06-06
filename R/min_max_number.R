#' Function to replace smaller values with a threshold value.
#' 
#' @param x A numeric vector or scalar.
#' @param y A numeric threshold value that serves as a cutoff.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples
#' min_number(3, 10)
#' 
min_number = function(x, y) {
  data.table::fifelse(x < y, y, x)
}

#' Function to replace larger values with a threshold value.
#' 
#' @param x A numeric vector or scalar.
#' @param y A numeric threshold value that serves as a cutoff.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples
#' max_number(100, 10)
#' 
max_number = function(x, y) {
  data.table::fifelse(x > y, y, x)
}
