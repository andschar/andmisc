#' A function to the number of individual entries to a vector.
#' 
#' @description To be used with ggplot2::labeller().
#' 
#' @param l A vector.
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#'
#' @examples 
#' add_n(iris$Species)
#'
add_n = function(v) {
  if (is.null(v)) {
    stop('v needs to be a vector.')
  }
  vt = table(v)
  vtn = names(vt)
  vr = as.character(vt)
  vr = paste0(vtn, '\n(n=', vr, ')')
  names(vr) = vtn
  
  vr
}
