#' Function to produce nice Labels for the cut function
#' 
#' @param v A numeric vector of \code{cit()} breaks.
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples 
#' cut_labeler(c(5, 10, 15))
#' 
cut_labeler = function(x, round_digits = 0) {
  v = c()
  for (i in 1:(length(x)-1)) {
    minimum = round(x[i], digits = round_digits)
    maximum = round(x[i+1], digits = round_digits)
    
    v[i] = paste0(minimum, '-', maximum)
  }
  
  v
}
