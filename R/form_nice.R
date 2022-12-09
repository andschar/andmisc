#' Function to produce nice numeric labels
#' 
#' @param x A scalar or vector passed on to \code{format()}.
#' @param big.mark Same as in \code{format()}.
#' @param scientific Same as in \code{format()}.
#' @param drop0trailing as in \code{format()}.
#' @param ... Other arguments passed on to \code{format()}.
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples
#' x = c(0.0001, 5000, 2.45e5)
#' form_nice(x)
#' 
form_nice = function(x,
                     big.mark = ',',
                     scientific = FALSE,
                     drop0trailing = TRUE) {
  trimws(
    format(
      x,
      big.mark = big.mark,
      scientific = scientific,
      drop0trailing = drop0trailing
    )  
  )
}
