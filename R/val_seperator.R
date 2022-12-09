#' Function to separate values at the nth position
#' 
#' @param v A numeric vector.
#' @param n A numeric vector indicating the position to separate.
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples 
#' 
#' v = 21032262 
#' val_seperator(v)
#' val_seperator(v, 2)
#' 
val_seperator = function(v, n = 3) {
  # helper function
  rev_str = function(x) {
    unname(sapply(x, function(x) {
        paste0(rev(strsplit(x, NULL)[[1]]), collapse = '')
    }))
  }
  # chck
  if (!all(is.numeric(v))) {
    stop('Please provide a numeric value.')
  }
  v = as.character(v)
  # reverse
  v2 = rev_str(v)
  # set seperator
  v3 = sapply(v2, function(x) {
    if (nchar(x) > n) {
      gsub(paste0("(.{", n, "})"), "\\1,", x)
    } else {
      x
    }
  })
  # reverse again
  sub('^\\,', '', rev_str(v3))
}
