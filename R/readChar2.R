#' Wrapper around \code{readChar()}
#' 
#' @param con A connection object, or a character string naming a file, or a raw vector.
#' Same as in \code{readChar()}.
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples
#' tmp = tempfile()
#' writeLines('Hello\nDarkness\nFriend', tmp)
#' readChar2(tmp)
#' 
readChar2 = function(con) {
  readChar(con, nchars = file.info(con)$size)
}
