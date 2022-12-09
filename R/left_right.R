#' Function to trim n leading characters from string.
#' 
#' @description Inspired by PosgreSQL
#' \url{https://www.postgresql.org/docs/current/functions-string.html}.
#' 
#' @param string String of vector of strings.
#' @param n Number of characters to be trimmed
#'
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#'
#' @export
#' 
#' @examples
#' v = c('1234567', 'abcdefg')
#' left(v, 2)
#' 
left = function(string, n) {
  substr(string, 1+n, nchar(string))
}
#' Function to trim n trailing characters from string.
#' 
#' @description Inspired by PosgreSQL
#' \url{https://www.postgresql.org/docs/current/functions-string.html}.
#' 
#' @param string String of vector of strings.
#' @param n Number of characters to be trimmed
#'
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#'
#' @export
#' 
#' @examples
#' v = c('1234567', 'abcdefg')
#' right(v, 2)
#' 
right = function (string, n) {
  substr(string, 1, nchar(string)-n)
}
