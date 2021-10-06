#' Function for Capitalize First Letters
#' 
#' @description Function for Capitalize First Letters of all Words in a string.
#' Taken from \code{?toupper()}.
#' 
#' @param x A string.
#'
#' @export
#'
#' @examples 
#' s = c('hello darkness my old Friend')
#' toupper_all(s)
#'
toupper_all = function(x) {
  s = strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
