#' Function to convert between hypen and hyphen-less CAS strings.
#' 
#' @param x A vector of CAS numbers, either with hyphen (e.g. 50-00-0) or
#' without hyphen (e.g. 50000)
#' @param hyphen Should the hyphen or the hyphen-less CAS string be returned?
#' @param na_999 Should NAs be replaced with 999-99-9 or 999999 ?
#' 
#' @export
#' 
#' @examples
#' vec = c('50-00-0', '1071836', '1071-83-6')
#' casconv(vec)
#' casconv(vec, hyphen = FALSE)
#' casconv(vec, hyphen = FALSE, na_999 = TRUE)
casconv = function(x,
                   hyphen = TRUE,
                   na_999 = FALSE) {
  # replace NAs
  x[ is.na(x) ] = '999-99-9'
  # remove all hyphen
  x = gsub('-', '', x)
  # hyphen
  if (hyphen) {
    out = paste(substr(x, 1, nchar(x)-3),
                substr(x, nchar(x)-2, nchar(x)-1),
                substr(x, nchar(x), nchar(x)),
                sep = '-')
  } else {
  # introduce CAS specific hyphen
    out = x
  }
  if (!na_999) {
    out[ out %in% c('999999', '999-99-9') ] = NA
  }
  out
}
