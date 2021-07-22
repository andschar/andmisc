#' Helper functions to apply match() to list column elements
#'
#' @param lhs Character vector.
#' @param rhs Character vector that should be looked up in x.
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
#' @export
#' 
#' @examples
#' require(data.table)
#' dt = data.table(a = c('R1', 'R2', 'R3', 'R4', 'R5'),
#'                 b = list(c('A', 'B'),
#'                 c('A'),
#'                 list('A', 'B', 'C'),
#'                 list(list(c('B', 'C'))),
#'                 'B'))
#' dt[ b %lin% 'B' ]
#'
`%lin%` = function(lhs, rhs) {
  sapply(lhs, function(lhs) {
    any(match(unlist(lhs), rhs))
  })
}


#' Helper functions to apply chmatch() to list column elements
#'
#' @param lhs Character vector.
#' @param rhs Character vector that should be looked up in x.
#'
#' @import data.table
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
#' @export
#'
#' @examples
#' require(data.table)
#' dt = data.table(a = c('R1', 'R2', 'R3', 'R4', 'R5'),
#'                 b = list(c('A', 'B'),
#'                 c('A'),
#'                 list('A', 'B', 'C'),
#'                 list(list(c('B', 'C'))),
#'                 'B'))
#' dt[ b %lchin% 'B' ]
#' 
`%lchin%` = function(lhs, rhs) {
  sapply(lhs, function(lhs) {
    any(chmatch(unlist(lhs), rhs))
  })
}
