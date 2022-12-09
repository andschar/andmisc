#' Bind a list of vectors of different lengths.
#' 
#' @param l A list of vectors.
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#'
#' @examples 
#' l = list(col1 = c('A', 'B'),
#' col2  =c('A'))
#' bind_l_vec(l)
#'
bind_l_vec = function(l) {
  data.table::rbindlist(
    lapply(l, function(x) data.table::data.table(t(x))),
    fill = TRUE
  )
}
