#' Helper function to reorder ggplot facets.
#'  
#' @param x The column to reorder.
#' @param by The column with which \code{x} should be ordered.
#' @param fun Passed on to stats::reorder().
#' @param sep Separator string used to concatenate columns.
#' @param ... Currently not used.
#' 
#' @description Taken from \url{https://stackoverflow.com/questions/52214071}.
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
reorder_within = function(x,
                          by,
                          within,
                          fun = mean,
                          sep = "___",
                          ...) {
  new_x = paste(x,
                within,
                sep = sep)
  stats::reorder(new_x,
                 by,
                 FUN = fun)
}

#' Helper function to reorder ggplot facets.
#'  
#' @param ... Parameters passed to \code{ggplot2::scale_x_discrete()}.
#' @param sep Separator string to split columns.
#' 
#' @description Taken from \url{https://stackoverflow.com/questions/52214071}.
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
scale_x_reordered = function(...,
                             sep = "___") {
  reg = paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}
