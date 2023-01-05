#' Function to produce a dummy ggplot
#'
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples
#' gg = gg_dummy() 
#'
gg_dummy = function() {
  ggplot2::ggplot() +
    ggplot2::geom_text(ggplot2::aes(y = 1, x = 1, label = 'No Data.')) +
    ggplot2::theme_void()
}
