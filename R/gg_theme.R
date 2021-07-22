#' Custom ggplot2 theme
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param text_size Size of text.
#' 
#' @import ggplot2
#' 
#' @export
#' 
#' @examples 
#' library(ggplot2)
#' p = ggplot(mtcars) + 
#'  geom_point(aes(x = wt, y = mpg, 
#' colour = factor(gear))) + facet_wrap(~am)
#' p
#' p + theme_as()
#' 
theme_as = function(base_size = 12,
                    base_family = 'Open Sans',
                    text_size = 14) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      text = element_text(size = text_size),
      axis.text = element_text(size = text_size + 1),
      axis.title.x = element_text(size = text_size, face = 'bold', vjust = 0),
      axis.title.y = element_text(size = text_size, face = 'bold', vjust = 1),
      legend.position = 'right',
      legend.key = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = text_size, face = 'bold')
    )
}
