#' Function to create different heights and widths in RMarkdown Code Chunks which are Created automatically
#' 
#' @description Taken from StackOverflow.
#' 
#' @param g A plot.
#' @param fig_height The Figure height as in an RMarkdown document.
#' @param fig_width The Figure width as in an RMarkdown document.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples
#' subchunkify(plot(iris, Sepal.Length ~ Sepal.Width))
#' 
subchunkify <- function(g, fig_height=7, fig_width=5) {
  g_deparsed <- paste0(deparse(
    function() {g}
  ), collapse = '')
  
  sub_chunk <- paste0("
  `","``{r sub_chunk_", floor(runif(1) * 10000), ", fig.height=",
                      fig_height, ", fig.width=", fig_width, ", echo=FALSE}",
                      "\n(", 
                      g_deparsed
                      , ")()",
                      "\n`","``
  ")
  
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}
