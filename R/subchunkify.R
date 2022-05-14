#' Function to create different heights and widths in RMarkdown Code Chunks which are Created automatically
#' 
#' @description Taken from StackOverflow.
#' 
#' @param g A plot.
#' @param fig.height The Figure height as in an RMarkdown document.
#' @param fig.width The Figure width as in an RMarkdown document.
#' @param echo As in an RMarkdown document.
#' @param message As in an RMarkdown document.
#' @param warning As in an RMarkdown document.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples
#' subchunkify(plot(iris, Sepal.Length ~ Sepal.Width))
#' 
subchunkify = function(g,
                       fig.height = 7,
                       fig.width = 5,
                       echo = FALSE,
                       message = FALSE,
                       warning = FALSE) {
  g_deparsed = paste0(deparse(
    function() {g}
  ), collapse = '')
  
  sub_chunk = paste0("```{r sub_chunk_", floor(runif(1) * 1e5),
                     ", fig.height=", fig.height,
                     ", fig.width=", fig.width,
                     ", echo=", echo,
                     ", warning=", warning,
                     ", message=", message,
                     " }",
                     "\n(", 
                     g_deparsed
                     , ")()",
                     "\n```
                     ")
  
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}



