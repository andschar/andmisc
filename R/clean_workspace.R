#' Function to clean workspace appart from some variables
#' 
#' @param keep Objects to not be removed from the workspace. Provide as 
#' character vector
#' @param load_on_exit character; Source a file on exit. Default: NULL - none.
#' @param envir Envrionment to be cleaned. Deafault: .GlobalEnv
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#'
#' @examples 
#' clean_workspace()
#' 
clean_workspace = function(keep = NULL,
                           load_on_exit = NULL,
                           envir = .GlobalEnv) {
  # clean
  l = setdiff(ls(envir = envir),
              c("clean_workspace", keep))
  rm(list = l,
     envir = envir)
  gc()
  # load
  if (!is.null(load_on_exit)) {
    source(load_on_exit)
  }
}

