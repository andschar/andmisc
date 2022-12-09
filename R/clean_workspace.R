#' Function to clean workspace appart from some variables
#'
#' @param keep Objects to not be removed from the workspace. Provide as
#' character vector. By default getOption('keep_in_workspace') is called.
#' @param load_on_exit Source a file on exit. Provide a file path. By default
#' getOption('load_in_workspace') is called.
#' @param envir Envrionment to be cleaned. Deafault: .GlobalEnv
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
#' @export
#'
#' @examples
#' clean_workspace()
#'
clean_workspace = function(keep = getOption('keep_in_workspace'),
                           load_on_exit = getOption('load_in_workspace'),
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
