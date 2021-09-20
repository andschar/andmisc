#' Function to Construct Path to Files
#' 
#' @description Works as \code{file.path()} but automatically creates
#' a directory when missing.
#'
#' @param ... Character vectors as in \code{file.path()}.
#' @param fsep Path separator as in \code{file.path()}.
#' @param recursive As in \code{dir.create()}.
#' @param mode As in \code{dir.create()}.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples 
#' file.path2(tempdir(), 'test', 'blub')
#' 
file.path2 = function(...,
                      recursive = TRUE,
                      mode = '0777',
                      fsep = .Platform$file.sep) {
  path = file.path(...,
                   fsep = fsep)
  dir.create(path,
             recursive = recursive,
             mode = mode,
             showWarnings = FALSE)
  path
}
