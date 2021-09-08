#' Function to Construct Path to Files
#' 
#' @description Works as \code{file.path()} but automatically creates
#' a directory when missing.
#'
#' @param ... Character vectors as in \code{file.path()}.
#' @param fsep Path seperator as in \code{file.path()}.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples 
#' file.path2(tempdir(), 'test', 'blub')
#' 
file.path2 = function(..., fsep = .Platform$file.sep) {
  path = file.path(..., fsep = fsep)
  dir.create(path, showWarnings = FALSE)
  path
}
