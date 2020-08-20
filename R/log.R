#' Function to log a msg to a file
#' 
#' @param msg Message to log.
#' @param file file to log to. Stored in the root directory.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples
#' \donttest{
#'   # connection to database required
#'   log_msg('Test')
#' }
#' 
log_msg = function(msg = 'script run.',
                   file = 'script.log') {
  script = basename(sys.frame(1)$ofile)
  out = paste(paste(Sys.time(), script, 'run: ', sep = ' '),
              msg,
              sep = '\t')
  write(out, file.path(prj, file), append = TRUE)
  message(out)
}