#' Function to log a msg to a file
#' 
#' @param msg Message to log.
#' @param dir A directory to write the report to.
#' @param file file to log to.
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
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
                   dir = NULL,
                   file = 'script.log') {
  # checks
  if (is.null(dir)) {
    dir = getwd()
  }
  script = basename(sys.frame(1)$ofile)
  out = paste(paste(Sys.time(), script, 'run: ', sep = ' '),
              msg,
              sep = '\t')
  write(out, file.path(dir, file), append = TRUE)
  message(out)
}
