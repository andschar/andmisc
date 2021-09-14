#' Function to create file name (path) string.
#' 
#' @description A file path is created in the form <dir>/<prequel><arg><ext>.
#' 
#' @param dir A directory to which the path should point to.
#' @param prequel Prequel that is appended to the file name (e.g. '~/project').
#' @param arg A named list or vector of arguments that are used for creating
#' the file name. Once created, they can be extracted with \code{denamer()}.
#' @param sep String that separates names and entries of the arg
#' (Default: <_>).
#' @param ext Should a file extension be added at the end of the string?
#' (e.g. .png)?
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples
#' namer(dir = tempdir(),
#'       prequel = 'test-prequel',
#'       arg = list(weight = 12,
#'                  color = 'white'),
#'       sep = '_',
#'       ext = '.png')
#'
namer = function(dir = NULL,
                 prequel = NULL,
                 arg = NULL,
                 sep = getOption('sep'),
                 ext = NULL) {
  if (is.null(dir) && is.null(prequel)) {
    stop('At least one of the arguments "dir" or "prequel" must be specified.')
  }
  # checks
  if (is.null(arg) || any(names(arg) == '')) {
    stop('At least one named string must be passed in arg.')
  }
  if (is.null(sep)) {
    sep = '_'
  }
  # concatenate
  if (is.null(prequel)) {
    middle = paste0(names(arg), sep, arg, collapse = sep)
  } else {
    middle = paste0(prequel, sep,
                    paste0(names(arg), sep, arg, collapse = sep))
  }
  if (!is.null(ext)) {
    ext = sub('\\.+', '.', paste0('.', ext))
  }
  # out
  if (is.null(dir)) {
    paste0(middle, ext)
  } else {
    file.path(dir, paste0(middle, ext))
  }
}

#' Function to extract arguments from an object created by \code{namer()}.
#' 
#' @param string A string created by \code{namer()}.
#' @param arg Which argument should be extracted?
#' @param sep Separator used.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples 
#' sep = '_'
#' denamer(
#'  namer(dir = tempdir(),
#'         prequel = 'test-prequel',
#'         arg = list(weight = 12,
#'                    color = 'white'),
#'         sep = sep,
#'         ext = '.png'),
#'   arg = c('color', 'weight'),
#'   sep = sep
#' )
#'
denamer = function(string = NULL,
                   arg = NULL,
                   sep = getOption('sep')) {
  # checks
  if (is.null(string)) {
    stop('Please provide a string to dename.')
  }
  if (is.null(arg)) {
    stop('Please provide at least one arg to be extracted from string.')
  }
  if (is.null(sep)) {
    sep = '_'
  }
  # extract
  string_split = strsplit(string, sep, fixed = TRUE)[[1]]
  out = string_split[ grep(paste0(arg, collapse = '|'), o) + 1 ]
  names(out) = arg
  
  return(out)
}
