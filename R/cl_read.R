#' Function to load lookup tables from the andschar/chem-lookup GitHub repository
#'
#' @param table Which chem-lookup table should be loaded? Can be one of \code{
#' 'cl_identifiers', 'cl_class', 'cl_role' 'cl_pesticide', 'cl_identifiers_meta'
#' }
#' @param token Provide a GitHub personal access token (PAT) here or store it in
#' ~/.Renviron
#' 
#' @export 
cl_read = function(table = 'cl_identifiers',
                   token = NULL) {
  baseurl = 'https://raw.githubusercontent.com/andschar/chem-lookup/master'
  tmp = tempfile()
  gh(paste0('GET ', file.path(baseurl, paste0(table, '.tsv'))),
     .destfile = tmp,
     .overwrite = TRUE,
     token = token)
  fread(tmp, na.strings = '')
}