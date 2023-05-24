#' Function to check what packages are used in multiple scripts in a directory
#' 
#' @param path Specify root directory.
#'
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples
#' \donttest{
#'   # Not certain if a root directory exists
#'   rdepend('.')
#' }
#' 
rdepend = function(path = '.') {
  
  fl_v = list.files(path,
                    include.dirs = TRUE,
                    full.names = TRUE,
                    pattern = '\\.R$',
                    recursive = TRUE)
  l = list()
  for (i in fl_v) {
    # pattern matching
    dep = grep('::', readLines(i), value = TRUE)
    dep = stringr::str_extract(dep, '[A-z0-9]+::[A-z0-9]+\\(')
    dep = unique(stringr::str_remove(dep, '::[A-z0-9]+\\('))
    # list 
    l[[i]] = dep
  }  
  # return
  sort(na.omit(unique(unlist(l))))
}
