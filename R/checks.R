#' Check if duplicated values are in columns of data.frame.
#' 
#' @param dat A data.frame.
#' @param col A character vector specifying the columns.
#' 
#' @export
#' 
chck_dupl = function(dat, col = NULL) {
  # checks
  checkmate::assert_data_frame(dat)
  if (is.null(col)) { col = names(dat) }
  checkmate::assert_vector(col)
  # check duplicates
  dat2 = dat[ names(dat) %in% col ]
  chck_val = which(sapply(dat2, function(x) any(duplicated(x))))
  if (length(chck_val) != 0) {
    stop("Duplicates in data. Column(s):\n\t", paste0(names(chck_val), collapse = ', '), ".")
  }
}

#' Check if NA values are in columns of data.frame.
#' 
#' @param dat A data.frame.
#' @param col A character vector specifying the columns.
#' 
#' @export
#'
chck_na = function(dat, col = NULL) {
  # checks
  checkmate::assert_data_frame(dat)
  if (is.null(col)) { col = names(dat) }
  checkmate::assert_vector(col)
  # check NA
  dat2 = dat[ names(dat) %in% col ]
  chck_val = which(sapply(dat2, function(x) any(is.na(x))))
  if (length(chck_val) != 0) {
    stop("NAs in data. Column(s):\n\t", paste0(names(chck_val), collapse = ', '), ".")
  }
}
