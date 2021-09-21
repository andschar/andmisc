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
  # data.table
  data.table::setDT(dat)
  # check NA
  dupl_n = sapply(dat[ , .SD, .SDcols = col ],
                function(x) length(which(is.na(x))))
  dupl_perc = round(dupl_n / nrow(dat) * 1e2, 1)
  # message
  if (any(na_perc > 0)) {
    na_col = na_perc[ na_perc > 0 ]
    warning('Duplicates in columns:\n\t',
            paste0(paste(names(na_col), na_col, sep = ': '),
                   collapse = '\n\t'))
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
  # data.table
  data.table::setDT(dat)
  # check NA
  na_n = sapply(dat[ , .SD, .SDcols = col ],
                function(x) length(which(is.na(x))))
  na_perc = round(na_n / nrow(dat) * 1e2, 1)
  # message
  if (any(na_perc > 0)) {
    na_col = na_perc[ na_perc > 0 ]
    warning('NAs in columns:\n\t',
            paste0(paste(names(na_col), na_col, sep = ': '),
                   collapse = '\n\t'))
  }
}
