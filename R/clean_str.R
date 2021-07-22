#' Function to clean string
#' @param x a character vector.2
#' @export
#' 
clean_str = function(x) {
  tolower(
    trimws(
      gsub(
        '\\s+', '_', x
      )
    )
  )
}

#' Function to clean names
#' @param df A data.frame or data.table.
#' @export
#' 
clean_names = function(df) {
  checkmate::assert_data_frame(df)
  data.table::setDT(df)
  data.table::setnames(df, clean_str(names(df)))
}
