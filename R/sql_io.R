#' Connection function that handles argument or file inputs.
#' 
#' @param drv A database driver.
#' @param dbname A database.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a different databases (e.g. PostgreSQL). Can contain variables: user,
#' host, port, password, dbname.
#' @param ... Arguments passed on to \code{DBI::dbConnect()}.
#' @param quiet FALSE. Should progress messages be printed?
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @noRd
#' 
connection = function(drv,
                      dbname,
                      cred_file = NULL,
                      ...,
                      quiet = FALSE) {
  # include ellipsis
  l = c(
    list(drv = drv,
         dbname = dbname),
    list(...)
  )
  # new environment cred_file
  ee = new.env()
  # credentials file
  if (!is.null(cred_file)) {
    if (!quiet) {
      message('Sourcing cred_file: ', cred_file)
    }
    sys.source(cred_file, envir = ee)
    # prefer ellipsis variables
    l = list(
      drv = l$drv,
      host = c(l$host, ee$host)[1],
      port = c(l$port, ee$port)[1],
      dbname = c(l$dbname, ee$dbname)[1],
      user = c(l$user, ee$user)[1],
      password = c(l$password, ee$password)[1]
    )
  }
  if (!quiet) {
    message('Database: ', dbname)
  }
  # pass the list of arguments to connection function
  do.call(DBI::dbConnect, l)
}

#' Function to read from a database.
#'
#' @param query A query string.
#' @param drv A database driver.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a different databases (e.g. PostgreSQL). Can contain variables: user,
#' host, port, password, dbname.
#' @param ... Arguments passed on to \code{DBI::dbConnect()}. 
#' @param quiet Quiet function call?
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # connection to database required
#' read_query(query = "SELECT * FROM schema.table LIMIT 1;",
#'            cred_file = cred.R)
#' }
#' 
read_query = function(query = NULL,
                      drv,
                      dbname,
                      cred_file = NULL,
                      ...,
                      quiet = FALSE) {
  # checks
  if (is.null(query)) {
    stop('No query supplied.')
  }
  # connection
  con = connection(drv = drv,
                   dbname = dbname,
                   cred_file = cred_file,
                   ...,
                   quiet = quiet)
  # bigint = 'integer') # to not return integer64 https://stackoverflow.com/questions/45171762/set-dbgetquery-to-return-integer64-as-integer
  on.exit(DBI::dbDisconnect(con))
  # query
  time = Sys.time()
  dat = DBI::dbGetQuery(con, query)
  if (!quiet) {
    message('Query took: ', format(Sys.time() - time, digits = 1))
  }
  
  data.table::as.data.table(dat)
}

#' Function to send to a database.
#'
#' @param query A query string.
#' @param drv A database driver.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a different databases (e.g. PostgreSQL). Can contain variables: user,
#' host, port, password, dbname.
#' @param ... Arguments passed on to \code{DBI::dbConnect()}. 
#' @param quiet Quiet function call?
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # connection to database required
#' read_query(query = "SELECT * FROM schema.table LIMIT 1;",
#'            cred_file = cred.R)
#' }
#'
send_query = function(query = NULL,
                      drv,
                      dbname,
                      cred_file = NULL,
                      ...,
                      quiet = FALSE) {
  # checks
  if (is.null(query)) {
    stop('No query supplied.')
  }
  # query string or path to file
  q = try(suppressWarnings(andmisc::readChar2(query)), silent = TRUE)
  if (!inherits(q, 'try-error')) {
    query = q
  }
  # connection
  con = connection(drv = drv,
                   dbname = dbname,
                   cred_file = cred_file,
                   ...,
                   quiet = quiet)
  # bigint = 'integer') # to not return integer64 https://stackoverflow.com/questions/45171762/set-dbgetquery-to-return-integer64-as-integer
  on.exit(DBI::dbDisconnect(con))
  # query
  time = Sys.time()
  DBI::dbSendQuery(con, query)
  if (!quiet) {
    message('Query took: ', format(Sys.time() - time, digits = 1))
  }
}

#' Functions to write a table to a database.
#'
#' @param dat data.frame, data.table or tibble.
#' @param schema database schema.
#' @param tbl database table.
#' @param key Set a primary key in table?
#' @param comment_str Add a comment to table? Recommended.
#' @param overwrite Should an existing database table be overwritten?
#' TRUE (default).
#' @param drv A database driver.
#' @param dbname The database name.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a different databases (e.g. PostgreSQL). Can contain variables: user,
#' host, port, password, dbname.
#' @param ... Arguments passed on to \code{DBI::dbConnect()}. 
#' @param quiet Quiet function call?
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # connection to database required
#' read_query(query = "SELECT * FROM schema.table LIMIT 1;",
#'            cred_file = cred.R)
#' }
#'
write_tbl = function(dat = NULL,
                     schema = NULL,
                     tbl = NULL,
                     key = NULL,
                     comment_str = NULL,
                     overwrite = TRUE,
                     drv,
                     dbname,
                     cred_file = NULL,
                     ...,
                     quiet = FALSE) {
  # browser()
  if (is.null(dat)) {
    stop('No data.frame supplied.')
  }
  if (is.null(tbl)) {
    stop('No table supplied.')
  }
  # connection
  con = connection(drv = drv,
                   dbname = dbname,
                   cred_file = cred_file,
                   ...,
                   quiet = quiet)
                   # bigint = 'integer') # to not return integer64 https://stackoverflow.com/questions/45171762/set-dbgetquery-to-return-integer64-as-integer
  on.exit(DBI::dbDisconnect(con))
  # query
  time = Sys.time()
  if (!is.null(schema)) {
    DBI::dbSendQuery(con, paste0("CREATE SCHEMA IF NOT EXISTS ", schema, ";"))
  }
  DBI::dbWriteTable(con,
                    name = c(schema, tbl),
                    value = dat,
                    overwrite = overwrite,
                    row.names = FALSE)
  if (!is.null(key)) {
    DBI::dbSendQuery(con, paste0("ALTER TABLE ",
                                 paste0(c(schema, tbl), collapse = "."), " ",
                                 "ADD PRIMARY KEY (", key, ");"))
  }
  if (!is.null(comment_str)) {
    DBI::dbSendQuery(con, paste0("COMMENT ON TABLE ",
                                 paste0(c(schema, tbl), collapse = "."), " ",
                                 "IS '", comment_str, "';"))
  }
  if (!quiet) {
    message('Table created: ', paste0(c(dbname, schema, tbl), collapse = '.'))
    message('Query took: ', format(Sys.time() - time, digits = 1))
  }
}

#' Function to read spatial data from a database.
#'
#' @param query A query string.
#' @param drv A database driver.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a different databases (e.g. PostgreSQL). Can contain variables: user,
#' host, port, password, dbname.
#' @param ... Arguments passed on to \code{DBI::dbConnect()}. 
#' @param quiet Quiet function call?
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # connection to database required
#' read_spatial(query = "SELECT * FROM schema.table LIMIT 1;",
#'              cred_file = cred.R)
#' }
#' 
read_sf = function(query = NULL,
                   drv,
                   dbname,
                   cred_file = NULL,
                   ...,
                   quiet = FALSE) {
  # checks
  if (is.null(query)) {
    stop('No query supplied.')
  }
  # connection
  con = connection(drv = drv,
                   dbname = dbname,
                   cred_file = cred_file,
                   ...,
                   quiet = quiet)
  # bigint = 'integer') # to not return integer64 https://stackoverflow.com/questions/45171762/set-dbgetquery-to-return-integer64-as-integer
  on.exit(DBI::dbDisconnect(con))
  # query
  if (!quiet) {
    message('Database: ', DBI::dbGetQuery(con, "SELECT current_database();"))
  }
  time = Sys.time()
  dat = sf::st_read(con, query = query)
  if (!quiet) {
    message('Query took: ', format(Sys.time() - time, digits = 1))
  }
  
  dat
}

#' Functions to write a spatial table to a database.
#'
#' @param dat a spatial data.frame (from the sf or sp package).
#' @param schema database schema.
#' @param tbl database table.
#' @param key Set a primary key in table?
#' @param comment_str Add a comment to table? Recommended.
#' @param overwrite Should an existing database table be overwritten?
#' TRUE (default).
#' @param drv A database driver.
#' @param dbname The database name.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a different databases (e.g. PostgreSQL). Can contain variables: user,
#' host, port, password, dbname.
#' @param ... Arguments passed on to \code{DBI::dbConnect()}. 
#' @param quiet Quiet function call?
#' 
#' @author Andreas Scharmueller, \email{andschar@@proton.me}
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # connection to database required
#' dat = sf::st_sf(id = 1,
#'                 sf::st_sfc(sf::st_point(1:2))) # better to name it!
#' 
#' write_sf(dat, 
#'          tbl = 'test',
#'          cred_file = 'TODO_cred.R',
#'          overwrite = TRUE)
#' }
#'
write_sf = function(dat = NULL,
                    schema = NULL,
                    tbl = NULL,
                    key = NULL,
                    comment_str = NULL,
                    overwrite = TRUE,
                    drv,
                    dbname,
                    cred_file = NULL,
                    ...,
                    quiet = FALSE) {
  # checks
  if (is.null(dat)) {
    stop('No data.frame supplied.')
  }
  if (is.null(tbl)) {
    stop('No table supplied.')
  }
  # connection
  con = connection(drv = drv,
                   dbname = dbname,
                   cred_file = cred_file,
                   ...,
                   quiet = quiet)
  # bigint = 'integer') # to not return integer64 https://stackoverflow.com/questions/45171762/set-dbgetquery-to-return-integer64-as-integer
  on.exit(DBI::dbDisconnect(con))
  # query
  db = DBI::dbGetQuery(con, "SELECT current_database();")$current_database
  time = Sys.time()
  if (!is.null(schema)) {
    DBI::dbSendQuery(con, paste0("CREATE SCHEMA IF NOT EXISTS ", schema, ";"))
  }
  sf::st_write(dat,
               dsn = con,
               layer = c(schema, tbl),
               delete_layer = overwrite,
               row.names = FALSE)
  if (!is.null(key)) {
    DBI::dbSendQuery(con, paste0("ALTER TABLE ",
                                 paste0(c(schema, tbl), collapse = "."), " ",
                                 "ADD PRIMARY KEY (", key, ");"))
  }
  if (!is.null(comment_str)) {
    DBI::dbSendQuery(con, paste0("COMMENT ON TABLE ",
                                 paste0(c(schema, tbl), collapse = "."), " ",
                                 "IS '", comment_str, "';"))
  }
  if (!quiet) {
    message('Table created: ', paste0(c(db, schema, tbl), collapse = '.'))
    message('Query took: ', format(Sys.time() - time, digits = 1))
  }
}
