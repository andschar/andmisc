#' Connection function that handles argument or file inputs.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @noRd
#' 
connection = function(cred_file,
                      host,
                      port,
                      dbname,
                      user,
                      password) {
  # arg list
  l = list(cred_file = cred_file,
           host = host,
           port = port,
           dbname = dbname,
           user = user,
           password = password)
  # new environment cred_file
  ee = new.env()
  # credentials file
  if (!is.null(cred_file)) {
    message('Sourcing cred_file: ', cred_file)
    sys.source(cred_file, envir = ee)
    # source(cred_file, local = TRUE) # OLD
  }
  # select only 1 variable, prefer match.call() variables
  host = c(l$host, ee$host)[1]
  port = c(l$port, ee$port)[1]
  dbname = c(l$dbname, ee$dbname)[1]
  user = c(l$user, ee$user)[1]
  password = c(l$password, ee$password)[1]
  # checking
  if (is.null(host)) {
    stop('Provide a host.')
  }
  if (is.null(port)) {
    stop('Provide a port.')
  }
  if (is.null(dbname)) {
    stop('Provide a dbname.')
  }
  if (is.null(password)) {
    stop('Provide a password.')
  }
  if (is.null(user)) {
    stop('Provide a user.')
  }
  # connection
  DBI::dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password
  )
}

#' Function to read from a database.
#'
#' @param query A query string.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a Postgres database. Can contain variables: user, host, port, password,
#' dbname.
#' @param host Optional host. Overwritten if \code{host} is defined in
#' cred_file.
#' @param port Optional port Overwritten if \code{port} is defined in
#' cred_file.
#' @param dbname Optional dbname Overwritten if \code{dbname} is defined
#' in cred_file.
#' @param user Optional user. Overwritten if \code{user} is defined in
#' cred_file.
#' @param password Optional password Overwritten if \code{password} is defined
#' in cred_file.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
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
                      cred_file = NULL,
                      host = NULL,
                      port = NULL,
                      dbname = NULL,
                      user = NULL,
                      password = NULL) {
  # checks
  if (is.null(query)) {
    stop('No query supplied.')
  }
  # connection
  con = connection(cred_file = cred_file,
                   host = host,
                   port = port,
                   dbname = dbname,
                   user = user,
                   password = password)
  # bigint = 'integer') # to not return integer64 https://stackoverflow.com/questions/45171762/set-dbgetquery-to-return-integer64-as-integer
  on.exit(DBI::dbDisconnect(con))
  # query
  message('Database: ', DBI::dbGetQuery(con, "SELECT current_database();"))
  time = Sys.time()
  dat = DBI::dbGetQuery(con, query)
  message('Query took: ', format(Sys.time() - time, digits = 1))
  
  data.table::as.data.table(dat)
}

#' Function to send to a database.
#'
#' @param query A query string.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a Postgres database. Can contain variables: user, host, port, password,
#' dbname.
#' @param user Optional user. Overwritten if \code{user} is defined in
#' cred_file.
#' @param host Optional host. Overwritten if \code{host} is defined in
#' cred_file.
#' @param port Optional port Overwritten if \code{port} is defined in
#' cred_file.
#' @param password Optional password Overwritten if \code{password} is defined
#' in cred_file.
#' @param dbname Optional dbname Overwritten if \code{dbname} is defined
#' in cred_file.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
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
                      cred_file = NULL,
                      user = NULL,
                      host = NULL,
                      port = NULL,
                      password = NULL,
                      dbname = NULL) {
  # checks
  if (is.null(query)) {
    stop('No query supplied.')
  }
  # connection
  con = connection(cred_file = cred_file,
                   host = host,
                   port = port,
                   dbname = dbname,
                   user = user,
                   password = password)
  # bigint = 'integer') # to not return integer64 https://stackoverflow.com/questions/45171762/set-dbgetquery-to-return-integer64-as-integer
  on.exit(DBI::dbDisconnect(con))
  # query
  message('Database: ', DBI::dbGetQuery(con, "SELECT current_database();"))
  time = Sys.time()
  DBI::dbSendQuery(con, query)
  message('Query took: ', format(Sys.time() - time, digits = 1))
}

#' Functions to write a table to a database.
#'
#' @param dat data.frame, data.table or tibble.
#' @param schema database schema.
#' @param tbl database table.
#' @param overwrite Should an existing database table be overwritten?
#' TRUE (default).
#' @param key Set a primary key in table?
#' @param comment_str Add a comment to table? Recommended.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a Postgres database. Can contain variables: user, host, port, password,
#' dbname.
#' @param user Optional user. Overwritten if \code{user} is defined in
#' cred_file.
#' @param host Optional host. Overwritten if \code{host} is defined in
#' cred_file.
#' @param port Optional port Overwritten if \code{port} is defined in
#' cred_file.
#' @param password Optional password Overwritten if \code{password} is defined
#' in cred_file.
#' @param dbname Optional dbname Overwritten if \code{dbname} is defined
#' in cred_file.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
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
                     cred_file = NULL,
                     host = NULL,
                     port = NULL,
                     dbname = NULL,
                     user = NULL,
                     password = NULL) {
  if (is.null(dat)) {
    stop('No data.frame supplied.')
  }
  if (is.null(tbl)) {
    stop('No table supplied.')
  }
  # connection
  con = connection(cred_file = cred_file,
                   host = host,
                   port = port,
                   dbname = dbname,
                   user = user,
                   password = password)
  # bigint = 'integer') # to not return integer64 https://stackoverflow.com/questions/45171762/set-dbgetquery-to-return-integer64-as-integer
  on.exit(DBI::dbDisconnect(con))
  # query
  db = DBI::dbGetQuery(con, "SELECT current_database();")$current_database
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
  
  message('Table created: ', paste0(c(db, schema, tbl), collapse = '.'))
  message('Query took: ', format(Sys.time() - time, digits = 1))
}

#' Function to read spatial data from a database.
#'
#' @param query A query string.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a Postgres database. Can contain variables: user, host, port, password,
#' dbname.
#' @param host Optional host. Overwritten if \code{host} is defined in
#' cred_file.
#' @param port Optional port Overwritten if \code{port} is defined in
#' cred_file.
#' @param dbname Optional dbname Overwritten if \code{dbname} is defined
#' in cred_file.
#' @param user Optional user. Overwritten if \code{user} is defined in
#' cred_file.
#' @param password Optional password Overwritten if \code{password} is defined
#' in cred_file.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
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
                   cred_file = NULL,
                   host = NULL,
                   port = NULL,
                   dbname = NULL,
                   user = NULL,
                   password = NULL) {
  # checks
  if (is.null(query)) {
    stop('No query supplied.')
  }
  # connection
  con = connection(cred_file = cred_file,
                   host = host,
                   port = port,
                   dbname = dbname,
                   user = user,
                   password = password)
  # bigint = 'integer') # to not return integer64 https://stackoverflow.com/questions/45171762/set-dbgetquery-to-return-integer64-as-integer
  on.exit(DBI::dbDisconnect(con))
  # query
  message('Database: ', DBI::dbGetQuery(con, "SELECT current_database();"))
  time = Sys.time()
  dat = sf::st_read(con, query = query)
  message('Query took: ', format(Sys.time() - time, digits = 1))
  
  dat
}

#' Functions to write a spatial table to a database.
#'
#' @param dat a spatial data.frame (from the sf or sp package).
#' @param schema database schema.
#' @param tbl database table.
#' @param overwrite Should an existing database table be overwritten?
#' TRUE (default).
#' @param key Set a primary key in table?
#' @param comment_str Add a comment to table? Recommended.
#' @param cred_file An .R file containing the necessary credentials to connect
#' to a Postgres database. Can contain variables: user, host, port, password,
#' dbname.
#' @param user Optional user. Overwritten if \code{user} is defined in
#' cred_file.
#' @param host Optional host. Overwritten if \code{host} is defined in
#' cred_file.
#' @param port Optional port Overwritten if \code{port} is defined in
#' cred_file.
#' @param password Optional password Overwritten if \code{password} is defined
#' in cred_file.
#' @param dbname Optional dbname Overwritten if \code{dbname} is defined
#' in cred_file.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # connection to database required
#' dat = sf::st_sf(id = 1,
#'                 sf::st_sfc(sf::st_point(1:2))) # better to name it!
#' 
#' write_sf(dat, tbl = 'test',
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
                    cred_file = NULL,
                    host = NULL,
                    port = NULL,
                    dbname = NULL,
                    user = NULL,
                    password = NULL) {
  # browser()
  # checks
  if (is.null(dat)) {
    stop('No data.frame supplied.')
  }
  if (is.null(tbl)) {
    stop('No table supplied.')
  }
  # connection
  con = connection(cred_file = cred_file,
                   host = host,
                   port = port,
                   dbname = dbname,
                   user = user,
                   password = password)
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
  message('Table created: ', paste0(c(db, schema, tbl), collapse = '.'))
  message('Query took: ', format(Sys.time() - time, digits = 1))
}
