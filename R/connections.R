


#' @export
connection_ldoe <- function() {

  DBI::dbConnect(
    RPostgres::Postgres(),
    host = keyring::key_get("ldoe", "host"),
    dbname = keyring::key_get("ldoe", "dbname"),
    user = keyring::key_get("ldoe", "user"),
    password = keyring::key_get("ldoe", "password"),
    port = 5432
  )

}



#' @export
write_ldoe <- function(x, tablename) {

  conn <- connection_ldoe()

  DBI::dbWriteTable(conn, name = tablename, value = x, overwrite = TRUE)

}



#' @export
connection_ldoe_test <- function() {

  DBI::dbConnect(
    odbc::odbc(),
    Driver = '{SQL Server}',
    Server = keyring::key_get('ldoe_test', 'server'),
    Database = keyring::key_get('ldoe_test', 'database'),
    UID = keyring::key_get('ldoe_test', 'username'),
    PWD = keyring::key_get('ldoe_test', 'password'),
    Port = 1433
  )

}


