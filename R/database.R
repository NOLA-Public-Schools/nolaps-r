


#' @importFrom magrittr %>%



#' @export
connection_ldoe <- function() {

  DBI::dbConnect(
    odbc::odbc(),
    Driver = '{SQL Server}',
    Server = keyring::key_get('server'),
    Database = keyring::key_get('database'),
    UID = keyring::key_get('username'),
    PWD = keyring::key_get('password'),
    Port = 1433
  )

}


