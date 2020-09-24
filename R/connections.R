


#' @importFrom magrittr %>%



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


