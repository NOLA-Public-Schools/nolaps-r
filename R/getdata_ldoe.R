


#' @importFrom magrittr %>%



#' @export
getdata_enrollmentderivation <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema('db_owner', 'enrollmentderivation')
  )

}



#' @export
getdata_sps <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema('db_owner', 'sps')
  )

}


