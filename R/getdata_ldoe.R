


#' @importFrom magrittr %>%



#' @export
getdata_demographic <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "demographic")
  )

}



#' @export
getdata_enrollmentderivation <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "enrollmentderivation")
  )

}



#' @export
getdata_multistat <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "multistat")
  )

}



#' @export
getdata_sped <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "sped")
  )

}



#' @export
getdata_sps <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "sps")
  )

}


