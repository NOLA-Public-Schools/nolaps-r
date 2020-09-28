


#' @importFrom magrittr %>%



# Database ----------------------------------------------------------------



#' @export
getdata_governance <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema('db_owner', 'governance')
  )

}



#' @export
getdata_derivation_discipline <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema('db_owner', 'derivation_discipline')
  )

}



#' @export
getdata_disadvantage <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema('db_owner', 'disadvantage')
  )

}



#' @export
getdata_match <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema('db_owner', 'matches')
  )

}



#' @export
getdata_roster_assessment <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema('db_owner', 'roster_assessment')
  )

}



#' @export
getdata_roster_graduation <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema('db_owner', 'roster_graduation')
  )

}



# Local -------------------------------------------------------------------



#' @export
getdata_assessment_roster <- function() {

  readr::read_csv(
    file = '~/Projects/Assessment/Data/assessment_roster_postcert_clean.csv',
    col_types = 'cccccccccccccccccccccccddcccccccccc'
  )

}


