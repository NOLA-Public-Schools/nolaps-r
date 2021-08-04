


#' @export
getdata_achievementsummary <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "achievementsummary")
  )

}



#' @export
getdata_act <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "act")
  )

}



#' @export
getdata_act_subgroup <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "act_subgroup")
  )

}



#' @export
getdata_ap <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "ap")
  )

}



#' @export
getdata_ap_subgroup <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "ap_subgroup")
  )

}



#' @export
getdata_assessment <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "assessment")
  )

}



#' @export
getdata_college <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "college")
  )

}



#' @export
getdata_demographic <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "demographic")
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
getdata_discipline <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema('db_owner', 'discipline')
  )

}



#' @export
getdata_enrollmentderivation <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "enrollmentderivation")
  )

}



#' @export
getdata_graduation <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema('db_owner', 'graduation')
  )

}



#' @export
getdata_masterysummary <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "masterysummary")
  )

}



#' @export
getdata_multistat <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "multistat")
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


