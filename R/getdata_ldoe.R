#' @export
getdata_actuals_10_1 <- function() {
  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "actuals_10_1")
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
getdata_demographic <- function(src = nolaps::connection_ldoe()) {
  dplyr::tbl(
    src = src,
    dbplyr::in_schema("public", "demographic")
  )
}


#' @export
getdata_disadvantage <- function() {
  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "disadvantage")
  )
}


#' @export
getdata_discipline <- function() {
  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "discipline")
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
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "graduation")
  )
}


#' @export
getdata_grad_roster_4 <- function() {
  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "grad_roster_4")
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
getdata_multistat <- function(src = nolaps::connection_ldoe()) {
  dplyr::tbl(
    src = src,
    dbplyr::in_schema("public", "multistat")
  )
}


#' @export
getdata_pep_staff <- function() {
  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "pep_staff")
  )
}


#' @export
getdata_sped <- function() {
  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "sped")
  )
}


#' @export
getdata_sps <- function() {
  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "sps")
  )
}
