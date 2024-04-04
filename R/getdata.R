#' @export
getdata_governance <- function(src = nolaps::connection_ldoe()) {
  dplyr::tbl(
    src = src,
    dbplyr::in_schema("public", "governance")
  )
}


#' @export
getdata_match <- function() {
  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "matches")
  )
}
