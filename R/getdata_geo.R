#' @importFrom magrittr %>%



# Credentials -------------------------------------------------------------



#' @export
key_googlemaps <- function() {

  keyring::key_get("googlemaps")

}



# Geographies -------------------------------------------------------------



#' @export
getdata_catchments <- function() {

  sf::st_read(path_data("Geographies/Catchments/catchments.shp"))

}



#' @export
getdata_districts <- function() {

  sf::st_read(path_data("Geographies/Districts/districts.shp"))

}



#' @export
getdata_neighborhoods <- function() {

  sf::st_read(path_data("Geographies/Neighborhoods/neighborhoods.shp")) %>%
    dplyr::rename(neighborhood = name)

}



#' @export
getdata_planning <- function() {

  sf::st_read(path_data("Geographies/Planning Districts/planning_districts.shp"))

}



#' @export
getdata_zips_orleans <- function() {

  sf::st_read(path_data("Geographies/Zip Codes/zips_orleans.shp"))

}



#' @export
getdata_zips_split <- function() {

  sf::st_read(path_data("Geographies/Zip Codes/zips_split.shp"))

}



# Students ----------------------------------------------------------------



#' @export
getdata_student_coordinates <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe(),
    dbplyr::in_schema("public", "student_coordinates")
  )

}



# School coordinates ------------------------------------------------------



#' @export
getdata_school_coordinates <- function() {

  dplyr::tbl(
    src = nolaps::connection_ldoe_test(),
    dbplyr::in_schema("db_owner", "school_coordinates")
  )

}


