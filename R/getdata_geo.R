#' @importFrom magrittr %>%



#' @export
key_googlemaps <- function() {

  keyring::key_get("googlemaps")

}



#' @export
getdata_catchments <- function() {

  sf::st_read(path_data("Geographies/Catchments/catchments.shp"))

}



#' @export
getdata_neighborhoods <- function() {

  sf::st_read(path_data("Geographies/Neighborhoods/neighborhoods.shp")) %>%
    dplyr::rename(neighborhood = name)

}



#' @export
getdata_zips_orleans <- function() {

  sf::st_read(path_data("Geographies/Zip Codes/zips_orleans.shp"))

}



#' @export
getdata_zips_split <- function() {

  sf::st_read(path_data("Geographies/Zip Codes/zips_split.shp"))

}


