#' @importFrom magrittr %>%



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


