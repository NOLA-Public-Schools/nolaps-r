


#' @export
getdata_zips_orleans <- function() {

  sf::st_read(path_data_clean("Geographies", "zips_orleans.shp"))

}



#' @export
getdata_zips_split <- function() {

  sf::st_read(path_data_clean("Geographies", "zips_split.shp"))

}


