


#' @importFrom magrittr %>%



#' @export
path_data <- function(x) {

  glue::glue(
    "C:/Users/",
    Sys.getenv("USERNAME"),
    "/Orleans Parish School Board/Data - Documents/",
    x
  )

}



#' @export
path_data_raw <- function(domain, filename) {

  glue::glue(path_data(domain), "/raw/", filename)

}



#' @export
path_data_clean <- function(domain, filename) {

  glue::glue(path_data(domain), "/clean/", filename)

}



#' @export
path_in <- function(x) {

  glue::glue(path_data(x), "/raw/")

}



#' @export
path_out <- function(x) {

  glue::glue(path_data(x), "/clean/")

}


