


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
path_in <- function(x) {

  glue::glue(path_data(x), "/raw/")

}



#' @export
path_out <- function(x) {

  glue::glue(path_data(x), "/clean/")

}


