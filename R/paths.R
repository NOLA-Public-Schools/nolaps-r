


#' @export
path_data <- function(x) {

  glue::glue(
    "C:/Users/",
    Sys.getenv("USERNAME"),
    "/Orleans Parish School Board/Data - Documents/",
    x
  )

}


