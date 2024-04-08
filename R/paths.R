#' @export
path_data <- function(x) {
  glue(
    "C:/Users/",
    Sys.getenv("USERNAME"),
    "/Orleans Parish School Board/Data - Documents/Clean Data/",
    x
  )
}
