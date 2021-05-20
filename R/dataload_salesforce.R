#' @importFrom magrittr %>%



#' @export
dataload_value <- function(args = commandArgs(trailingOnly = TRUE)) {

  object_name <- args[1]
  id <- args[2]
  field <- args[3]
  value <- args[4]

  input_data <- tibble::tibble(
    Id = id,
    !!field := value
  )

  print(object_name)

  print(input_data)

  result <-
    salesforcer::sf_update(
      input_data = input_data,
      object_name = object_name
    )

  print(result)

}
