#' @importFrom magrittr %>%



#' @export
date_appstart <- function() {"2020-11-01T00:00:00Z"}



#' @export
fix_grades <- function(x, var = GRADE) {

  x %>%
    dplyr::mutate("{{ var }}" := stringr::str_remove_all({{ var }}, "[:space:]")) %>%
    dplyr::mutate("{{ var }}" := forcats::fct_relevel(
      {{ var }},
      "INF", "1YR", "2YR", "PK3", "PK4",
      "K", "1", "2", "3", "4", "5", "6", "7", "8",
      "9", "10", "11", "12"
      )
    )

}



#' @export
grades_ec <- function() {c("INF", "1YR", "2YR", "PK3", "PK4")}



#' @export
grades_next <- function() {

  tibble::tribble(
    ~grade_current, ~grade_next,
    "INF", "1YR",
    "1YR", "2YR",
    "2YR", "PK3",
    "PK3", "PK4",
    "PK4", "K",
    "K", "1",
    "1", "2",
    "2", "3",
    "3", "4",
    "4", "5",
    "5", "6",
    "6", "7",
    "7", "8",
    "8", "9",
    "9", "10",
    "10", "11",
    "11", "12",
    "12", "13"
  )

}



#' @export
grades_text_numeric <- function() {

  tibble::tribble(
    ~grade, ~grade_numeric,
    "INF", -5,
    "1YR", -4,
    "2YR", -3,
    "PK3", -2,
    "PK4", -1,
    "K", 0,
    "1", 1,
    "2", 2,
    "3", 3,
    "4", 4,
    "5", 5,
    "6", 6,
    "7", 7,
    "8", 8,
    "9", 9,
    "10", 10,
    "11", 11,
    "12", 12,
    "13", 13,
  )

}


