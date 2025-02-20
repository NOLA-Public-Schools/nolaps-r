#' @export
date_appstart <- function() {
  "2024-11-01T00:00:00Z"
}


#' @export
date_appstart_3year <- function() {
  "2021-11-01T00:00:00Z"
}


#' @export
date_currentyear <- function() {
  "2024-2025"
}


#' @export
term_current <- function() {
  "2024-2025"
}


#' @export
term_next <- function() {
  "2025-2026"
}


#' @export
fix_grades <- function(x, var = GRADE) {
  x |>
    mutate("{{ var }}" := str_remove_all({{ var }}, "[:space:]")) %>%
    mutate("{{ var }}" := factor(
      {{ var }},
      levels = c(
        "UB", "INFSPED", "INF", "1YR", "2YR", "PK3", "PK4", "PKSPED", "PK",
        "K", "1", "2", "3", "4", "5", "6", "7", "8",
        "T9",
        "9", "10", "11", "12",
        "Guarantee",
        "Other"
      ), ordered = TRUE
    ))
}


#' @export
grades <- function() {
  c(
    "INF", "1YR", "2YR", "PK3", "PK4",
    "K", "1", "2", "3", "4", "5", "6", "7", "8",
    "9", "10", "11", "12"
  )
}


#' @export
grades_ec <- function() {
  c("INF", "1YR", "2YR", "PK3", "PK4")
}


#' @export
grades_k8 <- function() {
  c("K", "1", "2", "3", "4", "5", "6", "7", "8")
}


#' @export
grades_hs <- function() {
  c("T9", "9", "10", "11", "12")
}


#' @export
grades_k12 <- function() {
  c(grades_k8(), grades_hs())
}


#' @export
grades_inf8 <- function() {
  c(grades_ec(), grades_k8())
}


#' @export
grades_code_to_normal <- function() {
  tribble(
    ~grade_code, ~grade_normal,
    "15", "INFSPED",
    "20", "PKSPED",
    "24", "PK",
    "25", "K",
    "01", "1",
    "02", "2",
    "03", "3",
    "04", "4",
    "05", "5",
    "06", "6",
    "07", "7",
    "08", "8",
    "T9", "T9",
    "09", "9",
    "10", "10",
    "11", "11",
    "12", "12",
    "35", "Other"
  )
}


#' @export
grades_next <- function() {
  tribble(
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
grades_previous <- function() {
  tribble(
    ~grade_current, ~grade_previous,
    "1YR", "INF",
    "2YR", "1YR",
    "PK3", "2YR",
    "PK4", "PK3",
    "K", "PK4",
    "1", "K",
    "2", "1",
    "3", "2",
    "4", "3",
    "5", "4",
    "6", "5",
    "7", "6",
    "8", "7",
    "9", "8",
    "10", "9",
    "11", "10",
    "12", "11"
  )
}


#' @export
grades_text_numeric <- function() {
  tribble(
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


#' @export
fix_names <- function(x) {
  x |>
    str_replace_all("/", " ") |>
    str_replace_all(":", " ") |>
    str_replace_all("#", " ") |>
    str_remove("\\(DO NOT PLACE\\)") |>
    str_remove("DO NOT PLACE- ") |>
    str_remove("DO NOT PLACE - ") |>
    str_remove("DO NOT PLACE ") |>
    str_squish()
}


#' @export
fix_grades_vec <- function(x) {
  x |>
    str_remove_all("[:space:]") |>
    factor(
      levels = c(
        "UB",
        "INFSPED",
        "INF",
        "1YR",
        "2YR",
        "PK3",
        "PK4",
        "PKSPED",
        "PK",
        "K",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "T9",
        "9",
        "10",
        "11",
        "12",
        "Guarantee",
        "Other"
      ),
      ordered = TRUE
    )
}
