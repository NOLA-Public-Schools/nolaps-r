#' Calculate proficiency rates
#'
#' @param x tibble of assessment data
#' @param ... grouping variables present in data
#'
#' @export
calc_proficiency <- function(x, ...) {
  x |>
    group_by(.data$school_year, ...) |>
    summarize(
      n_students = n(),
      n_mastery_plus = sum(
        .data$achievement %in% c("Advanced", "Mastery"),
        na.rm = TRUE
      ),
      n_basic_plus = sum(
        .data$achievement %in% c("Advanced", "Mastery", "Basic"),
        na.rm = TRUE
      ),
      rate_mastery_plus = .data$n_mastery_plus / .data$n_students,
      rate_basic_plus = .data$n_basic_plus / .data$n_students
    )
}


#' Calculate proficiency rates, including difference from previous year
#'
#' @param x tibble of assessment data
#' @param ... grouping variables present in data
#'
#' @param digits number of decimals for pretty printing
#'
#' @export
calc_proficiency_diff <- function(x, ..., digits = 0) {
  x |>
    calc_proficiency(...) |>
    group_by(...) |>
    arrange(..., .data$school_year) |>
    mutate(
      diff_mastery =
        .data$rate_mastery_plus - lag(.data$rate_mastery_plus, n = 1)
    ) |>
    mutate(
      diff_basic = .data$rate_basic_plus - lag(.data$rate_basic_plus, n = 1)
    ) |>
    mutate(across(
      starts_with("rate_") | starts_with("diff_"),
      \(x) round(x, digits = digits)
    ))
}
