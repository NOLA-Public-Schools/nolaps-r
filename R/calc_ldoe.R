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


#' @export
calc_proficiency_diff <- function(x, ..., digits = 0) {
  x |>
    calc_proficiency(...) |>
    group_by(...) |>
    arrange(..., school_year) |>
    mutate(
      diff_mastery = rate_mastery_plus - lag(rate_mastery_plus, n = 1)
    ) |>
    mutate(
      diff_basic = rate_basic_plus - lag(rate_basic_plus, n = 1)
    ) |>
    mutate(across(
      starts_with("rate_") | starts_with("diff_"),
      \(x) round(x, digits = digits)
    ))
}


#' @export
count_enrollment <- function(x, ..., date) {
  x |>
    filter_enrolled_on(date) |>
    filter_enrolled_days() |>
    group_by(...) |>
    distinct(.data$StudentIdNum) |>
    summarize(n_enrolled = n())
}


#' @export
count_enrollment_cum <- function(x, ..., level) {
  x |>
    filter_enrolled_cum(level) |>
    group_by(...) |>
    distinct(.data$StudentIdNum) |>
    summarize(n_enrolled = n())
}


# Absenteeism and Attendance ----------------------------------------------



#' @export
prop_absent <- function(x, ..., days = 15, level) {
  x %>%
    filter_enrolled_cum(level) %>%
    filter_grades_exclude() %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      denom_absent = dplyr::n(),
      num_absent = sum(AggrDaysAbsCnt >= days),
      prop_absent = num_absent / denom_absent
    )
}



# Discipline --------------------------------------------------------------



#' @export
count_discipline <- function(x, ...) {
  x %>%
    dplyr::group_by(...) %>%
    dplyr::distinct(StudentIdNum, ActionInterventionCd) %>%
    dplyr::summarize(
      num_suspensions_out = sum(ActionInterventionCd == "002"),
      num_expulsions_out = sum(ActionInterventionCd == "003"),
      num_suspensions_in = sum(ActionInterventionCd == "004")
    )
}



#' @export
prop_discipline <- function(table_num, table_denom, ..., level) {
  denoms <-
    table_denom %>%
    count_enrollment_cum(..., level = level)

  nums <-
    table_num %>%
    count_discipline(...)

  denoms %>%
    left_join(nums) %>%
    mutate(
      rate_susp_out = num_suspensions_out / num_enrolled_cum
    )
}



# Graduation --------------------------------------------------------------



#' @export
prop_graduation <- function(x, ...) {
  x %>%
    dplyr::filter(`Site Flag` == "Y") %>%
    dplyr::filter(!is.na(`Grad Point`)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      denom_graduated = dplyr::n(),
      num_graduated = sum(`Grad Flag` == "Y"),
      prop_graduated = num_graduated / denom_graduated
    )
}



# Assessment --------------------------------------------------------------







#' @export
prop_mastery <- function(x, ...) {
  x %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      denom_basic = sum(!is.na(ach)),
      denom_mastery = sum(!is.na(ach)),
      num_basic = sum(ach %in% c("ADV", "EST", "EXC", "GOO", "MAS", "MST", "BAS")),
      num_mastery = sum(ach %in% c("ADV", "EST", "EXC", "GOO", "MAS", "MST")),
      prop_basic = num_basic / denom_basic,
      prop_mastery = num_mastery / denom_mastery
    )
}
