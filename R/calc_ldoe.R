#' @importFrom magrittr %>%



# Utility -----------------------------------------------------------------



#' @export
filter_enrolled_on <- function(x, date = "10-01") {

  if (date == "02-01") {

    x %>%
      dplyr::filter(EntryDt <= stringr::str_c(BegSchSessYr + 1, "-", date)) %>%
      dplyr::filter(is.na(ExitDt) | ExitDt > stringr::str_c(BegSchSessYr + 1, "-", date))

  } else {

    x %>%
      dplyr::filter(EntryDt <= stringr::str_c(BegSchSessYr, "-", date)) %>%
      dplyr::filter(is.na(ExitDt) | ExitDt > stringr::str_c(BegSchSessYr, "-", date))

  }

}



#' @export
filter_enrolled_days <- function(x, n_days = 1) {

  x %>% dplyr::filter(AggrDaysEnrlCnt >= n_days)

}



#' @export
filter_enrolled_cum <- function(x, level) {

  if (level == "state") {

    x %>% dplyr::filter(StateCumEnrlFlg == "Y")

  } else if (level == "lea") {

    x %>% dplyr::filter(LeaCumEnrlFlg == "Y")

  } else {

    x %>% dplyr::filter(SiteCumEnrlFlg == "Y")

  }

}



#' @export
filter_grades_exclude <- function(x, grades = c("15", "20")) {

  x %>% dplyr::filter(!(GradePlacementCd %in% grades))

}



# Enrollment --------------------------------------------------------------



#' @export
count_enrollment <- function(x, ..., date) {

  x %>%
    filter_enrolled_on(date) %>%
    filter_enrolled_days() %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(num_enrolled = dplyr::n())

}



#' @export
count_enrollment_cum <- function(x, ..., level) {

  x %>%
    filter_enrolled_cum(level) %>%
    dplyr::group_by(...) %>%
    dplyr::distinct(StudentIdNum) %>%
    dplyr::summarize(num_enrolled_cum = dplyr::n())

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
calc_proficiency <- function(x, ...) {
  x %>%
    dplyr::group_by(school_year, ...) %>%
    dplyr::summarize(
      n_students = dplyr::n(),
      n_mastery_plus = sum(
        achievement %in% c("Advanced", "Mastery"),
        na.rm = TRUE
      ),
      n_basic_plus = sum(
        achievement %in% c("Advanced", "Mastery", "Basic"),
        na.rm = TRUE
      ),
      rate_mastery_plus = n_mastery_plus / n_students,
      rate_basic_plus = n_basic_plus / n_students
    )
}



#' @export
calc_proficiency_diff <- function(x, ...) {
  x %>%
    calc_proficiency(...) %>%
    dplyr::group_by(...) %>%
    dplyr::arrange(..., school_year) %>%
    dplyr::mutate(
      diff_mastery = rate_mastery_plus - dplyr::lag(rate_mastery_plus, n = 1)
    ) %>%
    dplyr::mutate(
      diff_basic = rate_basic_plus - dplyr::lag(rate_basic_plus, n = 1)
    ) %>%
    dplyr::mutate(dplyr::across(
      tidyselect::starts_with("rate_") | tidyselect::starts_with("diff_"),
      ~ round(., digits = 3))
    )
}



#' @export
prop_mastery <- function(x, ...) {

  x %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      denom_basic = sum(!is.na(ach)),
      denom_mastery = sum(!is.na(ach)),
      num_basic =   sum(ach %in% c("ADV", "EST", "EXC", "GOO", "MAS", "MST", "BAS")),
      num_mastery = sum(ach %in% c("ADV", "EST", "EXC", "GOO", "MAS", "MST")),
      prop_basic = num_basic / denom_basic,
      prop_mastery = num_mastery / denom_mastery
    )

}


