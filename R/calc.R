
#' @importFrom magrittr %>%



# Utility -----------------------------------------------------------------



#' @export
filter_enrolled_on <- function(x, date = '10-01') {

  if (date == '02-01') {

    x %>%
      dplyr::filter(EntryDt <= stringr::str_c(as.integer(BegSchSessYr) + 1, '-', date)) %>%
      dplyr::filter(is.na(ExitDt) | ExitDt > stringr::str_c(as.integer(BegSchSessYr) + 1, '-', date))

  } else {

    x %>%
      dplyr::filter(EntryDt <= stringr::str_c(BegSchSessYr, '-', date)) %>%
      dplyr::filter(is.na(ExitDt) | ExitDt > stringr::str_c(BegSchSessYr, '-', date))

  }

}



#' @export
filter_enrolled_days <- function(x, n_days = 1) {

  x %>%
    dplyr::filter(AggrDaysEnrlCnt >= n_days)

}



#' @export
filter_enrolled_cum <- function(x) {

  x %>%
    dplyr::filter(SiteCumEnrlFlg == 'Y')

}



# Enrollment --------------------------------------------------------------



#' @export
count_enrollment <- function(x, ...) {

  x %>%
    filter_enrolled_on() %>%
    filter_enrolled_days() %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(num_enrolled = dplyr::n())

}



#' @export
prop_absent <- function(x, ..., days = 15) {

  x %>%
    filter_enrolled_cum() %>%
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
    dplyr::summarize(
      num_suspensions_out = sum(ActionInterventionCd == '002'),
      num_expulsions_out = sum(ActionInterventionCd == '003'),
      num_suspensions_in = sum(ActionInterventionCd == '004')
    )

}



# Graduation --------------------------------------------------------------



#' @export
prop_graduation <- function(x, ...) {

  x %>%
    dplyr::filter(`Site Flag` == 'Y') %>%
    dplyr::filter(!is.na(`Grad Point`)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      denom_graduated = dplyr::n(),
      num_graduated = sum(`Grad Flag` == 'Y'),
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
      num_basic =   sum(ach %in% c('ADV', 'EST', 'EXC', 'GOO', 'MAS', 'MST', 'BAS')),
      num_mastery = sum(ach %in% c('ADV', 'EST', 'EXC', 'GOO', 'MAS', 'MST')),
      prop_basic = num_basic / denom_basic,
      prop_mastery = num_mastery / denom_mastery
    )

}


