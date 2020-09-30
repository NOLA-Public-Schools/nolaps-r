#' @importFrom magrittr %>%



#' @export
matchcalcs_participants <- function(x) {

  x %>%
    dplyr::distinct(`STUDENT ID`, GRADE)

}



#' @export
matchcalcs_participants_accepted <- function(x) {

  x %>%
    dplyr::filter(`ASSIGNMENT STATUS` == 'Accepted') %>%
    dplyr::select(`STUDENT ID`, rank_accepted = `CHOICE RANK`)

}



#' @export
matchcalcs_participants_guaranteed <- function(x) {

  x %>%
    dplyr::filter(`GUARANTEED?` == 'YES') %>%
    dplyr::select(`STUDENT ID`, rank_guaranteed = `CHOICE RANK`)

}



#' @export
matchcalcs_participants_n_choices <- function(x) {

  x %>%
    dplyr::count(`STUDENT ID`, name = 'n_choices')

}



#' @export
matchcalcs_participants_n_eligibleprocessed <- function(x) {

  x %>%
    dplyr::filter(
      `ASSIGNMENT STATUS` != 'Ineligible',
      `ASSIGNMENT STATUS` != 'Not Processed'
    ) %>%
    dplyr::count(`STUDENT ID`, name = 'n_eligibleprocessed')

}



#' @export
matchcalcs_participants_n_ineligible <- function(x) {

  x %>%
    dplyr::filter(`ASSIGNMENT STATUS` == 'Ineligible') %>%
    dplyr::count(`STUDENT ID`, name = 'n_ineligible')

}



#' @export
matchcalcs_participants_all <- function(x) {

  participants <- matchcalcs_participants(x)

  participants %>%
    dplyr::left_join(matchcalcs_participants_accepted(x), by = 'STUDENT ID') %>%
    dplyr::left_join(matchcalcs_participants_guaranteed(x), by = 'STUDENT ID') %>%
    dplyr::left_join(matchcalcs_participants_n_choices(x), by = 'STUDENT ID') %>%
    dplyr::left_join(matchcalcs_participants_n_eligibleprocessed(x), by = 'STUDENT ID') %>%
    dplyr::left_join(matchcalcs_participants_n_ineligible(x), by = 'STUDENT ID')

}



#' @export
matchcalcs_results_seekingneweligible <- function(x, ...) {

  x %>%
    dplyr::filter(is.na(`GUARANTEED?`)) %>%
    dplyr::filter(`ASSIGNMENT STATUS` != 'Ineligible') %>%
    dplyr::filter(`ASSIGNMENT STATUS` != 'Not Processed') %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      n_seekingneweligible = length(unique(`STUDENT ID`)),
      n_seekingneweligible_accepted = sum(`ASSIGNMENT STATUS` == 'Accepted')
    ) %>%
    dplyr::mutate(rate_accepted = n_seekingneweligible_accepted / n_seekingneweligible)

}



#' @export
matchcalcs_results_availability <- function(x, ...) {

  x %>%
    dplyr::select(
      ..., `CHOICE SCHOOL`, GRADE,
      `NUMBER OF SEATS OFFERED`,
      `NUMBER OF GUARANTEED ASSIGNED`,
      `TOTAL SEATS AVAILABLE AFTER GUARANTEED ASSIGNMENT`,
      `TOTAL NUMBER ASSIGNED`
    ) %>%
    dplyr::distinct()

}



#' @export
matchcalcs_results_school_grade <- function(x, ...) {

  availability <-
    x %>%
    matchcalcs_results_availability(...)

  seekingneweligible <-
    x %>%
    matchcalcs_results_seekingneweligible(..., `CHOICE SCHOOL`, GRADE)

  availability %>% dplyr::left_join(seekingneweligible)

}


