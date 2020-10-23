#' @importFrom magrittr %>%



#' @export
matchcalcs_participants <- function(x) {

  x %>% dplyr::distinct(`STUDENT ID`, GRADE)

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

  x %>% dplyr::count(`STUDENT ID`, name = 'n_choices')

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
    dplyr::left_join(matchcalcs_participants_n_ineligible(x), by = 'STUDENT ID') %>%
    tidyr::replace_na(list(n_eligibleprocessed = 0, n_ineligible = 0))

}



#' @export
seekingnew <- function(x) {

  x %>%
    matchcalcs_participants_all() %>%
    filter(is.na(rank_guaranteed) | rank_guaranteed != 1)

}



#' @export
matchcalcs_results_seekingnew <- function(
  x,
  ...,
  excludechoices_ineligible = TRUE,
  excludechoices_notprocessed = TRUE
  ) {

  if (excludechoices_ineligible == TRUE) {
    x <- x %>% dplyr::filter(`ASSIGNMENT STATUS` != 'Ineligible')
  }

  if (excludechoices_notprocessed == TRUE) {
    x <- x %>% dplyr::filter(`ASSIGNMENT STATUS` != 'Not Processed')
  }

  x %>%
    dplyr::filter(`STUDENT ID` %in% seekingnew(x)$`STUDENT ID`) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      n_seekingnew = length(unique(`STUDENT ID`)),
      n_seekingnew_acceptednew =
        sum(`ASSIGNMENT STATUS` == 'Accepted' & is.na(`GUARANTEED?`)),
      n_seekingnew_acceptednew_top3 =
        sum(`ASSIGNMENT STATUS` == 'Accepted' & is.na(`GUARANTEED?`) & `CHOICE RANK` %in% 1:3)
    ) %>%
    dplyr::mutate(
      rate_acceptednew = n_seekingnew_acceptednew / n_seekingnew,
      rate_acceptednew_top3 = n_seekingnew_acceptednew_top3 / n_seekingnew
    )

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


