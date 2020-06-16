#' @importFrom magrittr %>%



#' @export
getparticipants_all <- function(x) {

  x %>%
    dplyr::distinct(`STUDENT ID`, GRADE)

}



#' @export
getparticipants_accepted <- function(x) {

  x %>%
    dplyr::filter(`ASSIGNMENT STATUS` == 'Accepted') %>%
    dplyr::select(`STUDENT ID`, rank_accepted = `CHOICE RANK`)

}



#' @export
getparticipants_guaranteed <- function(x) {

  x %>%
    dplyr::filter(`GUARANTEED?` == 'YES') %>%
    dplyr::select(`STUDENT ID`, rank_guaranteed = `CHOICE RANK`)

}



#' @export
count_choices <- function(x) {

  x %>%
    dplyr::count(`STUDENT ID`, name = 'n_choices')

}



#' @export
count_ineligible <- function(x) {

  x %>%
    dplyr::filter(`ASSIGNMENT STATUS` == 'Ineligible') %>%
    dplyr::count(`STUDENT ID`, name = 'n_ineligible')

}



#' @export
getparticipants_results <- function(x) {

  participants <- getparticipants_all(x)

  participants %>%
    dplyr::left_join(getparticipants_accepted(x), by = 'STUDENT ID') %>%
    dplyr::left_join(getparticipants_guaranteed(x), by = 'STUDENT ID') %>%
    dplyr::left_join(count_choices(x), by = 'STUDENT ID') %>%
    dplyr::left_join(count_ineligible(x), by = 'STUDENT ID')

}



#' @export
getresults_school_grade <- function(x, ...) {

  available <-
    x %>%
    dplyr::select(
      ..., `CHOICE SCHOOL`, GRADE,
      `NUMBER OF SEATS OFFERED`,
      `NUMBER OF GUARANTEED ASSIGNED`,
      `TOTAL SEATS AVAILABLE AFTER GUARANTEED ASSIGNMENT`
    ) %>%
    dplyr::distinct()

  accepted <-
    x %>%
    dplyr::filter(is.na(`GUARANTEED?`)) %>%
    dplyr::filter(`ASSIGNMENT STATUS` != 'Ineligible') %>%
    dplyr::filter(`ASSIGNMENT STATUS` != 'Not Processed') %>%
    dplyr::group_by(
      ..., `CHOICE SCHOOL`, GRADE
    ) %>%
    dplyr::summarize(
      n_seekingnew_all = dplyr::n(),
      n_seekingnew_accepted = sum(`ASSIGNMENT STATUS` == 'Accepted'),
      n_seekingnew_accepted_1 = sum(`ASSIGNMENT STATUS` == 'Accepted' & `CHOICE RANK` == 1),
      prop_accepted = n_seekingnew_accepted / n_seekingnew_all
    )

  available %>%
    dplyr::left_join(accepted)

}




