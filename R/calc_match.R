#' @importFrom magrittr %>%



#' @export
levelgrades_match <- function(x) {

  x %>%
    mutate(GRADE = forcats::fct_relevel(
      GRADE,
      "INF", "1YR", "2YR", "PK3", "PK4", "K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"
      )
    )

}



#' @export
matchcalcs_participants <- function(x, ...) {

  x %>% dplyr::distinct(`STUDENT ID`, ...)

}



#' @export
matchcalcs_participants_accepted <- function(x, ...) {

  x %>%
    dplyr::filter(`ASSIGNMENT STATUS` == 'Accepted') %>%
    dplyr::select(`STUDENT ID`, rank_accepted = `CHOICE RANK`, ...)

}



#' @export
matchcalcs_participants_guaranteed <- function(x, ...) {

  x %>%
    dplyr::filter(`GUARANTEED?` == 'YES') %>%
    dplyr::select(`STUDENT ID`, rank_guaranteed = `CHOICE RANK`, ...)

}



#' @export
matchcalcs_participants_n_choices <- function(x, ...) {

  x %>% dplyr::count(`STUDENT ID`, ..., name = 'n_choices')

}



#' @export
matchcalcs_participants_n_eligibleprocessed <- function(x, ...) {

  x %>%
    dplyr::filter(
      `ASSIGNMENT STATUS` != 'Ineligible',
      `ASSIGNMENT STATUS` != 'Not Processed'
    ) %>%
    dplyr::count(`STUDENT ID`, ..., name = 'n_eligibleprocessed')

}



#' @export
matchcalcs_participants_n_ineligible <- function(x, ...) {

  x %>%
    dplyr::filter(`ASSIGNMENT STATUS` == 'Ineligible') %>%
    dplyr::count(`STUDENT ID`, ..., name = 'n_ineligible')

}



#' @export
matchcalcs_participants_all <- function(x, ...) {

  x %>%
    matchcalcs_participants(...) %>%
    dplyr::left_join(matchcalcs_participants_accepted(x, ...)) %>%
    dplyr::left_join(matchcalcs_participants_guaranteed(x, ...)) %>%
    dplyr::left_join(matchcalcs_participants_n_choices(x, ...)) %>%
    dplyr::left_join(matchcalcs_participants_n_eligibleprocessed(x, ...)) %>%
    dplyr::left_join(matchcalcs_participants_n_ineligible(x, ...)) %>%
    tidyr::replace_na(list(n_eligibleprocessed = 0, n_ineligible = 0))

}



#' @export
matchcalcs_seekingnew <- function(x, ...) {

  x %>%
    matchcalcs_participants_all(...) %>%
    filter(is.na(rank_guaranteed) | rank_guaranteed != 1)

}



#' @export
matchcalcs_results_seekingnew <- function(
  x,
  ...,
  exclude_ineligible = TRUE,
  exclude_notprocessed = TRUE
  ) {

  if (exclude_ineligible == TRUE) {
    x <- x %>% dplyr::filter(`ASSIGNMENT STATUS` != 'Ineligible')
  }

  if (exclude_notprocessed == TRUE) {
    x <- x %>% dplyr::filter(`ASSIGNMENT STATUS` != 'Not Processed')
  }

  x %>%
    dplyr::semi_join(matchcalcs_seekingnew(x, ...)) %>%
    dplyr::filter(is.na(`GUARANTEED?`)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      n_seekingnew = length(unique(`STUDENT ID`)),
      n_seekingnew_rank1 = sum(`CHOICE RANK` == 1),
      n_seekingnew_acceptednew =
        sum(`ASSIGNMENT STATUS` == 'Accepted'),
      n_seekingnew_acceptednew_top3 =
        sum(`ASSIGNMENT STATUS` == 'Accepted' & `CHOICE RANK` %in% 1:3),
      n_seekingnew_acceptednew_top1 =
        sum(`ASSIGNMENT STATUS` == 'Accepted' & `CHOICE RANK` == 1),
      n_seekingnew_acceptednew_sibling =
        sum(`ASSIGNMENT STATUS` == 'Accepted' & str_detect(`QUALIFIED PRIORITIES`, "Sibling"), na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      rate_acceptednew = n_seekingnew_acceptednew / n_seekingnew,
      rate_acceptednew_top3 = n_seekingnew_acceptednew_top3 / n_seekingnew,
      rate_acceptednew_top1 = n_seekingnew_acceptednew_top1 / n_seekingnew,
      rate_acceptednew_sibling = n_seekingnew_acceptednew_sibling / n_seekingnew
    )

}


