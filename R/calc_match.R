#' @importFrom magrittr %>%



#' @export
levelgrades <- function(x, var = GRADE) {

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



#' @export
matchcalcs_participants <- function(x, ...) {

  x %>% dplyr::distinct(`STUDENT ID`, GRADE, ...)

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
matchcalcs_participants_n_full <- function(x, schools_waitlist, ...) {

  x %>%
    dplyr::filter(`ASSIGNMENT STATUS` %in% c(
      'Waiting List',
      'Waiting List - Family Link Rejection'
    )
    ) %>%
    dplyr::filter(!(GRADE %in% c('INF', '1YR', '2YR', 'PK3', 'PK4'))) %>%
    dplyr::filter(!(`CHOICE SCHOOL` %in% schools_waitlist)) %>%
    dplyr::count(`STUDENT ID`, ..., name = 'n_full')

}



#' @export
matchcalcs_participants_n_waiting <- function(x, schools_waitlist, ...) {

  x %>%
    dplyr::filter(`ASSIGNMENT STATUS` %in% c(
      'Waiting List',
      'Waiting List - Family Link Rejection'
      )
    ) %>%
    dplyr::filter(
      GRADE %in% c('INF', '1YR', '2YR', 'PK3', 'PK4') |
        `CHOICE SCHOOL` %in% schools_waitlist
    ) %>%
    dplyr::count(`STUDENT ID`, ..., name = 'n_waiting')

}



#' @export
matchcalcs_participants_all <- function(x, schools_waitlist, ...) {

  x %>%
    matchcalcs_participants(...) %>%
    dplyr::left_join(matchcalcs_participants_n_choices(x, ...)) %>%
    dplyr::left_join(matchcalcs_participants_accepted(x, ...)) %>%
    dplyr::left_join(matchcalcs_participants_guaranteed(x, ...)) %>%
    dplyr::left_join(matchcalcs_participants_n_waiting(x, schools_waitlist, ...)) %>%
    dplyr::left_join(matchcalcs_participants_n_full(x, schools_waitlist, ...)) %>%
    dplyr::left_join(matchcalcs_participants_n_ineligible(x, ...)) %>%
    dplyr::left_join(matchcalcs_participants_n_eligibleprocessed(x, ...)) %>%
    tidyr::replace_na(list(
      n_waiting = 0,
      n_full = 0,
      n_ineligible = 0,
      n_eligibleprocessed = 0
      )
    )

}



# Match scenarios ---------------------------------------------------------



#' @export
matchcalcs_guarantee1_only <- function(x) {

  x %>%
    filter(
      rank_accepted == 1,
      rank_guaranteed == 1,
      n_choices == 1
    )

}



#' @export
matchcalcs_guarantee1_haschoices <- function(x) {

  x %>%
    filter(
      rank_accepted == 1,
      rank_guaranteed == 1,
      n_choices > 1
    )

}



#' @export
matchcalcs_acceptednew_hasguarantee <- function(x) {

  x %>%
    filter(
      !is.na(rank_accepted),
      !is.na(rank_guaranteed),
      rank_accepted < rank_guaranteed
    )

}



#' @export
matchcalcs_acceptednew_noguarantee <- function(x) {

  x %>%
    filter(
      !is.na(rank_accepted),
      is.na(rank_guaranteed)
    )

}



#' @export
matchcalcs_fallback_waiting <- function(x) {

  x %>%
    filter(
      n_choices > 1,
      rank_accepted > 1,
      rank_accepted == rank_guaranteed,
      n_waiting > 0
    )

}



#' @export
matchcalcs_fallback_full <- function(x) {

  x %>%
    filter(
      n_choices > 1,
      rank_accepted > 1,
      rank_accepted == rank_guaranteed,
      n_waiting == 0,
      n_full > 0
    )

}



#' @export
matchcalcs_fallback_ineligible <- function(x) {

  x %>%
    filter(
      n_choices > 1,
      rank_accepted > 1,
      rank_accepted == rank_guaranteed,
      n_waiting == 0,
      n_full == 0,
      n_ineligible > 0
    )

}



#' @export
matchcalcs_unassigned_waiting <- function(x) {

  x %>%
    filter(
      is.na(rank_accepted),
      n_waiting > 0
    )

}



#' @export
matchcalcs_unassigned_full <- function(x) {

  x %>%
    filter(
      is.na(rank_accepted),
      n_waiting == 0,
      n_full > 0
    )

}



#' @export
matchcalcs_unassigned_ineligible <- function(x) {

  x %>%
    filter(
      is.na(rank_accepted),
      n_waiting == 0,
      n_full == 0,
      n_ineligible == n_choices
    )

}



# Summary stats -----------------------------------------------------------


#' @export
matchcalcs_summary_stats <- function(x, ...) {

  dplyr::count(matchcalcs_guarantee1_only(x), ..., name = "n_guarantee1_only") %>%
    dplyr::left_join(dplyr::count(matchcalcs_guarantee1_haschoices(x), ..., name = "n_guarantee1_haschoices")) %>%
    dplyr::left_join(dplyr::count(matchcalcs_acceptednew_hasguarantee(x), ..., name = "n_acceptednew_hasguarantee")) %>%
    dplyr::left_join(dplyr::count(matchcalcs_acceptednew_noguarantee(x), ..., name = "n_acceptednew_noguarantee")) %>%
    dplyr::left_join(dplyr::count(matchcalcs_fallback_waiting(x), ..., name = "n_fallback_waiting")) %>%
    dplyr::left_join(dplyr::count(matchcalcs_fallback_full(x), ..., name = "n_fallback_full")) %>%
    dplyr::left_join(dplyr::count(matchcalcs_fallback_ineligible(x), ..., name = "n_fallback_ineligible")) %>%
    dplyr::left_join(dplyr::count(matchcalcs_unassigned_waiting(x), ..., name = "n_unassigned_waiting")) %>%
    dplyr::left_join(dplyr::count(matchcalcs_unassigned_full(x), ..., name = "n_unassigned_full")) %>%
    dplyr::left_join(dplyr::count(matchcalcs_unassigned_ineligible(x), ..., name = "n_unassigned_ineligible")) %>%
    dplyr::mutate(dplyr::across(c(n_guarantee1_only:n_unassigned_ineligible), ~ tidyr::replace_na(., 0)))

}



# Match rates -------------------------------------------------------------



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


