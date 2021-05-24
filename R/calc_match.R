#' @importFrom magrittr %>%



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
matchcalcs_participants_sibling <- function(x, ...) {

  x %>%
    dplyr::filter(stringr::str_detect(`QUALIFIED PRIORITIES`, "Sibling")) %>%
    dplyr::distinct(`STUDENT ID`, ...)

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
    dplyr::filter(
      rank_accepted == 1,
      rank_guaranteed == 1,
      n_choices == 1
    )

}



#' @export
matchcalcs_guarantee1_haschoices <- function(x) {

  x %>%
    dplyr::filter(
      rank_accepted == 1,
      rank_guaranteed == 1,
      n_choices > 1
    )

}



#' @export
matchcalcs_accepted_belowguarantee <- function(x) {

  x %>%
    dplyr::filter(
      !is.na(rank_accepted),
      !is.na(rank_guaranteed),
      rank_accepted > rank_guaranteed
    )

}



#' @export
matchcalcs_acceptednew_hasguarantee <- function(x) {

  x %>%
    dplyr::filter(
      !is.na(rank_accepted),
      !is.na(rank_guaranteed),
      rank_accepted < rank_guaranteed
    )

}



#' @export
matchcalcs_acceptednew_noguarantee <- function(x) {

  x %>%
    dplyr::filter(
      !is.na(rank_accepted),
      is.na(rank_guaranteed)
    )

}



#' @export
matchcalcs_fallback_waiting <- function(x) {

  x %>%
    dplyr::filter(
      n_choices > 1,
      rank_accepted > 1,
      rank_accepted == rank_guaranteed,
      n_waiting > 0
    )

}



#' @export
matchcalcs_fallback_full <- function(x) {

  x %>%
    dplyr::filter(
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
    dplyr::filter(
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
    dplyr::filter(
      is.na(rank_accepted),
      n_waiting > 0
    )

}



#' @export
matchcalcs_unassigned_full <- function(x) {

  x %>%
    dplyr::filter(
      is.na(rank_accepted),
      n_waiting == 0,
      n_full > 0
    )

}



#' @export
matchcalcs_unassigned_ineligible <- function(x) {

  x %>%
    dplyr::filter(
      is.na(rank_accepted),
      n_waiting == 0,
      n_full == 0,
      n_ineligible == n_choices
    )

}



# Summary stats -----------------------------------------------------------



#' @export
matchcalcs_summarystats <- function(x, ...) {

  dplyr::count(matchcalcs_guarantee1_only(x), ..., name = "n_guarantee1_only") %>%
    dplyr::full_join(dplyr::count(matchcalcs_guarantee1_haschoices(x), ..., name = "n_guarantee1_haschoices")) %>%
    dplyr::full_join(dplyr::count(matchcalcs_accepted_belowguarantee(x), ..., name = "n_accepted_belowguarantee")) %>%
    dplyr::full_join(dplyr::count(matchcalcs_acceptednew_hasguarantee(x), ..., name = "n_acceptednew_hasguarantee")) %>%
    dplyr::full_join(dplyr::count(matchcalcs_acceptednew_noguarantee(x), ..., name = "n_acceptednew_noguarantee")) %>%
    dplyr::full_join(dplyr::count(matchcalcs_fallback_waiting(x), ..., name = "n_fallback_waiting")) %>%
    dplyr::full_join(dplyr::count(matchcalcs_fallback_full(x), ..., name = "n_fallback_full")) %>%
    dplyr::full_join(dplyr::count(matchcalcs_fallback_ineligible(x), ..., name = "n_fallback_ineligible")) %>%
    dplyr::full_join(dplyr::count(matchcalcs_unassigned_waiting(x), ..., name = "n_unassigned_waiting")) %>%
    dplyr::full_join(dplyr::count(matchcalcs_unassigned_full(x), ..., name = "n_unassigned_full")) %>%
    dplyr::full_join(dplyr::count(matchcalcs_unassigned_ineligible(x), ..., name = "n_unassigned_ineligible")) %>%
    dplyr::mutate(dplyr::across(c(n_guarantee1_only:n_unassigned_ineligible), ~ tidyr::replace_na(., 0)))

}



#' @export
matchcalcs_summarycols <- function(x) {

  x %>%
    dplyr::mutate(
      assigned = n_acceptednew_hasguarantee + n_acceptednew_noguarantee,
      fallback_not_ineligible = n_fallback_waiting + n_fallback_full,
      unassigned = n_unassigned_waiting + n_unassigned_full + n_unassigned_ineligible,
      subtotal = assigned + fallback_not_ineligible + unassigned,
      prop_assigned = assigned / subtotal,
      prop_fallback_not_ineligible = fallback_not_ineligible / subtotal,
      prop_unassigned = unassigned / subtotal
    ) %>%
    dplyr::select(
      GRADE,
      assigned,
      fallback_not_ineligible,
      unassigned,
      subtotal,
      prop_assigned,
      prop_fallback_not_ineligible,
      prop_unassigned,
      tidyselect::everything()
    )

}



#' @export
matchcalcs_summaryrow <- function(x, row_name) {

  x %>%
    dplyr::select(n_guarantee1_only:n_unassigned_ineligible) %>%
    colSums() %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(GRADE = row_name) %>%
    matchcalcs_summarycols()

}



#' @export
matchcalcs_summarystats_full <- function(x, schools_waitlist = c("323", "324", "846", "847")) {

  with_bin_grade <-
    x %>%
    matchcalcs_participants_all(schools_waitlist) %>%
    matchcalcs_summarystats(GRADE) %>%
    fix_grades() %>%
    dplyr::arrange(GRADE) %>%
    dplyr::mutate(
      is_inf_2yr = GRADE %in% c("INF", "1YR", "2YR"),
      is_pk3_pk4 = GRADE %in% c("PK3", "PK4"),
      is_k_8 = GRADE %in% c("K", "1", "2", "3", "4", "5", "6", "7", "8"),
      is_9_12 = GRADE %in% c("9", "10", "11", "12"),
      is_k_12 = GRADE %in% c("K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
      is_pk4_12 = GRADE %in% c("PK4", "K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
    )

  summary_inf_2yr <-
    with_bin_grade %>%
    dplyr::filter(is_inf_2yr) %>%
    matchcalcs_summaryrow("Grades INF - 2YR")

  summary_pk3_pk4 <-
    with_bin_grade %>%
    dplyr::filter(is_pk3_pk4) %>%
    matchcalcs_summaryrow("Grades PK3 - PK4")

  summary_k_8 <-
    with_bin_grade %>%
    dplyr::filter(is_k_8) %>%
    matchcalcs_summaryrow("Grades K - 8")

  summary_9_12 <-
    with_bin_grade %>%
    dplyr::filter(is_9_12) %>%
    matchcalcs_summaryrow("Grades 9 - 12")

  summary_k_12 <-
    with_bin_grade %>%
    dplyr::filter(is_k_12) %>%
    matchcalcs_summaryrow("Grades K - 12")

  summary_pk4_12 <-
    with_bin_grade %>%
    dplyr::filter(is_pk4_12) %>%
    matchcalcs_summaryrow("Grades PK4 - 12")

  summary_inf_12 <-
    with_bin_grade %>%
    matchcalcs_summaryrow("Grades INF - 12")

  summary_all <-
    with_bin_grade %>%
    dplyr::select(GRADE:n_unassigned_ineligible) %>%
    matchcalcs_summarycols()

  summarystats <-
    dplyr::bind_rows(
      summary_all,
      summary_inf_2yr,
      summary_pk3_pk4,
      summary_k_8,
      summary_9_12,
      summary_k_12,
      summary_pk4_12,
      summary_inf_12
    )

  summarystats

}



# Match rates -------------------------------------------------------------



#' @export
matchcalcs_seekingnew <- function(x, schools_waitlist, ...) {

  x %>%
    matchcalcs_participants_all(schools_waitlist, ...) %>%
    dplyr::filter(is.na(rank_guaranteed) | rank_guaranteed != 1)

}



#' @export
matchcalcs_results_seekingnew <- function(
  x,
  schools_waitlist,
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
    dplyr::semi_join(matchcalcs_seekingnew(x, schools_waitlist, ...)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      n_seekingnew = length(unique(`STUDENT ID`)),
      n_seekingnew_rank1 = sum(`CHOICE RANK` == 1),
      n_seekingnew_acceptednew =
        sum(`ASSIGNMENT STATUS` == 'Accepted' & is.na(`GUARANTEED?`)),
      n_seekingnew_acceptednew_top3 =
        sum(`ASSIGNMENT STATUS` == 'Accepted' & is.na(`GUARANTEED?`) & `CHOICE RANK` %in% 1:3),
      n_seekingnew_acceptednew_top1 =
        sum(`ASSIGNMENT STATUS` == 'Accepted' & is.na(`GUARANTEED?`) & `CHOICE RANK` == 1),
      n_seekingnew_fallback =
        sum(`ASSIGNMENT STATUS` == 'Accepted' & !is.na(`GUARANTEED?`)),
      n_seekingnew_unassigned = n_seekingnew - n_seekingnew_acceptednew - n_seekingnew_fallback
    ) %>%
    dplyr::mutate(
      rate_acceptednew = n_seekingnew_acceptednew / n_seekingnew,
      rate_acceptednew_top3 = n_seekingnew_acceptednew_top3 / n_seekingnew,
      rate_acceptednew_top1 = n_seekingnew_acceptednew_top1 / n_seekingnew,
      rate_fallback = n_seekingnew_fallback / n_seekingnew,
      rate_unassigned = n_seekingnew_unassigned / n_seekingnew
    )

}



#' @export
matchcalcs_results_seekingnew_sibling <- function(
  x,
  schools_waitlist,
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
    dplyr::semi_join(matchcalcs_seekingnew(x, schools_waitlist, ...)) %>%
    dplyr::semi_join(matchcalcs_participants_sibling(x, ...)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      n_seekingnew = length(unique(`STUDENT ID`)),
      n_seekingnew_acceptednew_sibling =
        sum(`ASSIGNMENT STATUS` == 'Accepted' & stringr::str_detect(`QUALIFIED PRIORITIES`, "Sibling"), na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      rate_acceptednew_sibling = n_seekingnew_acceptednew_sibling / n_seekingnew
    )

}



#' @export
matchcalcs_results_seekingnew_unassigned <- function(
  x,
  schools_waitlist,
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

  matchcalcs_seekingnew(x, schools_waitlist, ...) %>%
    dplyr::filter(is.na(rank_accepted)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      n_seekingnew_unassigned = dplyr::n(),
      n_seekingnew_unassigned_k9 = sum(GRADE %in% c("K", "9")),
      n_seekingnew_unassigned_3orless = sum(n_choices <= 3),
    ) %>%
    dplyr::mutate(
      rate_k9 = n_seekingnew_unassigned_k9 / n_seekingnew_unassigned,
      rate_3orless = n_seekingnew_unassigned_3orless / n_seekingnew_unassigned
    )

}



# Priority summaries ------------------------------------------------------



#' @export
matchcalcs_priorityoutcomes <- function(x) {

  x %>%
    dplyr::select(choice_name, `CHOICE SCHOOL`, GRADE, `STUDENT ID`, `QUALIFIED PRIORITIES`) %>%
    dplyr::mutate(dplyr::across(`QUALIFIED PRIORITIES`, ~ stringr::str_remove(., "/$"))) %>%
    tidyr::separate_rows(`QUALIFIED PRIORITIES`, sep = "/") %>%
    dplyr::mutate(has = 1) %>%
    tidyr::pivot_wider(names_from = `QUALIFIED PRIORITIES`, values_from = has) %>%
    dplyr::select(-`NA`) %>%
    fix_grades() %>%
    dplyr::arrange(choice_name, `CHOICE SCHOOL`, GRADE, `STUDENT ID`)

}



#' @export
matchcalcs_priorityoutcomes_summary <- function(x, ...) {

  x %>%
    matchcalcs_priorityoutcomes() %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      n_applicants = dplyr::n(),
      dplyr::across(c(Guaranteed:Ineligible), list(count = ~ sum(.x, na.rm = TRUE), rate = ~ sum(.x, na.rm = TRUE) / n_applicants))
    )

}


