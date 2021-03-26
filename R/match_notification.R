#' @importFrom magrittr %>%



#' @export
match_notification <- function(match, dir_out) {

  participants <-
    match %>%
    matchcalcs_participants_all(schools_waitlist = c("323", "324", "846", "847"))

  acceptednew <-
    participants %>%
    dplyr::filter(
      (`STUDENT ID` %in% matchcalcs_acceptednew_hasguarantee(participants)$`STUDENT ID`)
      | (`STUDENT ID` %in% matchcalcs_acceptednew_noguarantee(participants)$`STUDENT ID`)
    ) %>%
    dplyr::pull(`STUDENT ID`)

  fallback <-
    participants %>%
    dplyr::filter(
      (`STUDENT ID` %in% matchcalcs_guarantee1_haschoices(participants)$`STUDENT ID`)
      | (`STUDENT ID` %in% matchcalcs_fallback_waiting(participants)$`STUDENT ID`)
      | (`STUDENT ID` %in% matchcalcs_fallback_full(participants)$`STUDENT ID`)
      | (`STUDENT ID` %in% matchcalcs_fallback_ineligible(participants)$`STUDENT ID`)
    ) %>%
    dplyr::pull(`STUDENT ID`)

  unassigned <-
    participants %>%
    dplyr::filter(
      (`STUDENT ID` %in% matchcalcs_unassigned_waiting(participants)$`STUDENT ID`)
      | (`STUDENT ID` %in% matchcalcs_unassigned_full(participants)$`STUDENT ID`)
      | (`STUDENT ID` %in% matchcalcs_unassigned_ineligible(participants)$`STUDENT ID`)
    ) %>%
    dplyr::pull(`STUDENT ID`)

  guaranteed <-
    participants %>%
    dplyr::filter(
      (`STUDENT ID` %in% matchcalcs_guarantee1_only(participants)$`STUDENT ID`)
    ) %>%
    dplyr::pull(`STUDENT ID`)

  schools_accepted <-
    match %>%
    dplyr::filter(stringr::str_length(`STUDENT ID`) == 9) %>%
    dplyr::filter(`ASSIGNMENT STATUS` == "Accepted") %>%
    dplyr::select(`STUDENT ID`, school_accepted = `CHOICE SCHOOL`) %>%
    dplyr::mutate(is_scholarship = stringr::str_detect(school_accepted, "_[NR]$"))

  participants_aug <-
    participants %>%
    dplyr::mutate(is_see = stringr::str_length(`STUDENT ID`) != 9) %>%
    dplyr::mutate(is_waiting = n_waiting > 0) %>%
    dplyr::mutate(is_ec = GRADE %in% grades_ec()) %>%
    dplyr::mutate(is_acceptednew = `STUDENT ID` %in% acceptednew) %>%
    dplyr::mutate(is_fallback = `STUDENT ID` %in% fallback) %>%
    dplyr::mutate(is_unassigned = `STUDENT ID` %in% unassigned) %>%
    dplyr::mutate(is_guaranteed = `STUDENT ID` %in% guaranteed) %>%
    dplyr::left_join(schools_accepted, by = "STUDENT ID")

  participants_lettertypes <-
    participants_aug %>%
    dplyr::mutate(lettertype = dplyr::case_when(
      is_see ~ "see",

      is_ec & is_fallback ~ "ec_fallback",
      is_ec & is_acceptednew & rank_accepted == 1 ~ "ec_topchoice",
      is_ec & is_acceptednew ~ "ec_acceptednew",
      is_ec & is_unassigned & is_waiting ~ "ec_unassigned_wl",
      is_ec & is_unassigned & (n_ineligible == n_choices) ~ "ec_unassigned_ineligible",

      is_scholarship & is_acceptednew & is_waiting ~ "acceptednew_schol_wl",
      is_scholarship & is_acceptednew ~ "acceptednew_schol",

      is_acceptednew & is_waiting ~ "acceptednew_wl",
      is_acceptednew ~ "acceptednew",

      is_fallback & is_waiting ~ "fallback_wl",
      is_fallback ~ "fallback",

      is_unassigned & is_waiting ~ "unassigned_wl",
      is_unassigned ~ "unassigned",

      is_guaranteed ~ "guaranteed",

      TRUE ~ "other"
    )
    ) %>%
    dplyr::mutate(across(lettertype, ~ forcats::fct_relevel(., c("guaranteed", "see"), after = Inf))) %>%
    dplyr::relocate(lettertype, .after = `STUDENT ID`)



  participants_lettertypes %>%
    readr::write_excel_csv(glue::glue("{dir_out}/notifications.csv"), na = "")

  invisible(participants_lettertypes)



}


