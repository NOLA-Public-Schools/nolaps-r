#' @importFrom magrittr %>%
#' @importFrom dplyr arrange filter mutate select
#' @importFrom dplyr group_by ungroup summarize
#' @importFrom dplyr left_join
#' @importFrom dplyr n



#' @export
match_notification_waitlists <- function(match, schools_waitlist = c("323", "324", "846", "847")) {

  match %>%
    filter(!(GRADE %in% grades_ec())) %>%
    filter(`CHOICE SCHOOL` %in% schools_waitlist) %>%
    filter(`ASSIGNMENT STATUS` %in% c(
      "Waiting List",
      "Waiting List - Family Link Rejection"
      )
    ) %>%
    select(`STUDENT ID`, `CHOICE RANK`, waitlist_school = choice_name, `WAITLIST RANK`) %>%
    group_by(`STUDENT ID`) %>%
    arrange(`CHOICE RANK`) %>%
    mutate(waitlist_slot = 1:n()) %>%
    ungroup() %>%
    select(-`CHOICE RANK`) %>%
    tidyr::pivot_wider(
      names_from = waitlist_slot,
      values_from = c(waitlist_school, `WAITLIST RANK`)
    )

}



#' @export
match_notification <- function(match, overmatches, dir_out) {

  participants <-
    match %>%
    matchcalcs_participants_all(schools_waitlist = c("323", "324", "846", "847"))

  acceptednew <-
    participants %>%
    dplyr::filter(
      (`STUDENT ID` %in% matchcalcs_acceptednew_hasguarantee(participants)$`STUDENT ID`)
      | (`STUDENT ID` %in% matchcalcs_acceptednew_noguarantee(participants)$`STUDENT ID`)
      | (`STUDENT ID` %in% matchcalcs_accepted_belowguarantee(participants)$`STUDENT ID`)
    ) %>%
    dplyr::pull(`STUDENT ID`) %>%
    c(dplyr::pull(overmatches, `STUDENT ID`)) %>%
    unique()

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
    dplyr::filter(!(`STUDENT ID` %in% overmatches$`STUDENT ID`)) %>%
    dplyr::bind_rows(overmatches) %>%
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
    dplyr::mutate(is_guaranteed = !is.na(rank_guaranteed)) %>%
    dplyr::left_join(schools_accepted, by = "STUDENT ID") %>%
    dplyr::mutate(is_assigned = !is.na(school_accepted))

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
    )



  apps <- getdata_app_1year() %>% dplyr::select(-grade_applying)

  account_lookup <- match_lookup_account(match)

  accounts <- getdata_account_address()

  notifications <-
    participants_lettertypes %>%
    dplyr::rename(
      oneappid = `STUDENT ID`,
      grade_applying = GRADE
    ) %>%
    left_join(apps, by = "oneappid") %>%
    left_join(account_lookup, by = c("school_accepted" = "code_appschool")) %>%
    left_join(accounts, by = c("id_account")) %>%
    mutate(school_address = stringr::str_c(street.y, ", ", city.y, ", ", state.y, " ", zip.y)) %>%
    left_join(match_notification_waitlists(match), by = c("oneappid" = "STUDENT ID")) %>%
    left_join(nolaps::schools_eval, by = "code_site") %>%
    fix_grades(grade_applying) %>%
    fix_grades(grade_terminal) %>%
    mutate(gets_snippet_exitgrade = (grade_applying == grade_terminal)) %>%
    mutate(snippet_exitgrade = dplyr::if_else(gets_snippet_exitgrade, glue::glue(
      "
      Please note: Grade {grade_applying} at {name_account} does not continue after this year.
      To continue in public school, an application will be required next year.
      "
      ),
      NA_character_
      )
    ) %>%
    mutate(snippet_eval = dplyr::if_else(gets_snippet_eval, glue::glue(
      "
      As your child prepares for school this upcoming year, NOLA Public Schools Child Search Program and ReNEW Charter Schools are offering vision, hearing, and speech screenings prior to the beginning of the 2021-2022 school year.
      In addition, throughout the year, the Child Search Office provides educational evaluations in cases where there may be suspicion of any special learning challenges or disabilities.
      To schedule an appointment for your child, please contact the Child Search Office at 504-304-4988 or use our online Child Search Form at www.nolapublicschools.com, under the Child Search Tab.
      "
      ),
      NA_character_
      )
    ) %>%
    mutate(deadline = getdata_registration()[[1]][[1]]) %>%
    select(
      lettertype, oneappid, grade_applying,
      applicant_firstname:phone_2,
      school_name = name_account,
      school_address, school_phone = phone,
      school_welcome = welcome, school_registration = registration, deadline,
      waitlist_school_1:`WAITLIST RANK_4`,
      snippet_exitgrade,
      snippet_eval,
      id_account,
      is_assigned,
      is_guaranteed,
      is_scholarship
    )

  match_notification_salesforce(notifications = notifications, dir_out = dir_out)

  notifications %>%
    readr::write_excel_csv(glue::glue("{dir_out}/notifications.csv"), na = "")

  write_lettertypes <- function(x, dir_out) {

    notifications %>%
      dplyr::filter(lettertype == x) %>%
      readr::write_excel_csv(glue::glue(dir_out, "/", x, ".csv"), na = "")

  }

  dir.create(glue::glue("{dir_out}/notifications"))

  purrr::walk(unique(notifications$lettertype), write_lettertypes, dir_out = glue::glue("{dir_out}/notifications"))

  invisible(notifications)



}


