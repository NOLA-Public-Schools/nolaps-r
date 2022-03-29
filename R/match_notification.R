#' @import dplyr
#' @import glue
#' @import lubridate
#' @import purrr
#' @import readxl
#' @import salesforcer
#' @import stringr
#' @import tidyr



#' @export
match_notification_waitlists <- function(match, schools_waitlist = c("846", "847", "4012", "4013")) {

  match %>%
    filter(!(GRADE %in% grades_ec())) %>%
    filter(`CHOICE SCHOOL` %in% schools_waitlist) %>%
    filter(`ASSIGNMENT STATUS` %in% c(
      "Waiting List",
      "Waiting List - Family Link Rejection"
      )
    ) %>%
    select(`STUDENT ID`, `CHOICE RANK`, waitlist_school = choice_name, waitlist_rank = `WAITLIST RANK`) %>%
    group_by(`STUDENT ID`) %>%
    arrange(`CHOICE RANK`) %>%
    mutate(waitlist_slot = 1:n()) %>%
    ungroup() %>%
    select(-`CHOICE RANK`) %>%
    pivot_wider(
      names_from = waitlist_slot,
      values_from = c(waitlist_school, waitlist_rank)
    )

}



#' @export
match_notification <- function(match, overmatches, dir_out, apps, accounts, appschools, students_recent) {

  # special logic to collapse Lake Forest and Lusher choices into single row

  special_all <-
    match %>%
    filter(str_detect(`CHOICE SCHOOL`, "4012|4013")) %>%
    mutate(`CHOICE SCHOOL` = case_when(
      str_detect(`CHOICE SCHOOL`, "4012_") ~ "4012",
      str_detect(`CHOICE SCHOOL`, "4013_") ~ "4013",
      TRUE ~ `CHOICE SCHOOL`
    )) %>%
    group_by(`STUDENT ID`, GRADE, `CHOICE SCHOOL`) %>%
    summarize(
      `CHOICE RANK` = min(`CHOICE RANK`),
      n_guaranteed = sum(`GUARANTEED?` == 'YES'),
      n_accepted = sum(`ASSIGNMENT STATUS` == "Accepted"),
      n_waiting = sum(str_detect(`ASSIGNMENT STATUS`, "Waiting")),
      n_ineligible = sum(`ASSIGNMENT STATUS` == "Ineligible")
    ) %>%
    mutate(`GUARANTEED?` = if_else(n_guaranteed == 1, 'YES', NA_character_)) %>%
    ungroup()

  special_accepted <-
    special_all %>%
    filter(n_accepted == 1) %>%
    mutate(`ASSIGNMENT STATUS` = "Accepted")

  special_waiting <-
    special_all %>%
    filter(!(`STUDENT ID` %in% special_accepted$`STUDENT ID`)) %>%
    filter(n_waiting >= 1) %>%
    mutate(`ASSIGNMENT STATUS` = "Waiting List")

  special_ineligible <-
    special_all %>%
    filter(!(`STUDENT ID` %in% special_accepted$`STUDENT ID`)) %>%
    filter(!(`STUDENT ID` %in% special_waiting$`STUDENT ID`)) %>%
    filter(n_ineligible >= 1) %>%
    mutate(`ASSIGNMENT STATUS` = "Ineligible")

  special_notprocessed <-
    special_all %>%
    filter(!(`STUDENT ID` %in% special_accepted$`STUDENT ID`)) %>%
    filter(!(`STUDENT ID` %in% special_waiting$`STUDENT ID`)) %>%
    filter(!(`STUDENT ID` %in% special_ineligible$`STUDENT ID`)) %>%
    mutate(`ASSIGNMENT STATUS` = "Not Processed")

  match <-
    match %>%
    filter(str_length(`STUDENT ID`) == 9) %>%
    filter(!str_detect(`CHOICE SCHOOL`, "4012")) %>%
    filter(!str_detect(`CHOICE SCHOOL`, "4013")) %>%
    bind_rows(special_accepted) %>%
    bind_rows(special_waiting) %>%
    bind_rows(special_ineligible) %>%
    bind_rows(special_notprocessed) %>%
    mutate(choice_name = case_when(
      `CHOICE SCHOOL` == "4012" ~ "Lake Forest Elementary Charter School",
      `CHOICE SCHOOL` == "4013" ~ "Lusher Charter School",
      TRUE ~ choice_name
    )) %>%
    select(-c(n_accepted, n_waiting, n_ineligible, n_guaranteed))

  # normal notification logic

  participants <-
    match %>%
    matchcalcs_participants_all(schools_waitlist = c("846", "847", "4012", "4013"))

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
    dplyr::mutate(is_guaranteed_accepted = !is.na(`GUARANTEED?`)) %>%
    dplyr::select(`STUDENT ID`, school_accepted = `CHOICE SCHOOL`, is_guaranteed_accepted) %>%
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

  guarantees <-
    match %>%
    filter(!is.na(`GUARANTEED?`)) %>%
    select(oneappid = `STUDENT ID`, id_account_guaranteed = id_account)

  participants_lettertypes <-
    participants_aug %>%
    dplyr::mutate(lettertype = dplyr::case_when(
      is_see ~ "see",

      is_ec & is_fallback ~ "ec_fallback",
      is_ec & is_acceptednew & rank_accepted == 1 ~ "ec_topchoice",
      is_ec & is_acceptednew ~ "ec_acceptednew",
      is_ec & is_unassigned & is_waiting ~ "ec_unassigned_wl",
      is_ec & is_unassigned & (n_ineligible == n_choices) ~ "ec_unassigned_ineligible",

      # is_scholarship & is_acceptednew & is_waiting ~ "k12_acceptednew_schol_wl",
      # is_scholarship & is_acceptednew ~ "k12_acceptednew_schol",
      is_scholarship ~ "k12_acceptednew_schol",

      is_acceptednew & is_waiting ~ "k12_acceptednew_wl",
      is_acceptednew ~ "k12_acceptednew",

      is_fallback & is_waiting ~ "k12_fallback_wl",
      is_fallback ~ "k12_fallback",

      is_unassigned & is_waiting ~ "k12_unassigned_wl",
      is_unassigned ~ "k12_unassigned",

      is_guaranteed ~ "guaranteed",

      TRUE ~ "other"
      )
    )

  notifications <-
    participants_lettertypes %>%
    dplyr::rename(
      oneappid = `STUDENT ID`,
      grade_applying = GRADE
    ) %>%
    left_join(dplyr::select(apps, -grade_applying), by = "oneappid") %>%
    left_join(match_lookup_account(match, appschools = appschools, accounts = accounts), by = c("school_accepted" = "code_appschool")) %>%
    left_join(accounts, by = c("id_account")) %>%
    mutate(school_address = stringr::str_c(street.y, ", ", city.y, ", ", state.y, " ", zip.y)) %>%
    left_join(match_notification_waitlists(match), by = c("oneappid" = "STUDENT ID")) %>%
    left_join(nolaps::schools_eval, by = "code_site") %>%
    left_join(guarantees, by = "oneappid") %>%
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
    mutate(school_name = stringr::str_squish(stringr::str_remove(name_account, "\\(DO NOT PLACE\\)"))) %>%
    select(
      lettertype, oneappid, grade_applying,
      applicant_firstname:phone_2,
      school_name,
      school_address, school_phone = phone,
      school_welcome = welcome, school_registration = registration, deadline,
      waitlist_school_1:waitlist_rank_2,
      snippet_exitgrade,
      snippet_eval,
      language_app, language_pref,
      id_account,
      name_account,
      is_assigned,
      is_guaranteed,
      is_guaranteed_accepted,
      is_scholarship,
      id_account_guaranteed
    ) %>%
    mutate(is_guaranteed_scholarship = is_guaranteed & is_scholarship) %>%
    arrange(oneappid)

  match_notification_salesforce(
    notifications = notifications,
    dir_out = dir_out,
    students_recent = students_recent
  )

  notifications %>%
    readr::write_excel_csv(glue::glue("{dir_out}/notifications.csv"), na = "")

  return(NULL)

  write_lettertypes <- function(x, dir_out) {

    notifications %>%
      dplyr::filter(lettertype == x) %>%
      readr::write_excel_csv(glue::glue(dir_out, "/all/", x, ".csv"), na = "")

    spanish <-
      notifications %>%
      dplyr::filter(language_app == "Spanish" | language_pref == "Spanish") %>%
      dplyr::filter(lettertype == x)

    if (nrow(spanish) > 0) {
      readr::write_excel_csv(spanish, glue::glue(dir_out, "/spanish/", x, ".csv"), na = "")
    }

    vietnamese <-
      notifications %>%
      dplyr::filter(language_app == "Vietnamese" | language_pref == "Vietnamese") %>%
      dplyr::filter(lettertype == x)

    if (nrow(vietnamese) > 0) {
      readr::write_excel_csv(vietnamese, glue::glue(dir_out, "/vietnamese/", x, ".csv"), na = "")
    }

  }

  dir.create(glue::glue("{dir_out}/notifications"))
  dir.create(glue::glue("{dir_out}/notifications/all"))
  dir.create(glue::glue("{dir_out}/notifications/spanish"))
  dir.create(glue::glue("{dir_out}/notifications/vietnamese"))

  purrr::walk(unique(notifications$lettertype), write_lettertypes, dir_out = glue::glue("{dir_out}/notifications"))

  invisible(notifications)



}


