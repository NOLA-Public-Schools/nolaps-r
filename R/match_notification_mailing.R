#' @import dplyr
#' @import glue
#' @import purrr
#' @import readr
#' @import stringr



#' @export
match_notification_mailing <- function(
  dir_out,
  test_email = "test@test.com",
  test_sample = 1,
  students_recent = getdata_student_recent(),
  apps = getdata_app(round = "Round 1"),
  match
  ) {

  ###

  appschools <-
    getdata_appschool() %>%
    filter(status %in% c("Open", "Closing at End of School Year")) %>%
    distinct(code_appschool, id_account)

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
    mutate(`CHOICE SCHOOL` = if_else(`CHOICE SCHOOL` == "796" & GRADE == "PK4", "847", `CHOICE SCHOOL`)) %>%
    mutate(choice_name = case_when(
      `CHOICE SCHOOL` == "846" ~ "Audubon Uptown French",
      `CHOICE SCHOOL` == "847" ~ "Audubon Uptown Montessori",
      `CHOICE SCHOOL` == "4012" ~ "Lake Forest Elementary Charter School",
      `CHOICE SCHOOL` == "4013" ~ "Lusher Charter School",
      TRUE ~ ""
    )) %>%
    select(-c(n_accepted, n_waiting, n_ineligible, n_guaranteed))

  matched <-
    match %>%
    filter(`ASSIGNMENT STATUS` == "Accepted") %>%
    left_join(appschools, by = c("CHOICE SCHOOL" = "code_appschool")) %>%
    select(oneappid = `STUDENT ID`, id_account_matched = id_account)

  ###

  apps <-
    apps %>%
    select(
      id_student,
      grade_applying:email,
      language_app:language_pref
    )

  accounts <- getdata_account()

  notifications <-
    students_recent %>%
    select(-governance) %>%
    filter(!is.na(lettertype)) %>%
    left_join(apps, by = "id_student") %>%
    left_join(accounts, by = c("id_account_future" = "id_account")) %>%
    mutate(school_address = str_c(school_street, ", ", school_city, ", ", school_state, " ", school_zip)) %>%
    left_join(match_notification_waitlists(match), by = c("oneappid" = "STUDENT ID")) %>%
    left_join(matched, by = c("oneappid")) %>%
    # left_join(nolaps::schools_eval, by = c("code_site_future" = "code_site")) %>%
    # left_join(nolaps::lettertypes_salesforce, by = c("lettertype" = "lettertype_salesforce")) %>%
    mutate(lettertype = case_when(
      lettertype == "LT_MR_K12_MatchedToScholarship" & !is.na(waitlist_school_1) ~ "LT_MR_K12_MatchedToScholarship+WL",
      TRUE ~ lettertype
    )) %>%
    mutate(gets_snippet_exitgrade = dplyr::case_when(
      grade_future == "12" ~ FALSE,
      grade_future == grade_terminal ~ TRUE,
      governance == "Scholarship" & grade_future == "PK4" ~ TRUE,
      TRUE ~ FALSE
      )
    ) %>%
    mutate(across(name_account_future, ~ stringr::str_squish(stringr::str_remove(., "\\(DO NOT PLACE\\)")))) %>%
    # mutate(snippet_exitgrade = dplyr::if_else(gets_snippet_exitgrade, glue::glue(
    #   "
    #   Please note: {name_account_future} does not continue after grade {grade_future}.
    #   To continue in public school, an application will be required next year.
    #   "
    #   ),
    #   NA_character_
    #   )
    # ) %>%
    mutate(deadline = getdata_registration()[[1]][[1]]) %>%
    mutate(school_name = name_account_future) %>%
    # mutate(waitlist_school_1 = if_else(lettertype == "LT_MR_K12_Matched", NA_character_, waitlist_school_1)) %>%
    # mutate(waitlist_rank_1 = if_else(lettertype == "LT_MR_K12_Matched", NA_character_, waitlist_rank_1)) %>%
    # filter(is.na(governance) | (governance != "Scholarship")) %>%
    # filter(lettertype != "LT_R2_K12_NoAppNoDefault") %>%
    # filter(!(oneappid %in% nolaps::exclude$oneappid)) %>%
    mutate(email = if_else(is.na(pg_email), email_update, pg_email)) %>%
    # filter(is_active & ((id_account_future != id_account_matched) | (is.na(id_account_matched) & !is.na(id_account_future)))) %>%
    mutate(
      name_account_current = if_else(!is_active, NA_character_, name_account_current),
      grade_current = if_else(!is_active, NA_character_, as.character(grade_current))
    ) %>%
    select(
      lettertype, oneappid, grade_future,
      applicant_firstname:applicant_lastname, student_dob,
      pg_firstname:pg_lastname,
      email,
      school_name,
      school_address, school_phone,
      school_welcome, school_registration,
      deadline,
      waitlist_school_1:waitlist_rank_2,
      # snippet_exitgrade,
      # snippet_eval,
      language_app, language_pref,
      is_active, grade_current, name_account_current
    )

  if(!isFALSE(test_sample)) {

    notifications <-
      notifications %>%
      group_by(lettertype, language_pref) %>%
      slice_sample(n = test_sample) %>%
      ungroup()

  }

  if(!isFALSE(test_email)) {

    notifications <-
      notifications %>%
      mutate(email = test_email)

  }

  notifications %>%
    write_excel_csv(glue("{dir_out}/notifications_summary.csv"), na = "")

  write_lettertypes <- function(x, dir_out) {

    notifications %>%
      filter(lettertype == x) %>%
      write_excel_csv(glue(dir_out, "/all/", x, ".csv"), na = "")

    spanish <-
      notifications %>%
      filter(language_app == "Spanish" | language_pref == "Spanish") %>%
      filter(lettertype == x)

    if (nrow(spanish) > 0) {
      write_excel_csv(spanish, glue(dir_out, "/spanish/", x, ".csv"), na = "")
    }

    vietnamese <-
      notifications %>%
      filter(language_app == "Vietnamese" | language_pref == "Vietnamese") %>%
      filter(lettertype == x)

    if (nrow(vietnamese) > 0) {
      write_excel_csv(vietnamese, glue(dir_out, "/vietnamese/", x, ".csv"), na = "")
    }

  }

  dir.create(glue("{dir_out}/notifications"))
  dir.create(glue("{dir_out}/notifications/all"))
  dir.create(glue("{dir_out}/notifications/spanish"))
  dir.create(glue("{dir_out}/notifications/vietnamese"))

  walk(unique(notifications$lettertype), write_lettertypes, dir_out = glue("{dir_out}/notifications"))

  invisible(notifications)

}


