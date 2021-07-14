#' @importFrom magrittr %>%
#' @importFrom dplyr arrange filter mutate select
#' @importFrom dplyr group_by ungroup summarize
#' @importFrom dplyr left_join
#' @importFrom dplyr n



#' @export
match_notification_mailing <- function(dir_out, test_email = "sprimeaux@nolapublicschools.com", test_sample = 1) {

  apps <-
    getdata_app(round = "Round 2") %>%
    select(
      id_student,
      grade_applying:email,
      language_app:language_pref
    )

  notifications <-
    getdata_student_recent() %>%
    filter(!is.na(lettertype)) %>%
    left_join(apps, by = "id_student") %>%
    left_join(getdata_account(), by = c("id_account_future" = "id_account")) %>%
    mutate(school_address = stringr::str_c(school_street, ", ", school_city, ", ", school_state, " ", school_zip)) %>%
    # left_join(match_notification_waitlists(match), by = c("oneappid" = "STUDENT ID")) %>%
    left_join(nolaps::schools_eval, by = c("code_site_future" = "code_site")) %>%
    left_join(nolaps::lettertypes_salesforce, by = c("lettertype" = "lettertype_salesforce")) %>%
    mutate(gets_snippet_exitgrade = dplyr::case_when(
      grade_future == "12" ~ FALSE,
      grade_future == grade_terminal ~ TRUE,
      governance == "Scholarship" & grade_future == "PK4" ~ TRUE,
      TRUE ~ FALSE
      )
    ) %>%
    mutate(across(name_account_future, ~ stringr::str_squish(stringr::str_remove(., "\\(DO NOT PLACE\\)")))) %>%
    mutate(snippet_exitgrade = dplyr::if_else(gets_snippet_exitgrade, glue::glue(
      "
      Please note: {name_account_future} does not continue after grade {grade_future}.
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
    # mutate(deadline = getdata_registration()[[1]][[1]]) %>%
    mutate(school_name = name_account_future) %>%
    # mutate(waitlist_school_1 = if_else(lettertype == "LT_MR_K12_Matched", NA_character_, waitlist_school_1)) %>%
    # mutate(waitlist_rank_1 = if_else(lettertype == "LT_MR_K12_Matched", NA_character_, waitlist_rank_1)) %>%
    # filter(is.na(governance) | (governance != "Scholarship")) %>%
    filter(lettertype != "LT_R2_K12_NoAppNoDefault") %>%
    # filter(!(oneappid %in% nolaps::exclude$oneappid)) %>%
    mutate(email = if_else(is.na(pg_email), email_update, pg_email)) %>%
    select(
      lettertype, oneappid, grade_future,
      applicant_firstname:applicant_lastname,
      pg_firstname:pg_lastname,
      email,
      school_name,
      school_address, school_phone,
      school_welcome, school_registration,
      # deadline,
      # waitlist_school_1:waitlist_rank_4,
      snippet_exitgrade,
      snippet_eval,
      language_app, language_pref
    )

  if(!isFALSE(test_sample)) {

    notifications <-
      notifications %>%
      group_by(lettertype, language_pref) %>%
      dplyr::slice_sample(n = test_sample) %>%
      ungroup()

  }

  if(!isFALSE(test_email)) {

    notifications <-
      notifications %>%
      mutate(email = test_email)

  }

  notifications %>%
    readr::write_excel_csv(glue::glue("{dir_out}/notifications_summary.csv"), na = "")

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


