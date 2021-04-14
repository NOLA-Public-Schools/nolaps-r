#' @importFrom magrittr %>%
#' @importFrom dplyr arrange filter mutate select
#' @importFrom dplyr group_by ungroup summarize
#' @importFrom dplyr left_join
#' @importFrom dplyr n



#' @export
match_notification_mailing <- function(match, dir_out) {

  apps <-
    getdata_app_1year() %>%
    select(
      id_student,
      grade_applying:email,
      language_app:language_pref
    )

  notifications <-
    getdata_student_matchletter() %>%
    left_join(apps, by = "id_student") %>%
    left_join(getdata_account_gradespan(), by = c("id_account_future" = "id_account")) %>%
    mutate(school_address = stringr::str_c(street, ", ", city, ", ", state, " ", zip)) %>%
    left_join(match_notification_waitlists(match), by = c("oneappid" = "STUDENT ID")) %>%
    left_join(nolaps::schools_eval, by = "code_site") %>%
    left_join(nolaps::lettertypes_salesforce, by = c("lettertype" = "lettertype_salesforce")) %>%
    mutate(grade_max = dplyr::case_when(
      grade_max == "1YR" ~ "1 YR",
      grade_max == "2YR" ~ "2 YR",
      TRUE ~ as.character(grade_max)
      )
    ) %>%
    mutate(gets_snippet_exitgrade = dplyr::case_when(
      grade_applying == "12" ~ FALSE,
      grade_applying == grade_terminal ~ TRUE,
      governance == "Scholarship" & grade_applying == "PK4" ~ TRUE,
      TRUE ~ FALSE
      )
    ) %>%
    mutate(snippet_exitgrade = dplyr::if_else(gets_snippet_exitgrade, glue::glue(
      "
      Please note: {name_account_future} does not continue after grade {grade_applying}.
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
    mutate(school_name = stringr::str_squish(stringr::str_remove(name_account_future, "\\(DO NOT PLACE\\)"))) %>%
    mutate(waitlist_school_1 = if_else(lettertype == "LT_MR_K12_Matched", NA_character_, waitlist_school_1)) %>%
    mutate(waitlist_rank_1 = if_else(lettertype == "LT_MR_K12_Matched", NA_character_, waitlist_rank_1)) %>%
    filter(is.na(governance) | (governance != "Scholarship")) %>%
    filter(lettertype != "LT_MR_K12_NoAppNoDefault") %>%
    filter(!(oneappid %in% nolaps::exclude$oneappid)) %>%
    select(
      lettertype = lettertype_mailmerge, oneappid, grade_applying,
      applicant_firstname:email,
      school_name,
      school_address, school_phone,
      school_welcome, school_registration,
      deadline,
      waitlist_school_1:waitlist_rank_4,
      snippet_exitgrade,
      snippet_eval,
      language_app, language_pref
    ) %>%
    # group_by(lettertype, language_pref) %>%
    # dplyr::slice_sample(n = 1) %>%
    # ungroup() %>%
    # mutate(email = "enrollment_support@nolapublicschools.com") %>%
    arrange(oneappid)

  notifications %>%
    readr::write_excel_csv(glue::glue("{dir_out}/notifications.csv"), na = "")

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


