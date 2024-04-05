#' @import purrr
#' @import readr
#' @import stringr



#' @export
match_notification_mailing_r2 <- function(
  dir_out,
  test_email = "test@test.com",
  test_sample = 1,
  round,
  accounts = getdata_account(),
  apps = getdata_app(round = round),
  appschools = getdata_appschool(),
  students_recent = getdata_student_recent()
  ) {

  ###

  appschools <-
    appschools %>%
    filter(status %in% c("Open", "Closing at End of School Year")) %>%
    distinct(code_appschool, id_account)

  apps <-
    apps %>%
    select(
      id_student,
      grade_applying:email,
      language_app:language_pref
    )

  notifications <-
    students_recent %>%
    select(-governance) %>%
    filter(!is.na(lettertype)) %>%
    left_join(apps, by = "id_student") %>%
    left_join(accounts, by = c("id_account_future" = "id_account")) %>%
    mutate(school_address = str_c(school_street, ", ", school_city, ", ", school_state, " ", school_zip)) %>%
    mutate(school_name = name_account_future) %>%
    mutate(email = if_else(is.na(pg_email), email_update, pg_email)) %>%
    select(
      email,
      lettertype,
      oneappid,
      applicant_firstname, applicant_lastname,
      student_dob,
      grade_future,
      school_name,
      governance,
      school_address,
      school_phone,
      school_registration,
      school_welcome,
      language_app, language_pref
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


