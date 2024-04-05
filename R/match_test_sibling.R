#' @import lubridate
#' @import purrr
#' @import readxl
#' @import salesforcer
#' @import stringr
#' @import tidyr


#' @export
test_sibling <- function(dir_out, round, match_priorities, students_recent, siblings, appschoolrankings, appschools, apps, accounts, priorities) {

  cat("\nNon-verified sibling\n")

  sibling <-
    siblings %>%
    select(
      applicant_oneappid = student_oneappid,
      sibling_oneappid
    )

  asr <-
    appschoolrankings %>%
    select(
      id_app,
      id_appschoolranking,
      id_appschool,
      id_account,
      is_verifiedsibling
    )

  appschools_verifiedsibling <-
    appschools %>%
    filter(str_detect(questions_selective, "Verified Sibling")) %>%
    pull(id_appschool)

  asr <- asr %>% filter(!(id_appschool %in% appschools_verifiedsibling))

  app <-
    apps %>%
    select(
      id_app,
      id_student,
      grade_applying,
      applicant_oneappid = oneappid,
      applicant_firstname,
      applicant_lastname,
      pg_firstname,
      pg_lastname,
      email,
      phone_1,
      phone_2
    )

  account <-
    accounts %>%
    select(
      id_account,
      name_account,
      code_site,
      gradespan_nextyear,
      email_enrollment
    ) %>%
    mutate_code_site_group() %>%
    arrange(name_account)

  gradeinfo <-
    account %>%
    select(
      code_site_group,
      gradespan_nextyear
    ) %>%
    separate_rows(gradespan_nextyear, sep = ";") %>%
    filter(!is.na(code_site_group)) %>%
    filter(!is.na(gradespan_nextyear)) %>%
    distinct(code_site_group, gradespan_nextyear) %>%
    fix_grades(var = gradespan_nextyear) %>%
    group_by(code_site_group) %>%
    arrange(code_site_group, gradespan_nextyear) %>%
    slice_tail(n = 1) %>%
    rename(grade_terminal = gradespan_nextyear)

  sitecodes_terminalgrade <-
    account %>%
    select(id_account, code_site_group, code_site) %>%
    left_join(gradeinfo, by = "code_site_group") %>%
    left_join(grades_text_numeric(), by = c("grade_terminal" = "grade")) %>%
    rename(grade_terminal_numeric = grade_numeric)

  sitecodes <-
    sitecodes_terminalgrade %>%
    select(-c(grade_terminal, grade_terminal_numeric))

  schoolnames <-
    account %>%
    select(
      id_account,
      name_account
    )

  if (round == "Round 1") {

    students <- students_recent %>% filter(is_active)

    siblingschool <-
      students %>%
      select(
        sibling_oneappid = oneappid,
        sibling_firstname = student_firstname,
        sibling_lastname = student_lastname,
        sibling_grade_current = grade_current,
        sibling_school = id_account_current,
        sibling_id_student = id_student
      ) %>%
      fix_grades(var = sibling_grade_current) %>%
      left_join(sitecodes_terminalgrade, by = c("sibling_school_current" = "id_account")) %>%
      left_join(grades_next(), by = c("sibling_grade_current" = "grade_current")) %>%
      left_join(grades_text_numeric(), by = c("grade_next" = "grade")) %>%
      select(
        sibling_oneappid,
        sibling_firstname,
        sibling_lastname,
        sibling_grade_current,
        sibling_grade_next = grade_numeric,
        sibling_school,
        sibling_code_site_group = code_site_group,
        sibling_code_site = code_site,
        grade_terminal_numeric,
        sibling_id_student
      )

  } else if (round == "Round 2") {

    students_futureschool <- students_recent %>% filter(!is.na(id_account_future))

    siblingschool <-
      students_futureschool %>%
      select(
        sibling_oneappid = oneappid,
        sibling_firstname = student_firstname,
        sibling_lastname = student_lastname,
        sibling_grade_future = grade_future,
        sibling_school = id_account_future
      ) %>%
      left_join(sitecodes_terminalgrade, by = c("sibling_school" = "id_account")) %>%
      select(
        sibling_oneappid,
        sibling_firstname,
        sibling_lastname,
        sibling_grade_future,
        sibling_school,
        sibling_code_site_group = code_site_group,
        sibling_code_site = code_site,
      )

  }

  applicants_with_siblingschools <-
    asr %>%
    left_join(app, by = "id_app") %>%
    left_join(sibling, by = "applicant_oneappid") %>%
    left_join(siblingschool, by = "sibling_oneappid") %>%
    left_join(sitecodes, by = "id_account") %>%
    left_join(schoolnames, by = "id_account") %>%
    left_join(schoolnames, by = c("sibling_school" = "id_account"))

  if (round == "Round 1") {

    has_sibling <-
      applicants_with_siblingschools %>%
      filter(code_site_group == sibling_code_site_group) %>%
      filter(sibling_grade_next <= grade_terminal_numeric) %>%
      anti_join(
        students, by = c("id_student", "id_account" = "id_account_current")
      ) %>%
      filter((id_appschool %in% appschools))

  } else if (round == "Round 2") {

    has_sibling <-
      applicants_with_siblingschools %>%
      filter(code_site_group == sibling_code_site_group)

  }

  # EC Round 2, single test for verified and non-verified

  has_verified <-
    match_priorities %>%
    filter(is_verifiedsibling)

  has_sibling <-
    match_priorities %>%
    semi_join(has_sibling, by = c("id_account", "STUDENT ID" = "applicant_oneappid")) %>%
    bind_rows(has_verified)

  #

  offers <-
    priorities %>%
    filter(!is.na(Order_Sibling__c)) %>%
    select(code_appschool, grade)

  invalid_sibling <-
    match_priorities %>%
    #filter(!is_highdemand) %>%
    anti_join(has_sibling, by = c("id_account", "STUDENT ID")) %>%
    filter(!is.na(Sibling))

  missing_sibling <-
    match_priorities %>%
    #filter(!is_highdemand) %>%
    semi_join(has_sibling, by = c("id_account", "STUDENT ID")) %>%
    filter(is.na(Sibling)) %>%
    filter(is.na(Ineligible)) %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade"))

  have <-
    match_priorities %>%
    # filter(!is_verifiedsibling) %>%
    filter(!is.na(Sibling))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_sibling,
    "No student has an invalid sibling priority (excludes high-demand)."
  )

  test_helper(
    missing_sibling,
    "No student has a missing sibling priority (excludes high-demand)."
  )

  write_if_bad(invalid_sibling, dir_out)
  write_if_bad(missing_sibling, dir_out)

}


