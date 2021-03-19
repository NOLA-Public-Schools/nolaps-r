#' @importFrom magrittr %>%



#' @export
filter_priority <- function(x, priority, prioritytable) {

  doesnthave_all <-
    prioritytable %>%
    dplyr::filter(Grade == "ALL", {{ priority }} == 0) %>%
    dplyr::distinct(`School Code`)

  doesnthave_grade <-
    prioritytable %>%
    dplyr::filter(Grade != "ALL", {{ priority }} == 0) %>%
    dplyr::distinct(`School Code`, Grade)

  x %>%
    dplyr::anti_join(doesnthave_grade, by = c("CHOICE SCHOOL" = "School Code", "GRADE" = "Grade")) %>%
    dplyr::anti_join(doesnthave_all, by = c("CHOICE SCHOOL" = "School Code")) %>%
    dplyr::filter(is.na(Ineligible)) %>%
    dplyr::filter(is.na({{ priority }}))

}



#' @export
match_test <- function(match, dir_out, prioritytable) {



  apps <- getdata_app_1year() %>% dplyr::filter(recordtype == "Round 1")

  choices <- getdata_appschoolranking_1year() %>% dplyr::filter(id_app %in% apps$id_app)

  apps_with_choices <- apps %>% dplyr::filter(id_app %in% choices$id_app)

  accounts <- getdata_account()

  students <-
    getdata_student_active() %>%
    dplyr::filter(!is.na(id_account_current)) %>%
    dplyr::filter(is_terminalgrade == "false") %>%
    dplyr::filter(grade_current != 12) %>%
    dplyr::left_join(accounts, by = c("id_account_current" = "id_account"))



# Invalid match records ---------------------------------------------------



  missing_from_salesforce <-
    match %>%
    dplyr::filter(!(`STUDENT ID` %in% apps_with_choices$oneappid | `STUDENT ID` %in% students$oneappid)) %>%
    dplyr::select(`STUDENT ID`, `CHOICE SCHOOL`, GRADE) %>%
    dplyr::arrange(`CHOICE SCHOOL`, GRADE, `STUDENT ID`)

  testthat::test_that("All match records are in Salesforce", {

    testthat::expect_equal(nrow(missing_from_salesforce), 0)

  })

  if (nrow(missing_from_salesforce) > 0) {

    missing_from_salesforce %>%
      readr::write_excel_csv(glue::glue("{dir_out}/missing_from_salesforce.csv"), na = "")

  }



# Missing match records ---------------------------------------------------



  missing_apps_with_choices <-
    apps_with_choices %>%
    dplyr::filter(!(oneappid %in% match$`STUDENT ID`)) %>%
    dplyr::select(oneappid, id_student, id_app) %>%
    dplyr::arrange(oneappid)

  testthat::test_that("All applications with a choice are in match", {

    testthat::expect_equal(nrow(missing_apps_with_choices), 0)

  })

  if (nrow(missing_apps_with_choices) > 0) {

    missing_apps_with_choices %>%
      readr::write_excel_csv(glue::glue("{dir_out}/missing_apps_with_choices.csv"), na = "")

  }



  missing_students_nonterminal <-
    students %>%
    dplyr::filter(!(oneappid %in% match$`STUDENT ID`)) %>%
    dplyr::select(oneappid, id_student, school_current = name_account, grade_current) %>%
    dplyr::arrange(school_current, grade_current, oneappid)

  testthat::test_that("All non-terminal current students are in match", {

    testthat::expect_equal(nrow(missing_students_nonterminal), 0)

  })

  if (nrow(missing_students_nonterminal) > 0) {

    missing_students_nonterminal %>%
      readr::write_excel_csv(glue::glue("{dir_out}/missing_students_nonterminal.csv"), na = "")

  }



# Non-existent grades -----------------------------------------------------



  grades_nonexistent <-
    match %>%
    dplyr::left_join(
      getdata_appschool_with_account_gradespan(),
      by = c("CHOICE SCHOOL" = "code_appschool")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::filter(!(GRADE %in% gradespan_nextyear_vector)) %>%
    dplyr::ungroup() %>%
    dplyr::select(`STUDENT ID`, `CHOICE SCHOOL`, GRADE) %>%
    dplyr::arrange(`CHOICE SCHOOL`, GRADE, `STUDENT ID`)

  testthat::test_that("No match record involves a grade that will not exist next year", {

    testthat::expect_equal(nrow(grades_nonexistent), 0)

  })

  if (nrow(grades_nonexistent) > 0) {

    grades_nonexistent %>%
      readr::write_excel_csv(glue::glue("{dir_out}/grades_nonexistent.csv"), na = "")

  }



# -------------------------------------------------------------------------

# TODO same grade issues

# Eligibility -------------------------------------------------------------



  asr_eligibility <- getdata_appschoolranking_eligibility()

  ineligible_accepted <-
    match %>%
    dplyr::left_join(
      asr_eligibility,
      by = c("STUDENT ID" = "oneappid", "CHOICE SCHOOL" = "code_appschool")
    ) %>%
    dplyr::filter(eligibility == "Ineligible") %>%
    dplyr::filter(`ASSIGNMENT STATUS` == "Accepted") %>%
    dplyr::select(
      `STUDENT ID`,
      `CHOICE RANK`, `CHOICE SCHOOL`,
      `ASSIGNMENT STATUS`, `ELIGIBLE?`, `GUARANTEED?`,
      `SEAT TYPE`, `PRIORITY TYPE`, `QUALIFIED PRIORITIES`,
      id_appschoolranking, programtype, eligibility
    )

  testthat::test_that("No student is assigned to an ineligible choice", {

    testthat::expect_equal(nrow(ineligible_accepted), 0)

  })

  if (nrow(ineligible_accepted) > 0) {

    ineligible_accepted %>%
      readr::write_excel_csv(glue::glue("{dir_out}/ineligible_accepted.csv"), na = "")

  }

  ineligible_marked_eligible <-
    match %>%
    dplyr::left_join(
      asr_eligibility,
      by = c("STUDENT ID" = "oneappid", "CHOICE SCHOOL" = "code_appschool")
    ) %>%
    dplyr::filter(`ASSIGNMENT STATUS` != "Ineligible") %>%
    dplyr::filter(eligibility == "Ineligible") %>%
    dplyr::select(
      `STUDENT ID`,
      `CHOICE RANK`, `CHOICE SCHOOL`,
      `ASSIGNMENT STATUS`, `ELIGIBLE?`, `GUARANTEED?`,
      `SEAT TYPE`, `PRIORITY TYPE`, `QUALIFIED PRIORITIES`,
      id_appschoolranking, programtype, eligibility
    )

  testthat::test_that("No choice is marked eligible in match but ineligible in Salesforce", {

    testthat::expect_equal(nrow(ineligible_marked_eligible), 0)

  })

  if (nrow(ineligible_marked_eligible) > 0) {

    ineligible_marked_eligible %>%
      readr::write_excel_csv(glue::glue("{dir_out}/ineligible_marked_eligible.csv"), na = "")

  }



# Priorities --------------------------------------------------------------



  match_priorities <-
    match %>%
    matchcalcs_priorityoutcomes() %>%
    dplyr::left_join(
      getdata_appschoolranking_priorities(),
      by = c("STUDENT ID" = "oneappid", "CHOICE SCHOOL" = "code_appschool")
    )

  # Distance

  missing_distance <-
    match_priorities %>%
    filter_priority(`Child of Student`, prioritytable) %>%
    dplyr::filter(is_priority_distance)

  invalid_distance <-
    match_priorities %>%
    dplyr::filter(!is.na(`Child of Student`)) %>%
    dplyr::filter(!is_priority_distance)

  testthat::test_that(
    "Distance - everyone has it that should; no one has it that shouldn't", {

      testthat::expect_equal(nrow(missing_distance), 0)
      testthat::expect_equal(nrow(invalid_distance), 0)

  })

  write_if_bad <- function(x) {

    if (nrow(x) > 0) {

      filename <- deparse(substitute(x))
      readr::write_excel_csv(x, glue::glue("{dir_out}/{filename}.csv"), na = "")

    }

  }

  write_if_bad(missing_distance)
  write_if_bad(invalid_distance)

  # Zone



}


