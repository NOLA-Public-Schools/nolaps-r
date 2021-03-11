#' @importFrom magrittr %>%



#' @export
match_test <- function(args = commandArgs(trailingOnly = TRUE)) {



  dir_in <- args[1]

  match <-
    readr::read_csv(
      glue::glue("{dir_in}/3_MasterMatch.csv"),
      col_types = stringr::str_c(stringr::str_dup("c", 9), stringr::str_dup("i", 1), stringr::str_dup("c", 29))
    )



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



  n_match <- nrow(match)

  n_match_in_salesforce <-
    match %>%
    dplyr::filter(
      `STUDENT ID` %in% apps_with_choices$oneappid
      | `STUDENT ID` %in% students$oneappid
    ) %>%
    nrow()

  n_apps_with_choices <- nrow(apps_with_choices)

  n_apps_with_choices_in_match <-
    apps_with_choices %>%
    dplyr::filter(
      oneappid %in% match$`STUDENT ID`
    ) %>%
    nrow()

  n_students_nonterminal <- nrow(students)

  n_students_nonterminal_in_match <-
    students %>%
    dplyr::filter(oneappid %in% match$`STUDENT ID`) %>%
    nrow()



  testthat::test_that("All match records are in Salesforce", {

    testthat::expect_identical(n_match, n_match_in_salesforce)

  })

  print(
    match %>%
      dplyr::filter(!(`STUDENT ID` %in% apps_with_choices$oneappid | `STUDENT ID` %in% students$oneappid)) %>%
      dplyr::select(`STUDENT ID`) %>%
      dplyr::slice_sample(n = 10)
  )



  testthat::test_that("All applications with a choice are in match", {

    testthat::expect_identical(n_apps_with_choices, n_apps_with_choices_in_match)

  })

  print(
    apps_with_choices %>%
      dplyr::filter(!(oneappid %in% match$`STUDENT ID`)) %>%
      dplyr::select(oneappid, id_app) %>%
      dplyr::slice_sample(n = 10)
  )



  testthat::test_that("All non-terminal current students are in match", {

    testthat::expect_identical(n_students_nonterminal, n_students_nonterminal_in_match)

  })

  print(
    students %>%
      dplyr::filter(!(oneappid %in% match$`STUDENT ID`)) %>%
      dplyr::select(oneappid, id_student) %>%
      dplyr::slice_sample(n = 10)
  )



}


