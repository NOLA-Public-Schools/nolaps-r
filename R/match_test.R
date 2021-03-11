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

  students <-
    getdata_student_active() %>%
    dplyr::filter(!is.na(id_account_current))

  n_match <- nrow(match)

  n_match_in_salesforce <-
    match %>%
    dplyr::filter(
      `STUDENT ID` %in% apps_with_choices$oneappid
      | `STUDENT ID` %in% students$oneappid
    ) %>%
    nrow()

  testthat::test_that("All match records in Salesforce", {

    testthat::expect_identical(n_match, n_match_in_salesforce)

  })

  print(
    match %>%
      dplyr::filter(!(`STUDENT ID` %in% apps_with_choices$oneappid | `STUDENT ID` %in% students$oneappid)) %>%
      dplyr::select(`STUDENT ID`) %>%
      dplyr::slice_sample(n = 10)
  )

  print("Done!")

}


