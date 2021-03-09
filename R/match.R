#' @importFrom magrittr %>%



#' @export
match_augment <- function(x) {

  appschools <- getdata_appschool()

  accounts <- getdata_account()

  students <-
    getdata_student_active() %>%
    dplyr::left_join(accounts, by = c("id_account_current" = "id_account")) %>%
    dplyr::select(oneappid, grade_current, school_current = name_account)

  names_matchschool <-
    appschools %>%
    dplyr::left_join(accounts, by = c("id_account")) %>%
    dplyr::select(code_appschool, choice_name = name_account) %>%
    dplyr::distinct()

  x %>%
    dplyr::left_join(names_matchschool, by = c("CHOICE SCHOOL" = "code_appschool")) %>%
    dplyr::left_join(students, by = c("STUDENT ID" = "oneappid"))

}



#' @export
match_process <- function(args = commandArgs(trailingOnly = TRUE)) {

  match <- readr::read_csv(args[1], col_types = stringr::str_dup("c", 39))

  match %>%
    match_augment() %>%
    readr::write_excel_csv(args[2], na = "")

}


