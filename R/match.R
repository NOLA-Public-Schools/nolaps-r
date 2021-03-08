#' @importFrom magrittr %>%



#' @export
match_augment <- function(x) {

  students <-
    getdata_student_active() %>%
    dplyr::select(oneappid, grade_current, id_account_current)

  accounts <-
    getdata_account() %>%
    dplyr::select(id_account, school_current = name_account)

  x %>%
    dplyr::left_join(students, by = c("STUDENT ID" = "oneappid")) %>%
    dplyr::left_join(accounts, by = c("id_account_current" = "id_account")) %>%
    dplyr::relocate(c(`STUDENT ID`, grade_current, school_current))

}



#' @export
match_process <- function(args = commandArgs(trailingOnly = TRUE)) {

  match <- readr::read_csv(args[1], col_types = stringr::str_dup("c", 39))

  match %>%
    match_augment() %>%
    readr::write_excel_csv(args[2], na = "")

}


