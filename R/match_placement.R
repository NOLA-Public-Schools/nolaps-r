#' @importFrom magrittr %>%



#' @export
match_placement <- function(match, dir_out) {

  # OVERMATCH!

  placements <-
    match %>%
    dplyr::filter(stringr::str_length(`STUDENT ID`) == 9) %>%
    dplyr::filter(`ASSIGNMENT STATUS` == "Accepted") %>%
    dplyr::left_join(getdata_student_recent(), by = c("STUDENT ID" = "oneappid")) %>%
    dplyr::select(id_student_recent = id_student.y, id_account)

  placements %>%
    readr::write_excel_csv(glue::glue("{dir_out}/placements.csv"), na = "")

  invisible(placements)

}


