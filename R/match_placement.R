#' @importFrom magrittr %>%



#' @export
match_placement <- function(match, overmatches, dir_out) {

  overmatches <-
    match %>%
    dplyr::semi_join(overmatches) %>%
    dplyr::select(`STUDENT ID`, GRADE, `CHOICE SCHOOL`, id_account)

  placements <-
    match %>%
    dplyr::filter(stringr::str_length(`STUDENT ID`) == 9) %>%
    dplyr::filter(`ASSIGNMENT STATUS` == "Accepted") %>%
    dplyr::select(`STUDENT ID`, GRADE, `CHOICE SCHOOL`, id_account, `GUARANTEED?`) %>%
    dplyr::filter(!(`STUDENT ID` %in% overmatches$`STUDENT ID`)) %>%
    dplyr::bind_rows(overmatches) %>%
    dplyr::left_join(getdata_student_recent(), by = c("STUDENT ID" = "oneappid")) %>%
    dplyr::mutate(is_scholarship = stringr::str_detect(`CHOICE SCHOOL`, "_[NR]$")) %>%
    dplyr::mutate(is_guaranteed = !is.na(`GUARANTEED?`)) %>%
    dplyr::select(
      id_student_recent,
      id_account_future = id_account,
      grade_future = GRADE,
      is_guaranteed,
      is_scholarship
    ) %>%
    dplyr::mutate(
      id_schoolyear = "a100W000009Reex",
      is_active = TRUE,
      is_archived = FALSE
    ) %>%
    dplyr::mutate(grade_future = as.character(grade_future)) %>%
    dplyr::mutate(grade_future = dplyr::case_when(
      grade_future == "1YR" ~ "1 YR",
      grade_future == "2YR" ~ "2 YR",
      TRUE ~ grade_future
      )
    ) %>%
    dplyr::relocate(c(is_guaranteed, is_scholarship), .after = tidyselect::last_col()) %>%
    dplyr::mutate(is_guaranteed_scholarship = is_guaranteed & is_scholarship)

  placements %>%
    readr::write_excel_csv(glue::glue("{dir_out}/placements.csv"), na = "")

  invisible(placements)

}


