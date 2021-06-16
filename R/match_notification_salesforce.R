#' @importFrom magrittr %>%



#' @export
match_notification_salesforce <- function(notifications, dir_out, students_recent) {

  cat("Generating Salesforce letters\n")

  letters_salesforce <-
    notifications %>%
    dplyr::filter(stringr::str_length(oneappid) == 9) %>%
    dplyr::left_join(nolaps::lettertypes_salesforce, by = c("lettertype" = "lettertype_mailmerge")) %>%
    dplyr::left_join(students_recent, by = c("oneappid")) %>%
    dplyr::mutate(is_newmatch = dplyr::if_else(
      is_assigned
      & (!is_active | (is_active & (id_account != id_account_current))),
      TRUE,
      FALSE
      )
    ) %>%
    dplyr::select(
      oneappid,
      id_student_recent = id_student,
      lettertype_salesforce,
      matchtype,
      lettertype_salesforce_r2,
      matchtype_r2,
      is_assigned,
      is_newmatch,
      is_guaranteed,
      is_scholarship,
      is_guaranteed_scholarship
    )

  letters_salesforce %>%
    readr::write_excel_csv(glue::glue("{dir_out}/letters_salesforce.csv"), na = "")

  invisible(letters_salesforce)

}


