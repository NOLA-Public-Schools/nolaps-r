#' @importFrom magrittr %>%



#' @export
match_notification_salesforce <- function(notifications, dir_out, students_recent) {

  cat("Generating Salesforce letters\n")

  letters_salesforce <-
    notifications %>%
    dplyr::filter(stringr::str_length(oneappid) == 9) %>%
    dplyr::left_join(nolaps::lettertypes_salesforce, by = c("lettertype" = "lettertype_mailmerge")) %>%
    dplyr::left_join(students_recent, by = c("oneappid")) %>%
    dplyr::mutate(is_new_r1 = dplyr::if_else(
      is_assigned
      & (!is_active | (is_active & (id_account != id_account_current)) | is.na(id_account_current)),
      TRUE,
      FALSE
      )
    ) %>%
    dplyr::mutate(is_new_r2 = dplyr::case_when(
      is.na(id_account) ~ FALSE,
      is.na(id_account_guaranteed) ~ TRUE,
      id_account_guaranteed != id_account ~ TRUE,
      TRUE ~ FALSE
      )
    ) %>%
    mutate(year_letter = "SY 20-21") %>%
    dplyr::select(
      oneappid,
      id_student_recent = id_student,
      grade_applying,
      year_letter,
      lettertype_salesforce,
      matchtype,
      lettertype_salesforce_r2,
      matchtype_r2,
      is_assigned,
      is_new_r1,
      is_new_r2,
      is_guaranteed,
      is_scholarship,
      is_guaranteed_scholarship
    )

  letters_salesforce %>%
    readr::write_excel_csv(glue::glue("{dir_out}/letters_salesforce.csv"), na = "")

  invisible(letters_salesforce)

}


