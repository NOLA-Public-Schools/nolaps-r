#' @importFrom magrittr %>%



#' @export
match_notification_salesforce <- function(notifications, dir_out) {

  letters_salesforce <-
    notifications %>%
    dplyr::filter(stringr::str_length(oneappid) == 9) %>%
    dplyr::left_join(nolaps::lettertypes_salesforce, by = c("lettertype" = "lettertype_mailmerge")) %>%
    dplyr::left_join(getdata_student_recent(), by = c("oneappid")) %>%
    dplyr::mutate(is_newmatch = dplyr::if_else(
      is_assigned
      & (!is_active | (is_active & (school_name != school_current))),
      TRUE,
      FALSE
      )
    ) %>%
    dplyr::select(
      oneappid,
      id_student_recent,
      lettertype_salesforce,
      matchtype,
      is_assigned,
      is_newmatch,
      is_guaranteed,
      is_scholarship
    ) %>%
    dplyr::mutate(is_guaranteed_scholarship = is_guaranteed & is_scholarship)


  letters_salesforce %>%
    readr::write_excel_csv(glue::glue("{dir_out}/letters_salesforce.csv"), na = "")

  invisible(letters_salesforce)

}


