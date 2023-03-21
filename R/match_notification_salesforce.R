#' @import dplyr
#' @import glue
#' @import lubridate
#' @import purrr
#' @import readxl
#' @import salesforcer
#' @import stringr
#' @import tidyr



#' @export
match_notification_salesforce <- function(notifications, dir_out, students_recent) {

  cat("\nGenerating Salesforce letters\n")

  letters_salesforce <-
    notifications %>%
    left_join(nolaps::lettertypes_salesforce, by = c("lettertype" = "lettertype_mailmerge")) %>%
    left_join(students_recent, by = c("oneappid")) %>%
    mutate(is_new = is_assigned & !is_guaranteed_accepted) %>%
    # mutate(year_letter = "SY 22-23") %>%
    select(
      oneappid,
      id_student_recent = id_student,
      grade_applying,
      # year_letter,
      lettertype_salesforce,
      matchtype,
      is_assigned,
      is_new,
      is_guaranteed,
      is_guaranteed_accepted
    )

  letters_salesforce %>%
    write_excel_csv(glue("{dir_out}/letters_salesforce.csv"), na = "")

  invisible(letters_salesforce)

}


