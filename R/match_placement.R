#' @import dplyr
#' @import glue
#' @import readr
#' @import stringr



#' @export
match_placement <- function(match, overmatches, dir_out, students_recent, appschools) {

  cat("\nGenerating placements upload\n")

  appschools <-
    appschools %>%
    filter(status %in% c("Open", "Closing at End of School Year")) %>%
    distinct(code_appschool, id_account, name_account)

  placements <-
    match %>%
    filter(str_length(`STUDENT ID`) == 9) %>%
    filter(str_detect(`CHOICE SCHOOL`, "_[NR]$", negate = TRUE)) %>%
    filter(`ASSIGNMENT STATUS` == "Accepted") %>%
    select(`STUDENT ID`, GRADE, `CHOICE SCHOOL`) %>%
    mutate(`CHOICE SCHOOL` = case_when(
      str_detect(`CHOICE SCHOOL`, "4012_") ~ "4012",
      str_detect(`CHOICE SCHOOL`, "4013_") ~ "4013",
      TRUE ~ `CHOICE SCHOOL`
    )) %>%
    filter(!(`STUDENT ID` %in% overmatches$`STUDENT ID`)) %>%
    bind_rows(overmatches) %>%
    mutate(`CHOICE SCHOOL` = if_else(
      `CHOICE SCHOOL` == "796" & GRADE == "PK4", "847",
      `CHOICE SCHOOL`
    )) %>%
    left_join(appschools, by = c("CHOICE SCHOOL" = "code_appschool")) %>%
    left_join(students_recent, by = c("STUDENT ID" = "oneappid")) %>%
    select(
      name_account_future = name_account,
      id_account_future = id_account,
      grade_future = GRADE,
      id_student_recent = id_student
    ) %>%
    mutate(
      id_schoolyear = "a106T00000AQgdMQAT", # 2021-2022
      id_recordtype = "012d0000000tDWHAA2", # Round 1
      is_active = TRUE,
      is_archived = FALSE
    ) %>%
    arrange(id_account_future, grade_future) %>%
    mutate(grade_future = as.character(grade_future)) %>%
    mutate(grade_future = case_when(
      grade_future == "1YR" ~ "1 YR",
      grade_future == "2YR" ~ "2 YR",
      TRUE ~ grade_future
      )
    )

  placements %>% write_excel_csv(glue("{dir_out}/placements.csv"), na = "")

  invisible(placements)

}


