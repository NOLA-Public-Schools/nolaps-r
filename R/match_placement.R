#' @import readr
#' @import stringr


#' @export
match_placement <- function(match, overmatches, dir_out, students_recent, appschools) {

  cat("\nGenerating placements upload\n")

  appschools <-
    appschools %>%
    filter(
      status %in% c("Open", "Opening Next Year", "Closing at End of School Year")
    ) %>%
    distinct(code_appschool, id_account, name_account)

  placements <-
    match %>%
    filter(`ASSIGNMENT STATUS` == "Accepted") %>%
    select(`STUDENT ID`, GRADE, `CHOICE SCHOOL`) %>%
    mutate(`CHOICE SCHOOL` = case_when(
      str_detect(`CHOICE SCHOOL`, "4012_") ~ "4012",
      str_detect(`CHOICE SCHOOL`, "4013_") ~ "4013",
      TRUE ~ `CHOICE SCHOOL`
    )) %>%
    # filter(!(`STUDENT ID` %in% overmatches$`STUDENT ID`)) %>%
    # bind_rows(overmatches) %>%
    left_join(appschools, by = c("CHOICE SCHOOL" = "code_appschool")) %>%
    left_join(students_recent, by = c("STUDENT ID" = "oneappid")) %>%
    select(
      oneappid = `STUDENT ID`,
      name_account_future = name_account,
      id_account_future = id_account,
      grade_future = GRADE,
      id_student_recent = id_student
    ) %>%
    mutate(
      id_schoolyear = "a106T00000ARcym", # 2022-2023
      # id_recordtype_r1 = "0120W000001tdvs",
      id_recordtype_r2 = "0120W000001tdvt",
      is_active = TRUE,
      is_archived = FALSE
    ) %>%
    fix_grades(grade_future) %>%
    arrange(name_account_future, grade_future) %>%
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


