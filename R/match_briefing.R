#' @import dplyr
#' @import glue
#' @import readr
#' @import stringr



#' @export
match_briefing <- function(match, dir_out) {

  cat("\nGenerating summary data for media briefing\n")

  dir.create(glue("{dir_out}/briefing"))

  # special logic to collapse Lake Forest and Lusher choices into single row

  special_all <-
    match %>%
    filter(str_detect(`CHOICE SCHOOL`, "4012|4013")) %>%
    mutate(`CHOICE SCHOOL` = case_when(
      str_detect(`CHOICE SCHOOL`, "4012_") ~ "4012",
      str_detect(`CHOICE SCHOOL`, "4013_") ~ "4013",
      TRUE ~ `CHOICE SCHOOL`
    )) %>%
    group_by(`STUDENT ID`, GRADE, `CHOICE SCHOOL`, `GUARANTEED?`) %>%
    summarize(
      `CHOICE RANK` = min(`CHOICE RANK`),
      n_accepted = sum(`ASSIGNMENT STATUS` == "Accepted"),
      n_waiting = sum(str_detect(`ASSIGNMENT STATUS`, "Waiting")),
      n_ineligible = sum(`ASSIGNMENT STATUS` == "Ineligible")
    ) %>%
    ungroup()

  special_accepted <-
    special_all %>%
    filter(n_accepted == 1) %>%
    mutate(`ASSIGNMENT STATUS` = "Accepted")

  special_waiting <-
    special_all %>%
    filter(!(`STUDENT ID` %in% special_accepted$`STUDENT ID`)) %>%
    filter(n_waiting >= 1) %>%
    mutate(`ASSIGNMENT STATUS` = "Waiting List")

  special_ineligible <-
    special_all %>%
    filter(!(`STUDENT ID` %in% special_accepted$`STUDENT ID`)) %>%
    filter(!(`STUDENT ID` %in% special_waiting$`STUDENT ID`)) %>%
    filter(n_ineligible >= 1) %>%
    mutate(`ASSIGNMENT STATUS` = "Ineligible")

  special_notprocessed <-
    special_all %>%
    filter(!(`STUDENT ID` %in% special_accepted$`STUDENT ID`)) %>%
    filter(!(`STUDENT ID` %in% special_waiting$`STUDENT ID`)) %>%
    filter(!(`STUDENT ID` %in% special_ineligible$`STUDENT ID`)) %>%
    mutate(`ASSIGNMENT STATUS` = "Not Processed")

  match_clean <-
    match %>%
    filter(str_length(`STUDENT ID`) == 9) %>%
    # filter(str_detect(`CHOICE SCHOOL`, "_[NR]$", negate = TRUE)) %>%
    filter(!str_detect(`CHOICE SCHOOL`, "4012")) %>%
    filter(!str_detect(`CHOICE SCHOOL`, "4013")) %>%
    bind_rows(special_accepted) %>%
    bind_rows(special_waiting) %>%
    bind_rows(special_ineligible) %>%
    bind_rows(special_notprocessed) %>%
    mutate(choice_name = case_when(
      `CHOICE SCHOOL` == "4012" ~ "Lake Forest Elementary Charter School",
      `CHOICE SCHOOL` == "4013" ~ "Lusher Charter School",
      TRUE ~ choice_name
    )) %>%
    select(-c(n_accepted, n_waiting, n_ineligible))

  write_if_bad(match_clean, dir_out)

  # normal briefing summary logic

  match_clean %>%
    filter(!(GRADE %in% grades_ec())) %>%
    matchcalcs_results_seekingnew(
      schools_waitlist = c("846", "847", "4012", "4013"),
      exclude_ineligible = FALSE,
      exclude_notprocessed = FALSE
    ) %>%
    write_excel_csv(glue("{dir_out}/briefing/n_seekingnew_all_k12.csv"), na = "")

  match_clean %>%
    filter(!(GRADE %in% grades_ec())) %>%
    matchcalcs_results_seekingnew(GRADE,
      schools_waitlist = c("846", "847", "4012", "4013"),
      exclude_ineligible = FALSE,
      exclude_notprocessed = FALSE
    ) %>%
    write_excel_csv(glue("{dir_out}/briefing/n_seekingnew_all_by_grade.csv"), na = "")

  match_clean %>%
    filter(GRADE %in% grades_ec()) %>%
    matchcalcs_results_seekingnew(schools_waitlist = c("846", "847", "4012", "4013")) %>%
    write_excel_csv(glue("{dir_out}/briefing/n_seekingnew_eligible_ec.csv"), na = "")

  match_clean %>%
    filter(!(GRADE %in% grades_ec())) %>%
    matchcalcs_results_seekingnew(schools_waitlist = c("846", "847", "4012", "4013")) %>%
    write_excel_csv(glue("{dir_out}/briefing/rate_newplacement_k12.csv"), na = "")

  match_clean %>%
    filter(GRADE %in% c("K", "9")) %>%
    matchcalcs_results_seekingnew(schools_waitlist = c("846", "847", "4012", "4013")) %>%
    write_excel_csv(glue("{dir_out}/briefing/rate_newplacement_k9.csv"), na = "")

  match_clean %>%
    matchcalcs_results_seekingnew(GRADE, schools_waitlist = c("846", "847", "4012", "4013")) %>%
    write_excel_csv(glue("{dir_out}/briefing/rate_newplacement_by_grade.csv"), na = "")

  match_clean %>%
    filter(!(GRADE %in% grades_ec())) %>%
    matchcalcs_results_seekingnew_sibling(schools_waitlist = c("846", "847", "4012", "4013")) %>%
    write_excel_csv(glue("{dir_out}/briefing/rate_newplacement_siblingmatch_k12.csv"), na = "")

  match_clean %>%
    filter(GRADE %in% c("K", "9")) %>%
    matchcalcs_results_seekingnew_sibling(schools_waitlist = c("846", "847", "4012", "4013")) %>%
    write_excel_csv(glue("{dir_out}/briefing/rate_newplacement_siblingmatch_k9.csv"), na = "")

  match_clean %>%
    filter(!(GRADE %in% grades_ec())) %>%
    matchcalcs_results_seekingnew_unassigned(
      schools_waitlist = c("846", "847", "4012", "4013"),
      exclude_ineligible = FALSE,
      exclude_notprocessed = FALSE
    ) %>%
    write_excel_csv(glue("{dir_out}/briefing/unassigned_k12.csv"), na = "")

  match_clean %>%
    matchcalcs_results_seekingnew(
      choice_name, `CHOICE SCHOOL`, GRADE,
      schools_waitlist = c("846", "847", "4012", "4013"),
      exclude_ineligible = FALSE,
      exclude_notprocessed = FALSE
    ) %>%
    write_excel_csv(glue("{dir_out}/briefing/n_seekingnew_all_by_school_grade.csv"), na = "")

  match_clean %>%
    matchcalcs_results_seekingnew(
      choice_name, `CHOICE SCHOOL`, GRADE,
      schools_waitlist = c("846", "847", "4012", "4013")
    ) %>%
    write_excel_csv(glue("{dir_out}/briefing/rate_newplacement_by_school_grade.csv"), na = "")

  match_clean %>%
    matchcalcs_participants_all(schools_waitlist = c("846", "847", "4012", "4013")) %>%
    filter(GRADE %in% grades_ec()) %>%
    summarize(
      n_ec = n(),
      n_eligible_1ormore = sum(n_ineligible < n_choices),
      rate_eligible_1ormore = n_eligible_1ormore / n_ec
    ) %>%
    write_excel_csv(glue("{dir_out}/briefing/eligibility_ec.csv"), na = "")

}


