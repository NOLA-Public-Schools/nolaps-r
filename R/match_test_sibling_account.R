#' @import lubridate
#' @import purrr
#' @import readxl
#' @import salesforcer
#' @import stringr
#' @import tidyr


#' @export
test_sibling_account <- function(dir_out, round, match_priorities, students_recent, siblings, appschoolrankings, appschools, apps, accounts, accounts_gradespan) {

  cat("\nNon-verified sibling\n")

  siblings <-
    siblings %>%
    select(
      applicant_oneappid = student_oneappid,
      sibling_oneappid
    )

  asr <- appschoolrankings

  appschools_verifiedsibling <-
    appschools %>%
    filter(str_detect(questions_selective, "Verified Sibling"))

  accounts_guarantee <-
    accounts %>%
    select(id_account_current = id_account, id_account_guarantee) %>%
    left_join(accounts_gradespan, by = join_by(id_account_current == id_account))

  grades_next <- grades_next() %>% filter(grade_current != "12")

  students_active <-
    students_recent %>%
    filter(is_active) %>%
    left_join(accounts_guarantee, by = join_by(id_account_current)) %>%
    left_join(grades_next, by = join_by(grade_current))

  site_codes <-
    accounts %>%
    select(id_account, code_site) %>%
    mutate(code_site_root = str_remove(code_site, "_.+$"))

  underage <-
    students_active %>%
    filter(
      (grade_current == "INF" & student_dob > "2022-09-30")
      | (grade_current == "1YR" & student_dob > "2021-09-30")
      | (grade_current == "2YR" & student_dob > "2020-09-30")
      | (grade_current == "PK3" & student_dob > "2019-09-30")
      | (grade_current == "PK4" & student_dob > "2018-09-30")
    ) %>%
    transmute(
      oneappid,
      name_account_current,
      grade_current,
      grade_applying = grade_current,
      guarantee = id_account_current
    )

  prohibited <-
    students_active %>%
    filter((expelled_status == "Re-entry Prohibited")
           |
             ((expelled_status == "Re-entry Allowed") &
                (expelled_date_end >= "2023-10-01")
             )) %>%
    mutate(is_expelled = TRUE) %>%
    select(oneappid, id_account_expelled, is_expelled)

  shouldhave <-
    students_active %>%
    rowwise() %>%
    mutate(can_roll = (grade_next %in% gradespan_nextyear_vector)) %>%
    ungroup() %>%
    mutate(
      guarantee = case_when(
        can_roll ~ id_account_current,!can_roll &
          grade_current == "12" &
          promotion == "Retained" ~ id_account_current,!can_roll &
          !is_terminalgrade &
          !is.na(id_account_guarantee) ~ id_account_guarantee,!can_roll ~ NA_character_
      )
    ) %>%
    mutate(grade_next = if_else(promotion == "Retained",
                                grade_current,
                                grade_next)) %>%
    filter((grade_current != "12" | promotion == "Retained")) %>%
    select(oneappid,
           name_account_current,
           grade_current,
           grade_applying = grade_next,
           guarantee) %>%
    filter(!(oneappid %in% underage$oneappid)) %>%
    bind_rows(underage) %>%
    filter(!is.na(guarantee)) %>%
    anti_join(prohibited, by = join_by(oneappid, guarantee == id_account_expelled)) %>%
    fix_grades(grade_current) %>%
    fix_grades(grade_applying) %>%
    arrange(name_account_current, grade_current, grade_applying, guarantee)

  guaranteed_siblings <-
    siblings %>%
    filter(applicant_oneappid %in% asr$oneappid) %>%
    filter(sibling_oneappid %in% shouldhave$oneappid) %>%
    left_join(shouldhave, by = join_by(sibling_oneappid == oneappid)) %>%
    left_join(site_codes, by = join_by(guarantee == id_account))

  guaranteed_siblings_distinct <-
    guaranteed_siblings %>%
    select(applicant_oneappid, code_site_guarantee = code_site_root) %>%
    distinct()

  has_sibling <-
    asr %>%
    left_join(site_codes, by = join_by(id_account)) %>%
    left_join(guaranteed_siblings_distinct, by = join_by(oneappid == applicant_oneappid, code_site_root == code_site_guarantee), keep = TRUE) %>%
    filter(!is.na(code_site_guarantee)) %>%
    filter(!(id_account %in% (accounts %>% filter(is_highdemand))$id_account)) %>%
    filter(is.na(questions_selective) | !str_detect(questions_selective, "Verified Sibling")) %>%
    relocate(c(code_site_guarantee))

  ###

  invalid_sibling <-
    match_priorities %>%
    filter(is.na(questions_selective) | !str_detect(questions_selective, "Verified Sibling")) %>%
    anti_join(has_sibling, by = c("id_account", "STUDENT ID" = "applicant_oneappid")) %>%
    filter(!is.na(Sibling))

  missing_sibling <-
    match_priorities %>%
    filter(is.na(questions_selective) | !str_detect(questions_selective, "Verified Sibling")) %>%
    semi_join(has_sibling, by = c("id_account", "STUDENT ID" = "applicant_oneappid")) %>%
    filter(is.na(Sibling)) %>%
    filter(is.na(Ineligible))

  # have <-
  #   match_priorities %>%
  #   filter(is_verifiedsibling) %>%
  #   filter(!is.na(Sibling))

  # cat(
  #   glue(
  #     "
  #     {nrow(distinct(have, `STUDENT ID`))} students
  #     {nrow(distinct(have, `CHOICE SCHOOL`))} schools
  #     \n
  #     "
  #   )
  # )
  #
  # have %>%
  #   count(choice_name, GRADE) %>%
  #   slice_sample(n = nrow(.)) %>%
  #   print()
  #
  # cat("\n")

  test_helper(
    invalid_sibling,
    "No student has an invalid sibling priority (excludes high-demand)."
  )

  test_helper(
    missing_sibling,
    "No student has a missing sibling priority (excludes high-demand)."
  )

  write_if_bad(invalid_sibling, dir_out)
  write_if_bad(missing_sibling, dir_out)

}


