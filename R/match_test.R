#' @export
match_test <- function(
    dir_review, match,
    gradelevels, contactsmatch, choices, eps_gradelevel, eps_choice) {
  cat("\nValidating match file\n")

  students_active <- contactsmatch |> filter(.data$is_active)

  match_test_grades(
    dir_review = dir_review,
    match = match
  )

  match_test_age(
    dir_review = dir_review,
    match = match
  )

  match_test_choices(
    dir_review = dir_review,
    match = match,
    choices = choices
  )

  match_test_guarantee(
    dir_review = dir_review,
    match = match,
    students_active = students_active
  )

  match_test_priorities(
    dir_review = dir_review,
    match = match,
    eps_gradelevel = eps_gradelevel,
    eps_choice = eps_choice
  )

  return(NULL)


  # Family link and twin data

  dob <-
    students %>%
    select(oneappid, student_dob)

  sibling_edges <-
    siblings %>%
    filter(
      student_oneappid %in% choices$oneappid,
      sibling_oneappid %in% choices$oneappid
    ) %>%
    select(from = student_oneappid, to = sibling_oneappid) %>%
    arrange(from, to)

  families_comp <-
    tbl_graph(edges = sibling_edges, directed = FALSE) %>%
    to_components()

  familify <- function(i) {
    as_tibble(families_comp[[i]]) %>% mutate(id_family = i)
  }

  families <-
    map_dfr(1:length(families_comp), familify) %>%
    rename(oneappid = name)

  students_dob <-
    families %>%
    left_join(dob, by = "oneappid")

  twins <-
    students_dob %>%
    select(id_family, student_dob) %>%
    count(id_family, student_dob, sort = T) %>%
    filter(n > 1) %>%
    mutate(is_twin = TRUE) %>%
    select(-n) %>%
    left_join(students_dob, by = c("id_family", "student_dob")) %>%
    select(oneappid, is_twin)

  pref_nested <-
    families %>%
    left_join(match, by = c("oneappid" = "STUDENT ID")) %>%
    left_join(twins, by = "oneappid") %>%
    select(id_family, is_twin, oneappid, `CHOICE RANK`, `CHOICE SCHOOL`) %>%
    arrange(id_family, is_twin, oneappid, `CHOICE RANK`) %>%
    nest(data = c(`CHOICE RANK`, `CHOICE SCHOOL`)) %>%
    mutate(n_choices = map_int(.$data, nrow)) %>%
    filter(n_choices > 1 | is_twin) %>%
    select(-n_choices)

  count_twin <-
    pref_nested %>%
    filter(is_twin) %>%
    count(id_family, data, name = "n_twin") %>%
    filter(n_twin > 1)

  students_with_family <-
    pref_nested %>%
    count(id_family, data) %>%
    filter(n > 1) %>%
    group_by(id_family) %>%
    mutate(subid_family = 1:n()) %>%
    left_join(pref_nested, by = c("id_family", "data")) %>%
    left_join(count_twin, by = c("id_family", "data")) %>%
    mutate(id_family = stringr::str_c(id_family, subid_family, sep = ".")) %>%
    mutate(is_twin = dplyr::if_else(is_twin & !is.na(n_twin), TRUE, FALSE)) %>%
    replace_na(list(is_twin = FALSE)) %>%
    mutate(is_family = TRUE) %>%
    select(oneappid, id_family, is_twin, is_family)

  # Begin tests

  ###

  # Retentions

  # test_retentions(
  #   dir_out = dir_out,
  #   match = match,
  #   students_active = students_active
  # )

  # Eligibility tests

  # Eligibility

  test_eligibility(
    dir_out = dir_out,
    match = match,
    choices = choices,
    appinputs = appinputs
  )

  # Expulsions

  test_expulsion(
    dir_out = dir_out,
    match = match,
    students = students_active
  )

  # Family tests

  # Family link

  test_family(
    dir_out = dir_out,
    match = match,
    siblings = siblings,
    students_with_family = students_with_family,
    appinputs = appinputs
  )

  # Twin

  test_twin(
    dir_out = dir_out,
    match = match,
    siblings = siblings,
    students_with_family = students_with_family
  )
}


# Basic data quality tests ------------------------------------------------


#' @export
test_retentions <- function(dir_out, match, students_active) {
  cat("\nRetentions\n")

  retained <-
    students_active %>%
    filter(promotion == "Retained") %>%
    filter(!(grade_current == "8" & is_t9)) %>%
    select(id_student, oneappid, grade_current)

  invalid_retained <-
    match %>%
    filter(is_active & (GRADE == grade_current)) %>%
    select(GRADE, `STUDENT ID`, id_student) %>%
    distinct() %>%
    anti_join(
      retained,
      by = c("STUDENT ID" = "oneappid", "GRADE" = "grade_current")
    ) %>%
    filter(!(GRADE %in% grades_ec())) %>%
    filter(GRADE != "12") %>%
    arrange(GRADE)

  test_helper(
    invalid_retained,
    "All students applying to current grade in match are marked as retained."
  )

  write_if_bad(invalid_retained, dir_out)

  missing_retained <-
    retained %>%
    anti_join(
      match,
      by = c("oneappid" = "STUDENT ID", "grade_current" = "GRADE")
    ) %>%
    filter(!(grade_current %in% grades_ec()))

  test_helper(
    missing_retained,
    "All retained students except for T9 are in the match for their current grade."
  )

  write_if_bad(missing_retained, dir_out)
}



# Eligibility tests -------------------------------------------------------


#' @export
test_eligibility <- function(dir_out, match, choices, appinputs) {
  cat("\nEligibility\n")

  appinputs_iep <-
    appinputs %>%
    filter(has_iep)

  appinputs_gt <-
    appinputs %>%
    filter(has_gt)

  match <-
    match %>%
    # mutate(`CHOICE SCHOOL` = str_remove(
    #   `CHOICE SCHOOL`, "_((tulane)|(community)|(ed)|(tier))_[12]$"
    #   )
    # ) %>%
    left_join(
      choices,
      by = c("STUDENT ID" = "oneappid", "CHOICE SCHOOL" = "code_appschool")
    ) %>%
    select(
      `ELIGIBLE?`, `GUARANTEED?`,
      id_account.x, choice_name, `CHOICE SCHOOL`, GRADE, `STUDENT ID`, id_appschoolranking,
      eligibility, eligibility_decision, programtype, is_selective, questions_selective
    ) %>%
    distinct() %>%
    arrange(`ELIGIBLE?`, choice_name, GRADE)

  invalid_eligibility_ec <-
    match %>%
    filter(GRADE %in% grades_ec()) %>%
    filter(
      eligibility != "Eligible"
      # & !(str_detect(programtype, "Tuition") & eligibility != "Ineligible")
      # & !(programtype == "EC Special Needs" & `STUDENT ID` %in% appinputs_iep$oneappid & eligibility != "Ineligible")
      # & !(programtype == "LA4 & 8(g) OPSB" & `STUDENT ID` %in% appinputs_iep$oneappid & eligibility != "Ineligible")
      # & !(programtype == "PK4 - Type II" & `STUDENT ID` %in% appinputs_iep$oneappid & eligibility != "Ineligible")
      # & !(programtype == "PK GT" & `STUDENT ID` %in% appinputs_gt$oneappid & eligibility != "Ineligible")
    ) %>%
    filter(`ELIGIBLE?` == "YES") %>%
    filter(is.na(`GUARANTEED?`))

  invalid_eligibility_k12 <-
    match %>%
    filter(!(GRADE %in% grades_ec())) %>%
    filter(eligibility_decision != "Eligible") %>%
    filter(is_selective & !(questions_selective %in% c(
      "Verified Sibling",
      "Financial Eligibility;Verified Sibling"
    ))) %>%
    filter(`ELIGIBLE?` == "YES") %>%
    filter(is.na(`GUARANTEED?`))

  #

  invalid_grades <-
    match %>%
    left_join(
      getdata_account_gradespan(),
      by = join_by(id_account.x == id_account)
    ) %>%
    rowwise() %>%
    filter(!(GRADE %in% gradespan_nextyear_vector)) %>%
    ungroup() %>%
    pull(`STUDENT ID`)

  #

  missing_eligibility_ec <-
    match %>%
    filter(GRADE %in% grades_ec()) %>%
    filter(
      eligibility == "Eligible" |
        ((eligibility != "Ineligible") & (
          str_detect(programtype, "Tuition") |
            (programtype == "EC Special Needs" & `STUDENT ID` %in% appinputs_iep$oneappid) |
            (programtype == "LA4 & 8(g) OPSB" & `STUDENT ID` %in% appinputs_iep$oneappid) |
            (programtype == "PK4 - Type II" & `STUDENT ID` %in% appinputs_iep$oneappid) |
            (programtype == "PK GT" & `STUDENT ID` %in% appinputs_gt$oneappid)
        ))
    ) %>%
    filter(`ELIGIBLE?` == "NO") %>%
    filter(!(`STUDENT ID` %in% invalid_grades))

  cat(
    glue(
      "
      {nrow(invalid_eligibility_ec)} records with invalid EC eligibility
      {nrow(invalid_eligibility_k12)} records with invalid K-12 eligibility

      {nrow(missing_eligibility_ec)} records with missing EC eligibility
      \n
      "
    )
  )

  test_helper(
    invalid_eligibility_ec,
    "No ineligible EC applicants are marked eligible in the match."
  )

  test_helper(
    invalid_eligibility_k12,
    "No ineligible K-12 applicants are marked eligible in the match."
  )

  test_helper(
    missing_eligibility_ec,
    "No eligible EC applicants are marked ineligible in the match."
  )

  write_if_bad(invalid_eligibility_ec, dir_out)
  write_if_bad(invalid_eligibility_k12, dir_out)

  write_if_bad(missing_eligibility_ec, dir_out)
}



#' @export
test_expulsion <- function(dir_out, match, students) {
  cat("\nExpulsions\n")

  prohibited <-
    students %>%
    filter(
      (expelled_status == "Re-entry Prohibited") |
        ((expelled_status == "Re-entry Allowed") & (expelled_date_end >= "2023-10-01"))
    ) %>%
    mutate(is_expelled = TRUE) %>%
    select(oneappid, id_account_expelled, is_expelled)

  invalid_return_prohibited <-
    match %>%
    left_join(prohibited, by = c("STUDENT ID" = "oneappid", "id_account" = "id_account_expelled")) %>%
    filter(is_expelled) %>%
    filter(`ELIGIBLE?` == "YES")

  test_helper(
    invalid_return_prohibited,
    "No prohibited student is marked eligible in the match."
  )

  write_if_bad(invalid_return_prohibited, dir_out)
}



# Family tests ------------------------------------------------------------



#' @export
test_family <- function(dir_out, siblings, match, students_with_family, appinputs) {
  cat("\nFamily link\n")

  optouts <-
    appinputs %>%
    filter(optout_family)

  diff_1 <-
    match %>%
    mutate(`CHOICE SCHOOL` = str_remove(`CHOICE SCHOOL`, "_.+$")) %>%
    distinct(`FAMILY ID`, `STUDENT ID`, `CHOICE SCHOOL`) %>%
    group_by(`STUDENT ID`) %>%
    mutate(`CHOICE RANK` = 1:n()) %>%
    filter(!is.na(`FAMILY ID`), `CHOICE RANK` == 1) %>%
    distinct(`FAMILY ID`, `CHOICE SCHOOL`) %>%
    count(`FAMILY ID`) %>%
    filter(n > 1)

  diff_2 <-
    match %>%
    mutate(`CHOICE SCHOOL` = str_remove(`CHOICE SCHOOL`, "_.+$")) %>%
    distinct(`FAMILY ID`, `STUDENT ID`, `CHOICE SCHOOL`) %>%
    group_by(`STUDENT ID`) %>%
    mutate(`CHOICE RANK` = 1:n()) %>%
    filter(!is.na(`FAMILY ID`), `CHOICE RANK` == 2) %>%
    distinct(`FAMILY ID`, `CHOICE SCHOOL`) %>%
    count(`FAMILY ID`) %>%
    filter(n > 1)

  invalid_family <-
    match %>%
    left_join(students_with_family, by = c("STUDENT ID" = "oneappid")) %>%
    filter(!is_family) %>%
    filter(!is.na(`FAMILY ID`)) %>%
    select(`FAMILY ID`, `STUDENT ID`) %>%
    distinct() %>%
    arrange(`FAMILY ID`, `STUDENT ID`)

  missing_family <-
    match %>%
    left_join(students_with_family, by = c("STUDENT ID" = "oneappid")) %>%
    filter(is_family) %>%
    filter(is.na(`FAMILY ID`)) %>%
    filter(!(`STUDENT ID` %in% optouts$oneappid)) %>%
    select(id_family, `STUDENT ID`) %>%
    distinct() %>%
    arrange(id_family, `STUDENT ID`)

  missing_subfamily <-
    match %>%
    filter(!is.na(`FAMILY ID`)) %>%
    select(`FAMILY ID`, `STUDENT ID`, `CHOICE RANK`, `CHOICE SCHOOL`) %>%
    nest(data = c(`CHOICE RANK`, `CHOICE SCHOOL`)) %>%
    distinct(`FAMILY ID`, data) %>%
    count(`FAMILY ID`) %>%
    filter(n > 1) %>%
    filter(`FAMILY ID` %in% diff_1$`FAMILY ID` | `FAMILY ID` %in% diff_2$`FAMILY ID`)

  test_helper(
    invalid_family,
    "All match families are siblings with applications and same match choices."
  )

  test_helper(
    missing_family,
    "All siblings with applications and same match choices are marked as family."
  )

  test_helper(
    missing_subfamily,
    "All applicants within a family have the same first and second choice."
  )

  write_if_bad(invalid_family, dir_out)
  write_if_bad(missing_family, dir_out)
  write_if_bad(missing_subfamily, dir_out)
}



#' @export
test_twin <- function(dir_out, siblings, match, students_with_family) {
  cat("\nTwin\n")

  invalid_twin <-
    match %>%
    left_join(students_with_family, by = c("STUDENT ID" = "oneappid")) %>%
    filter(!is_twin) %>%
    filter(!is.na(`TWIN?`)) %>%
    select(`FAMILY ID`, `STUDENT ID`) %>%
    distinct() %>%
    arrange(`FAMILY ID`, `STUDENT ID`)

  missing_twin <-
    match %>%
    left_join(students_with_family, by = c("STUDENT ID" = "oneappid")) %>%
    filter(is_twin) %>%
    filter(is.na(`TWIN?`)) %>%
    select(id_family, `STUDENT ID`) %>%
    distinct() %>%
    arrange(id_family, `STUDENT ID`)

  test_helper(
    invalid_twin,
    "All match twins are siblings with applications, same birthdate, and same match choices."
  )

  test_helper(
    missing_twin,
    "All siblings with applications, same birthdate, and same match choices are marked as twins."
  )

  write_if_bad(invalid_twin, dir_out)
  write_if_bad(missing_twin, dir_out)
}


# Utils -------------------------------------------------------------------


test_helper <- function(bad, test_text) {
  testthat::with_reporter(
    "stop",
    {
      testthat::test_that(test_text, {
        testthat::expect_equal(nrow(bad), 0)
      })
    }
  )
}


write_if_bad <- function(x, dir_out) {
  if (nrow(x) > 0) {
    filename <- deparse(substitute(x))
    write_csv(x, glue("{dir_out}/{filename}.csv"), na = "")
  }
}
