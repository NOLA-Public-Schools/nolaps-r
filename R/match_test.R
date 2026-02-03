#' @export
match_test <- function(
    dir_review, match,
    gradelevels, contactsmatch, choices, eps_gradelevel, eps_choice) {
  cat("\nValidating match file.\n")

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

  match_test_eligibility_k12(
    dir_review = dir_review,
    match = match,
    choices = choices
  )

  match_test_eligibility_ec(
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
  siblings <- contactsmatch
  students <- contactsmatch
  student_oneappid <- students$oneappid
  sibling_oneappid <- siblings$oneappid

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


# Family tests ------------------------------------------------------------


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
  if (nrow(bad) > 0) {
    cat(glue::glue("WARNING: {test_text}\n"))
    cat(glue::glue("Found {nrow(bad)} failing rows:\n"))
    print(head(bad, 5))
  } else {
    cat(glue::glue("PASS: {test_text}\n"))
  }
}


# write_if_bad <- function(x, dir_out) {
#   if (nrow(x) > 0) {
#     filename <- deparse(substitute(x))
#     write_csv(x, glue("{dir_out}/{filename}.csv"), na = "")
#   }
# }
write_if_bad <- function(x, dir_out) {
  if (nrow(x) > 0) {
    filename <- deparse(substitute(x))
    out_path <- glue("{dir_out}/{filename}.csv")
    write_csv(x, out_path, na = "")
    cat(glue("Wrote {nrow(x)} rows to {out_path}\n"))
    print(head(x, 5))
  } else {
    cat(glue("No rows to write for {deparse(substitute(x))}\n"))
  }
}