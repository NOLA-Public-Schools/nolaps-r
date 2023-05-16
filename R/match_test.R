#' @import dplyr
#' @import lubridate
#' @import purrr
#' @import readxl
#' @import salesforcer
#' @import stringr
#' @import tidyr

#' @importFrom glue glue glue_safe
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom tidygraph tbl_graph to_components



#' @export
match_test <- function(
  match, dir_external, dir_out, round,
  students, apps, choices,
  choices_external = NULL, appschools, priorities, feeders, appinputs, siblings,
  accounts
  ) {

  cat("\nValidating match file\n")

  # Data preparation

  apps_with_choices <- apps %>% filter(id_app %in% choices$id_app)

  students_app <- apps_with_choices$id_student

  accounts_gradespan <- getdata_account_gradespan()

  accounts_guarantee <-
    accounts %>%
    select(id_account_current = id_account, id_account_guarantee) %>%
    left_join(
      accounts_gradespan, by = join_by(id_account_current == id_account)
    )

  grades_next <- grades_next() %>% filter(grade_current != "12")

  students_active <-
    students %>%
    filter(is_active) %>%
    left_join(accounts_guarantee, by = join_by(id_account_current)) %>%
    left_join(grades_next, by = join_by(grade_current))

  students_futureschool <-
    students %>%
    filter(!is.na(id_account_future))

  match_priorities <-
    match %>%
    matchcalcs_priorityoutcomes() %>%
    left_join(
      choices,
      by = c("id_account", "STUDENT ID" = "oneappid")
    )

  accounts_gradespan <- getdata_account_gradespan()

  accounts_guarantee <-
    accounts %>%
    select(id_account_current = id_account, id_account_guarantee) %>%
    left_join(
      accounts_gradespan, by = join_by(id_account_current == id_account)
    )

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

  # Basic data quality tests

  test_participants(
    dir_out = dir_out,
    round = round,
    match = match,
    apps_with_choices = apps_with_choices,
    students_active = students_active,
    students_futureschool = students_futureschool,
    choices_external = choices_external
  )

  # Invalid choices and rank numbering

  test_ranks(
    dir_out = dir_out,
    match = match,
    apps_with_choices = apps_with_choices,
    choices = choices
  )

  # Invalid grades

  test_grades(
    dir_out = dir_out,
    match = match
  )

  # Retentions

  # test_retentions(
  #   dir_out = dir_out,
  #   match = match,
  #   students_active = students_active
  # )

  # Eligibility tests

  # Age

  test_age(
    dir_out = dir_out,
    match = match,
    dob = dob
  )

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

  # Priorities

  # Current-school priorities

  # Guarantee

  test_guarantee(
    dir_out = dir_out,
    round = round,
    match_priorities = match_priorities,
    students_active = students_active,
    students_futureschool = students_futureschool,
    dob = dob,
    choices_external = choices_external
  )

  # Closing school

  # test_closing(
  #   dir_out = dir_out,
  #   priorities = priorities,
  #   match_priorities = match_priorities,
  #   students_active = students_active,
  #   students_app = students_app
  # )

  # Feeder

  # test_feeder(
  #   dir_out = dir_out,
  #   feeders = feeders,
  #   match_priorities = match_priorities,
  #   students_active = students_active,
  #   choices = choices
  # )

  # Application priorities

  # 100% FPL

  test_100fpl(
    dir_out = dir_out,
    priorities = priorities,
    appinputs = appinputs,
    match_priorities = match_priorities
  )

  # Economic disadvantage

  # test_disadvantage(
  #   dir_out = dir_out,
  #   priorities = priorities,
  #   appinputs = appinputs,
  #   match_priorities = match_priorities
  # )

  # IEP

  test_iep(
    dir_out = dir_out,
    priorities = priorities,
    appinputs = appinputs,
    match_priorities = match_priorities
  )

  # French

  # test_french(
  #   dir_out = dir_out,
  #   priorities = priorities,
  #   appinputs = appinputs,
  #   match_priorities = match_priorities
  # )

  # Montessori

  # test_montessori(
  #   dir_out = dir_out,
  #   priorities = priorities,
  #   appinputs = appinputs,
  #   match_priorities = match_priorities
  # )

  # Military

  # test_military(
  #   dir_out = dir_out,
  #   priorities = priorities,
  #   appinputs = appinputs,
  #   match_priorities = match_priorities
  # )

  # UNO

  # test_uno(
  #   dir_out = dir_out,
  #   priorities = priorities,
  #   appinputs = appinputs,
  #   match_priorities = match_priorities
  # )

  # Choice-specific priorities

  # Distance

  test_distance(
    dir_out = dir_out,
    match_priorities = match_priorities
  )

  # Zone

  test_zone(
    dir_out = dir_out,
    match_priorities = match_priorities
  )

  # Non-verified siblings

  # test_sibling_account(
  #   dir_out = dir_out,
  #   round = round,
  #   match_priorities = match_priorities,
  #   students_recent = students,
  #   siblings = siblings,
  #   appschoolrankings = choices,
  #   appschools = appschools,
  #   apps = apps_with_choices,
  #   accounts = accounts,
  #   accounts_gradespan = accounts_gradespan
  # )

  test_sibling(
    dir_out = dir_out,
    round = round,
    match_priorities = match_priorities,
    students_recent = students,
    siblings = siblings,
    appschoolrankings = choices,
    appschools = appschools,
    apps = apps_with_choices,
    accounts = accounts,
    priorities = priorities
  )

  # Verified sibling

  # test_sibling_verified(
  #   dir_out = dir_out,
  #   match_priorities = match_priorities
  # )

  # Staff child

  test_staffchild(
    dir_out = dir_out,
    match_priorities = match_priorities
  )

  # Sibling or staff child

  # test_sibling_staffchild(
  #   dir_out = dir_out,
  #   match_priorities = match_priorities
  # )

  # Assignment tests

  # Assignment status

  test_assignment(dir_out = dir_out, match = match)

  # end tests

}



# Basic data quality tests ------------------------------------------------



#' @export
test_participants <- function(dir_out, round, match, students_active, students_futureschool, apps_with_choices, choices_external = NULL) {

  cat("\nInvalid match records\n")

  if (round == "Round 1") {

    invalid_participants <-
      match %>%
      filter(
        !(`STUDENT ID` %in% apps_with_choices$oneappid) &
        !(`STUDENT ID` %in% students_active$oneappid)
      ) %>%
      select(choice_name, `CHOICE SCHOOL`, GRADE, `STUDENT ID`, id_student) %>%
      arrange(choice_name, `CHOICE SCHOOL`, GRADE)
    # %>%
    #   anti_join(choices_external, by = join_by(
    #     `STUDENT ID` == `Student ID`,
    #     `CHOICE SCHOOL` == `School Code`
    #   ))

    test_text <- "All match records trace back to application with choices or active student."

  } else if (round == "Round 2") {

    invalid_participants <-
      match %>%
      filter(
        !(`STUDENT ID` %in% apps_with_choices$oneappid) &
        !(`STUDENT ID` %in% students_futureschool$oneappid)
      ) %>%
      select(choice_name, `CHOICE SCHOOL`, GRADE, `STUDENT ID`, id_student) %>%
      arrange(choice_name, `CHOICE SCHOOL`, GRADE)

    test_text <- "All match records trace back to application with choices or recent student with future school."

  }

  cat(
    glue(
      "
      {nrow(invalid_participants)} records with invalid participants
      \n
      "
    )
  )

  test_helper(
    invalid_participants,
    test_text
  )

  write_if_bad(invalid_participants, dir_out)


  cat("\nMissing applications\n")

  missing_apps <-
    apps_with_choices %>%
    filter(!(oneappid %in% match$`STUDENT ID`)) %>%
    select(id_app, id_student, oneappid)

  cat(
    glue(
      "
      {nrow(missing_apps)} missing applications
      \n
      "
    )
  )

  test_helper(
    missing_apps,
    "All applications with a choice are in the match."
  )

  write_if_bad(missing_apps, dir_out)

  cat("\nMissing roll-forwards\n")

  if (round == "Round 1") {

    missing_rollforwards <-
      students_active %>%
      filter(!is.na(id_account_current)) %>%
      filter(is_terminalgrade == FALSE) %>%
      filter(
        grade_current != 12
      ) %>%
      filter(!(oneappid %in% match$`STUDENT ID`)) %>%
      filter(!(grade_current %in% c("INF", "1YR", "2YR", "PK3"))) %>%
      filter(student_dob <= "2018-09-30") %>%
      select(name_account_current, grade_current, oneappid, id_student) %>%
      arrange(name_account_current, grade_current)

    test_text <- "All active students in non-terminal grade are in the match."

  } else if (round == "Round 2") {

    missing_rollforwards <-
      students_futureschool %>%
      filter(!(oneappid %in% match$`STUDENT ID`)) %>%
      select(name_account_future, grade_future, oneappid, id_student) %>%
      arrange(name_account_future, grade_future)

    # EC Round 2

    missing_rollforwards <-
      missing_rollforwards %>%
      filter(grade_future %in% grades_ec())

    #

    test_text <- "All recent students with future school are in the match."

  }

  cat(
    glue(
      "
      {nrow(missing_rollforwards)} missing roll-forwards
      \n
      "
    )
  )

  test_helper(
    missing_rollforwards,
    test_text
  )

  write_if_bad(missing_rollforwards, dir_out)

}



#' @export
test_ranks <- function(dir_out, match, apps_with_choices, choices) {

  cat("\nChoices and rank ordering\n")

  match_clean <-
    match %>%
    filter(`STUDENT ID` %in% apps_with_choices$oneappid) %>%
    mutate(`CHOICE SCHOOL` = str_remove(
      `CHOICE SCHOOL`, "_((tulane)|(community)|(ed)|(tier))_[12]$"
      )
    ) %>%
    group_by(`STUDENT ID`) %>%
    arrange(`CHOICE RANK`) %>%
    select(`STUDENT ID`, `CHOICE SCHOOL`) %>%
    ungroup() %>%
    distinct() %>%
    group_by(`STUDENT ID`) %>%
    mutate(rank = 1:n())

  choices <-
    choices %>%
    mutate(rank = as.numeric(str_remove(rank, "Rank ")))

  invalid_choices <-
    match_clean %>%
    anti_join(choices, by = c(
      "STUDENT ID" = "oneappid",
      "CHOICE SCHOOL" = "code_appschool",
      "rank"
    )) %>%
    left_join(match, by = c("STUDENT ID", "CHOICE SCHOOL")) %>%
    filter(`GUARANTEED?` != "YES")

  missing_choices <-
    choices %>%
    anti_join(match_clean, by = c(
      "oneappid" = "STUDENT ID",
      "code_appschool" = "CHOICE SCHOOL",
      "rank"
    ))

  test_helper(
    invalid_choices,
    "Every match record traces back to application or guarantee school."
  )

  write_if_bad(invalid_choices, dir_out)

  test_helper(
    missing_choices,
    "Every application choice is reflected in the match."
  )

  write_if_bad(missing_choices, dir_out)



  cat("\nInvalid rank numbers\n")

  invalid_ranks <-
    match %>%
    filter(`CHOICE RANK` > 20)

  cat(
    glue(
      "
      {nrow(invalid_ranks)} records with invalid ranks
      \n
      "
    )
  )

  test_helper(
    invalid_ranks,
    "No match record has a rank larger than 20."
  )

  write_if_bad(invalid_ranks, dir_out)

}



#' @export
test_grades <- function(dir_out, match) {

  cat("\nInvalid grades\n")

  invalid_grades <-
    match %>%
    left_join(
      getdata_account_gradespan(),
      by = c("id_account")
    ) %>%
    rowwise() %>%
    filter(!(GRADE %in% gradespan_nextyear_vector)) %>%
    ungroup() %>%
    filter(
      !(`CHOICE SCHOOL` %in% c(
        "4013_tulane_1",
        "4013_tulane_2",
        "4013_community_1",
        "4013_community_2",
        "4013_ed_1",
        "4012_tier_1",
        "4012_tier_2"
      ))
    ) %>%
    select(`ELIGIBLE?`, choice_name, GRADE, `STUDENT ID`, id_student, id_account) %>%
    arrange(`ELIGIBLE?`, choice_name, GRADE)

  invalid_grades_eligible <-
    invalid_grades %>%
    filter(`ELIGIBLE?` == "YES")

  cat(
    glue(
      "
      {nrow(invalid_grades)} records with invalid grades
      {nrow(invalid_grades_eligible)} eligible records with invalid grades
      \n
      "
    )
  )

  test_helper(
    invalid_grades_eligible,
    "No eligible match record involves a grade that will not exist next year."
  )

  write_if_bad(invalid_grades, dir_out)
  write_if_bad(invalid_grades_eligible, dir_out)

}



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
      retained, by = c("STUDENT ID" = "oneappid", "GRADE" = "grade_current")
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
test_age <- function(dir_out, match, dob) {

  cat("\nAge\n")

  match <-
    match %>%
    left_join(dob, by = c("STUDENT ID" = "oneappid"))

  invalid_ages <-
    match %>%
    filter(
      (GRADE == "INF" & student_dob <= "2022-09-30")
      | (GRADE == "1YR" & student_dob > "2022-09-30")
      | (GRADE == "2YR" & student_dob > "2021-09-30")
      | (GRADE == "PK3" & student_dob > "2020-09-30")
      | (GRADE == "PK4" & student_dob > "2019-09-30")
      | (!(GRADE %in% grades_ec()) & student_dob > "2018-09-30")
      | (((`CHOICE SCHOOL` %in% c("315", "702", "703")) & GRADE == "8") & student_dob > "2008-09-30")
    ) %>%
    select(`ELIGIBLE?`, choice_name, GRADE, school_current, `STUDENT ID`, student_dob, id_student) %>%
    arrange(`ELIGIBLE?`, choice_name, GRADE, school_current)

  invalid_ages_eligible <-
    invalid_ages %>%
    filter(`ELIGIBLE?` == "YES")

  cat(
    glue(
      "
      {nrow(invalid_ages)} records with invalid ages
      {nrow(invalid_ages_eligible)} eligible records with invalid ages
      \n
      "
    )
  )

  test_helper(
    invalid_ages_eligible,
    "No student missing an age cutoff is marked eligible."
  )

  write_if_bad(invalid_ages, dir_out)
  write_if_bad(invalid_ages_eligible, dir_out)

}



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
      choice_name, `CHOICE SCHOOL`, GRADE, `STUDENT ID`, id_appschoolranking,
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

  missing_eligibility_ec <-
    match %>%
    filter(GRADE %in% grades_ec()) %>%
    filter(
      eligibility == "Eligible"
      | ((eligibility != "Ineligible") & (
        str_detect(programtype, "Tuition")
        | (programtype == "EC Special Needs" & `STUDENT ID` %in% appinputs_iep$oneappid)
        | (programtype == "LA4 & 8(g) OPSB" & `STUDENT ID` %in% appinputs_iep$oneappid)
        | (programtype == "PK4 - Type II" & `STUDENT ID` %in% appinputs_iep$oneappid)
        | (programtype == "PK GT" & `STUDENT ID` %in% appinputs_gt$oneappid)
      ))
    ) %>%
    filter(`ELIGIBLE?` == "NO") %>%
    filter(`CHOICE SCHOOL` != "835")

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
      (expelled_status == "Re-entry Prohibited")
      | ((expelled_status == "Re-entry Allowed") & (expelled_date_end >= "2023-10-01"))
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



# Priority tests ----------------------------------------------------------

# Current-school priorities -----------------------------------------------



#' @export
test_guarantee <- function(
    dir_out, round, match_priorities, students_active, students_futureschool, dob, choices_external = NULL
    ) {

  cat("\nGuarantee\n")

  if (round == "Round 1") {

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
        name_account_current, grade_current,
        grade_applying = grade_current, guarantee = id_account_current
      )

    prohibited <-
      students_active %>%
      filter(
        (expelled_status == "Re-entry Prohibited")
        | ((expelled_status == "Re-entry Allowed") & (expelled_date_end >= "2023-10-01"))
      ) %>%
      mutate(is_expelled = TRUE) %>%
      select(oneappid, id_account_expelled, is_expelled)

    return_prohibited <-
      match_priorities %>%
      select(`STUDENT ID`, id_account) %>%
      left_join(
        prohibited, by = c("STUDENT ID" = "oneappid", "id_account" = "id_account_expelled")
      ) %>%
      filter(is_expelled)

    shouldhave <-
      students_active %>%
      rowwise() %>%
      mutate(can_roll = (grade_next %in% gradespan_nextyear_vector)) %>%
      ungroup() %>%
      mutate(guarantee = case_when(
        can_roll ~ id_account_current,
        !can_roll &
          grade_current == "12" &
          promotion == "Retained" ~ id_account_current,
        !can_roll &
          !is_terminalgrade &
          !is.na(id_account_guarantee) ~ id_account_guarantee,
        !can_roll ~ NA_character_
        )
      ) %>%
      mutate(
        grade_next = if_else(
          promotion == "Retained",
          grade_current,
          grade_next
        )
      ) %>%
      mutate(
        grade_next = if_else(
          id_account_current == "001d000000ALnWRAA1" & grade_current == "PK3" & student_dob <= "2018-09-30",
          "K",
          grade_next
        )
      ) %>%
      filter((grade_current != "12" | promotion == "Retained")) %>%
      select(
        oneappid,
        name_account_current, grade_current,
        grade_applying = grade_next, guarantee,
        governance
      ) %>%
      filter(!(oneappid %in% underage$oneappid)) %>%
      bind_rows(underage) %>%
      filter(!is.na(guarantee))

    invalid_guarantee <-
      match_priorities %>%
      anti_join(
        shouldhave,
        by = c(
          "id_account" = "guarantee",
          "GRADE" = "grade_applying",
          "STUDENT ID" = "oneappid"
        )
      ) %>%
      filter(!is.na(Guaranteed))
    # %>%
    #   anti_join(choices_external, by = join_by(
    #     `STUDENT ID` == `Student ID`,
    #     `CHOICE SCHOOL` == `School Code`
    #   ))

    have <-
      match_priorities %>%
      filter(!is.na(Guaranteed))

    missing_guarantee <-
      shouldhave %>%
      anti_join(
        have,
        by = c(
          "guarantee" = "id_account",
          "grade_applying" = "GRADE",
          "oneappid" = "STUDENT ID"
        )
      ) %>%
      anti_join(
        return_prohibited,
        by = c(
          "guarantee" = "id_account",
          "oneappid" = "STUDENT ID"
        )
      ) %>%
      filter(!(grade_applying %in% grades_ec())) %>%
      # filter(!(oneappid %in% choices_external$`Student ID`)) %>%
      filter(governance != "Scholarship") %>%
      arrange(name_account_current, grade_current, grade_applying, guarantee)

  } else if (round == "Round 2") {

    underage <-
      students_futureschool %>%
      filter(
        (grade_future == "1YR" & student_dob > "2022-09-30") |
        (grade_future == "2YR" & student_dob > "2021-09-30") |
        (grade_future == "PK3" & student_dob > "2020-09-30") |
        (grade_future == "PK4" & student_dob > "2019-09-30") |
        (grade_future == "K" & student_dob > "2018-09-30")
      ) %>%
      transmute(
        id_account_guarantee = id_account_current,
        name_account_future = name_account_current,
        grade_future = grade_current,
        oneappid,
        id_student
      )

    return_prohibited <-
      students_futureschool %>%
      filter(
        (expelled_status == "Re-entry Prohibited")
        | ((expelled_status == "Re-entry Allowed") & (expelled_date_end > "2023-10-01"))
      ) %>%
      mutate(is_expelled = TRUE) %>%
      select(oneappid, id_account_expelled)

    shouldhave <-
      students_futureschool %>%
      select(
        id_account_guarantee = id_account_future,
        name_account_future,
        grade_future,
        oneappid,
        id_student
      ) %>%
      filter(!(oneappid %in% underage$oneappid)) %>%
      bind_rows(underage) %>%
      filter(grade_future %in% grades_ec())

    invalid_guarantee <-
      match_priorities %>%
      anti_join(
        shouldhave,
        by = c(
          "id_account" = "id_account_guarantee",
          "GRADE" = "grade_future",
          "STUDENT ID" = "oneappid"
        )
      ) %>%
      filter(!is.na(Guaranteed)) %>%
      arrange(choice_name, GRADE)

    have <-
      match_priorities %>%
      filter(!is.na(Guaranteed))

    missing_guarantee <-
      shouldhave %>%
      anti_join(
        have,
        by = c(
          "id_account_guarantee" = "id_account",
          "grade_future" = "GRADE",
          "oneappid" = "STUDENT ID"
        )
      ) %>%
      anti_join(
        return_prohibited,
        by = c(
          "id_account_guarantee" = "id_account_expelled",
          "oneappid"
        )
      ) %>%
      arrange(name_account_future, grade_future)

  }

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      \n
      "
    )
  )

  print(
    count(have, GRADE)
  )

  cat("\n")

  test_helper(
    invalid_guarantee,
    "No student has an invalid guarantee."
  )

  test_helper(
    missing_guarantee,
    "No student has a missing guarantee."
  )

  write_if_bad(invalid_guarantee, dir_out)
  write_if_bad(missing_guarantee, dir_out)

}



#' @export
test_closing <- function(dir_out, priorities, match_priorities, students_active, students_app) {

  cat("\nClosing school\n")

  offers <-
    priorities %>%
    filter(!is.na(Order_Closing_Public__c)) %>%
    select(code_appschool, grade)

  shouldhave <-
    students_active %>%
    filter(is_priority_closing) %>%
    filter(id_student %in% students_app) %>%
    filter(grade_current != grade_terminal) %>%
    filter(!(grade_current %in% grades_ec())) %>%
    select(oneappid)

  invalid_closing <-
    match_priorities %>%
    anti_join(
      shouldhave,
      by = c("STUDENT ID" = "oneappid")
    ) %>%
    filter(!is.na(`Closing Public School`))

  missing_closing <-
    match_priorities %>%
    semi_join(
      shouldhave,
      by = c("STUDENT ID" = "oneappid")
    ) %>%
    semi_join(
      offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")
    ) %>%
    filter(is.na(`Closing Public School`)) %>%
    filter(is.na(Ineligible)) %>%
    filter(!(`CHOICE SCHOOL` %in% c(
      "4012",
      "4012_tier_1",
      "4012_tier_2"
    )))

  have <-
    match_priorities %>%
    filter(!is.na(`Closing Public School`))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      \n
      "
    )
  )

  print(
    count(distinct(have, `STUDENT ID`, GRADE), GRADE)
  )

  cat("\n")

  test_helper(
    invalid_closing,
    "No student has an invalid closing school priority."
  )

  test_helper(
    missing_closing,
    "No student has a missing closing school priority."
  )

  write_if_bad(invalid_closing, dir_out)
  write_if_bad(missing_closing, dir_out)

}



#' @export
test_feeder <- function(dir_out, feeders, match_priorities, students_active, choices) {

  cat("\nFeeder\n")

  shouldhave <-
    choices %>%
    filter(id_student %in% students_active$id_student) %>%
    select(oneappid, id_account_current, id_account, grade_applying) %>%
    semi_join(
      feeders,
      by = c("id_account_current", "id_account" = "id_account_applying", "grade_applying")
    )

  invalid_feeder <-
    match_priorities %>%
    anti_join(
      shouldhave,
      by = c(
        "id_account",
        "GRADE" = "grade_applying",
        "STUDENT ID" = "oneappid"
      )
    ) %>%
    filter(!is.na(Feeder))

  missing_feeder <-
    match_priorities %>%
    semi_join(
      shouldhave,
      by = c(
        "id_account",
        "GRADE" = "grade_applying",
        "STUDENT ID" = "oneappid"
      )
    ) %>%
    filter(is.na(Feeder)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    filter(!is.na(Feeder))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_feeder,
    "No student has an invalid feeder."
  )

  test_helper(
    missing_feeder,
    "No student has a missing feeder."
  )

  write_if_bad(invalid_feeder, dir_out)
  write_if_bad(missing_feeder, dir_out)

}



# Application priorities --------------------------------------------------



#' @export
test_100fpl <- function(dir_out, priorities, appinputs, match_priorities) {

  cat("\n100% FPL\n")

  offers <-
    priorities %>%
    filter(!is.na(Order_100_Federal_Poverty__c)) %>%
    select(code_appschool, grade)

  appinputs <-
    appinputs %>%
    filter(has_100fpl)

  invalid_100fpl <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!(`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(!is.na(`Military child`))

  missing_100fpl <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter((`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(is.na(`Military child`)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!is.na(`Military child`))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_100fpl,
    "No student has an invalid 100% FPL priority."
  )

  test_helper(
    missing_100fpl,
    "No student has a missing 100% FPL priority."
  )

  write_if_bad(invalid_100fpl, dir_out)
  write_if_bad(missing_100fpl, dir_out)

}



#' @export
test_disadvantage <- function(dir_out, priorities, appinputs, match_priorities) {

  cat("\nEconomic disadvantage\n")

  offers <-
    priorities %>%
    filter(!is.na(Order_Disadvantage__c)) %>%
    select(code_appschool, grade)

  appinputs <-
    appinputs %>%
    filter(has_disadvantage)

  invalid_disadvantage <-
    match_priorities %>%
    filter(!(`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(!is.na(`At Risk`))

  missing_disadvantage <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter((`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(is.na(`At Risk`)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    filter(!is.na(`At Risk`))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_disadvantage,
    "No student has an invalid economic disadvantage priority."
  )

  test_helper(
    missing_disadvantage,
    "No student has a missing economic disadvantage priority."
  )

  write_if_bad(invalid_disadvantage, dir_out)
  write_if_bad(missing_disadvantage, dir_out)

}



#' @export
test_french <- function(dir_out, priorities, appinputs, match_priorities) {

  cat("\nFrench\n")

  offers <-
    priorities %>%
    filter(!is.na(Order_French__c)) %>%
    select(code_appschool, grade)

  appinputs <-
    appinputs %>%
    filter(has_french)

  invalid_french <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!(`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(!is.na(`School Specific 2`))

  missing_french <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter((`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(is.na(`School Specific 2`)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!is.na(`School Specific 2`))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_french,
    "No student has an invalid French priority."
  )

  test_helper(
    missing_french,
    "No student has a missing French priority."
  )

  write_if_bad(invalid_french, dir_out)
  write_if_bad(missing_french, dir_out)

}



#' @export
test_iep <- function(dir_out, priorities, appinputs, match_priorities) {

  cat("\nIEP\n")

  offers <-
    priorities %>%
    filter(!is.na(Order_IEP__c)) %>%
    select(code_appschool, grade)

  appinputs <-
    appinputs %>%
    filter(has_iep)

  invalid_iep <-
    match_priorities %>%
    filter(!(`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(!is.na(IEP))

  missing_iep <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter((`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(is.na(IEP)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    filter(!is.na(IEP))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_iep,
    "No student has an invalid IEP priority."
  )

  test_helper(
    missing_iep,
    "No student has a missing IEP priority."
  )

  write_if_bad(invalid_iep, dir_out)
  write_if_bad(missing_iep, dir_out)

}



#' @export
test_montessori <- function(dir_out, priorities, appinputs, match_priorities) {

  cat("\nMontessori\n")

  offers <-
    priorities %>%
    filter(!is.na(Order_Montessori__c)) %>%
    select(code_appschool, grade)

  appinputs <-
    appinputs %>%
    filter(has_montessori)

  invalid_montessori <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!(`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(!is.na(`School Specific 2`))

  missing_montessori <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter((`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(is.na(`School Specific 2`)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!is.na(`School Specific 2`))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_montessori,
    "No student has an invalid Montessori priority."
  )

  test_helper(
    missing_montessori,
    "No student has a missing Montessori priority."
  )

  write_if_bad(invalid_montessori, dir_out)
  write_if_bad(missing_montessori, dir_out)

}



#' @export
test_military <- function(dir_out, priorities, appinputs, match_priorities) {

  cat("\nMilitary\n")

  offers <-
    priorities %>%
    filter(!is.na(Order_Military__c)) %>%
    select(code_appschool, grade)

  appinputs <-
    appinputs %>%
    filter(has_military)

  invalid_military <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!(`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(!is.na(`Military child`))

  missing_military <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter((`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(is.na(`Military child`)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!is.na(`Military child`))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_military,
    "No student has an invalid military priority."
  )

  test_helper(
    missing_military,
    "No student has a missing military priority."
  )

  write_if_bad(invalid_military, dir_out)
  write_if_bad(missing_military, dir_out)

}



#' @export
test_uno <- function(dir_out, priorities, appinputs, match_priorities) {

  cat("\nUNO\n")

  offers <-
    priorities %>%
    filter(!is.na(Order_UNO_Staff__c)) %>%
    select(code_appschool, grade)

  appinputs <-
    appinputs %>%
    filter(has_uno)

  invalid_uno <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!(`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(!is.na(`School Specific 1`))

  missing_uno <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter((`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(is.na(`School Specific 1`)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!is.na(`School Specific 1`))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_uno,
    "No student has an invalid UNO priority."
  )

  test_helper(
    missing_uno,
    "No student has a missing UNO priority."
  )

  write_if_bad(invalid_uno, dir_out)
  write_if_bad(missing_uno, dir_out)

}



#' @export
test_distance <- function(dir_out, match_priorities) {

  cat("\nDistance\n")

  invalid_distance <-
    match_priorities %>%
    filter(!is_priority_distance) %>%
    filter(!is.na(`Child of Student`))

  missing_distance <-
    match_priorities %>%
    filter(is_priority_distance) %>%
    filter(is.na(`Child of Student`)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    filter(!is.na(`Child of Student`))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_distance,
    "No student has an invalid distance priority."
  )

  test_helper(
    missing_distance,
    "No student has a missing distance priority."
  )

  write_if_bad(invalid_distance, dir_out)
  write_if_bad(missing_distance, dir_out)

}



#' @export
test_zone <- function(dir_out, match_priorities) {

  cat("\nZone\n")

  invalid_zone <-
    match_priorities %>%
    filter(!is_priority_zone) %>%
    filter(!is.na(Geography))

  missing_zone <-
    match_priorities %>%
    filter(is_priority_zone) %>%
    filter(is.na(Geography)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    filter(!is.na(Geography))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_zone,
    "No student has an invalid zone priority."
  )

  test_helper(
    missing_zone,
    "No student has a missing zone priority."
  )

  write_if_bad(invalid_zone, dir_out)
  write_if_bad(missing_zone, dir_out)

}



#' @export
test_sibling_verified <- function(dir_out, match_priorities) {

  cat("\nVerified sibling\n")

  invalid_sibling_verified <-
    match_priorities %>%
    # filter(is_highdemand) %>%
    filter(!is_verifiedsibling) %>%
    filter(!is.na(Sibling))

  missing_sibling_verified <-
    match_priorities %>%
    # filter(is_highdemand) %>%
    filter(is_verifiedsibling) %>%
    filter(is.na(Sibling)) %>%
    filter(is.na(Ineligible))
  # %>%
  #   filter(is.na(`School Specific 1`))

  have <-
    match_priorities %>%
    filter(is_verifiedsibling) %>%
    filter(!is.na(Sibling))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_sibling_verified,
    "No student has an invalid sibling priority (high-demand only)."
  )

  test_helper(
    missing_sibling_verified,
    "No student has a missing sibling priority (high-demand only)."
  )

  write_if_bad(invalid_sibling_verified, dir_out)
  write_if_bad(missing_sibling_verified, dir_out)

}



#' @export
test_staffchild <- function(dir_out, match_priorities) {

  cat("\nStaff child\n")

  invalid_staffchild <-
    match_priorities %>%
    filter(!is_staffchild) %>%
    filter(!is.na(`Staff Child`))

  missing_staffchild <-
    match_priorities %>%
    filter(is_staffchild) %>%
    filter(!(`CHOICE SCHOOL` %in% c("796", "797", "798", "846", "847"))) %>%
    filter(GRADE %in% grades_ec()) %>%
    filter(is.na(`Staff Child`)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    filter(!is.na(`Staff Child`))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_staffchild,
    "No student has an invalid staff child priority."
  )

  test_helper(
    missing_staffchild,
    "No student has a missing staff child priority."
  )

  write_if_bad(invalid_staffchild, dir_out)
  write_if_bad(missing_staffchild, dir_out)

}



#' @export
test_sibling_staffchild <- function(dir_out, match_priorities) {

  cat("\nSibling or staff child\n")

  invalid_sibling_staffchild <-
    match_priorities %>%
    filter(`CHOICE SCHOOL` %in% c("796", "797", "798", "846", "847")) %>%
    filter(!is_verifiedsibling & !is_staffchild) %>%
    filter(!is.na(`School Specific 1`))

  missing_sibling_staffchild <-
    match_priorities %>%
    filter(`CHOICE SCHOOL` %in% c("796", "797", "798", "846", "847")) %>%
    filter(GRADE %in% grades_ec()) %>%
    filter(is_verifiedsibling | is_staffchild) %>%
    filter(is.na(`School Specific 1`)) %>%
    filter(is.na(Ineligible))

  have <-
    match_priorities %>%
    filter(`CHOICE SCHOOL` %in% c("796", "797", "798", "846", "847")) %>%
    filter(GRADE %in% grades_ec()) %>%
    filter(!is.na(`School Specific 1`))

  cat(
    glue(
      "
      {nrow(distinct(have, `STUDENT ID`))} students
      {nrow(distinct(have, `CHOICE SCHOOL`))} schools
      \n
      "
    )
  )

  have %>%
    count(choice_name, GRADE) %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  test_helper(
    invalid_sibling_staffchild,
    "No student has an invalid sibling or staff child priority."
  )

  test_helper(
    missing_sibling_staffchild,
    "No student has a missing sibling or staff child priority."
  )

  write_if_bad(invalid_sibling_staffchild, dir_out)
  write_if_bad(missing_sibling_staffchild, dir_out)

}



# Assignment tests --------------------------------------------------------



#' @export
test_assignment <- function(dir_out, match) {

  cat("\nAssignment status\n")

  assignments_all <-
    match %>%
    count(choice_name, `CHOICE SCHOOL`, GRADE, `ASSIGNMENT STATUS`) %>%
    pivot_wider(names_from = "ASSIGNMENT STATUS", values_from = "n") %>%
    select(
      school = choice_name,
      code_appschool = `CHOICE SCHOOL`,
      grade = GRADE,
      accepted = Accepted,
      ineligible = Ineligible,
      waiting = `Waiting List`,
      waiting_family = `Waiting List - Family Link Rejection`,
      notprocessed = `Not Processed`
    )

  noassignments <-
    assignments_all %>%
    filter(is.na(accepted))

  allineligible <-
    assignments_all %>%
    filter(
      !is.na(ineligible),
      is.na(accepted),
      is.na(waiting),
      is.na(waiting_family)
    )

  cat("\nGrades with no assignments\n")

  noassignments %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  cat("\nGrades with no eligible students processed\n")

  allineligible %>%
    slice_sample(n = nrow(.)) %>%
    print()

  cat("\n")

  write_if_bad(assignments_all, dir_out)
  write_if_bad(noassignments, dir_out)
  write_if_bad(allineligible, dir_out)

}



# Utility functions -------------------------------------------------------



#' @export
test_helper <- function(bad_table, test_text) {

  testthat::with_reporter(
    "stop", {
      testthat::test_that(test_text, {
        testthat::expect_equal(nrow(bad_table), 0)
      })
    }
  )

}



#' @export
write_if_bad <- function(x, dir_out) {

  if (nrow(x) > 0) {

    filename <- deparse(substitute(x))
    readr::write_excel_csv(x, glue::glue("{dir_out}/{filename}.csv"), na = "")

  }

}


