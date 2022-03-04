#' @import dplyr
#' @import lubridate
#' @import readxl
#' @import salesforcer
#' @import stringr

#' @importFrom glue glue glue_safe
#' @importFrom magrittr %>%



#' @export
write_if_bad <- function(x, dir_out) {

  if (nrow(x) > 0) {

    filename <- deparse(substitute(x))
    readr::write_excel_csv(x, glue::glue("{dir_out}/{filename}.csv"), na = "")

  }

}



#' @export
filter_priority <- function(x, priority, prioritytable) {

  doesnthave_all <-
    prioritytable %>%
    dplyr::filter(Grade == "ALL", {{ priority }} == 0) %>%
    dplyr::distinct(`School Code`)

  doesnthave_grade <-
    prioritytable %>%
    dplyr::filter(Grade != "ALL", {{ priority }} == 0) %>%
    dplyr::distinct(`School Code`, Grade)

  x %>%
    dplyr::anti_join(doesnthave_grade, by = c("CHOICE SCHOOL" = "School Code", "GRADE" = "Grade")) %>%
    dplyr::anti_join(doesnthave_all, by = c("CHOICE SCHOOL" = "School Code")) %>%
    dplyr::filter(is.na(Ineligible)) %>%
    dplyr::filter(is.na({{ priority }}))


}



#' @export
match_test <- function(match, dir_external, dir_out, round, students, apps, choices, appschools, priorities, appinputs) {

  # oaretentions <- readr::read_csv(glue::glue("{dir_external}/oa-retentions.csv"), col_types = "c")

  # oaretentions <-
  #   readr::read_csv(
  #     glue::glue("{dir_external}/oa-retentions.csv"),
  #     col_types = "c"
  #   ) %>%
  #   dplyr::select(oneappid = `OneApp ID`)

  # students <-
  #   getdata_student_active() %>%
  #   dplyr::filter(!is.na(id_account_current)) %>%
  #   dplyr::filter(is_terminalgrade == "false") %>%
  #   dplyr::filter(grade_current != 12 | (grade_current == 12 & (oneappid %in% oaretentions$`OneApp ID`))) %>%
  #   dplyr::left_join(accounts, by = c("id_account_current" = "id_account"))

  # students_futureschool <-
  #   students %>%
  #   filter(!is.na(id_account_future))

  apps_with_choices <- apps %>% filter(id_app %in% choices$id_app)

  students_active <-
    students %>%
    filter(is_active)

  match_priorities <-
    match %>%
    matchcalcs_priorityoutcomes() %>%
    left_join(
      choices,
      by = c("id_account", "STUDENT ID" = "oneappid")
    )

  prioritykey <-
    read_excel(
      glue("{dir_external}/priority-key.xlsx"),
      col_types = "text"
    ) %>%
    select(
      code_site = `Site Code`,
      grade_current = `You are in grade`,
      grade_applying = `And you are applying to`,
      guarantee = `Guaranteed (Active Students have guarantee to x)`,
      feeder = `Feeder School (Active Students feed into x)`
    ) %>%
    mutate(code_site = stringr::str_pad(
      code_site, width = 6, side = "left", pad = "0")
    ) %>%
    mutate(code_site = stringr::str_replace(
      code_site,
      "^(36[:digit:]{3})_(.+)",
      "0\\1_\\2"
    ))



# Invalid match records ---------------------------------------------------

  cat("\nInvalid match records\n")

  if (round == "Round 1") {

    # see <-
    #   readr::read_csv(
    #     glue::glue("{dir_external}/additional-student-information-rows.csv")
    #   ) %>%
    #   dplyr::pull(`Student ID`)

    invalid_participants <-
      match %>%
      dplyr::filter(
        !(`STUDENT ID` %in% apps_with_choices$oneappid)
        & !(`STUDENT ID` %in% students_active$oneappid)
        # & !(`STUDENT ID` %in% see)
      ) %>%
      dplyr::select(`CHOICE SCHOOL`, GRADE, `STUDENT ID`) %>%
      dplyr::arrange(`CHOICE SCHOOL`, GRADE, `STUDENT ID`)

    test_text <- "All match records trace back to application with choices or active student."

  } else if (round == "Round 2") {

    invalid_participants <-
      match %>%
      dplyr::filter(
        !(`STUDENT ID` %in% apps_with_choices$oneappid)
        & !(`STUDENT ID` %in% students_futureschool$oneappid)
      ) %>%
      dplyr::select(id_student, `STUDENT ID`, GRADE, `CHOICE SCHOOL`, choice_name) %>%
      dplyr::arrange(choice_name, `CHOICE SCHOOL`, GRADE, `STUDENT ID`)

    test_text <- "All match records trace back to application with choices or recent student with future school."

  }

  testthat::with_reporter(
    "stop", {
      testthat::test_that(test_text, {
        testthat::expect_equal(nrow(invalid_participants), 0)
      })
    }
  )

  write_if_bad(invalid_participants, dir_out)



# Missing match records ---------------------------------------------------

  cat("\nMissing applications\n")

  missing_apps <-
    apps_with_choices %>%
    dplyr::filter(!(oneappid %in% match$`STUDENT ID`)) %>%
    dplyr::select(id_student, oneappid, id_app) %>%
    dplyr::arrange(oneappid)

  test_text <- "All applications with a choice are in the match."

  testthat::with_reporter(
    "stop", {
      testthat::test_that(test_text, {
        testthat::expect_equal(nrow(missing_apps), 0)
      })
    }
  )

  write_if_bad(missing_apps, dir_out)



  cat("\nMissing roll-forwards\n")

  if (round == "Round 1") {

    missing_rollforwards <-
      students_active %>%
      dplyr::filter(!is.na(id_account_current)) %>%
      dplyr::filter(is_terminalgrade == FALSE) %>%
      dplyr::filter(
        grade_current != 12
        # | (grade_current == 12 & (oneappid %in% oaretentions$`OneApp ID`))
      ) %>%
      dplyr::filter(!(oneappid %in% match$`STUDENT ID`)) %>%
      dplyr::select(name_account_current, grade_current, oneappid, id_student) %>%
      dplyr::arrange(name_account_current, grade_current)

    test_text <- "All active students in non-terminal grade are in the match."

  } else if (round == "Round 2") {

    missing_rollforwards <-
      students_futureschool %>%
      dplyr::filter(!(oneappid %in% match$`STUDENT ID`)) %>%
      dplyr::select(id_student, oneappid, grade_future, name_account_future) %>%
      dplyr::arrange(name_account_future, grade_future, oneappid)

    test_text <- "All recent students with future school are in match"

  }

  testthat::with_reporter(
    "stop", {
      testthat::test_that(test_text, {
        testthat::expect_equal(nrow(missing_rollforwards), 0)
      })
    }
  )

  write_if_bad(missing_rollforwards, dir_out)



# Invalid grades -----------------------------------------------------

  cat("\nInvalid grades\n")

  invalid_grades <-
    match %>%
    dplyr::filter(stringr::str_length(`STUDENT ID`) == 9) %>%
    # dplyr::filter(`ELIGIBLE?` == "YES") %>%
    dplyr::left_join(
      getdata_account_gradespan(),
      by = c("id_account")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::filter(!(GRADE %in% gradespan_nextyear_vector)) %>%
    dplyr::ungroup() %>%
    dplyr::select(choice_name, id_account, `CHOICE SCHOOL`, GRADE, `STUDENT ID`, id_student) %>%
    dplyr::arrange(choice_name, GRADE)

  test_text <- "No match record involves a grade that will not exist next year."

  testthat::with_reporter(
    "stop", {
      testthat::test_that(test_text, {
        testthat::expect_equal(nrow(invalid_grades), 0)
      })
    }
  )

  write_if_bad(invalid_grades, dir_out)

  # Priorities

  # Guarantee

  test_guarantee(
    dir_out = dir_out,
    round = round,
    prioritykey = prioritykey,
    match_priorities = match_priorities,
    students = students_active
  )

  # Feeder

  test_feeder(
    dir_out = dir_out,
    round = round,
    prioritykey = prioritykey,
    match_priorities = match_priorities,
    students = students_active,
    choices = choices
  )

  # IEP

  test_iep(
    dir_out = dir_out,
    round = round,
    priorities = priorities,
    appinputs = appinputs,
    match_priorities = match_priorities
  )

  # Military

  test_military(
    dir_out = dir_out,
    round = round,
    priorities = priorities,
    appinputs = appinputs,
    match_priorities = match_priorities
  )

  # French

  test_french(
    dir_out = dir_out,
    round = round,
    priorities = priorities,
    appinputs = appinputs,
    match_priorities = match_priorities
  )

  # Montessori

  test_montessori(
    dir_out = dir_out,
    round = round,
    priorities = priorities,
    appinputs = appinputs,
    match_priorities = match_priorities
  )

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

  # Verified sibling

  test_sibling_verified(
    dir_out = dir_out,
    match_priorities = match_priorities
  )

  # Sibling or staff child

  test_sibling_staffchild(
    dir_out = dir_out,
    match_priorities = match_priorities
  )

  return(NULL)



# Retentions --------------------------------------------------------------

  placements_inactive <- getdata_placement() %>% filter(!is_active)

  retained <-
    students_active %>%
    dplyr::filter(promotion == "Retained") %>%
    dplyr::filter(!(grade_current == "8" & is_t9)) %>%
    dplyr::select(id_student, oneappid, grade_current)

  shouldbe_retained <-
    match %>%
    dplyr::filter(`STUDENT ID` %in% retained$oneappid) %>%
    dplyr::select(`STUDENT ID`, GRADE)

  missing_retained <-
    retained %>%
    dplyr::anti_join(
      shouldbe_retained,
      by = c("oneappid" = "STUDENT ID", "grade_current" = "GRADE")
    ) %>%
    dplyr::anti_join(placements_inactive, by = c("id_student"))

  cat("Missing retentions\n")

  testthat::test_that("All retained students except for rising T9 and placement deactivations are assigned to current grade", {

    testthat::expect_equal(nrow(missing_retained), 0)

  })

  write_if_bad(missing_retained, dir_out)

  invalid_retained <-
    match %>%
    dplyr::filter(is_active & !is.na(id_account_current) & (GRADE == grade_current)) %>%
    dplyr::select(id_student, `STUDENT ID`, GRADE, choice_name) %>%
    dplyr::distinct() %>%
    dplyr::anti_join(retained, by = c("STUDENT ID" = "oneappid", "GRADE" = "grade_current")) %>%
    # dplyr::filter(GRADE != "12") %>%
    dplyr::filter(!(GRADE %in% grades_ec())) %>%
    dplyr::arrange(GRADE)

  cat("Invalid retentions\n")

  testthat::test_that("All students applying to current grade in match are marked retained in Salesforce", {

    testthat::expect_equal(nrow(invalid_retained), 0)

  })

  write_if_bad(invalid_retained, dir_out)



# Eligibility -------------------------------------------------------------

  # TODO
  # gt
  # scholarship

  print("Invalid eligibility")

  dob <-
    students %>%
    dplyr::select(oneappid, student_dob)

  badgrades <-
    readr::read_csv(
      glue::glue("{dir_external}/auto-ineligibilities.csv"),
      col_types = "cc"
    ) %>%
    dplyr::select(code_appschool = `School Code`, grade = `Grade`) %>%
    dplyr::mutate(ineligible_badgrades = TRUE)

  expelled <-
    readr::read_csv(
      glue::glue("{dir_external}/expelled-students.csv"),
      col_types = "ccccccccccccccc"
    ) %>%
    dplyr::select(oneappid = `OneApp ID`, code_appschool = `Current School App Code`) %>%
    dplyr::mutate(ineligible_expelled = TRUE)

  noreturn <-
    readr::read_csv(
      glue::glue("{dir_external}/no-return-students.csv"),
      col_types = "cccccccccccccccccc"
    ) %>%
    dplyr::select(oneappid = `OneApp ID`, code_appschool = `Referring School App Code`) %>%
    dplyr::mutate(ineligible_noreturn = TRUE)

  asr_ineligible <-
    choices %>%
    dplyr::filter(eligibility == "Ineligible") %>%
    dplyr::mutate(ineligible_asr = TRUE)

  ineligibilities <-
    readr::read_csv(
      glue::glue("{dir_external}/student-ineligibilities.csv"),
      col_types = "ccccccc"
    ) %>%
    dplyr::select(oneappid = `OneApp ID`, code_appschool = `App Code`) %>%
    dplyr::mutate(ineligible_ineligibilities = TRUE)

  invalid_eligibility <-
    match %>%
    dplyr::filter(`ASSIGNMENT STATUS` != "Ineligible") %>%
    dplyr::left_join(
      dob,
      by = c("STUDENT ID" = "oneappid")
    ) %>%
    dplyr::left_join(
      badgrades,
      by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")
    ) %>%
    dplyr::left_join(
      expelled,
      by = c("STUDENT ID" = "oneappid", "CHOICE SCHOOL" = "code_appschool")
    ) %>%
    dplyr::left_join(
      noreturn,
      by = c("STUDENT ID" = "oneappid", "CHOICE SCHOOL" = "code_appschool")
    ) %>%
    dplyr::left_join(
      asr_ineligible,
      by = c("STUDENT ID" = "oneappid", "CHOICE SCHOOL" = "code_appschool")
    ) %>%
    dplyr::left_join(
      ineligibilities,
      by = c("STUDENT ID" = "oneappid", "CHOICE SCHOOL" = "code_appschool")
    ) %>%
    dplyr::filter(
      (student_dob > "2016-09-30" & !(GRADE %in% grades_ec()))
      | (student_dob > "2006-09-30" & (`CHOICE SCHOOL` %in% c("315", "702")) & GRADE == "8")
      | (ineligible_badgrades & is.na(`GUARANTEED?`))
      | ineligible_expelled
      | ineligible_noreturn
      | (ineligible_asr & is.na(`GUARANTEED?`))
      | (ineligible_ineligibilities & is.na(`GUARANTEED?`))
    )

  testthat::test_that("No choice is marked eligible in match but ineligible in Salesforce or external input", {

    testthat::expect_equal(nrow(invalid_eligibility), 0)

  })

  write_if_bad(invalid_eligibility, dir_out)



# Family ------------------------------------------------------------------

  optouts <-
    readr::read_csv(
      glue::glue("{dir_external}/family-links-opt-out.csv"),
      col_types = "c"
    ) %>%
    dplyr::pull(`OneApp ID`)

  siblings <- getdata_sibling()

  sibling_edges <-
    siblings %>%
    dplyr::filter(
      student_oneappid %in% choices$oneappid,
      sibling_oneappid %in% choices$oneappid
    ) %>%
    dplyr::select(from = student_oneappid, to = sibling_oneappid) %>%
    dplyr::arrange(from, to)

  families_comp <-
    tidygraph::tbl_graph(edges = sibling_edges, directed = FALSE) %>%
    tidygraph::to_components()

  familify <- function(i) {

    tibble::as_tibble(families_comp[[i]]) %>% dplyr::mutate(id_family = i)

  }

  families <-
    purrr::map_dfr(1:length(families_comp), familify) %>%
    dplyr::rename(oneappid = name)

  students_dob <-
    families %>%
    dplyr::left_join(dob, by = "oneappid")

  twins <-
    students_dob %>%
    dplyr::select(id_family, student_dob) %>%
    dplyr::count(id_family, student_dob, sort = T) %>%
    dplyr::filter(n > 1) %>%
    dplyr::mutate(is_twin = TRUE) %>%
    dplyr::select(-n) %>%
    dplyr::left_join(students_dob, by = c("id_family", "student_dob")) %>%
    dplyr::select(oneappid, is_twin)

  pref_nested <-
    families %>%
    dplyr::left_join(match, by = c("oneappid" = "STUDENT ID")) %>%
    dplyr::left_join(twins, by = "oneappid") %>%
    dplyr::select(id_family, is_twin, oneappid, `CHOICE RANK`, `CHOICE SCHOOL`) %>%
    dplyr::arrange(id_family, is_twin, oneappid, `CHOICE RANK`) %>%
    tidyr::nest(data = c(`CHOICE RANK`, `CHOICE SCHOOL`)) %>%
    dplyr::mutate(n_choices = purrr::map_int(.$data, nrow)) %>%
    dplyr::filter(n_choices > 1 | is_twin) %>%
    dplyr::select(-n_choices)

  count_twin <-
    pref_nested %>%
    dplyr::filter(is_twin) %>%
    dplyr::count(id_family, data, name = "n_twin") %>%
    dplyr::filter(n_twin > 1)

  students_with_family <-
    pref_nested %>%
    dplyr::count(id_family, data) %>%
    dplyr::filter(n > 1) %>%
    dplyr::group_by(id_family) %>%
    dplyr::mutate(subid_family = 1:n()) %>%
    dplyr::left_join(pref_nested, by = c("id_family", "data")) %>%
    dplyr::left_join(count_twin, by = c("id_family", "data")) %>%
    dplyr::mutate(id_family = stringr::str_c(id_family, subid_family, sep = ".")) %>%
    dplyr::mutate(is_twin = dplyr::if_else(is_twin & !is.na(n_twin), TRUE, FALSE)) %>%
    tidyr::replace_na(list(is_twin = FALSE)) %>%
    dplyr::mutate(is_family = TRUE) %>%
    dplyr::select(oneappid, id_family, is_twin, is_family)

  print("Twin")

  invalid_twin <-
    match %>%
    dplyr::left_join(students_with_family, by = c("STUDENT ID" = "oneappid")) %>%
    dplyr::filter(!is.na(`TWIN?`)) %>%
    dplyr::filter(!is_twin) %>%
    dplyr::select(`FAMILY ID`, `STUDENT ID`) %>%
    dplyr::distinct() %>%
    dplyr::arrange(`FAMILY ID`, `STUDENT ID`)

  missing_twin <-
    match %>%
    dplyr::left_join(students_with_family, by = c("STUDENT ID" = "oneappid")) %>%
    dplyr::filter(is.na(`TWIN?`)) %>%
    dplyr::filter(is_twin) %>%
    dplyr::arrange(id_family) %>%
    dplyr::select(id_family, `STUDENT ID`) %>%
    dplyr::distinct()

  testthat::test_that(
    "All siblings with applications, same birthdate, and same match choices are marked as twins",
    {
      testthat::expect_equal(nrow(missing_twin), 0)
    }
  )

  write_if_bad(missing_twin, dir_out)

  testthat::test_that(
    "All match twins are siblings with applications, same birthdate, and same match choices",
    {
      testthat::expect_equal(nrow(invalid_twin), 0)
    }
  )

  write_if_bad(invalid_twin, dir_out)

  print("Family")

  invalid_family <-
    match %>%
    dplyr::left_join(students_with_family, by = c("STUDENT ID" = "oneappid")) %>%
    dplyr::filter(!is.na(`FAMILY ID`)) %>%
    dplyr::filter(!is_family) %>%
    dplyr::select(`FAMILY ID`, `STUDENT ID`) %>%
    dplyr::distinct() %>%
    dplyr::arrange(`FAMILY ID`, `STUDENT ID`)

  missing_family <-
    match %>%
    dplyr::left_join(students_with_family, by = c("STUDENT ID" = "oneappid")) %>%
    dplyr::filter(is.na(`FAMILY ID`)) %>%
    dplyr::filter(is_family) %>%
    dplyr::filter(!(`STUDENT ID` %in% optouts)) %>%
    dplyr::select(id_family, `STUDENT ID`) %>%
    dplyr::distinct() %>%
    dplyr::arrange(id_family, `STUDENT ID`)

  testthat::test_that(
    "All siblings with applications and same match choices are marked as family",
    {
      testthat::expect_equal(nrow(missing_family), 0)
    }
  )

  write_if_bad(missing_family, dir_out)

  testthat::test_that(
    "All match families are siblings with applications and same match choices",
    {
      testthat::expect_equal(nrow(invalid_family), 0)
    }
  )

  write_if_bad(invalid_family, dir_out)

}



# Priority tests ----------------------------------------------------------

#' @export
test_guarantee <- function(dir_out, round, prioritykey, match_priorities, students) {

  cat("\nGuarantee\n")

  key_guarantee <-
    prioritykey %>%
    filter(!is.na(guarantee))

  if (round == "Round 1") {

    shouldhave <-
      students %>%
      select(code_site_current, grade_current, oneappid) %>%
      left_join(
        key_guarantee,
        by = c("code_site_current" = "code_site", "grade_current")
      ) %>%
      filter(!is.na(guarantee))

  } else if (round == "Round 2") {

    shouldhave <-
      students %>%
      select(oneappid, guarantee = id_account_future)

  }

  invalid_guarantee <-
    match_priorities %>%
    filter(str_length(`STUDENT ID`) == 9) %>%
    anti_join(
      shouldhave,
      by = c(
        "CHOICE SCHOOL" = "guarantee",
        "GRADE" = "grade_applying",
        "STUDENT ID" = "oneappid"
      )
    ) %>%
    filter(!is.na(Guaranteed))

  missing_guarantee <-
    match_priorities %>%
    filter(str_length(`STUDENT ID`) == 9) %>%
    semi_join(
      shouldhave,
      by = c(
        "CHOICE SCHOOL" = "guarantee",
        "GRADE" = "grade_applying",
        "STUDENT ID" = "oneappid"
      )
    ) %>%
    filter(is.na(Guaranteed))

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
test_feeder <- function(dir_out, round, prioritykey, match_priorities, students, choices) {

  cat("\nFeeder\n")

  key_feeder <-
    prioritykey %>%
    filter(!is.na(feeder))

  if (round == "Round 1") {

    students <-
      students %>%
      select(code_site_current, grade_current, oneappid)

  } else if (round == "Round 2") {

    students_feeder <-
      students %>%
      dplyr::filter(!is.na(id_account_future) | is_active) %>%
      dplyr::mutate(code_site = dplyr::case_when(
        !is.na(code_site_future) ~ code_site_future,
        TRUE ~ code_site
      )
      ) %>%
      dplyr::filter(code_site %in% key_feeder$code_site) %>%
      dplyr::select(oneappid, code_site, grade_current) %>%
      fix_grades(grade_current)

  }

  shouldhave <-
    choices %>%
    select(code_appschool, grade_applying, oneappid) %>%
    left_join(students, by = "oneappid") %>%
    left_join(
      key_feeder,
      by = c("code_site_current" = "code_site", "grade_current", "grade_applying")
    ) %>%
    mutate(gets_feeder = str_detect(feeder, code_appschool)) %>%
    filter(gets_feeder == TRUE)

  invalid_feeder <-
    match_priorities %>%
    anti_join(
      shouldhave,
      by = c(
        "CHOICE SCHOOL" = "code_appschool",
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
        "CHOICE SCHOOL" = "code_appschool",
        "GRADE" = "grade_applying",
        "STUDENT ID" = "oneappid"
      )
    ) %>%
    filter(is.na(Feeder)) %>%
    filter(is.na(Ineligible))

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



#' @export
test_french <- function(dir_out, round, priorities, appinputs, match_priorities) {

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
    filter(!(`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(!is.na(`School Specific 2`))

  missing_french <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter((`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(is.na(`School Specific 2`)) %>%
    filter(is.na(Ineligible))

  n_have <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!is.na(`School Specific 2`)) %>%
    nrow()

  cat(glue("\n{n_have} records have French priority.\n\n"))

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
test_iep <- function(dir_out, round, priorities, appinputs, match_priorities) {

  cat("\nIEP\n")

  offers_iep <-
    priorities %>%
    filter(!is.na(Order_IEP__c)) %>%
    select(code_appschool, grade)

  appinputs_iep <-
    appinputs %>%
    filter(has_iep)

  invalid_iep <-
    match_priorities %>%
    filter(!is.na(IEP)) %>%
    filter(!(`STUDENT ID` %in% appinputs_iep$oneappid))

  missing_iep <-
    match_priorities %>%
    semi_join(offers_iep, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(is.na(IEP)) %>%
    filter(is.na(Ineligible)) %>%
    filter((`STUDENT ID` %in% appinputs_iep$oneappid))

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
test_montessori <- function(dir_out, round, priorities, appinputs, match_priorities) {

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
    filter(!(`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(!is.na(`School Specific 2`))

  missing_montessori <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter((`STUDENT ID` %in% appinputs$oneappid)) %>%
    filter(is.na(`School Specific 2`)) %>%
    filter(is.na(Ineligible))

  n_have <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(!is.na(`School Specific 2`)) %>%
    nrow()

  cat(glue("\n{n_have} records have Montessori priority.\n\n"))

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
test_military <- function(dir_out, round, priorities, appinputs, match_priorities) {

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
    filter(!is.na(`Military child`)) %>%
    filter(!(`STUDENT ID` %in% appinputs$oneappid))

  missing_military <-
    match_priorities %>%
    semi_join(offers, by = c("CHOICE SCHOOL" = "code_appschool", "GRADE" = "grade")) %>%
    filter(is.na(`Military child`)) %>%
    filter(is.na(Ineligible)) %>%
    filter((`STUDENT ID` %in% appinputs$oneappid))

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
    filter(is_highdemand) %>%
    filter(!is_verifiedsibling) %>%
    filter(!is.na(Sibling))

  missing_sibling_verified <-
    match_priorities %>%
    filter(is_highdemand) %>%
    filter(is_verifiedsibling) %>%
    filter(is.na(Sibling)) %>%
    filter(is.na(Ineligible)) %>%
    filter(is.na(`School Specific 1`))

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
test_sibling_staffchild <- function(dir_out, match_priorities) {

  cat("\nSibling or staff child\n")

  invalid_sibling_staffchild <-
    match_priorities %>%
    filter(!is_verifiedsibling & !is_staffchild) %>%
    filter(!is.na(`School Specific 1`))

  missing_sibling_staffchild <-
    match_priorities %>%
    filter(is_verifiedsibling | is_staffchild) %>%
    filter(`CHOICE SCHOOL` %in% c("796", "797", "798", "846", "847")) %>%
    filter(GRADE %in% grades_ec()) %>%
    filter(is.na(`School Specific 1`)) %>%
    filter(is.na(Ineligible))

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


