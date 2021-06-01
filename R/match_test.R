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
match_test <- function(match, dir_external, dir_out, round, students, apps, choices, appschools) {



  apps_with_choices <- apps %>% dplyr::filter(id_app %in% choices$id_app)

  # accounts <- getdata_account()

  # oaretentions <- readr::read_csv(glue::glue("{dir_external}/oa-retentions.csv"), col_types = "c")

  # students <-
  #   getdata_student_active() %>%
  #   dplyr::filter(!is.na(id_account_current)) %>%
  #   dplyr::filter(is_terminalgrade == "false") %>%
  #   dplyr::filter(grade_current != 12 | (grade_current == 12 & (oneappid %in% oaretentions$`OneApp ID`))) %>%
  #   dplyr::left_join(accounts, by = c("id_account_current" = "id_account"))

  students_futureschool <-
    students %>%
    filter(!is.na(id_account_future))

  students_active <-
    students %>%
    filter(is_active)

  oaretentions <-
    readr::read_csv(
      glue::glue("{dir_external}/oa-retentions.csv"),
      col_types = "c"
    ) %>%
    dplyr::select(oneappid = `OneApp ID`)



# Invalid match records ---------------------------------------------------

  print("Invalid match records")

  if (round == "Round 1") {

    see <-
      readr::read_csv(
        glue::glue("{dir_external}/additional-student-information-rows.csv")
      ) %>%
      dplyr::pull(`Student ID`)

    invalid_participants <-
      match %>%
      dplyr::filter(
        !(`STUDENT ID` %in% apps_with_choices$oneappid)
        & !(`STUDENT ID` %in% students$oneappid)
        & !(`STUDENT ID` %in% see)
      ) %>%
      dplyr::select(`STUDENT ID`, `CHOICE SCHOOL`, GRADE) %>%
      dplyr::arrange(`CHOICE SCHOOL`, GRADE, `STUDENT ID`)

  } else if (round == "Round 2") {

    invalid_participants <-
      match %>%
      dplyr::filter(
        !(`STUDENT ID` %in% apps_with_choices$oneappid)
        & !(`STUDENT ID` %in% students_futureschool$oneappid)
      ) %>%
      dplyr::select(id_student, `STUDENT ID`, GRADE, `CHOICE SCHOOL`, choice_name) %>%
      dplyr::arrange(choice_name, `CHOICE SCHOOL`, GRADE, `STUDENT ID`)

    test_text <- "All match participants trace back to application with choices or recent student with future school"

  }

  testthat::test_that(test_text, {

    testthat::expect_equal(nrow(invalid_participants), 0)

  })

  write_if_bad(invalid_participants, dir_out)



# Missing match records ---------------------------------------------------

  print("Missing applications")

  missing_apps <-
    apps_with_choices %>%
    dplyr::filter(!(oneappid %in% match$`STUDENT ID`)) %>%
    dplyr::select(id_student, oneappid, id_app) %>%
    dplyr::arrange(oneappid)

  testthat::test_that("All applications with a choice are in match", {

    testthat::expect_equal(nrow(missing_apps), 0)

  })

  write_if_bad(missing_apps, dir_out)

  print("Missing roll-forwards")

  if (round == "Round 1") {

    missing_rollforwards <-
      students %>%
      dplyr::filter(!(oneappid %in% match$`STUDENT ID`)) %>%
      dplyr::select(oneappid, id_student, school_current = name_account, grade_current) %>%
      dplyr::arrange(school_current, grade_current, oneappid)

  } else if (round == "Round 2") {

    missing_rollforwards <-
      students_futureschool %>%
      dplyr::filter(!(oneappid %in% match$`STUDENT ID`)) %>%
      dplyr::select(id_student, oneappid, grade_future, name_account_future) %>%
      dplyr::arrange(name_account_future, grade_future, oneappid)

    test_text <- "All recent students with future school are in match"

  }

  testthat::test_that(test_text, {

    testthat::expect_equal(nrow(missing_rollforwards), 0)

  })

  write_if_bad(missing_rollforwards, dir_out)



# Non-existent grades -----------------------------------------------------

  print("Invalid grades")

  # autoineligibilities <-
  #   readr::read_csv(
  #     glue::glue("{dir_external}/auto-ineligibilities.csv"),
  #     col_types = "cc"
  #   )

  invalid_grades <-
    match %>%
    dplyr::left_join(
      getdata_account_gradespan(),
      by = c("id_account")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::filter(!(GRADE %in% gradespan_nextyear_vector)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(stringr::str_length(`STUDENT ID`) == 9) %>%
    dplyr::select(id_student, `STUDENT ID`, GRADE, `CHOICE SCHOOL`, choice_name) %>%
    dplyr::arrange(choice_name, `CHOICE SCHOOL`, GRADE, `STUDENT ID`)

  # %>%
  #   dplyr::anti_join(autoineligibilities, by = c("CHOICE SCHOOL" = "School Code", "GRADE" = "Grade"))

  testthat::test_that("No match record involves a grade that will not exist next year", {

    testthat::expect_equal(nrow(invalid_grades), 0)

  })

  write_if_bad(invalid_grades, dir_out)



# Eligibility -------------------------------------------------------------

  # TODO
  # gt

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
      | ineligible_badgrades
      | ineligible_expelled
      | ineligible_noreturn
      | (ineligible_asr & is.na(`GUARANTEED?`))
      | (ineligible_ineligibilities & is.na(`GUARANTEED?`))
    )

  testthat::test_that("No choice is marked eligible in match but ineligible in Salesforce or external input", {

    testthat::expect_equal(nrow(invalid_eligibility), 0)

  })

  write_if_bad(invalid_eligibility, dir_out)



# Priorities --------------------------------------------------------------

  match_priorities <-
    match %>%
    matchcalcs_priorityoutcomes() %>%
    dplyr::left_join(
      choices,
      by = c("STUDENT ID" = "oneappid", "id_account")
    )

  prioritytable <- readr::read_csv(
    glue::glue("{dir_external}/PriorityTable.csv"),
    col_types = "ccdddddddddddddddddddddddddddddddddd"
  )

  prioritykey <- readr::read_csv(
    glue::glue("{dir_external}/priority-key.csv"),
    col_types = "cccccccccccccccccccc"
  )

# Guarantees --------------------------------------------------------------

  print("Guarantee")

  key_guarantee <-
    prioritykey %>%
    dplyr::select(
      code_site = `Site Code`,
      grade_current = `You are in grade`,
      guarantee = `Guaranteed (Active Students have guarantee to x)`,
    ) %>%
    dplyr::filter(guarantee != "-") %>%
    dplyr::distinct()

  if (round == "Round 1") {

    students_guarantee <-
      getdata_student_active() %>%
      dplyr::filter(code_site %in% key_guarantee$code_site) %>%
      dplyr::select(oneappid, code_site, grade_current) %>%
      fix_grades(grade_current)

    shouldhave <-
      students_guarantee %>%
      dplyr::left_join(key_guarantee, by = c("code_site", "grade_current")) %>%
      dplyr::filter(!is.na(guarantee))

  } else if (round == "Round 2") {

    shouldhave <-
      students_futureschool %>%
      dplyr::select(oneappid, guarantee = id_account_future)

  }

  missing_guarantee <-
    match_priorities %>%
    dplyr::filter(is.na(Guaranteed)) %>%
    dplyr::semi_join(shouldhave, by = c("STUDENT ID" = "oneappid", "id_account" = "guarantee"))
  # %>%
  #   dplyr::anti_join(autoineligibilities, by = c("CHOICE SCHOOL" = "School Code", "GRADE" = "Grade"))

  invalid_guarantee <-
    match_priorities %>%
    dplyr::filter(!is.na(Guaranteed)) %>%
    dplyr::anti_join(shouldhave, by = c("STUDENT ID" = "oneappid", "id_account" = "guarantee")) %>%
    dplyr::filter(stringr::str_length(`STUDENT ID`) == 9)
  # %>%
  #   dplyr::filter(!(`STUDENT ID` %in% oaretentions$`OneApp ID`)) %>%
  #   dplyr::left_join(students_guarantee, by = c("STUDENT ID" = "oneappid")) %>%
  #   dplyr::left_join(codes, by = c("CHOICE SCHOOL" = "code_appschool")) %>%
  #   dplyr::filter(grade_current != 12 | (code_site.x != code_site.y))

  testthat::test_that(
    "Guarantee - everyone has it that should; no one has it that shouldn't", {

      testthat::expect_equal(nrow(missing_guarantee), 0)
      testthat::expect_equal(nrow(invalid_guarantee), 0)

    })

  write_if_bad(missing_guarantee, dir_out)
  write_if_bad(invalid_guarantee, dir_out)



# Priorities --------------------------------------------------------------

  # Feeder

  print("Feeder")

  key_feeder <-
    prioritykey %>%
    dplyr::select(
      code_site = `Site Code`,
      grade_current = `You are in grade`,
      grade_applying = `And you are applying to`,
      feeder = `Feeder School (Active Students feed into x)`
    ) %>%
    dplyr::filter(feeder != "-") %>%
    dplyr::distinct()

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

  asr_feeder <-
    choices %>%
    dplyr::filter(oneappid %in% students_feeder$oneappid) %>%
    dplyr::select(oneappid, grade_applying, code_appschool) %>%
    fix_grades(grade_applying)

  shouldhave <-
    asr_feeder %>%
    dplyr::left_join(students_feeder, by = "oneappid") %>%
    dplyr::left_join(key_feeder, by = c("code_site", "grade_current", "grade_applying")) %>%
    dplyr::relocate(c(code_site, grade_current), .before = grade_applying) %>%
    dplyr::mutate(gets_feeder = stringr::str_detect(feeder, code_appschool)) %>%
    dplyr::filter(gets_feeder == TRUE)

  missing_feeder <-
    match_priorities %>%
    filter_priority(Feeder, prioritytable) %>%
    dplyr::semi_join(shouldhave, by = c("STUDENT ID" = "oneappid", "CHOICE SCHOOL" = "code_appschool"))

  invalid_feeder <-
    match_priorities %>%
    dplyr::filter(!is.na(Feeder)) %>%
    dplyr::anti_join(shouldhave, by = c("STUDENT ID" = "oneappid", "CHOICE SCHOOL" = "code_appschool"))

  testthat::test_that(
    "Feeder - everyone has it that should; no one has it that shouldn't", {

      testthat::expect_equal(nrow(missing_feeder), 0)
      testthat::expect_equal(nrow(invalid_feeder), 0)

    })

  write_if_bad(missing_feeder, dir_out)
  write_if_bad(invalid_feeder, dir_out)



  # IEP

  print("IEP")

  iep <- readr::read_csv(glue::glue("{dir_external}/iep.csv"), col_types = "c")

  missing_iep <-
    match_priorities %>%
    filter_priority(IEP, prioritytable) %>%
    dplyr::filter(
      (`STUDENT ID` %in% iep$`OneApp ID`)
    )

  invalid_iep <-
    match_priorities %>%
    dplyr::filter(!is.na(IEP)) %>%
    dplyr::filter(
      !(`STUDENT ID` %in% iep$`OneApp ID`)
    )

  testthat::test_that(
    "IEP - everyone has it that should; no one has it that shouldn't", {

      testthat::expect_equal(nrow(missing_iep), 0)
      testthat::expect_equal(nrow(invalid_iep), 0)

    })

  write_if_bad(missing_iep, dir_out)
  write_if_bad(invalid_iep, dir_out)

  return(NULL)



  # Distance

  print("Distance")

  validatedgeo <- readr::read_csv(
    glue::glue("{dir_external}/validated-geo.csv")
  )

  missing_distance <-
    match_priorities %>%
    filter_priority(`Child of Student`, prioritytable) %>%
    dplyr::filter(
      (is_priority_distance & !(`CHOICE SCHOOL` %in% c("323", "324")))
      | (is_priority_distance & (`CHOICE SCHOOL` %in% c("323", "324")) & `STUDENT ID` %in% validatedgeo$`OneApp ID`)
    )

  invalid_distance <-
    match_priorities %>%
    dplyr::filter(!is.na(`Child of Student`)) %>%
    dplyr::filter(!is_priority_distance)

  testthat::test_that(
    "Distance - everyone has it that should; no one has it that shouldn't", {

      testthat::expect_equal(nrow(missing_distance), 0)
      testthat::expect_equal(nrow(invalid_distance), 0)

  })

  write_if_bad(missing_distance, dir_out)
  write_if_bad(invalid_distance, dir_out)

  # Zone

  print("Zone")

  missing_zone <-
    match_priorities %>%
    filter_priority(`Geography`, prioritytable) %>%
    dplyr::filter(
      (is_priority_zone & !(`CHOICE SCHOOL` %in% c("323", "324")))
      | (is_priority_zone & (`CHOICE SCHOOL` %in% c("323", "324")) & `STUDENT ID` %in% validatedgeo$`OneApp ID`)
    )

  invalid_zone <-
    match_priorities %>%
    dplyr::filter(!is.na(`Geography`)) %>%
    dplyr::filter(
      (!is_priority_zone)
      | (is_priority_zone & (`CHOICE SCHOOL` %in% c("323", "324")) & !(`STUDENT ID` %in% validatedgeo$`OneApp ID`))
    )

  testthat::test_that(
    "Zone - everyone has it that should; no one has it that shouldn't", {

      testthat::expect_equal(nrow(missing_zone), 0)
      testthat::expect_equal(nrow(invalid_zone), 0)

    })

  write_if_bad(missing_zone, dir_out)
  write_if_bad(invalid_zone, dir_out)



}


