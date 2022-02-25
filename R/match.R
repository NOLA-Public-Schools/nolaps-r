#' @importFrom magrittr %>%



#' @export
match_lookup_account <- function(x, appschools, accounts) {

  accepted <-
    x %>%
    dplyr::filter(stringr::str_length(`STUDENT ID`) == 9) %>%
    dplyr::distinct(code_appschool = `CHOICE SCHOOL`) %>%
    dplyr::arrange(code_appschool)

  appschools <-
    appschools %>%
    dplyr::select(code_appschool, code_site, id_account) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::distinct()

  accounts <-
    accounts %>%
    dplyr::select(code_site, id_account) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::distinct()

  schools_scholarship <-
    accepted %>%
    dplyr::filter(stringr::str_detect(code_appschool, "_[NR]$")) %>%
    dplyr::mutate(code_site = stringr::str_remove(code_appschool, "_[NR]$")) %>%
    dplyr::left_join(accounts, by = c("code_site")) %>%
    dplyr::select(code_appschool, id_account)

  schools_nonscholarship <-
    accepted %>%
    dplyr::filter(stringr::str_detect(code_appschool, "_[NR]$", negate = TRUE)) %>%
    dplyr::left_join(appschools, by = c("code_appschool")) %>%
    dplyr::select(code_appschool, id_account)

  lookup_account <-
    dplyr::bind_rows(
      schools_scholarship,
      schools_nonscholarship
    )

  lookup_account

}



#' @export
match_augment <- function(x, appschools, accounts, students) {

  # appschools <- getdata_appschool()

  # funding <-
  #   appschools %>%
  #   dplyr::select(code_appschool, choice_funding = ec_type) %>%
  #   dplyr::distinct() %>%
  #   dplyr::filter(!is.na(choice_funding))

  names_matchschool <-
    x %>%
    match_lookup_account(appschools = appschools, accounts = accounts) %>%
    dplyr::left_join(accounts, by = c("id_account")) %>%
    dplyr::select(code_appschool, choice_name = name_account, id_account, is_highdemand) %>%
    dplyr::distinct()

  students <-
    students %>%
    dplyr::left_join(accounts, by = c("id_account_current" = "id_account")) %>%
    dplyr::select(
      oneappid, id_student, directcert_medicaid, directcert_snap,
      grade_current, school_current = name_account, grade_terminal, id_account_current, is_active
    )

  x %>%
    dplyr::left_join(names_matchschool, by = c("CHOICE SCHOOL" = "code_appschool")) %>%
    dplyr::left_join(students, by = c("STUDENT ID" = "oneappid"))

}



#' @export
match_process <- function(args = commandArgs(trailingOnly = TRUE)) {

  dir_in <- args[1]
  dir_out <- args[2]

  dir_external <- glue::glue("{dir_in}/Final Inputs")

  dir_business <- glue::glue("{dir_out}/business")

  dir_review <- glue::glue("{dir_out}/validation")

  if(!dir.exists(dir_business)){dir.create(dir_business)}
  if(!dir.exists(dir_review)){dir.create(dir_review)}

  cat("\nLoading students\n")

  students_recent <- getdata_student_recent()
  students_active <- students_recent %>% dplyr::filter(is_active)

  cat("\nLoading schools\n")

  accounts <- getdata_account()
  appschools <- getdata_appschool()

  cat("\nLoading choices\n")

  apps <- getdata_app(round = "Round 1")
  choices <- getdata_appschoolranking(round = "Round 1")

  match <-
    readr::read_csv(
      glue::glue("{dir_in}/3_MasterMatch.csv"),
      col_types = stringr::str_c(stringr::str_dup("c", 9), stringr::str_dup("i", 1), stringr::str_dup("c", 29))
    ) %>%
    match_augment(appschools = appschools, accounts = accounts, students = students_recent) %>%
    fix_grades()

  # overmatches <- readr::read_csv(
  #   glue::glue("{dir_external}/sibling-overmatches.csv"),
  #   col_types = "ccic"
  # )

  # prioritytable <- readr::read_csv(
  #   glue::glue("{dir_external}/PriorityTable.csv")
  # )

  match %>% readr::write_excel_csv(glue::glue("{dir_review}/000_match_to_review.csv"), na = "")

  results <- match %>% matchcalcs_participants_all(schools_waitlist = c("846", "847"))

  results %>% readr::write_excel_csv(glue::glue("{dir_review}/results_by_student.csv"), na = "")

  match %>%
    matchcalcs_summarystats_full(schools_waitlist = c("846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_review}/summarystats.csv"), na = "")

  # match %>%
  #   matchcalcs_priorityoutcomes() %>%
  #   readr::write_excel_csv(glue::glue("{dir_review}/qualpriorities.csv"), na = "")
  #
  # match %>%
  #   matchcalcs_priorityoutcomes_summary(choice_name, `CHOICE SCHOOL`) %>%
  #   readr::write_excel_csv(glue::glue("{dir_review}/qualpriorities_school.csv"), na = "")
  #
  # match %>%
  #   matchcalcs_priorityoutcomes_summary(choice_name, `CHOICE SCHOOL`, GRADE) %>%
  #   readr::write_excel_csv(glue::glue("{dir_review}/qualpriorities_school_grade.csv"), na = "")



  match_test(
    match = match,
    dir_external = dir_external,
    dir_out = dir_review,
    round = "Round 1",
    students = students_recent,
    apps = apps,
    choices = choices,
    appschools = appschools
  )

  # match_placement(
  #   match = match,
  #   overmatches = NULL,
  #   dir_out = dir_business,
  #   students_recent = students_recent
  # )

  # match_notification(
  #   match = match,
  #   overmatches = NULL,
  #   dir_out = dir_business,
  #   apps = apps,
  #   accounts = accounts,
  #   appschools = appschools,
  #   students_recent = students_recent
  # )

  # match_briefing(
  #   match = match,
  #   dir_out = dir_business
  # )



  cat(glue::glue("\nFinished at {Sys.time()}\n"))

}


