#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @importFrom readr read_rds write_rds
#' @importFrom rlang is_na



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

  special <- tibble::tribble(
    ~code_appschool, ~id_account, ~choice_name,
    "4013_tulane_1", "001d000000ALnWzAAL", "Lusher Charter School",
    "4013_tulane_2", "001d000000ALnWzAAL", "Lusher Charter School",
    "4013_community_1", "001d000000ALnWzAAL", "Lusher Charter School",
    "4013_community_2", "001d000000ALnWzAAL", "Lusher Charter School",
    "4013_ed_1", "001d000000ALnWzAAL", "Lusher Charter School",
    "4012_tier_1", "001d000000ALnWyAAL", "Lake Forest Elementary Charter School",
    "4012_tier_2", "001d000000ALnWyAAL", "Lake Forest Elementary Charter School",
  )

  names_matchschool <-
    x %>%
    match_lookup_account(appschools = appschools, accounts = accounts) %>%
    dplyr::left_join(accounts, by = c("id_account")) %>%
    dplyr::select(code_appschool, choice_name = name_account, id_account, is_highdemand) %>%
    dplyr::distinct()
  # %>%
  #   dplyr::bind_rows(special)

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
  use_cache <- args[3]

  dir_external <- glue::glue("{dir_in}/external")

  dir_business <- glue::glue("{dir_out}/business")

  dir_review <- glue::glue("{dir_out}/validation")

  if(!dir.exists(dir_business)){dir.create(dir_business)}
  if(!dir.exists(dir_review)){dir.create(dir_review)}

  cat("\nLoading data. Please wait.\n")

  if (is_na(use_cache)) {

    accounts <- getdata_account()
    accounts %>% write_rds(glue("{dir_in}/accounts.rds"))

    apps <- getdata_app(round = "Round 1")
    apps %>% write_rds(glue("{dir_in}/apps.rds"))

    appinputs <- getdata_appinput()
    appinputs %>% write_rds(glue("{dir_in}/appinputs.rds"))

    appschools <- getdata_appschool()
    appschools %>% write_rds(glue("{dir_in}/appschools.rds"))

    choices <- getdata_appschoolranking(round = "Round 1")
    choices %>% write_rds(glue("{dir_in}/choices.rds"))

    priorities <- getdata_priority()
    priorities %>% write_rds(glue("{dir_in}/priorities.rds"))

    siblings <- getdata_sibling()
    siblings %>% write_rds(glue("{dir_in}/siblings.rds"))

    students_recent <- getdata_student_recent()
    students_recent %>% write_rds(glue("{dir_in}/students_recent.rds"))

  } else {

    accounts <- read_rds(glue("{dir_in}/accounts.rds"))
    apps <- read_rds(glue("{dir_in}/apps.rds"))
    appinputs <- read_rds(glue("{dir_in}/appinputs.rds"))
    appschools <- read_rds(glue("{dir_in}/appschools.rds"))
    choices <- read_rds(glue("{dir_in}/choices.rds"))
    priorities <- read_rds(glue("{dir_in}/priorities.rds"))
    siblings <- read_rds(glue("{dir_in}/siblings.rds"))
    students_recent <- read_rds(glue("{dir_in}/students_recent.rds"))

  }

  students_active <- students_recent %>% dplyr::filter(is_active)

  cat("\n")

  match <-
    readr::read_csv(
      glue::glue("{dir_in}/3_MasterMatch.csv"),
      col_types = stringr::str_c(stringr::str_dup("c", 9), stringr::str_dup("i", 1), stringr::str_dup("c", 29))
    ) %>%
    match_augment(appschools = appschools, accounts = accounts, students = students_recent) %>%
    fix_grades()

  overmatches <-
    readxl::read_excel(
      glue::glue("{dir_external}/sibling-overmatches.xlsx"),
      col_types = "text"
    ) %>%
    dplyr::select(-`CHOICE RANK`)

  match %>% readr::write_excel_csv(glue::glue("{dir_review}/000_match_to_review.csv"), na = "")

  results <- match %>% matchcalcs_participants_all(schools_waitlist = c("846", "847"))

  results %>% readr::write_excel_csv(glue::glue("{dir_review}/results_by_student.csv"), na = "")

  match %>%
    matchcalcs_summarystats_full(schools_waitlist = c("846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_review}/summarystats.csv"), na = "")

  match %>%
    matchcalcs_priorityoutcomes() %>%
    readr::write_excel_csv(glue::glue("{dir_review}/qualpriorities.csv"), na = "")

  match %>%
    matchcalcs_priorityoutcomes_summary(choice_name, `CHOICE SCHOOL`) %>%
    readr::write_excel_csv(glue::glue("{dir_review}/qualpriorities_school.csv"), na = "")

  match %>%
    matchcalcs_priorityoutcomes_summary(choice_name, `CHOICE SCHOOL`, GRADE) %>%
    readr::write_excel_csv(glue::glue("{dir_review}/qualpriorities_school_grade.csv"), na = "")

  match_test(
    match = match,
    dir_external = dir_external,
    dir_out = dir_review,
    round = "Round 1",
    students = students_recent,
    apps = apps,
    choices = choices,
    appschools = appschools,
    priorities = priorities,
    appinputs = appinputs,
    siblings = siblings
  )

  match_placement(
    match = match,
    overmatches = overmatches,
    dir_out = dir_business,
    students_recent = students_recent,
    appschools = appschools
  )

  match_notification(
    match = match,
    overmatches = overmatches,
    dir_out = dir_business,
    apps = apps,
    accounts = accounts,
    appschools = appschools,
    students_recent = students_recent
  )

  match_briefing(
    match = match,
    dir_out = dir_business
  )



  cat(glue("\n\nFinished at {Sys.time()}\n\n"))

}


