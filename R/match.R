#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @importFrom readr read_rds write_rds
#' @importFrom rlang is_na
#'
#' @import dplyr



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
match_process <- function(args = commandArgs(trailingOnly = TRUE)) {

  dir_in <- str_remove(args[str_detect(args, "--in=")], "--in=")
  dir_out <- str_remove(args[str_detect(args, "--out=")], "--out=")
  round <- str_remove(args[str_detect(args, "--round=")], "--round=")
  use_cache <- FALSE

  if (length(args[str_detect(args, "--cache=")] == 1)) {
    if (str_remove(args[str_detect(args, "--cache=")], "--cache=") == "true") {
      use_cache <- TRUE
    }
  }

  dir_external <- glue::glue("{dir_in}/external")

  dir_business <- glue::glue("{dir_out}/business")

  dir_review <- glue::glue("{dir_out}/validation")

  if(!dir.exists(dir_business)){dir.create(dir_business)}
  if(!dir.exists(dir_review)){dir.create(dir_review)}

  cat("\nLoading data. Please wait.\n")

  if (use_cache) {

    #accounts <- read_rds(glue("{dir_in}/accounts.rds"))
    #apps <- read_rds(glue("{dir_in}/apps.rds"))
    #appinputs <- read_rds(glue("{dir_in}/appinputs.rds"))
    #appschools <- read_rds(glue("{dir_in}/appschools.rds"))
    #choices <- read_rds(glue("{dir_in}/choices.rds"))
    #feeders <- read_rds(glue("{dir_in}/feeders.rds"))
    #priorities <- read_rds(glue("{dir_in}/priorities.rds"))
    #siblings <- read_rds(glue("{dir_in}/siblings.rds"))
    #students_recent <- read_rds(glue("{dir_in}/students_recent.rds"))
    gradelevels <- read_rds(glue("{dir_in}/gradelevels.rds"))
    contactsmatch <- read_rds(glue("{dir_in}/contactsmach.rds"))

  } else {

    #accounts <- getdata_account()
    #accounts %>% write_rds(glue("{dir_in}/accounts.rds"))

    #appschools <- getdata_appschool()
    #appschools %>% write_rds(glue("{dir_in}/appschools.rds"))

    #feeders <- getdata_feeder()
    #feeders %>% write_rds(glue("{dir_in}/feeders.rds"))

    #priorities <- getdata_priority()
    #priorities %>% write_rds(glue("{dir_in}/priorities.rds"))

    #apps <- getdata_app(round = round)
    #apps %>% write_rds(glue("{dir_in}/apps.rds"))

    #appinputs <- getdata_appinput(round = round)
    #appinputs %>% write_rds(glue("{dir_in}/appinputs.rds"))

    #choices <- getdata_appschoolranking(round = round)
    #choices %>% write_rds(glue("{dir_in}/choices.rds"))

    #siblings <- getdata_sibling()
    #siblings %>% write_rds(glue("{dir_in}/siblings.rds"))

    #students_recent <- getdata_student_recent()
    #students_recent %>% write_rds(glue("{dir_in}/students_recent.rds"))

    gradelevels <- getdata_gradelevel()
    gradelevels %>% write_rds(glue("{dir_in}/gradelevels.rds"))

    contactsmatch <- getdata_contact_match()
    contactsmatch %>% write_rds(glue("{dir_in}/contactsmatch.rds"))

  }

  #students_active <- students_recent %>% dplyr::filter(is_active)

  cat("\n")

  # Print the directory path
  print(glue::glue("Directory in: {dir_in}"))


  match <-
    readr::read_csv(
      glue::glue("{dir_in}/3_MasterMatch.csv"),
      col_types = stringr::str_c(stringr::str_dup("c", 9), stringr::str_dup("i", 1), stringr::str_dup("c", 29))
    ) %>%
    match_augment(gradelevels = gradelevels, contactsmatch = contactsmatch) %>%
    fix_grades()

  # choices_external <-
  #   readr::read_csv(
  #     glue::glue("{dir_in}/additional-student-preference-priority-rows.csv"),
  #     col_types = stringr::str_c(stringr::str_dup("c", 14))
  #   )
  #
  # overmatches <-
  #   readxl::read_excel(
  #     glue::glue("{dir_external}/sibling-overmatches.xlsx"),
  #     col_types = "text"
  #   )

  match %>% readr::write_excel_csv(glue::glue("{dir_review}/000_match_to_review.csv"), na = "")

  return(match)

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
    round = round,
    students = students_recent,
    apps = apps,
    choices = choices,
    # choices_external = choices_external,
    appschools = appschools,
    priorities = priorities,
    feeders = feeders,
    appinputs = appinputs,
    siblings = siblings,
    accounts = accounts,
    gradelevels = gradelevels,
    contactsmatch = contactsmatch
  )

  match_placement(
    match = match,
    overmatches = NULL,
    dir_out = dir_business,
    students_recent = students_recent,
    appschools = appschools
  )
  #
  # match_notification(
  #   match = match,
  #   overmatches = overmatches,
  #   dir_out = dir_business,
  #   apps = apps,
  #   accounts = accounts,
  #   appschools = appschools,
  #   students_recent = students_recent
  # )
  #
  match_briefing(
    match = match,
    dir_out = dir_business
  )



  cat(glue("\n\nFinished at {Sys.time()}\n\n"))

}


