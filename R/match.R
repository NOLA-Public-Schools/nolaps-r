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

  dir_external <- glue("{dir_in}/external")

  dir_business <- glue("{dir_out}/business")

  dir_review <- glue("{dir_out}/validation")

  if (!dir.exists(dir_business)) {
    dir.create(dir_business)
  }
  if (!dir.exists(dir_review)) {
    dir.create(dir_review)
  }

  cat("\nLoading data. Please wait.\n")

  if (use_cache) {
    # accounts <- read_rds(glue("{dir_in}/accounts.rds"))
    # apps <- read_rds(glue("{dir_in}/apps.rds"))
    # appinputs <- read_rds(glue("{dir_in}/appinputs.rds"))
    # appschools <- read_rds(glue("{dir_in}/appschools.rds"))
    # choices <- read_rds(glue("{dir_in}/choices.rds"))
    # feeders <- read_rds(glue("{dir_in}/feeders.rds"))
    # priorities <- read_rds(glue("{dir_in}/priorities.rds"))
    # siblings <- read_rds(glue("{dir_in}/siblings.rds"))
    # students_recent <- read_rds(glue("{dir_in}/students_recent.rds"))

    gradelevels <- read_rds(glue("{dir_in}/gradelevels.rds"))
    contactsmatch <- read_rds(glue("{dir_in}/contactsmatch.rds"))
  } else {
    # accounts <- getdata_account()
    # accounts %>% write_rds(glue("{dir_in}/accounts.rds"))

    # appschools <- getdata_appschool()
    # appschools %>% write_rds(glue("{dir_in}/appschools.rds"))

    # feeders <- getdata_feeder()
    # feeders %>% write_rds(glue("{dir_in}/feeders.rds"))

    # priorities <- getdata_priority()
    # priorities %>% write_rds(glue("{dir_in}/priorities.rds"))

    # apps <- getdata_app(round = round)
    # apps %>% write_rds(glue("{dir_in}/apps.rds"))

    # appinputs <- getdata_appinput(round = round)
    # appinputs %>% write_rds(glue("{dir_in}/appinputs.rds"))

    # choices <- getdata_appschoolranking(round = round)
    # choices %>% write_rds(glue("{dir_in}/choices.rds"))

    # siblings <- getdata_sibling()
    # siblings %>% write_rds(glue("{dir_in}/siblings.rds"))

    # students_recent <- getdata_student_recent()
    # students_recent %>% write_rds(glue("{dir_in}/students_recent.rds"))

    gradelevels <- getdata_gradelevel()
    gradelevels |> write_rds(glue("{dir_in}/gradelevels.rds"))
    gradelevels |> write_csv(glue("{dir_in}/gradelevels.csv"), na = "")

    contactsmatch <- getdata_contact_match()
    contactsmatch |> write_rds(glue("{dir_in}/contactsmatch.rds"))
    contactsmatch |> write_csv(glue("{dir_in}/contactsmatch.csv"), na = "")
  }

  contactsmatch |>
    count(oneappid, sort = TRUE) |>
    filter(n > 1) |>
    write_csv(glue("{dir_review}/dupe_contacts.csv"), na = "") |>
    print()

  cat("\n")

  match <-
    read_csv(
      glue("{dir_in}/3_MasterMatch.csv"),
      col_types = str_c(str_dup("c", 9), str_dup("i", 1), str_dup("c", 29))
    ) |>
    fix_grades() |>
    match_augment(gradelevels = gradelevels, contactsmatch = contactsmatch)

  # choices_external <-
  #   readr::read_csv(
  #     glue("{dir_in}/additional-student-preference-priority-rows.csv"),
  #     col_types = stringr::str_c(stringr::str_dup("c", 14))
  #   )
  #
  # overmatches <-
  #   readxl::read_excel(
  #     glue("{dir_external}/sibling-overmatches.xlsx"),
  #     col_types = "text"
  #   )

  match |> write_csv(glue("{dir_review}/000_match.csv"), na = "")

  match |>
    match_parts_all(
      schools_waitlist = c("846", "847", "4012", "4013")
    ) |>
    write_csv(glue("{dir_review}/match_participants_all.csv"), na = "")

  match |>
    matchcalc_summarystats(
      GRADE, schools_waitlist = c("846", "847", "4012", "4013")
    ) |>
    write_csv(glue("{dir_review}/summarystats.csv"), na = "")

  return(NULL)

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
