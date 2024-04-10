#' Process match file
#'
#' @param dir_in character
#' @param dir_out character
#' @param use_cache logical
#'
#' @export
match_process <- function(dir_in = "in", dir_out = "out", use_cache = FALSE) {
  dir_business <- glue("{dir_out}/business")
  dir_external <- glue("{dir_in}/external")
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
    # feeders <- read_rds(glue("{dir_in}/feeders.rds"))
    # priorities <- read_rds(glue("{dir_in}/priorities.rds"))
    # siblings <- read_rds(glue("{dir_in}/siblings.rds"))
    # students_recent <- read_rds(glue("{dir_in}/students_recent.rds"))

    gradelevels <- read_rds(glue("{dir_in}/gradelevels.rds"))
    contactsmatch <- read_rds(glue("{dir_in}/contactsmatch.rds"))
    choices <- read_rds(glue("{dir_in}/choices.rds"))
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

    choices <- getdata_appschoolranking()
    choices |> write_rds(glue("{dir_in}/choices.rds"))
    choices |> write_csv(glue("{dir_in}/choices.csv"), na = "")
  }

  # contactsmatch |>
  #   count(oneappid, sort = TRUE) |>
  #   filter(n > 1) |>
  #   write_csv(glue("{dir_review}/dupe_contacts.csv"), na = "") |>
  #   print()
  #
  # cat("\n")

  schools_waitlist <- c(
    "WAZ001_FAUFrenchLS",
    "WAZ001_FAUFrenchUS",
    "WAZ001_MAUMontessoriLSLA4",
    "WAZ001_MAUMontessoriUS",
    "WBE001Willow",
    "WBE001Willow_community_1",
    "WBE001Willow_community_2",
    "WBE001Willow_ed_1",
    "WBE001Willow_tulane_1",
    "WBE001Willow_tulane_2",
    "WBH001LakeForest",
    "WBH001LakeForest_tier_1",
    "WBH001LakeForest_tier_2"
  )

  match <-
    read_csv(
      glue("{dir_in}/3_MasterMatch.csv"),
      col_types = str_c(str_dup("c", 9), str_dup("i", 1), str_dup("c", 29))
    ) |>
    fix_grades() |>
    match_augment(
      gradelevels = gradelevels,
      contactsmatch = contactsmatch,
      choices = choices
    )

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

  cat("\nGenerating match review file.\n")

  match |> write_csv(glue("{dir_review}/000_match.csv"), na = "")

  cat("\nGenerating participant outcomes.\n")

  match |>
    match_parts_all(
      schools_waitlist = schools_waitlist
    ) |>
    write_csv(glue("{dir_review}/participants.csv"), na = "")

  cat("\nGenerating summary stats.\n")

  match |>
    matchcalc_summarystats(
      schools_waitlist = schools_waitlist,
      GRADE
    ) |>
    write_csv(glue("{dir_review}/summarystats.csv"), na = "")

  match_test(
    dir_review = dir_review,
    match = match,
    gradelevels = gradelevels,
    contactsmatch = contactsmatch,
    choices = choices
  )

  return(NULL)

  match_briefing(
    match = match,
    dir_out = dir_business
  )

  match_placement(
    match = match,
    overmatches = NULL,
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

  cat(glue("\n\nFinished at {Sys.time()}\n\n"))
}
