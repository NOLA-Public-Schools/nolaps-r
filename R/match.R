#' K12 programs with waitlists
#'
#' @return character vector
#'
#' @export
schools_waitlist <- function() {
  c(
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
}


#' Programs requiring 8th grade applicants to be at least 15 by 9/30
#'
#' @return character vector
#'
#' @export
schools_net <- function() {
  c(
    "360001NETCC",
    "360002NETGentilly",
    "360003NETEast"
  )
}


#' Process match file
#'
#' @param run integer
#' @param dir_in character
#' @param dir_out character
#' @param use_cache logical
#'
#' @export
match_process <- function(
    run, dir_in = "in", dir_out = "out", use_cache = FALSE) {
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
    gradelevels <- read_rds(glue("{dir_in}/gradelevels.rds"))
    contactsmatch <- read_rds(glue("{dir_in}/contactsmatch.rds"))
    choices <- read_rds(glue("{dir_in}/choices.rds"))
    eps_gradelevel <- read_rds(glue("{dir_in}/eps_gradelevel.rds"))
    eps_choice <- read_rds(glue("{dir_in}/eps_choice.rds"))
    expulsions <- read_rds(glue("{dir_in}/expulsions.rds"))
  } else {
    gradelevels <- getdata_gradelevel()
    gradelevels |> write_rds(glue("{dir_in}/gradelevels.rds"))
    gradelevels |> write_csv(glue("{dir_in}/gradelevels.csv"), na = "")

    contactsmatch <- getdata_contact_match()
    contactsmatch |> write_rds(glue("{dir_in}/contactsmatch.rds"))
    contactsmatch |> write_csv(glue("{dir_in}/contactsmatch.csv"), na = "")

    choices <- getdata_appschoolranking()
    choices |> write_rds(glue("{dir_in}/choices.rds"))
    choices |> write_csv(glue("{dir_in}/choices.csv"), na = "")

    eps_gradelevel <- getdata_ep_gradelevel()
    eps_gradelevel |> write_rds(glue("{dir_in}/eps_gradelevel.rds"))
    eps_gradelevel |> write_csv(glue("{dir_in}/eps_gradelevel.csv"), na = "")

    eps_choice <- getdata_ep_choice()
    eps_choice |> write_rds(glue("{dir_in}/eps_choice.rds"))
    eps_choice |> write_csv(glue("{dir_in}/eps_choice.csv"), na = "")

    expulsions <- getdata_expulsion()
    expulsions |> write_rds(glue("{dir_in}/expulsions.rds"))
    expulsions |> write_csv(glue("{dir_in}/expulsions.csv"), na = "")
  }

  contactsmatch |>
    count(.data$oneappid, sort = TRUE) |>
    filter(n > 1) |>
    write_csv(glue("{dir_review}/dupe_contacts.csv"), na = "") |>
    print()

  gradelevels |>
    count(.data$choice_school, .data$grade, sort = TRUE) |>
    filter(n > 1) |>
    write_csv(glue("{dir_review}/dupe_gradelevels.csv"), na = "") |>
    print()

  gradelevels |>
    distinct(.data$choice_school, .data$name_program) |>
    count(.data$choice_school, sort = TRUE) |>
    filter(n > 1) |>
    write_csv(glue("{dir_review}/dupe_programnames.csv"), na = "") |>
    print()

  choices |>
    count(.data$id_contact, .data$id_gradelevel, sort = TRUE) |>
    filter(n > 1) |>
    write_csv(glue("{dir_review}/dupe_choices.csv"), na = "") |>
    print()

  cat("\nGenerating match review file.\n")

  match <-
    read_csv(
      glue("{dir_in}/3_MasterMatch.csv"),
      col_types = str_c(str_dup("c", 9), str_dup("i", 1), str_dup("c", 29))
    ) |>
    fix_grades() |>
    match_augment(
      gradelevels = gradelevels,
      contactsmatch = contactsmatch,
      choices = choices,
      expulsions
    )

  # overmatches <-
  #   readxl::read_excel(
  #     glue("{dir_external}/sibling-overmatches.xlsx"),
  #     col_types = "text"
  #   )

  match |> write_csv(glue("{dir_review}/000_match_{run}.csv"), na = "")
  match |> write_rds(glue("{dir_review}/000_match_{run}.rds"))

  cat("\nGenerating participant outcomes.\n")

  match |>
    match_parts_all(
      schools_waitlist = schools_waitlist()
    ) |>
    write_csv(glue("{dir_review}/participants.csv"), na = "")

  cat("\nGenerating summary stats.\n")

  match |>
    match_summary_student(
      schools_waitlist = schools_waitlist(),
      GRADE
    ) |>
    write_csv(glue("{dir_review}/summarystats_grade.csv"), na = "")

  match |>
    match_summary_program(
      schools_waitlist = schools_waitlist(),
      `CHOICE SCHOOL`, GRADE
    ) |>
    write_csv(glue("{dir_review}/summarystats_program_grade.csv"), na = "")

  # match_test(
  #   dir_review = dir_review,
  #   match = match,
  #   gradelevels = gradelevels,
  #   contactsmatch = contactsmatch,
  #   choices = choices,
  #   eps_gradelevel = eps_gradelevel,
  #   eps_choice = eps_choice
  # )

  match_placement(
    dir_business = dir_business,
    match = match,
    overmatches = NULL
  )

  match_notification(
    dir_business = dir_business,
    match = match,
    overmatches = NULL
  )

  cat(glue("\n\nFinished at {Sys.time()}\n\n"))

  return(NULL)

  match_briefing(
    match = match,
    dir_out = dir_business
  )
}
