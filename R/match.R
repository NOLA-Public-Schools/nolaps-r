library(glue)
library(dplyr)
library(readr)
library(salesforcer)
library(stringr)
library(lubridate)
library(tidyr)

# Uncomment this out if you are using VS Code and want to source all .R files in the same directory as match.R
# Source all .R files in the same directory as match.R
# current_file <- "C:/Users/dpalacios/Documents/github/nolaps-r/R/match.R"
# dir_path <- dirname(current_file)
# cat("Directory path: ", dir_path, "\n")

# # List all .R files in the directory
# files <- list.files(dir_path, pattern = "\\.R$", full.names = TRUE)
# cat("All .R files in the directory:\n")
# print(files)

# # Exclude match.R
# files <- files[basename(files) != basename(current_file)]
# cat("Files to be sourced (excluding match.R):\n")
# print(files)

# # Source the files
# if (length(files) > 0) {
#   invisible(sapply(files, function(file) {
#     cat("Sourcing file: ", file, "\n")
#     source(file)
#   }))
# } else {
#   cat("No files to source.\n")
# }


#' Process match file
#'
#' @param run integer
#' @param dir_in character
#' @param dir_out character
#' @param use_cache logical
#'
#' @export
#'
#'

match_process <- function(
    run, dir_in = "in", dir_out = "out", use_cache = FALSE) {
  dir_business <- glue("{dir_out}/business")
  dir_review <- glue("{dir_out}/validation")

  if (!dir.exists(dir_out)) {
    dir.create(dir_out)
  }
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

    # grade_PK4_12 <- c("PK4", "K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
    contactsmatch <- getdata_contact_match() |>
      # filter(grade_current %in% grade_PK4_12) |>
      arrange(oneappid) %>%  # Sort by ID and most recent date
      group_by(oneappid) %>% # Group by contact ID
      # slice(1) %>%           # Keep only the first (most recent) row
      ungroup()
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
    count(.data$oneappid, .data$grade_current, sort = TRUE) |>
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

  match_detiered <- match_detier(match)
  match_detiered |> write_csv(glue("{dir_business}/match_detier.csv"), na = "")

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
      schools_waitlist = schools_waitlist(),
      GRADE
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

  match_detiered |>
    match_summary_program(
      schools_waitlist = schools_waitlist(),
      `CHOICE SCHOOL`, GRADE
    ) |>
    write_csv(
      glue("{dir_review}/summarystats_program_grade_detiered.csv"),
      na = ""
    )


  #comment out/in####
   match_test(
    dir_review = dir_review,
    match = match,
    gradelevels = gradelevels,
    contactsmatch = contactsmatch,
    choices = choices,
    eps_gradelevel = eps_gradelevel,
    eps_choice = eps_choice
  )

  match_placement(
    dir_business,
    match,
    overmatches = NULL
  )

  match_notification(
    dir_business,
    match,
    overmatches = NULL
  )

  match_test_sibling(
    dir_review = dir_review,
    match = match,
    eps_gradelevel = eps_gradelevel,
    eps_choice = eps_choice
  )

  match_test_uno(
    dir_review = dir_review,
    match = match,
    eps_gradelevel = eps_gradelevel,
    eps_choice = eps_choice
  )

  match_test_french(
    dir_review = dir_review,
    match = match,
    eps_gradelevel = eps_gradelevel,
    eps_choice = eps_choice
  )

  #comment out/in####

  match_briefing(
    match,
    dir_business
  )

  cat(glue("\n\nFinished at {Sys.time()}\n\n"))
}


match_process(run = 54)