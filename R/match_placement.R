#' Generate upload template for future-year placements
#'
#' @param dir_business character
#' @param match tibble of match records
#' @param overmatches tibble of siblings assigned over capacity
#'
#' @export
match_placement <- function(dir_business, match, overmatches) {
  cat("\nGenerating placements upload.\n")
  match_type <- read_csv(glue("{dir_business}/participants_matchtype.csv"))
  placements <-
    match |>
    filter(.data$`ASSIGNMENT STATUS` == "Accepted") |>
    mutate(
      is_newmatch = is.na(.data$`GUARANTEED?`),
      family_linked_selection = if_else(.data$`LINKED APPLICANTS ASSIGNED TOGETHER` == "YES", TRUE, FALSE)
    ) |>

    # filter(!(`STUDENT ID` %in% overmatches$`STUDENT ID`)) |>
    # bind_rows(overmatches) |>
    select(
      "name_program_future" = "name_program",
      "grade_future" = "GRADE",
      "id_gradelevel_future" = "id_gradelevel",
      "id_contact",
      "id_account",
      "is_newmatch",
      "family_linked_selection"
    ) |>
    mutate(
      family_linked_selection = if_else(is.na(family_linked_selection) | family_linked_selection == "", FALSE, family_linked_selection),
      id_recordtype = "012Do000000KvJvIAK",
      id_term = "0vMDo0000004CgeMAE",
      is_active = TRUE,
      date_enroll_start = today(),
      reason_enroll_start = "Main Round Placement",
      enrollment_reason = "Main Round Match"
    ) |>
    arrange(.data$name_program_future, .data$grade_future) |>
    mutate(grade_future = case_when(
      .data$grade_future == "1YR" ~ "1 YR",
      .data$grade_future == "2YR" ~ "2 YR",
      .default = .data$grade_future
    ))

  placements |> write_csv(glue("{dir_business}/placements.csv"), na = "")
}
