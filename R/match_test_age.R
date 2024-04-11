match_test_age <- function(dir_review, match) {
  cat("\n", "Test: Age", "\n")

  schools_net <- c(
    "360001NETCC",
    "360002NETGentilly",
    "360003NETEast"
  )

  invalid_ages <-
    match |>
    mutate(across(.data$student_dob, ymd)) |>
    filter(
      (!(.data$GRADE %in% grades_ec()) & .data$student_dob > "2019-09-30") |
        (.data$GRADE == "PK4" & .data$student_dob > "2020-09-30") |
        (.data$GRADE == "PK3" & .data$student_dob > "2021-09-30") |
        (.data$GRADE == "2YR" & .data$student_dob > "2022-09-30") |
        (.data$GRADE == "1YR" & .data$student_dob > "2023-09-30") |
        (.data$GRADE == "INF" & .data$student_dob <= "2023-09-30") |
        (((.data$`CHOICE SCHOOL` %in% schools_net) & .data$GRADE == "8") &
          .data$student_dob > "2009-09-30")
    )

  invalid_ages_eligible <-
    invalid_ages |>
    filter(.data$`ELIGIBLE?` == "YES")

  cat(
    "\n",
    glue(
      "
      {nrow(invalid_ages_eligible)} eligible records with invalid ages
      {nrow(invalid_ages)} total records with invalid ages
      "
    ),
    "\n"
  )

  test_helper(
    invalid_ages_eligible,
    "No student missing an age cutoff is marked eligible."
  )
  write_if_bad(invalid_ages_eligible, dir_review)
  write_if_bad(invalid_ages, dir_review)
}
