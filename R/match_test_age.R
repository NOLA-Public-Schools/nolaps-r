match_test_age <- function(dir_review, match) {
  cat("\nTest: Age\n")

  invalid_ages <-
    match |>
    filter(.data$is_underage) |>
    select("GRADE", "student_dob", "id_contact", "ELIGIBLE?") |>
    arrange(.data$GRADE, .data$student_dob)

  invalid_ages_eligible <-
    invalid_ages |>
    filter(.data$`ELIGIBLE?` == "YES")

  cat(
    glue(
      "
      \n
      {nrow(invalid_ages_eligible)} eligible records with invalid ages
      {nrow(invalid_ages)} total records with invalid ages
      \n
      "
    )
  )

  test_helper(
    invalid_ages_eligible,
    "No student missing an age cutoff is marked eligible."
  )
  write_if_bad(invalid_ages_eligible, dir_review)
  write_if_bad(invalid_ages, dir_review)
}
