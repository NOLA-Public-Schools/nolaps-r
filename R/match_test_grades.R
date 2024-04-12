match_test_grades <- function(dir_review, match) {
  cat("\nTest: Grade Levels\n")

  invalid_grades <-
    match |>
    filter(is.na(.data$id_gradelevel))

  invalid_grades_eligible <-
    invalid_grades %>%
    filter(.data$`ELIGIBLE?` == "YES")

  cat(
    glue(
      "
      \n
      {nrow(invalid_grades_eligible)} eligible records with invalid grades
      {nrow(invalid_grades)} total records with invalid grades
      \n
      "
    )
  )

  test_helper(
    invalid_grades_eligible,
    "No eligible match record involves a grade that will not exist next year."
  )
  write_if_bad(invalid_grades_eligible, dir_review)
  write_if_bad(invalid_grades, dir_review)
}
