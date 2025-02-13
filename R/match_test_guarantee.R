match_test_guarantee <- function(dir_review, match, students_active) {
  cat("\nTest: Guarantees\n")

  prohibited <-
    match |>
    filter(.data$is_prohibited)

  shouldhave <-
    students_active |>
    mutate(id_gradelevel_guarantee = if_else(
      .data$promotion == "Retained", .data$id_gradelevel_current,
      .data$id_gradelevel_guarantee
    )) |>
    filter(!is.na(.data$id_gradelevel_guarantee)) |>
    anti_join(
      prohibited,
      by = c("id_contact", "id_gradelevel_guarantee" = "id_gradelevel")
    ) |>
    select(
      "oneappid","id_contact", "id_gradelevel_guarantee",
      "student_dob", "promotion", "create_date"
    )

  have <-
    match |>
    filter(.data$`GUARANTEED?` == "YES") |>
    select(
      "STUDENT ID", "id_contact", "id_gradelevel_guarantee" = "id_gradelevel",
      "CHOICE SCHOOL","student_dob", "promotion",
      "name_program", "GRADE", "create_date"
    ) |>
    arrange(.data$GRADE, .data$student_dob)
  output_path <- "C:/Users/dpalacios/Documents/github/nolaps-r/nolaps-r/R/have_guaranteed.csv"
  write_csv(have, output_path, na = "")
  if (file.exists(output_path)) {
    cat("File created successfully at", output_path, "\n")
  } else {
    cat("Failed to create file at", output_path, "\n")
  }
  invalid_guarantee <-
    have |>
    anti_join(
      shouldhave,
      by = c(
        "id_contact",
        "id_gradelevel_guarantee"
      )
    )

  missing_guarantee <-
    shouldhave |>
    anti_join(
      have,
      by = c(
        "id_contact",
        "id_gradelevel_guarantee"
      )
    ) |>
    arrange(.data$oneappid, .data$student_dob)

  cat(glue("\n{nrow(distinct(have, id_contact))} students\n\n"))

  print(count(have, .data$GRADE))

  test_helper(
    invalid_guarantee,
    "No student has an invalid guarantee."
  )
  write_if_bad(invalid_guarantee, dir_review)

  test_helper(
    missing_guarantee,
    "No student has a missing guarantee."
  )
  write_if_bad(missing_guarantee, dir_review)
  # write_csv(have, " C:/Users/dpalacios/Documents/github/nolaps-r/nolaps-r/have_guaranteed.csv", na = "")
  write_csv(shouldhave, "C:/Users/dpalacios/Documents/github/nolaps-r/nolaps-r/shouldhave_guaranteed.csv", na = "")
}
