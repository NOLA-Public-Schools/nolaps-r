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
      "student_dob", "promotion"
    )

  have <-
    match |>
    filter(.data$`GUARANTEED?` == "YES") |>
    select(
      "id_contact", "id_gradelevel_guarantee" = "id_gradelevel",
      "student_dob", "promotion",
      "name_program", "GRADE"
    ) |>
    arrange(.data$GRADE, .data$student_dob)

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
}
