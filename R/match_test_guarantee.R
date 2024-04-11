match_test_guarantee <- function(dir_review, match, students_active) {
  cat("\n", "Test: Guarantees", "\n")

  # TODO
  # too young for next grade
  # return prohibited due to expulsion

  shouldhave <-
    students_active |>
    filter(!is.na(.data$id_gradelevel_guarantee)) |>
    select("id_contact", "id_gradelevel_guarantee")

  have <-
    match |>
    filter(.data$`GUARANTEED?` == "YES") |>
    select(
      "id_contact", "id_gradelevel_guarantee" = "id_gradelevel",
      "name_program", "GRADE", "STUDENT ID"
    )

  invalid_guarantee <-
    have |>
    anti_join(
      shouldhave,
      by = c(
        "id_contact",
        "id_gradelevel_guarantee"
      )
    ) |>
    arrange(.data$name_program, .data$GRADE, .data$`STUDENT ID`)

  missing_guarantee <-
    shouldhave |>
    anti_join(
      have,
      by = c(
        "id_contact",
        "id_gradelevel_guarantee"
      )
    )

  cat("\n", glue("{nrow(distinct(have, id_contact))} students"), "\n\n")

  print(count(have, .data$GRADE))

  cat("\n")

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
