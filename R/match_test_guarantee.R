match_test_guarantee <- function(dir_review, match, students_active) {
  cat("\nGuarantee\n")

  # TODO
  # too young for next grade
  # return prohibited due to expulsion

  shouldhave <-
    students_active |>
    filter(!is.na(.data$id_gradelevel_guarantee)) |>
    select(all_of(c("id_contact", "id_gradelevel_guarantee")))

  have <-
    match |>
    filter(!is.na(.data$`GUARANTEED?`)) |>
    select(
      all_of(
        c(
          "id_contact",
          "id_gradelevel_guarantee" = "id_gradelevel",
          "name_program", "GRADE", "STUDENT ID"
        )
      )
    ) |>
    arrange(.data$name_program, .data$GRADE, .data$`STUDENT ID`)

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
    )

  cat(glue("{nrow(distinct(have, id_contact))} students\n"))

  print(count(have, .data$GRADE))

  cat("\n")

  test_helper(
    invalid_guarantee,
    "No student has an invalid guarantee."
  )

  test_helper(
    missing_guarantee,
    "No student has a missing guarantee."
  )

  write_if_bad(invalid_guarantee, dir_review)
  write_if_bad(missing_guarantee, dir_review)
}
