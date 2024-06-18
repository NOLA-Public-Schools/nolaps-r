match_test_closing <- function(dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: Closing School\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "Closing School Priority") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "Closing School Priority") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "Closing Public School"))

  invalid_closing <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_closing <-
    match |>
    filter(
      !str_detect(.data$`QUALIFIED PRIORITIES`, "Closing Public School")
    ) |>
    filter(.data$`ELIGIBLE?` == "YES") |>
    filter(!str_detect(.data$`QUALIFIED PRIORITIES`, "Priority Score")) |>
    filter(.data$id_appschoolranking %in% shouldhave$id_appschoolranking)

  cat(
    glue(
      "
      \n
      {nrow(distinct(have, `STUDENT ID`))} students
      \n
      "
    )
  )

  print(
    count(
      distinct(
        have,
        .data$name_program_current, .data$grade_current, .data$GRADE,
        .data$`STUDENT ID`
      ),
      .data$name_program_current, .data$grade_current, .data$GRADE
    )
  )

  test_helper(
    invalid_closing,
    "No student has an invalid closing school priority."
  )
  write_if_bad(invalid_closing, dir_review)

  test_helper(
    missing_closing,
    "No student has a missing closing school priority."
  )
  write_if_bad(missing_closing, dir_review)
}
