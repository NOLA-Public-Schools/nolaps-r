match_test_feeder <- function(dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: Feeder\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "Feeder Priority") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "Feeder Priority") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "Feeder"))

  invalid_feeder <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_feeder <-
    match |>
    filter(
      !str_detect(.data$`QUALIFIED PRIORITIES`, "Feeder")
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
      have,
      .data$name_program_current, .data$grade_current,
      .data$name_program, .data$GRADE
    ) |>
      slice_sample(n = 10)
  )

  test_helper(
    invalid_feeder,
    "No student has an invalid feeder priority."
  )
  write_if_bad(invalid_feeder, dir_review)

  test_helper(
    missing_feeder,
    "No student has a missing feeder priority."
  )
  write_if_bad(missing_feeder, dir_review)
}
