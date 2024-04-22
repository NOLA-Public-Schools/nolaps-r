match_test_montessori <- function(
    dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: Montessori\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "Montessori Priority") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "Montessori Priority") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "School Specific 1")) |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel)

  invalid_montessori <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_montessori <-
    match |>
    filter(
      !str_detect(.data$`QUALIFIED PRIORITIES`, "School Specific 1")
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

  print(count(have, .data$name_program, .data$GRADE))

  test_helper(
    invalid_montessori,
    "No student has an invalid Montessori priority."
  )
  write_if_bad(invalid_montessori, dir_review)

  test_helper(
    missing_montessori,
    "No student has a missing Montessori priority."
  )
  write_if_bad(missing_montessori, dir_review)
}
