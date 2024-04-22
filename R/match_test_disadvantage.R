match_test_disadvantage <- function(
    dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: Economic Disadvantage\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "Economic Disadvantage") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "Economic Disadvantage") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "At Risk"))

  invalid_disadvantage <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_disadvantage <-
    match |>
    filter(
      !str_detect(.data$`QUALIFIED PRIORITIES`, "At Risk")
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
    count(have, .data$name_program, .data$GRADE) |> slice_sample(n = 10)
  )

  test_helper(
    invalid_disadvantage,
    "No student has an invalid economic disadvantage priority."
  )
  write_if_bad(invalid_disadvantage, dir_review)

  test_helper(
    missing_disadvantage,
    "No student has a missing economic disadvantage priority."
  )
  write_if_bad(missing_disadvantage, dir_review)
}
