match_test_iep <- function(dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: IEP\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "IEP Priority") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "IEP Priority") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "IEP"))

  invalid_iep <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_iep <-
    match |>
    filter(
      !str_detect(.data$`QUALIFIED PRIORITIES`, "IEP")
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
    invalid_iep,
    "No student has an invalid IEP priority."
  )
  write_if_bad(invalid_iep, dir_review)

  test_helper(
    missing_iep,
    "No student has a missing IEP priority."
  )
  write_if_bad(missing_iep, dir_review)
}
