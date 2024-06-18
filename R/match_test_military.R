match_test_military <- function(dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: Military\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "Military Priority") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "Military Priority") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "Military child"))

  invalid_military <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_military <-
    match |>
    filter(
      !str_detect(.data$`QUALIFIED PRIORITIES`, "Military child")
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
    invalid_military,
    "No student has an invalid military priority."
  )
  write_if_bad(invalid_military, dir_review)

  test_helper(
    missing_military,
    "No student has a missing military priority."
  )
  write_if_bad(missing_military, dir_review)
}
