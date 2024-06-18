match_test_uno <- function(dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: UNO Staff\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "UNO Staff") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "UNO Staff") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "School Specific 1")) |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel)

  invalid_uno <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_uno <-
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
    invalid_uno,
    "No student has an invalid UNO staff priority."
  )
  write_if_bad(invalid_uno, dir_review)

  test_helper(
    missing_uno,
    "No student has a missing UNO staff priority."
  )
  write_if_bad(missing_uno, dir_review)
}
