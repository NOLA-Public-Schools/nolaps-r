match_test_zone <- function(dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: Zone\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "Zone Priority") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "Zone Priority") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "Geography"))

  invalid_zone <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_zone <-
    match |>
    filter(!str_detect(.data$`QUALIFIED PRIORITIES`, "Geography")) |>
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

  print(count(distinct(have, .data$`STUDENT ID`, .data$GRADE), .data$GRADE))

  test_helper(
    invalid_zone,
    "No student has an invalid zone priority."
  )
  write_if_bad(invalid_zone, dir_review)

  test_helper(
    missing_zone,
    "No student has a missing zone priority."
  )
  write_if_bad(missing_zone, dir_review)
}
