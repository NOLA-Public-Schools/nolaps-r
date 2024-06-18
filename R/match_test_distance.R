match_test_distance <- function(dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: Distance\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "Half Mile Priority") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "Half Mile Priority") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "Child of Student"))

  invalid_distance <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_distance <-
    match |>
    filter(!str_detect(.data$`QUALIFIED PRIORITIES`, "Child of Student")) |>
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
    invalid_distance,
    "No student has an invalid distance priority."
  )
  write_if_bad(invalid_distance, dir_review)

  test_helper(
    missing_distance,
    "No student has a missing distance priority."
  )
  write_if_bad(missing_distance, dir_review)
}
