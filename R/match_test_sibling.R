match_test_sibling <- function(dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: Sibling\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "Sibling Priority") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "Sibling Priority") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "Sibling"))

  invalid_sibling <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_sibling <-
    match |>
    filter(!str_detect(.data$`QUALIFIED PRIORITIES`, "Sibling")) |>
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
    invalid_sibling,
    "No student has an invalid sibling priority."
  )
  write_if_bad(invalid_sibling, dir_review)

  test_helper(
    missing_sibling,
    "No student has a missing sibling priority."
  )
  write_if_bad(missing_sibling, dir_review)
}
