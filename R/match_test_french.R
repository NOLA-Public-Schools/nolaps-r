match_test_french <- function(dir_review, match, eps_gradelevel, eps_choice) {
  cat("\nTest: French\n")

  offers_priority <-
    eps_gradelevel |>
    filter(.data$name_ep == "French Priority") |>
    select("id_gradelevel")

  shouldhave <-
    eps_choice |>
    filter(.data$name_ep == "French Priority") |>
    filter(.data$status == "Approved") |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "School Specific 1")) |>
    filter(.data$id_gradelevel %in% offers_priority$id_gradelevel)

  invalid_french <-
    have |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_french <-
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
    invalid_french,
    "No student has an invalid French priority."
  )
  write_if_bad(invalid_french, dir_review)

  test_helper(
    missing_french,
    "No student has a missing French priority."
  )
  write_if_bad(missing_french, dir_review)
}
