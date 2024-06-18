match_test_eligibility_k12 <- function(dir_review, match, choices) {
  cat("\nTest: Eligibility: K12\n")

  prohibited <-
    match |>
    filter(.data$is_prohibited)

  shouldhave <-
    choices |>
    filter(
      .data$eligibility_k12 == "Eligible" | !.data$needs_eligibility_k12
    ) |>
    anti_join(prohibited, by = c("id_contact", "id_gradelevel")) |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(.data$GRADE %in% grades_k12()) |>
    filter(.data$`ASSIGNMENT STATUS` != "Ineligible")

  donthave <-
    match |>
    filter(.data$GRADE %in% grades_k12()) |>
    filter(.data$`ASSIGNMENT STATUS` == "Ineligible")

  invalid_eligibility_k12 <-
    have |>
    filter(is.na(.data$`GUARANTEED?`)) |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_eligibility_k12 <-
    match |>
    filter(.data$GRADE %in% grades_k12()) |>
    filter(.data$`ASSIGNMENT STATUS` == "Ineligible") |>
    filter(.data$id_appschoolranking %in% shouldhave$id_appschoolranking) |>
    filter(!.data$is_prohibited) |>
    filter(!.data$is_underage) |>
    filter(!str_detect(
      .data$`CHOICE SCHOOL`,
      "_tulane_[12]|_community_[12]|_ed_1"
    ))

  print(
    count(donthave, .data$name_program, .data$GRADE) |> slice_sample(n = 10)
  )

  test_helper(
    invalid_eligibility_k12,
    "No student has invalid K12 eligibility in the match."
  )
  write_if_bad(invalid_eligibility_k12, dir_review)

  test_helper(
    missing_eligibility_k12,
    "No student has missing K12 eligibility in the match."
  )
  write_if_bad(missing_eligibility_k12, dir_review)
}


match_test_eligibility_ec <- function(dir_review, match, choices) {
  cat("\nTest: Eligibility: EC\n")

  shouldhave <-
    choices |>
    filter(.data$eligibility_k12 == "Eligible") |>
    select("id_appschoolranking")

  have <-
    match |>
    filter(.data$GRADE %in% grades_ec()) |>
    filter(.data$`ASSIGNMENT STATUS` != "Ineligible")

  donthave <-
    match |>
    filter(.data$GRADE %in% grades_ec()) |>
    filter(.data$`ASSIGNMENT STATUS` == "Ineligible")

  invalid_eligibility_ec <-
    have |>
    filter(is.na(.data$`GUARANTEED?`)) |>
    filter(!(.data$id_appschoolranking %in% shouldhave$id_appschoolranking))

  missing_eligibility_ec <-
    match |>
    filter(.data$GRADE %in% grades_ec()) |>
    filter(.data$`ASSIGNMENT STATUS` == "Ineligible") |>
    filter(.data$id_appschoolranking %in% shouldhave$id_appschoolranking) |>
    filter(!.data$is_underage)

  print(
    count(donthave, .data$name_program, .data$GRADE) |> slice_sample(n = 10)
  )

  test_helper(
    invalid_eligibility_ec,
    "No student has invalid EC eligibility in the match."
  )
  write_if_bad(invalid_eligibility_ec, dir_review)

  test_helper(
    missing_eligibility_ec,
    "No student has missing EC eligibility in the match."
  )
  write_if_bad(missing_eligibility_ec, dir_review)
}
