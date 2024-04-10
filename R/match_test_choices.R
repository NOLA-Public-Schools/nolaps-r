match_test_choices <- function(dir_review, match, choices) {
  cat("\nChoices and rank ordering\n")

  shouldhave <-
    choices |>
    group_by(.data$id_contact) |>
    arrange(.data$id_contact, .data$rank) |>
    mutate(rank = seq_len(n())) |>
    ungroup() |>
    select(
      "id_contact", "id_gradelevel", "rank",
      "id_app", "id_appschoolranking"
    )

  have <-
    match |>
    group_by(.data$id_contact) |>
    arrange(.data$id_contact, .data$`CHOICE RANK`) |>
    ungroup() |>
    distinct(.data$id_contact, .data$id_gradelevel) |>
    group_by(.data$id_contact) |>
    mutate(rank = seq_len(n())) |>
    ungroup()

  invalid_choices <-
    have |>
    anti_join(shouldhave, by = c(
      "id_contact",
      "id_gradelevel",
      "rank"
    )) |>
    left_join(match, by = c("id_contact", "id_gradelevel")) |>
    filter(.data$`GUARANTEED?` != "YES")

  missing_choices <-
    shouldhave |>
    anti_join(have, by = c(
      "id_contact",
      "id_gradelevel",
      "rank"
    ))

  test_helper(
    invalid_choices,
    "Every match record traces back to application choice or guarantee."
  )
  write_if_bad(invalid_choices, dir_review)

  test_helper(
    missing_choices,
    "Every application choice is reflected in the match."
  )
  write_if_bad(missing_choices, dir_review)
}
