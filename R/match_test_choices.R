library(readr)
library(dplyr)
match_test_choices <- function(dir_review, match, choices) {
    # Remove rows with missing or blank id_contact
    choices <- choices %>% filter(!is.na(id_contact) & id_contact != "")
    match <- match %>% filter(!is.na(id_contact) & id_contact != "")
  cat("\nTest: Choices and Rank Ordering\n")

  shouldhave <-
    choices |>
    group_by(.data$id_contact) |>
    arrange(.data$id_contact, .data$rank) |>
    mutate(rank = seq_len(n())) |>
    ungroup() |>
    select(
      "id_contact", "oneappid", "id_gradelevel", "rank",
      "id_app", "id_appschoolranking"
    )


  have <-
    match |>
    filter(!is.na(.data$id_gradelevel)) |>
    group_by(.data$id_contact) |>
    arrange(.data$id_contact, .data$`CHOICE RANK`) |>
    ungroup() |>
    distinct(.data$id_contact, .data$id_gradelevel) |>
    group_by(.data$id_contact) |>
    mutate(rank = seq_len(n())) |>
    ungroup()
    
  shouldhave |> write_csv(glue("choices_shouldhave.csv"), na = "")
  have |> write_csv(glue("choices_have.csv"), na = "")
  
  invalid_choices <-
    have |>
    anti_join(shouldhave, by = c(
      "id_contact",
      "id_gradelevel",
      "rank"
    )) |>
    left_join(match, by = c("id_contact", "id_gradelevel")) |>
    filter(.data$`GUARANTEED?` != "YES") |>
    select(
      "id_contact", "id_gradelevel", "rank",
    )

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

  # test_helper(
  #   missing_choices,
  #   "Every application choice is reflected in the match."
  # )
  write_if_bad(missing_choices, dir_review)

  invalid_ranks <-
    match |>
    group_by(.data$id_contact) |>
    summarize(n_ranks = n(), max_ranks = max(.data$`CHOICE RANK`), choice_school = first(.data$`CHOICE SCHOOL`)) |>
    filter(.data$max_ranks != .data$n_ranks)

  test_helper(
    invalid_ranks,
    "Largest rank number is equal to the number of choices."
  )
  write_if_bad(invalid_ranks, dir_review)
}



# library(readr)
# library(dplyr)
# match_test_choices <- function(dir_review, match, choices) {
#   cat("\nTest: Choices and Rank Ordering\n")

#   shouldhave <-
#     choices |>
#     group_by(.data$id_contact) |>
#     arrange(.data$id_contact, .data$rank) |>
#     mutate(rank = seq_len(n())) |>
#     ungroup() |>
#     select(
#       "id_contact", "oneappid", "id_gradelevel", "rank",
#       "id_app", "id_appschoolranking"
#     )

#   have <-
#     match |>
#     filter(!is.na(.data$id_gradelevel)) |>
#     group_by(.data$id_contact) |>
#     arrange(.data$id_contact, .data$`CHOICE RANK`) |>
#     mutate(rank = seq_len(n())) |>
#     ungroup() |>
#     select(
#       "id_contact", "STUDENT ID", "id_gradelevel", "rank",
#       "id_app", "id_appschoolranking"
#     )
#   # shouldhave to csvn and have to csv
#   shouldhave |> write_csv(glue("choices_shouldhave.csv"), na = "")
#   have |> write_csv(glue("choices_have.csv"), na = "")

#   invalid_choices <-
#     have |>
#     anti_join(shouldhave, by = c(
#       "id_contact",
#       "id_gradelevel",
#       "rank"
#     )) |>
#     left_join(match, by = c("id_contact", "id_gradelevel")) |>
#     filter(.data$`GUARANTEED?` != "YES") |>
#     select(
#       "id_contact", "id_gradelevel", "rank",
#       "id_app", "id_appschoolranking"
#     )

#   missing_choices <-
#     shouldhave |>
#     anti_join(have, by = c(
#       "id_contact",
#       "id_gradelevel",
#       "rank"
#     ))

#   test_helper(
#     invalid_choices,
#     "Every match record traces back to application choice or guarantee."
#   )
#   write_if_bad(invalid_choices, dir_review)

#   if (nrow(invalid_choices) > 0) {
#     cat("\nFirst 5 invalid_choices:\n")
#     print(head(invalid_choices, 5))
#   }

#   test_helper(
#     missing_choices,
#     "Every application choice is reflected in the match."
#   )
#   write_if_bad(missing_choices, dir_review)

#   if (nrow(missing_choices) > 0) {
#     cat("\nFirst 5 missing_choices:\n")
#     print(head(missing_choices, 5))
#   }

#   invalid_ranks <-
#     match |>
#     group_by(.data$id_contact) |>
#     summarize(n_ranks = n(), max_ranks = max(.data$`CHOICE RANK`), choice_school = first(.data$`CHOICE SCHOOL`)) |>
#     filter(.data$max_ranks != .data$n_ranks)

#   test_helper(
#     invalid_ranks,
#     "Largest rank number is equal to the number of choices."
#   )
#   write_if_bad(invalid_ranks, dir_review)
# }