match_parts <- function(x, ...) {
  x |> distinct(.data$`STUDENT ID`, .data$GRADE, ...)
}


match_parts_accepted <- function(x, ...) {
  x |>
    filter(.data$`ASSIGNMENT STATUS` == "Accepted") |>
    select(
      "STUDENT ID",
      "rank_accepted" = "CHOICE RANK", "choice_accepted" = "CHOICE SCHOOL",
      ...
    )
}


match_parts_guaranteed <- function(x, ...) {
  x |>
    filter(.data$`GUARANTEED?` == "YES") |>
    select(
      "STUDENT ID",
      "rank_guaranteed" = "CHOICE RANK", "choice_guaranteed" = "CHOICE SCHOOL",
      ...
    )
}


match_parts_n_choices <- function(x, ...) {
  x |> count(.data$`STUDENT ID`, ..., name = "n_choices")
}


match_parts_n_eligibleprocd <- function(x, ...) {
  x |>
    filter(
      .data$`ASSIGNMENT STATUS` != "Ineligible",
      .data$`ASSIGNMENT STATUS` != "Not Processed"
    ) |>
    count(.data$`STUDENT ID`, ..., name = "n_eligibleprocd")
}


match_parts_n_ineligible <- function(x, ...) {
  x |>
    filter(.data$`ASSIGNMENT STATUS` == "Ineligible") |>
    count(.data$`STUDENT ID`, ..., name = "n_ineligible")
}


match_parts_n_full <- function(x, schools_waitlist, ...) {
  x |>
    filter(.data$`ASSIGNMENT STATUS` %in% c(
      "Waiting List",
      "Waiting List - Family Link Rejection"
    )) |>
    filter(!(.data$GRADE %in% c("INF", "1YR", "2YR", "PK3", "PK4"))) |>
    filter(!(.data$`CHOICE SCHOOL` %in% schools_waitlist)) |>
    count(.data$`STUDENT ID`, ..., name = "n_full")
}


match_parts_n_waiting <- function(x, schools_waitlist, ...) {
  x |>
    filter(.data$`ASSIGNMENT STATUS` %in% c(
      "Waiting List",
      "Waiting List - Family Link Rejection"
    )) |>
    filter(
      .data$GRADE %in% c("INF", "1YR", "2YR", "PK3", "PK4") |
        .data$`CHOICE SCHOOL` %in% schools_waitlist
    ) |>
    count(.data$`STUDENT ID`, ..., name = "n_waiting")
}


match_parts_sibling <- function(x, ...) {
  x |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "Sibling")) |>
    distinct(.data$`STUDENT ID`, ...)
}


match_parts_all <- function(x, schools_waitlist, ...) {
  x |>
    match_parts(...) |>
    left_join(match_parts_n_choices(x, ...)) |>
    left_join(match_parts_accepted(x, ...)) |>
    left_join(match_parts_guaranteed(x, ...)) |>
    left_join(match_parts_n_waiting(x, schools_waitlist, ...)) |>
    left_join(match_parts_n_full(x, schools_waitlist, ...)) |>
    left_join(match_parts_n_ineligible(x, ...)) |>
    left_join(match_parts_n_eligibleprocd(x, ...)) |>
    replace_na(list(
      n_waiting = 0,
      n_full = 0,
      n_ineligible = 0,
      n_eligibleprocd = 0
    ))
}
