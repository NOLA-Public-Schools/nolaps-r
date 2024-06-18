#' Remove tier structure from match choices
#'
#' @param match tibble of match records
#'
#' @return tibble of match records, but with a single row for each tiered choice
#' instead of multiple tiers
#'
#' @export
match_detier <- function(match) {
  special_all <-
    match |>
    filter(str_detect(.data$`CHOICE SCHOOL`, "LakeForest|Willow")) |>
    mutate(`CHOICE SCHOOL` = .data$choice_school_clean) |>
    group_by(
      .data$`STUDENT ID`, .data$`CHOICE SCHOOL`, .data$name_program, .data$GRADE
    ) |>
    summarize(
      `CHOICE RANK` = min(.data$`CHOICE RANK`),
      n_guaranteed = sum(.data$`GUARANTEED?` == "YES"),
      n_accepted = sum(.data$`ASSIGNMENT STATUS` == "Accepted"),
      n_waiting = sum(str_detect(.data$`ASSIGNMENT STATUS`, "Waiting")),
      n_ineligible = sum(.data$`ASSIGNMENT STATUS` == "Ineligible")
    ) |>
    mutate(`GUARANTEED?` = if_else(.data$n_guaranteed == 1, "YES", NA)) |>
    ungroup()

  special_accepted <-
    special_all |>
    filter(.data$n_accepted == 1) |>
    mutate(`ASSIGNMENT STATUS` = "Accepted")

  special_waiting <-
    special_all |>
    anti_join(special_accepted, by = c("CHOICE SCHOOL", "STUDENT ID")) |>
    filter(.data$n_waiting >= 1) |>
    mutate(`ASSIGNMENT STATUS` = "Waiting List")

  special_ineligible <-
    special_all |>
    anti_join(special_accepted, by = c("CHOICE SCHOOL", "STUDENT ID")) |>
    anti_join(special_waiting, by = c("CHOICE SCHOOL", "STUDENT ID")) |>
    filter(.data$n_ineligible >= 1) |>
    mutate(`ASSIGNMENT STATUS` = "Ineligible")

  special_notprocessed <-
    special_all |>
    anti_join(special_accepted, by = c("CHOICE SCHOOL", "STUDENT ID")) |>
    anti_join(special_waiting, by = c("CHOICE SCHOOL", "STUDENT ID")) |>
    anti_join(special_ineligible, by = c("CHOICE SCHOOL", "STUDENT ID")) |>
    mutate(`ASSIGNMENT STATUS` = "Not Processed")

  match |>
    filter(!str_detect(.data$`CHOICE SCHOOL`, "LakeForest|Willow")) |>
    bind_rows(special_accepted) |>
    bind_rows(special_waiting) |>
    bind_rows(special_ineligible) |>
    bind_rows(special_notprocessed) |>
    group_by(.data$`STUDENT ID`) |>
    arrange(.data$`CHOICE RANK`) |>
    ungroup()
}
