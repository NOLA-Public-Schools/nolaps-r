# Match rates -------------------------------------------------------------


#' @export
matchcalc_seekingnew <- function(x, schools_waitlist, ...) {
  x |>
    matchcalc_participants_all(schools_waitlist, ...) |>
    filter(is.na(rank_guaranteed) | rank_guaranteed != 1)
}



#' @export
matchcalc_results_seekingnew <- function(
    x,
    schools_waitlist,
    ...,
    exclude_ineligible = TRUE,
    exclude_notprocessed = TRUE) {
  if (exclude_ineligible == TRUE) {
    x <- x |> filter(`ASSIGNMENT STATUS` != "Ineligible")
  }

  if (exclude_notprocessed == TRUE) {
    x <- x |> filter(`ASSIGNMENT STATUS` != "Not Processed")
  }

  x |>
    semi_join(matchcalc_seekingnew(x, schools_waitlist, ...)) |>
    group_by(...) |>
    summarize(
      n_seekingnew = length(unique(`STUDENT ID`)),
      n_seekingnew_rank1 = sum(`CHOICE RANK` == 1),
      n_seekingnew_acceptednew =
        sum(`ASSIGNMENT STATUS` == "Accepted" & is.na(`GUARANTEED?`)),
      n_seekingnew_acceptednew_top3 =
        sum(`ASSIGNMENT STATUS` == "Accepted" & is.na(`GUARANTEED?`) & `CHOICE RANK` %in% 1:3),
      n_seekingnew_acceptednew_top1 =
        sum(`ASSIGNMENT STATUS` == "Accepted" & is.na(`GUARANTEED?`) & `CHOICE RANK` == 1),
      n_seekingnew_fallback =
        sum(`ASSIGNMENT STATUS` == "Accepted" & !is.na(`GUARANTEED?`)),
      n_seekingnew_unassigned = n_seekingnew - n_seekingnew_acceptednew - n_seekingnew_fallback,
      n_records_waiting = sum(stringr::str_detect(`ASSIGNMENT STATUS`, "Waiting"))
    ) |>
    mutate(
      rate_acceptednew = n_seekingnew_acceptednew / n_seekingnew,
      rate_acceptednew_top3 = n_seekingnew_acceptednew_top3 / n_seekingnew,
      rate_acceptednew_top1 = n_seekingnew_acceptednew_top1 / n_seekingnew,
      rate_fallback = n_seekingnew_fallback / n_seekingnew,
      rate_unassigned = n_seekingnew_unassigned / n_seekingnew
    )
}



#' @export
matchcalc_results_seekingnew_sibling <- function(
    x,
    schools_waitlist,
    ...,
    exclude_ineligible = TRUE,
    exclude_notprocessed = TRUE) {
  if (exclude_ineligible == TRUE) {
    x <- x |> filter(`ASSIGNMENT STATUS` != "Ineligible")
  }

  if (exclude_notprocessed == TRUE) {
    x <- x |> filter(`ASSIGNMENT STATUS` != "Not Processed")
  }

  x |>
    semi_join(matchcalc_seekingnew(x, schools_waitlist, ...)) |>
    semi_join(matchcalc_participants_sibling(x, ...)) |>
    group_by(...) |>
    summarize(
      n_seekingnew = length(unique(`STUDENT ID`)),
      n_seekingnew_acceptednew_sibling =
        sum(`ASSIGNMENT STATUS` == "Accepted" & stringr::str_detect(`QUALIFIED PRIORITIES`, "Sibling"), na.rm = TRUE)
    ) |>
    mutate(
      rate_acceptednew_sibling = n_seekingnew_acceptednew_sibling / n_seekingnew
    )
}



#' @export
matchcalc_results_seekingnew_unassigned <- function(
    x,
    schools_waitlist,
    ...,
    exclude_ineligible = TRUE,
    exclude_notprocessed = TRUE) {
  if (exclude_ineligible == TRUE) {
    x <- x |> filter(`ASSIGNMENT STATUS` != "Ineligible")
  }

  if (exclude_notprocessed == TRUE) {
    x <- x |> filter(`ASSIGNMENT STATUS` != "Not Processed")
  }

  matchcalc_seekingnew(x, schools_waitlist, ...) |>
    filter(is.na(rank_accepted)) |>
    group_by(...) |>
    summarize(
      n_seekingnew_unassigned = n(),
      n_seekingnew_unassigned_k9 = sum(GRADE %in% c("K", "9")),
      n_seekingnew_unassigned_3orless = sum(n_choices <= 3),
    ) |>
    mutate(
      rate_k9 = n_seekingnew_unassigned_k9 / n_seekingnew_unassigned,
      rate_3orless = n_seekingnew_unassigned_3orless / n_seekingnew_unassigned
    )
}



# Priority summaries ------------------------------------------------------



#' @export
matchcalc_priorityoutcomes <- function(x) {
  x |>
    select(
      id_student, choice_name, is_highdemand,
      `CHOICE SCHOOL`, GRADE, `STUDENT ID`, `QUALIFIED PRIORITIES`, `ASSIGNMENT PRIORITY`, id_account
    ) |>
    mutate(across(`QUALIFIED PRIORITIES`, ~ stringr::str_remove(., "/$"))) |>
    tidyr::separate_rows(`QUALIFIED PRIORITIES`, sep = "/") |>
    mutate(has = 1) |>
    tidyr::pivot_wider(names_from = `QUALIFIED PRIORITIES`, values_from = has) |>
    select(-`NA`) |>
    fix_grades() |>
    arrange(choice_name, `CHOICE SCHOOL`, GRADE, `STUDENT ID`)
}



#' @export
matchcalc_priorityoutcomes_summary <- function(x, ...) {
  x |>
    matchcalc_priorityoutcomes() |>
    group_by(...) |>
    summarize(
      n_applicants = n(),
      across(c(Guaranteed:Ineligible), list(count = ~ sum(.x, na.rm = TRUE), rate = ~ sum(.x, na.rm = TRUE) / n_applicants))
    )
}
