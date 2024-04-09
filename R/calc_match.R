# Summary stats -----------------------------------------------------------



#' @export
matchcalcs_summarystats <- function(x, ...) {
  count(matchcalcs_guarantee1_only(x), ..., name = "n_guarantee1_only") |>
    full_join(count(matchcalcs_guarantee1_haschoices(x), ..., name = "n_guarantee1_haschoices")) |>
    full_join(count(matchcalcs_accepted_belowguarantee(x), ..., name = "n_accepted_belowguarantee")) |>
    full_join(count(matchcalcs_acceptednew_hasguarantee(x), ..., name = "n_acceptednew_hasguarantee")) |>
    full_join(count(matchcalcs_acceptednew_noguarantee(x), ..., name = "n_acceptednew_noguarantee")) |>
    full_join(count(matchcalcs_fallback_waiting(x), ..., name = "n_fallback_waiting")) |>
    full_join(count(matchcalcs_fallback_full(x), ..., name = "n_fallback_full")) |>
    full_join(count(matchcalcs_fallback_ineligible(x), ..., name = "n_fallback_ineligible")) |>
    full_join(count(matchcalcs_unassigned_waiting(x), ..., name = "n_unassigned_waiting")) |>
    full_join(count(matchcalcs_unassigned_full(x), ..., name = "n_unassigned_full")) |>
    full_join(count(matchcalcs_unassigned_ineligible(x), ..., name = "n_unassigned_ineligible")) |>
    mutate(across(c(n_guarantee1_only:n_unassigned_ineligible), ~ tidyr::replace_na(., 0)))
}



#' @export
matchcalcs_summarycols <- function(x) {
  x |>
    mutate(
      assigned = n_acceptednew_hasguarantee + n_acceptednew_noguarantee,
      fallback_not_ineligible = n_fallback_waiting + n_fallback_full,
      unassigned = n_unassigned_waiting + n_unassigned_full + n_unassigned_ineligible,
      subtotal = assigned + fallback_not_ineligible + unassigned,
      prop_assigned = assigned / subtotal,
      prop_fallback_not_ineligible = fallback_not_ineligible / subtotal,
      prop_unassigned = unassigned / subtotal
    ) |>
    select(
      GRADE,
      assigned,
      fallback_not_ineligible,
      unassigned,
      subtotal,
      prop_assigned,
      prop_fallback_not_ineligible,
      prop_unassigned,
      everything()
    )
}



#' @export
matchcalcs_summaryrow <- function(x, row_name) {
  x |>
    select(n_guarantee1_only:n_unassigned_ineligible) |>
    colSums() |>
    bind_rows() |>
    mutate(GRADE = row_name) |>
    matchcalcs_summarycols()
}



#' @export
matchcalcs_summarystats_full <- function(x, schools_waitlist = c("846", "847", "4012", "4013")) {
  with_bin_grade <-
    x |>
    matchcalcs_participants_all(schools_waitlist) |>
    matchcalcs_summarystats(GRADE) |>
    fix_grades() |>
    arrange(GRADE) |>
    mutate(
      is_inf_2yr = GRADE %in% c("INF", "1YR", "2YR"),
      is_pk3_pk4 = GRADE %in% c("PK3", "PK4"),
      is_k_8 = GRADE %in% c("K", "1", "2", "3", "4", "5", "6", "7", "8"),
      is_9_12 = GRADE %in% c("9", "10", "11", "12"),
      is_k_12 = GRADE %in% c("K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
      is_pk4_12 = GRADE %in% c("PK4", "K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
    )

  summary_inf_2yr <-
    with_bin_grade |>
    filter(is_inf_2yr) |>
    matchcalcs_summaryrow("Grades INF - 2YR")

  summary_pk3_pk4 <-
    with_bin_grade |>
    filter(is_pk3_pk4) |>
    matchcalcs_summaryrow("Grades PK3 - PK4")

  summary_k_8 <-
    with_bin_grade |>
    filter(is_k_8) |>
    matchcalcs_summaryrow("Grades K - 8")

  summary_9_12 <-
    with_bin_grade |>
    filter(is_9_12) |>
    matchcalcs_summaryrow("Grades 9 - 12")

  summary_k_12 <-
    with_bin_grade |>
    filter(is_k_12) |>
    matchcalcs_summaryrow("Grades K - 12")

  summary_pk4_12 <-
    with_bin_grade |>
    filter(is_pk4_12) |>
    matchcalcs_summaryrow("Grades PK4 - 12")

  summary_inf_12 <-
    with_bin_grade |>
    matchcalcs_summaryrow("Grades INF - 12")

  summary_all <-
    with_bin_grade |>
    select(GRADE:n_unassigned_ineligible) |>
    matchcalcs_summarycols()

  summarystats <-
    bind_rows(
      summary_all,
      summary_inf_2yr,
      summary_pk3_pk4,
      summary_k_8,
      summary_9_12,
      summary_k_12,
      summary_pk4_12,
      summary_inf_12
    )

  summarystats
}



# Match rates -------------------------------------------------------------



#' @export
matchcalcs_seekingnew <- function(x, schools_waitlist, ...) {
  x |>
    matchcalcs_participants_all(schools_waitlist, ...) |>
    filter(is.na(rank_guaranteed) | rank_guaranteed != 1)
}



#' @export
matchcalcs_results_seekingnew <- function(
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
    semi_join(matchcalcs_seekingnew(x, schools_waitlist, ...)) |>
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
matchcalcs_results_seekingnew_sibling <- function(
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
    semi_join(matchcalcs_seekingnew(x, schools_waitlist, ...)) |>
    semi_join(matchcalcs_participants_sibling(x, ...)) |>
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
matchcalcs_results_seekingnew_unassigned <- function(
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

  matchcalcs_seekingnew(x, schools_waitlist, ...) |>
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
matchcalcs_priorityoutcomes <- function(x) {
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
matchcalcs_priorityoutcomes_summary <- function(x, ...) {
  x |>
    matchcalcs_priorityoutcomes() |>
    group_by(...) |>
    summarize(
      n_applicants = n(),
      across(c(Guaranteed:Ineligible), list(count = ~ sum(.x, na.rm = TRUE), rate = ~ sum(.x, na.rm = TRUE) / n_applicants))
    )
}
