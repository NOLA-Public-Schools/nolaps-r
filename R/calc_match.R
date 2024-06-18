#' @param x tibble of match records
#' @param schools_waitlist character vector of choice school codes
#'
#' @param ... grouping variables present in data
#'
#' @export
matchcalc_results_seekingnew <- function(x, schools_waitlist, ...) {
  p <- match_parts_all(x, schools_waitlist, ...)

  seekingnew <- match_parts_seekingnew(x, schools_waitlist, ...)
  fallback_inelig <- matchcalc_fallback_inelig(p)
  unassign_inelig <- matchcalc_unassign_inelig(p)

  x |>
    filter((.data$`STUDENT ID` %in% seekingnew$`STUDENT ID`)) |>
    filter(!(.data$`STUDENT ID` %in% fallback_inelig$`STUDENT ID`)) |>
    filter(!(.data$`STUDENT ID` %in% unassign_inelig$`STUDENT ID`)) |>
    filter(.data$`ASSIGNMENT STATUS` != "Ineligible") |>
    filter(.data$`ASSIGNMENT STATUS` != "Not Processed") |>
    mutate(
      is_accepted = .data$`ASSIGNMENT STATUS` == "Accepted",
      is_guaranteed = !is.na(.data$`GUARANTEED?`)
    ) |>
    group_by(...) |>
    summarize(
      n_seekingnew = length(unique(.data$`STUDENT ID`)),
      n_seekingnew_accept =
        sum(.data$is_accepted & !.data$is_guaranteed),
      n_seekingnew_accept_top3 =
        sum(.data$is_accepted & !.data$is_guaranteed & .data$`CHOICE RANK` %in% 1:3),
      n_seekingnew_accept_top1 =
        sum(.data$is_accepted & !.data$is_guaranteed & .data$`CHOICE RANK` == 1),
      n_seekingnew_fallback =
        sum(.data$is_accepted & .data$is_guaranteed),
      n_seekingnew_unassigned =
        .data$n_seekingnew - .data$n_seekingnew_accept - .data$n_seekingnew_fallback
    ) |>
    mutate(
      rate_accept = .data$n_seekingnew_accept / .data$n_seekingnew,
      rate_accept_top3 = .data$n_seekingnew_accept_top3 / .data$n_seekingnew,
      rate_accept_top1 = .data$n_seekingnew_accept_top1 / .data$n_seekingnew,
      rate_fallback = .data$n_seekingnew_fallback / .data$n_seekingnew,
      rate_unassigned = .data$n_seekingnew_unassigned / .data$n_seekingnew
    )
}


#' Summarize match outcomes by program
#'
#' @param m tibble of match records
#' @param schools_waitlist character vector of choice school codes
#'
#' @param ... grouping variables present in data
#'
#' @export
match_calc_program_all <- function(m, schools_waitlist, ...) {
  m |>
    mutate(
      is_eligproc = !(.data$`ASSIGNMENT STATUS` %in% c(
        "Ineligible", "Not Processed"
      )),
      is_gtee = !is.na(.data$`GUARANTEED?`),
      is_seeknew = !.data$is_gtee,
      is_accept = .data$`ASSIGNMENT STATUS` == "Accepted",
      is_acceptgtee = .data$is_accept & .data$is_gtee,
      is_acceptnew = .data$is_accept & .data$is_seeknew,
      is_top1 = .data$`CHOICE RANK` == 1,
      is_top3 = .data$`CHOICE RANK` %in% 1:3
    ) |>
    group_by(...) |>
    summarize(
      n_choices = n(),
      n_gtee = sum(.data$is_gtee),
      n_seeknew = sum(.data$is_seeknew),
      n_seeknew_top3 = sum(.data$is_seeknew & .data$is_top3),
      n_seeknew_top1 = sum(.data$is_seeknew & .data$is_top1),
      n_choices_eligproc = sum(.data$is_eligproc),
      n_seeknew_eligproc = sum(.data$is_eligproc & .data$is_seeknew),
      n_seeknew_top3_eligproc = sum(
        .data$is_eligproc & .data$is_seeknew & .data$is_top3
      ),
      n_seeknew_top1_eligproc = sum(
        .data$is_eligproc & .data$is_seeknew & .data$is_top1
      ),
      n_accept = sum(.data$is_accept),
      n_rollforward = sum(.data$is_acceptgtee & .data$is_top1),
      n_fallback = sum(.data$is_acceptgtee & !.data$is_top1),
      n_acceptnew = sum(.data$is_acceptnew),
      n_acceptnew_top3 = sum(.data$is_acceptnew & .data$is_top3),
      n_acceptnew_top1 = sum(.data$is_acceptnew & .data$is_top1)
    ) |>
    mutate(
      rate_acceptnew = .data$n_acceptnew / .data$n_seeknew_eligproc,
      rate_acceptnew_top3 = .data$n_acceptnew_top3 / .data$n_seeknew_eligproc,
      rate_acceptnew_top1 = .data$n_acceptnew_top1 / .data$n_seeknew_eligproc
    )
}


#' @param x tibble of match records
#' @param schools_waitlist character vector of choice school codes
#'
#' @param ... grouping variables present in data
#'
#' @export
matchcalc_results_seekingnew_sibling <- function(x, schools_waitlist, ...) {
  p <- match_parts_all(x, schools_waitlist, ...)

  seekingnew <- match_parts_seekingnew(x, schools_waitlist, ...)
  fallback_inelig <- matchcalc_fallback_inelig(p)
  unassign_inelig <- matchcalc_unassign_inelig(p)

  x |>
    filter((`STUDENT ID` %in% seekingnew$`STUDENT ID`)) |>
    filter(!(`STUDENT ID` %in% fallback_inelig$`STUDENT ID`)) |>
    filter(!(`STUDENT ID` %in% unassign_inelig$`STUDENT ID`)) |>
    filter(str_detect(.data$`QUALIFIED PRIORITIES`, "Sibling")) |>
    filter(`ASSIGNMENT STATUS` != "Ineligible") |>
    filter(`ASSIGNMENT STATUS` != "Not Processed") |>
    group_by(`STUDENT ID`) |>
    summarize(is_acceptednew_sibling = "Accepted" %in% `ASSIGNMENT STATUS`) |>
    summarize(rate_acceptednew_sibling = mean(is_acceptednew_sibling))
}


#' @param x tibble of match records
#' @param schools_waitlist character vector of choice school codes
#'
#' @param ... grouping variables present in data
#'
#' @export
matchcalc_results_seekingnew_unassigned <- function(x, schools_waitlist, ...) {
  p <- match_parts_all(x, schools_waitlist, ...)

  seekingnew <- match_parts_seekingnew(x, schools_waitlist, ...)
  fallback_inelig <- matchcalc_fallback_inelig(p)
  unassign_inelig <- matchcalc_unassign_inelig(p)

  seekingnew |>
    filter(!(`STUDENT ID` %in% fallback_inelig$`STUDENT ID`)) |>
    filter(!(`STUDENT ID` %in% unassign_inelig$`STUDENT ID`)) |>
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
