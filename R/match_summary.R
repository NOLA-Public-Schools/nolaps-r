#' Summarize student-level match outcomes
#'
#' @param m tibble of match records
#'
#' @param schools_waitlist character vector of choice school codes
#' @param ... additional student-level grouping variables, like grade
#'
#' @export
matchcalc_summarystats <- function(m, schools_waitlist, ...) {
  m |>
    matchcalc_summarycounts(schools_waitlist, ...) |>
    arrange(...) |>
    mutate(
      n_seekingnew_elig =
        .data$n_acceptnew_hasgtee + .data$n_acceptnew_nogtee +
          .data$n_fallback_waiting + .data$n_fallback_full +
          .data$n_unassign_waiting + .data$n_unassign_full,
      n_accept = .data$n_acceptnew_hasgtee + .data$n_acceptnew_nogtee,
      n_fallback = .data$n_fallback_waiting + .data$n_fallback_full,
      n_unassign = .data$n_unassign_waiting + .data$n_unassign_full,
      .before = .data$n_gtee1_only
    ) |>
    mutate(
      rate_accept = .data$n_accept / .data$n_seekingnew_elig,
      rate_fallback = .data$n_fallback / .data$n_seekingnew_elig,
      rate_unassign = .data$n_unassign / .data$n_seekingnew_elig,
      .before = .data$n_gtee1_only
    ) |>
    mutate(across(starts_with("rate_"), \(x) round(x, 3)))
}


matchcalc_summarycounts <- function(m, schools_waitlist, ...) {
  p <-
    m |>
    match_parts_all(schools_waitlist, ...)

  count(matchcalc_gtee1_only(p), ..., name = "n_gtee1_only") |>
    full_join(
      count(matchcalc_gtee1_haschoices(p), ..., name = "n_gtee1_haschoices"),
      by = join_by(...)
    ) |>
    full_join(
      count(matchcalc_accept_belowgtee(p), ..., name = "n_accept_belowgtee"),
      by = join_by(...)
    ) |>
    full_join(
      count(matchcalc_acceptnew_hasgtee(p), ..., name = "n_acceptnew_hasgtee"),
      by = join_by(...)
    ) |>
    full_join(
      count(matchcalc_acceptnew_nogtee(p), ..., name = "n_acceptnew_nogtee"),
      by = join_by(...)
    ) |>
    full_join(
      count(matchcalc_fallback_waiting(p), ..., name = "n_fallback_waiting"),
      by = join_by(...)
    ) |>
    full_join(
      count(matchcalc_fallback_full(p), ..., name = "n_fallback_full"),
      by = join_by(...)
    ) |>
    full_join(
      count(matchcalc_fallback_inelig(p), ..., name = "n_fallback_inelig"),
      by = join_by(...)
    ) |>
    full_join(
      count(matchcalc_unassign_waiting(p), ..., name = "n_unassign_waiting"),
      by = join_by(...)
    ) |>
    full_join(
      count(matchcalc_unassign_full(p), ..., name = "n_unassign_full"),
      by = join_by(...)
    ) |>
    full_join(
      count(matchcalc_unassign_inelig(p), ..., name = "n_unassign_inelig"),
      by = join_by(...)
    ) |>
    mutate(
      across(starts_with("n_"), \(x) replace_na(x, 0))
    )
}
