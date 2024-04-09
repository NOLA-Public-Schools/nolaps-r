#' @export
matchcalc_counts <- function(x, ...) {
  count(matchcalc_gtee1_only(x), ..., name = "n_gtee1_only") |>
    full_join(
      count(matchcalc_gtee1_haschoices(x), ..., name = "n_gtee1_haschoices")
    ) |>
    full_join(
      count(matchcalc_accepted_belowgtee(x), ..., name = "n_accepted_belowgtee")
    ) |>
    full_join(
      count(matchcalc_acceptednew_hasgtee(x), ..., name = "n_acceptednew_hasgtee")
    ) |>
    full_join(
      count(matchcalc_acceptednew_nogtee(x), ..., name = "n_acceptednew_nogtee")
    ) |>
    full_join(
      count(matchcalc_fallback_waiting(x), ..., name = "n_fallback_waiting")
    ) |>
    full_join(
      count(matchcalc_fallback_full(x), ..., name = "n_fallback_full")
    ) |>
    full_join(
      count(matchcalc_fallback_ineligible(x), ..., name = "n_fallback_ineligible")
    ) |>
    full_join(
      count(matchcalc_unassign_waiting(x), ..., name = "n_unassign_waiting")
    ) |>
    full_join(
      count(matchcalc_unassign_full(x), ..., name = "n_unassign_full")
    ) |>
    full_join(
      count(matchcalc_unassign_ineligible(x), ..., name = "n_unassign_ineligible")
    ) |>
    mutate(
      across(starts_with("n_"), \(x) replace_na(x, 0))
    )
}


#' @export
matchcalc_summarystats <- function(x, ..., schools_waitlist) {
  x |>
    match_parts_all(schools_waitlist) |>
    matchcalc_counts(...) |>
    arrange(...) |>
    mutate(
      n_seekingnew_eligible =
        .data$n_acceptednew_hasgtee + .data$n_acceptednew_nogtee +
        .data$n_fallback_waiting + .data$n_fallback_full +
        .data$n_unassign_waiting + .data$n_unassign_full,
      n_accepted = .data$n_acceptednew_hasgtee + .data$n_acceptednew_nogtee,
      n_fallback = .data$n_fallback_waiting + .data$n_fallback_full,
      n_unassign = .data$n_unassign_waiting + .data$n_unassign_full,
      .before = .data$n_gtee1_only
    ) |>
    mutate(
      rate_accepted = .data$n_accepted / .data$n_seekingnew_eligible,
      rate_fallback = .data$n_fallback / .data$n_seekingnew_eligible,
      rate_unassign = .data$n_unassign / .data$n_seekingnew_eligible,
      .before = .data$n_gtee1_only
    ) |>
    mutate(across(starts_with("rate_"), \(x) round(x, 3)))
}
