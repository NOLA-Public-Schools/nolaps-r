#' @export
matchcalc_gtee1_only <- function(x) {
  x |>
    filter(
      .data$n_choices == 1,
      .data$rank_accepted == 1,
      .data$rank_guaranteed == 1
    )
}


#' @export
matchcalc_gtee1_haschoices <- function(x) {
  x |>
    filter(
      .data$n_choices > 1,
      .data$rank_accepted == 1,
      .data$rank_guaranteed == 1
    )
}


#' @export
matchcalc_accepted_belowgtee <- function(x) {
  x |>
    filter(
      !is.na(.data$rank_accepted),
      !is.na(.data$rank_guaranteed),
      .data$rank_accepted > .data$rank_guaranteed
    )
}


#' @export
matchcalc_acceptednew_hasgtee <- function(x) {
  x |>
    filter(
      !is.na(.data$rank_accepted),
      !is.na(.data$rank_guaranteed),
      .data$rank_accepted < .data$rank_guaranteed
    )
}


#' @export
matchcalc_acceptednew_nogtee <- function(x) {
  x |>
    filter(
      !is.na(.data$rank_accepted),
      is.na(.data$rank_guaranteed)
    )
}


#' @export
matchcalc_fallback_waiting <- function(x) {
  x |>
    filter(
      .data$n_choices > 1,
      .data$rank_accepted > 1,
      .data$rank_accepted == .data$rank_guaranteed,
      .data$n_waiting > 0
    )
}


#' @export
matchcalc_fallback_full <- function(x) {
  x |>
    filter(
      .data$n_choices > 1,
      .data$rank_accepted > 1,
      .data$rank_accepted == .data$rank_guaranteed,
      .data$n_waiting == 0,
      .data$n_full > 0
    )
}


#' @export
matchcalc_fallback_ineligible <- function(x) {
  x |>
    filter(.data$n_choices > 1) |>
    filter(
      .data$rank_accepted > 1,
      .data$rank_accepted == .data$rank_guaranteed,
      .data$n_waiting == 0,
      .data$n_full == 0,
      .data$n_ineligible > 0
    )
}


#' @export
matchcalc_unassigned_waiting <- function(x) {
  x |>
    filter(
      is.na(.data$rank_accepted),
      .data$n_waiting > 0
    )
}


#' @export
matchcalc_unassigned_full <- function(x) {
  x |>
    filter(
      is.na(.data$rank_accepted),
      .data$n_waiting == 0,
      .data$n_full > 0
    )
}


#' @export
matchcalc_unassign_ineligible <- function(x) {
  x |>
    filter(
      is.na(.data$rank_accepted),
      .data$n_waiting == 0,
      .data$n_full == 0,
      .data$n_ineligible == .data$n_choices
    )
}
