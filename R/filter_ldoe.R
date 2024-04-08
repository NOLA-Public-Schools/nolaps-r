#' Filter derivation for records with cumulative enrollment flag
#'
#' @param x derivation
#'
#' @param level "site", "lea", or "state"
#'
#' @export
filter_enrolled_cum <- function(x, level) {
  if (level == "site") {
    x |> filter(.data$SiteCumEnrlFlg == "Y")
  } else if (level == "lea") {
    x |> filter(.data$LeaCumEnrlFlg == "Y")
  } else if (level == "state") {
    x |> filter(.data$StateCumEnrlFlg == "Y")
  } else {
    stop()
  }
}


#' Filter derivation for records enrolled for a minimum number of days
#'
#' @param x derivation
#'
#' @param n_days integer
#'
#' @export
filter_enrolled_days <- function(x, n_days = 1) {
  x |> filter(.data$AggrDaysEnrlCnt >= n_days)
}


#' Filter derivation for records enrolled on 10/1 or 2/1
#'
#' @param x derivation
#'
#' @param date "10-01" or "02-01"
#'
#' @export
filter_enrolled_on <- function(x, date = "10-01") {
  if (date == "10-01") {
    x |>
      filter(
        .data$EntryDt <= str_c(.data$BegSchSessYr, "-", date),
        is.na(.data$ExitDt) |
          .data$ExitDt > str_c(.data$BegSchSessYr, "-", date)
      )
  } else if (date == "02-01") {
    x |>
      filter(
        .data$EntryDt <= str_c(.data$BegSchSessYr + 1, "-", date),
        is.na(.data$ExitDt) |
          .data$ExitDt > str_c(.data$BegSchSessYr + 1, "-", date)
      )
  } else {
    stop()
  }
}


#' Filter derivation for records NOT in certain grades
#'
#' @param x derivation
#'
#' @param grades character vector of LDOE numeric codes
#'
#' @export
filter_grades_exclude <- function(x, grades = c("15", "20")) {
  x |> filter(!(.data$GradePlacementCd %in% grades))
}
