#' Generate upload template for match types
#'
#' @param dir_business character
#' @param match tibble of match records
#' @param overmatches tibble of siblings assigned over capacity
#'
#' @export
match_notification <- function(dir_business, match, overmatches) {
  cat("\nGenerating match type categories.\n")

  contacts <- match |> distinct(.data$`STUDENT ID`, .data$id_contact)
  match <- match_detier(match)

  p <-
    match |>
    match_parts_all(schools_waitlist = schools_waitlist(), .data$GRADE)

  acceptednew <-
    p |>
    filter(
      (.data$`STUDENT ID` %in% matchcalc_acceptnew_hasgtee(p)$`STUDENT ID`) |
        (.data$`STUDENT ID` %in% matchcalc_acceptnew_nogtee(p)$`STUDENT ID`) |
        (.data$`STUDENT ID` %in% matchcalc_accept_belowgtee(p)$`STUDENT ID`)
    ) |>
    pull(.data$`STUDENT ID`) |>
    # c(pull(overmatches, `STUDENT ID`)) %>%
    unique()

  fallback <-
    p |>
    filter(
      (.data$`STUDENT ID` %in% matchcalc_fallback_waiting(p)$`STUDENT ID`) |
        (.data$`STUDENT ID` %in% matchcalc_fallback_full(p)$`STUDENT ID`) |
        (.data$`STUDENT ID` %in% matchcalc_fallback_inelig(p)$`STUDENT ID`)
    ) |>
    pull(.data$`STUDENT ID`)

  unassigned <-
    p |>
    filter(
      (.data$`STUDENT ID` %in% matchcalc_unassign_waiting(p)$`STUDENT ID`) |
        (.data$`STUDENT ID` %in% matchcalc_unassign_full(p)$`STUDENT ID`) |
        (.data$`STUDENT ID` %in% matchcalc_unassign_inelig(p)$`STUDENT ID`)
    ) |>
    pull(.data$`STUDENT ID`)

  guaranteed <-
    p |>
    filter(
      (.data$`STUDENT ID` %in% matchcalc_gtee1_only(p)$`STUDENT ID`) |
        (.data$`STUDENT ID` %in% matchcalc_gtee1_haschoices(p)$`STUDENT ID`)
    ) |>
    pull(.data$`STUDENT ID`)

  p_aug <-
    p %>%
    mutate(is_ec = .data$GRADE %in% grades_ec()) %>%
    mutate(is_waiting = .data$n_waiting > 0) %>%
    mutate(is_acceptednew = .data$`STUDENT ID` %in% acceptednew) %>%
    mutate(is_fallback = .data$`STUDENT ID` %in% fallback) %>%
    mutate(is_unassigned = .data$`STUDENT ID` %in% unassigned) %>%
    mutate(is_guaranteed = .data$`STUDENT ID` %in% guaranteed)

  p_matchtype <-
    p_aug %>%
    mutate(matchtype = case_when(

      is_ec & is_acceptednew & is_waiting ~ "ec_acceptednew_wl",
      is_ec & is_acceptednew ~ "ec_acceptednew",
      is_ec & is_fallback & is_waiting ~ "ec_fallback_wl",
      is_ec & is_fallback ~ "ec_fallback",
      is_ec & is_unassigned & is_waiting ~ "ec_unassigned_wl",
      is_ec & is_unassigned ~ "ec_unassigned",
      is_ec & is_guaranteed ~ "ec_guaranteed",
      is_acceptednew & is_waiting ~ "k12_acceptednew_wl",
      is_acceptednew ~ "k12_acceptednew",
      is_fallback & is_waiting ~ "k12_fallback_wl",
      is_fallback ~ "k12_fallback",
      is_unassigned & is_waiting ~ "k12_unassigned_wl",
      is_unassigned ~ "k12_unassigned",
      is_guaranteed ~ "k12_guaranteed",
      .default = "other"
    )) |>
    left_join(matchtypes_salesforce(), by = "matchtype") |>
    left_join(contacts, by = "STUDENT ID") |>
    select(
      "matchtype", "GRADE", "STUDENT ID", "id_contact", "matchtype_salesforce"
    ) |>
    arrange(.data$matchtype_salesforce, .data$GRADE, .data$`STUDENT ID`)

  p_matchtype |>
    # group_by(matchtype_salesforce) |>
    # slice_sample(n = 3) |>
    write_csv(glue("{dir_business}/participants_matchtype.csv"), na = "")

  return(NULL)
}
