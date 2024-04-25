#' @export
match_notification_waitlists <- function(match, schools_waitlist = c("846", "847", "4012", "4013")) {
  match %>%
    filter(!(GRADE %in% grades_ec())) %>%
    filter(`CHOICE SCHOOL` %in% schools_waitlist) %>%
    filter(`ASSIGNMENT STATUS` %in% c(
      "Waiting List",
      "Waiting List - Family Link Rejection"
    )) %>%
    select(`STUDENT ID`, `CHOICE RANK`, waitlist_school = choice_name, waitlist_rank = `WAITLIST RANK`) %>%
    group_by(`STUDENT ID`) %>%
    arrange(`CHOICE RANK`) %>%
    mutate(waitlist_slot = 1:n()) %>%
    ungroup() %>%
    select(-`CHOICE RANK`) %>%
    pivot_wider(
      names_from = waitlist_slot,
      values_from = c(waitlist_school, waitlist_rank)
    )
}


#' @export
match_notification <- function(dir_business, match, overmatches) {
  match <- match_detier(dir_business, match)

  participants <-
    match |>
    match_parts_all(schools_waitlist = schools_waitlist(), GRADE)

  acceptednew <-
    participants |>
    filter(
      (`STUDENT ID` %in% matchcalc_acceptnew_hasgtee(participants)$`STUDENT ID`) |
        (`STUDENT ID` %in% matchcalc_acceptnew_nogtee(participants)$`STUDENT ID`) |
        (`STUDENT ID` %in% matchcalc_accept_belowgtee(participants)$`STUDENT ID`)
    ) |>
    pull(`STUDENT ID`) |>
    # c(pull(overmatches, `STUDENT ID`)) %>%
    unique()

  fallback <-
    participants |>
    filter(
      (`STUDENT ID` %in% matchcalc_fallback_waiting(participants)$`STUDENT ID`) |
        (`STUDENT ID` %in% matchcalc_fallback_full(participants)$`STUDENT ID`) |
        (`STUDENT ID` %in% matchcalc_fallback_inelig(participants)$`STUDENT ID`)
    ) |>
    pull(`STUDENT ID`)

  unassigned <-
    participants |>
    filter(
      (`STUDENT ID` %in% matchcalc_unassign_waiting(participants)$`STUDENT ID`) |
        (`STUDENT ID` %in% matchcalc_unassign_full(participants)$`STUDENT ID`) |
        (`STUDENT ID` %in% matchcalc_unassign_inelig(participants)$`STUDENT ID`)
    ) |>
    pull(`STUDENT ID`)

  guaranteed <-
    participants |>
    filter(
      (`STUDENT ID` %in% matchcalc_gtee1_only(participants)$`STUDENT ID`) |
        (`STUDENT ID` %in% matchcalc_gtee1_haschoices(participants)$`STUDENT ID`)
    ) |>
    pull(`STUDENT ID`)

  participants_aug <-
    participants %>%
    mutate(is_waiting = n_waiting > 0) %>%
    mutate(is_ec = GRADE %in% grades_ec()) %>%
    mutate(is_acceptednew = `STUDENT ID` %in% acceptednew) %>%
    mutate(is_fallback = `STUDENT ID` %in% fallback) %>%
    mutate(is_unassigned = `STUDENT ID` %in% unassigned) %>%
    mutate(is_guaranteed = `STUDENT ID` %in% guaranteed)

  participants_matchtype <-
    participants_aug %>%
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
    ))

  participants_matchtype |>
    write_csv(glue("{dir_business}/participants_matchtype.csv"), na = "")

  return(NULL)

  match_notification_salesforce(
    notifications = notifications,
    dir_out = dir_out,
    students_recent = students_recent
  )
}
