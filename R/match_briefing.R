#' @importFrom magrittr %>%



#' @export
match_briefing <- function(match, dir_out) {

  dir.create(glue::glue("{dir_out}/briefing"))

  match %>%
    dplyr::filter(!(GRADE %in% grades_ec())) %>%
    matchcalcs_results_seekingnew(
      schools_waitlist = c("323", "324", "846", "847"),
      exclude_ineligible = FALSE,
      exclude_notprocessed = FALSE
    ) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/n_seekingnew_all_k12.csv"), na = "")

  match %>%
    dplyr::filter(!(GRADE %in% grades_ec())) %>%
    matchcalcs_results_seekingnew(GRADE,
      schools_waitlist = c("323", "324", "846", "847"),
      exclude_ineligible = FALSE,
      exclude_notprocessed = FALSE
    ) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/n_seekingnew_all_by_grade.csv"), na = "")

  match %>%
    dplyr::filter(GRADE %in% grades_ec()) %>%
    matchcalcs_results_seekingnew(schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/n_seekingnew_eligible_ec.csv"), na = "")

  match %>%
    dplyr::filter(!(GRADE %in% grades_ec())) %>%
    matchcalcs_results_seekingnew(schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/rate_newplacement_k12.csv"), na = "")

  match %>%
    dplyr::filter(GRADE %in% c("K", "9")) %>%
    matchcalcs_results_seekingnew(schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/rate_newplacement_k9.csv"), na = "")

  match %>%
    matchcalcs_results_seekingnew(GRADE, schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/rate_newplacement_by_grade.csv"), na = "")

  match %>%
    dplyr::filter(!(GRADE %in% grades_ec())) %>%
    matchcalcs_results_seekingnew_sibling(schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/rate_newplacement_siblingmatch_k12.csv"), na = "")

  match %>%
    dplyr::filter(GRADE %in% c("K", "9")) %>%
    matchcalcs_results_seekingnew_sibling(schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/rate_newplacement_siblingmatch_k9.csv"), na = "")

  match %>%
    dplyr::filter(!(GRADE %in% grades_ec())) %>%
    matchcalcs_results_seekingnew_unassigned(
      schools_waitlist = c("323", "324", "846", "847"),
      exclude_ineligible = FALSE,
      exclude_notprocessed = FALSE
    ) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/unassigned_k12.csv"), na = "")

  match %>%
    matchcalcs_results_seekingnew(
      choice_name, `CHOICE SCHOOL`, GRADE,
      schools_waitlist = c("323", "324", "846", "847"),
      exclude_ineligible = FALSE,
      exclude_notprocessed = FALSE
    ) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/n_seekingnew_all_by_school_grade.csv"), na = "")

  match %>%
    matchcalcs_results_seekingnew(
      choice_name, `CHOICE SCHOOL`, GRADE,
      schools_waitlist = c("323", "324", "846", "847")
    ) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/rate_newplacement_by_school_grade.csv"), na = "")

  match %>%
    matchcalcs_participants_all(schools_waitlist = c("323", "324", "846", "847")) %>%
    dplyr::filter(GRADE %in% grades_ec()) %>%
    dplyr::summarize(
      n_ec = dplyr::n(),
      n_eligible_1ormore = sum(n_ineligible < n_choices),
      rate_eligible_1ormore = n_eligible_1ormore / n_ec
    ) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing/eligibility_ec.csv"), na = "")

}


