#' @importFrom magrittr %>%



#' @export
match_briefing <- function(match, dir_out) {

  match %>%
    matchcalcs_results_seekingnew(schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing_seekingnew.csv"), na = "")

  match %>%
    matchcalcs_results_seekingnew(GRADE, schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing_seekingnew_grade.csv"), na = "")

  match %>%
    dplyr::filter(GRADE %in% c("K", "9")) %>%
    matchcalcs_results_seekingnew(schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing_seekingnew_k9.csv"), na = "")

  match %>%
    dplyr::filter(GRADE %in% grades_ec()) %>%
    matchcalcs_results_seekingnew(schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing_seekingnew_ec.csv"), na = "")

  match %>%
    matchcalcs_results_seekingnew_sibling(schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing_seekingnew_sibling.csv"), na = "")

  match %>%
    matchcalcs_results_seekingnew_unassigned(schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing_seekingnew_unassigned.csv"), na = "")

  match %>%
    matchcalcs_results_seekingnew(`CHOICE SCHOOL`, schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing_seekingnew_school.csv"), na = "")

  match %>%
    matchcalcs_results_seekingnew(`CHOICE SCHOOL`, GRADE, schools_waitlist = c("323", "324", "846", "847")) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing_seekingnew_school_grade.csv"), na = "")

  match %>%
    matchcalcs_participants_all(schools_waitlist = c("323", "324", "846", "847")) %>%
    dplyr::filter(GRADE %in% grades_ec()) %>%
    dplyr::summarize(
      n_ec = dplyr::n(),
      n_eligible_1ormore = sum(n_ineligible < n_choices),
      rate_eligible_1ormore = n_eligible_1ormore / n_ec
    ) %>%
    readr::write_excel_csv(glue::glue("{dir_out}/briefing_eligibility.csv"), na = "")

}


