#' @importFrom magrittr %>%



grades_numeric <- function() {

  tibble::tribble(
    ~grade, ~grade_numeric,
    "20", -1,
    "25", 0,
    "01", 1,
    "02", 2,
    "03", 3,
    "04", 4,
    "05", 5,
    "06", 6,
    "07", 7,
    "08", 8,
    "09", 9,
    "10", 10,
    "11", 11,
    "12", 12,
  )

}



get_enrollment_1year <- function(x, year) {

  x %>%
    dplyr::filter(BegSchSessYr == year) %>%
    filter_enrolled_on() %>%
    filter_enrolled_days()

}



rate_stability_1year <- function(year, x, enrollment, ...) {

  year_1 <- year
  year_2 <- year + 1

  enrollment_year1 <-
    get_enrollment_1year(x, year_1)

  enrollment_year2 <-
    get_enrollment_1year(enrollment, year_2) %>%
    dplyr::select(StudentIdNum, SiteCd_year2 = SiteCd)

  hasgrade_nextyear <-
    get_enrollment_1year(enrollment, year_2) %>%
    dplyr::distinct(SiteCd, GradePlacementCd) %>%
    dplyr::left_join(grades_numeric(), by = c("GradePlacementCd" = "grade")) %>%
    dplyr::mutate(
      grade_current = grade_numeric - 1,
      grade_next = grade_numeric
    ) %>%
    dplyr::select(SiteCd, grade_current, grade_next)

  enrollment_years_wide <-
    enrollment_year1 %>%
    dplyr::filter(!(GradePlacementCd %in% c("12", "15", "20", "24", "T9"))) %>%
    dplyr::left_join(grades_numeric(), by = c("GradePlacementCd" = "grade")) %>%
    dplyr::left_join(hasgrade_nextyear, by = c("SiteCd", "grade_numeric" = "grade_current")) %>%
    dplyr::left_join(enrollment_year2, by = c("CorrectedIDNum" = "StudentIdNum")) %>%
    dplyr::filter(!is.na(grade_next)) %>%
    dplyr::mutate(is_sameschool = SiteCd == SiteCd_year2)

  # enrollment_years_wide %>% write_csv(glue("stability_studentlevel.csv"), na = "", append = TRUE)

  stability <-
    enrollment_years_wide %>%
    dplyr::group_by(BegSchSessYr, ...) %>%
    dplyr::summarize(
      n_couldreturn = dplyr::n(),
      n_sameschool = sum(is_sameschool == TRUE, na.rm = TRUE),
      rate_sameschool = n_sameschool / n_couldreturn
    )

  stability

}



#' @export
rate_stability <- function(x, years, ...) {

  years_lookup <- c(years, max(years) + 1)

  enrollment <-
    getdata_enrollmentderivation() %>%
    dplyr::filter(BegSchSessYr %in% years_lookup) %>%
    dplyr::select(
      BegSchSessYr,
      SiteCd,
      GradePlacementCd,
      StudentIdNum, CorrectedIDNum,
      EntryDt, ExitDt,
      AggrDaysEnrlCnt,
      code_site_current
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(SiteCd = code_site_current)

  purrr::map_dfr(years, rate_stability_1year, x, enrollment, ...)

}


