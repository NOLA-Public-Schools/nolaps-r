#' @importFrom magrittr %>%
#' @import dplyr
#' @import lubridate
#' @import stringr


# Data --------------------------------------------------------------------


#' @export
getdata_demand_app <-
  function(round = "Round 1",
           date_appclose = today()) {
    getdata_app_3year(round = round) %>%
      mutate(across(date_created, as_date)) %>%
      mutate(
        year_applyingto = case_when(
          date_created < "2020-11-01" ~ 2020,
          date_created < "2021-11-01" ~ 2021,
          TRUE ~ 2022
        )
      ) %>%
      mutate(date_created = if_else(
        date_created == "2020-02-29",
        as_date("2020-02-28"),
        date_created
      )) %>%
      mutate(date_created_current = case_when(
        month(date_created) >= 11 ~ as_date(str_c(
          2021, "-", month(date_created), "-", day(date_created)
        )),
        month(date_created) < 11 ~ as_date(str_c(
          2022, "-", month(date_created), "-", day(date_created)
        ))
      )) %>%
      mutate(week_created_current = floor_date(
        date_created_current,
        unit = "weeks",
        week_start = 1
      )) %>%
      mutate(
        week_created_current = case_when(
          year_applyingto == 2020 ~ week_created_current - days(21),
          year_applyingto == 2021 ~ week_created_current - days(14),
          TRUE ~ week_created_current
        )
      ) %>%
      filter(date_created_current <= date_appclose) %>%
      select(
        year_applyingto,
        week_created_current,
        id_student,
        id_app,
        grade_applying,
        id_account_current,
        grade_current,
        id_account_future
      )

  }


#' @export
getdata_demand_appschoolranking <-
  function(round = "Round 1",
           date_appclose = today()) {
    getdata_appschoolranking_3year(round = round) %>%
      mutate(across(date_created, as_date)) %>%
      mutate(
        year_applyingto = case_when(
          date_created < "2020-11-01" ~ 2020,
          date_created < "2021-11-01" ~ 2021,
          TRUE ~ 2022
        )
      ) %>%
      mutate(date_created = if_else(
        date_created == "2020-02-29",
        as_date("2020-02-28"),
        date_created
      )) %>%
      mutate(date_created_current = case_when(
        month(date_created) >= 11 ~ as_date(str_c(
          2021, "-", month(date_created), "-", day(date_created)
        )),
        month(date_created) < 11 ~ as_date(str_c(
          2022, "-", month(date_created), "-", day(date_created)
        ))
      )) %>%
      mutate(week_created_current = floor_date(
        date_created_current,
        unit = "weeks",
        week_start = 1
      )) %>%
      mutate(
        week_created_current = case_when(
          year_applyingto == 2020 ~ week_created_current - days(21),
          year_applyingto == 2021 ~ week_created_current - days(14),
          TRUE ~ week_created_current
        )
      ) %>%
      filter(date_created_current <= date_appclose) %>%
      mutate(across(rank, ~ as.integer(str_remove(., "Rank ")))) %>%
      select(
        year_applyingto,
        week_created_current,
        id_app,
        grade_applying,
        id_appschoolranking,
        rank,
        id_account,
        is_ec,
        programtype,
        eligibility
      )
  }


# Charts ------------------------------------------------------------------


#' @export
plot_cum_line <- function(x, title = "Citywide") {
  x %>%
    # filter(grade_applying %in% unique(appschoolranking_school$grade_applying)) %>%
    count(year_applyingto, week_created_current) %>%
    group_by(year_applyingto) %>%
    arrange(year_applyingto, week_created_current) %>%
    mutate(n_created_cum = cumsum(n)) %>%
    plot_line_comp(
      x = week_created_current,
      y = n_created_cum,
      color = year_applyingto,
      maxy = max(.$n_created_cum) * 1.1,
      colors = c(grDevices::hcl.colors(n = 3, palette = "Dark 3")),
      title = title
    )

}


