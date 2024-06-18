#' @export
match_briefing <- function(match, dir_business) {
  cat("\nGenerating summary data for media briefing.\n")

  dir.create(glue("{dir_business}/briefing"))

  match_clean <- match |> match_detier()

  match_clean |>
    match_calc_program_all(
      schools_waitlist = schools_waitlist(),
      name_program, `CHOICE SCHOOL`, GRADE
    ) |>
    write_csv(
      glue("{dir_business}/briefing/seekingnew_school_grade.csv"),
      na = ""
    )

  match_clean |>
    filter(GRADE %in% grades_k12()) |>
    matchcalc_results_seekingnew(
      schools_waitlist = schools_waitlist()
    ) |>
    write_csv(
      glue("{dir_business}/briefing/seekingnew_k12.csv"),
      na = ""
    )

  match_clean |>
    filter(GRADE %in% c("K", "9")) |>
    matchcalc_results_seekingnew(
      schools_waitlist = schools_waitlist()
    ) |>
    write_csv(
      glue("{dir_business}/briefing/seekingnew_k9.csv"),
      na = ""
    )

  match_clean |>
    filter(GRADE %in% grades_k12()) |>
    matchcalc_results_seekingnew_sibling(
      schools_waitlist = schools_waitlist()
    ) |>
    write_csv(glue("{dir_business}/briefing/sibling_k12.csv"), na = "")

  match_clean |>
    filter(GRADE %in% c("K", "9")) |>
    matchcalc_results_seekingnew_sibling(
      schools_waitlist = schools_waitlist()
    ) |>
    write_csv(glue("{dir_business}/briefing/sibling_k9.csv"), na = "")

  match_clean |>
    filter(GRADE %in% grades_k12()) |>
    matchcalc_results_seekingnew_unassigned(
      schools_waitlist = schools_waitlist(),
      GRADE
    ) |>
    write_csv(glue("{dir_business}/briefing/unassigned.csv"), na = "")
}
