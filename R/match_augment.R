#' Augment match records with additional information
#'
#' @param m tibble of match records
#'
#' @param gradelevels tibble of grade levels
#' @param contactsmatch tibble of contacts
#' @param choices tibble of application school rankings
#' @param expulsions tibble of expulsions
#'
#' @export
match_augment <- function(m, gradelevels, contactsmatch, choices, expulsions) {
  names_lookup <-
    gradelevels |>
    select("choice_school", "name_program") |>
    distinct()

  gradelevels <-
    gradelevels |>
    select("choice_school", "grade", "id_program", "id_gradelevel")

  choices <-
    choices |>
    select(
      "id_contact", "id_gradelevel", "id_appschoolranking", "type_program_ec"
    )

  expulsions <-
    expulsions |>
    distinct(.data$id_contact, .data$id_program_expelledfrom) |>
    mutate(is_prohibited = TRUE)

  m |>
    mutate(
      choice_school_clean =
        if_else(str_detect(.data$`CHOICE SCHOOL`, "Willow|LakeForest"),
          str_remove(.data$`CHOICE SCHOOL`, "_.*"),
          .data$`CHOICE SCHOOL`
        )
    ) |>
    left_join(
      names_lookup,
      by = c("choice_school_clean" = "choice_school"),
      relationship = "many-to-one"
    ) |>
    left_join(
      gradelevels,
      by = c("choice_school_clean" = "choice_school", "GRADE" = "grade"),
      relationship = "many-to-one"
    ) |>
    left_join(
      contactsmatch,
      by = c("STUDENT ID" = "oneappid"),
      relationship = "many-to-one"
    ) |>
    left_join(
      expulsions,
      by = c("id_contact", "id_program" = "id_program_expelledfrom"),
      relationship = "many-to-one"
    ) |>
    left_join(
      choices,
      by = c("id_contact", "id_gradelevel"),
      relationship = "many-to-one"
    ) |>
    mutate(is_underage = case_when(
      .data$`CHOICE SCHOOL` %in% schools_net() & .data$GRADE == "8" &
        .data$student_dob > "2009-09-30" ~ TRUE,
      .data$GRADE == "1YR" & .data$student_dob > "2023-09-30" ~ TRUE,
      .data$GRADE == "2YR" & .data$student_dob > "2022-09-30" ~ TRUE,
      .data$GRADE == "PK3" & .data$student_dob > "2021-09-30" ~ TRUE,
      .data$GRADE == "PK4" & .data$student_dob > "2020-09-30" ~ TRUE,
      .data$GRADE %in% grades_k12() & .data$student_dob > "2019-09-30" ~ TRUE,
      .default = FALSE
    )) |>
    mutate(is_prohibited = !is.na(.data$is_prohibited)) |>
    mutate(id_gradelevel_guarantee = case_when(
      .data$promotion == "Retained" ~ .data$id_gradelevel_current,
      .data$is_prohibited ~ NA,
      .default = .data$id_gradelevel_guarantee
    )) |>
    relocate(
      c("promotion", "is_underage", "is_prohibited", "id_gradelevel_guarantee"),
      .after = "grade_current"
    )
}
