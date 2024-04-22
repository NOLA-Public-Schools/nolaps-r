#' Augment match records with additional information
#'
#' @param m tibble of match records
#'
#' @param gradelevels tibble of Grade Levels
#' @param contactsmatch tibble of Contacts
#' @param choices tibble of Application School Rankings
#'
#' @export
match_augment <- function(m, gradelevels, contactsmatch, choices) {
  names_lookup <-
    gradelevels |>
    select("choice_school", "name_program") |>
    distinct()

  gradelevels <-
    gradelevels |>
    select("choice_school", "grade", "id_program", "id_gradelevel")

  choices <-
    choices |>
    select("id_contact", "id_gradelevel", "id_appschoolranking")

  m |>
    mutate(
      choice_school_clean =
        if_else(str_detect(.data$`CHOICE SCHOOL`, "Willow|LakeForest"),
          str_remove(.data$`CHOICE SCHOOL`, "_.*"),
          .data$`CHOICE SCHOOL`
        )
    ) |>
    left_join(names_lookup,
      by = c("choice_school_clean" = "choice_school"),
      relationship = "many-to-one"
    ) |>
    left_join(gradelevels,
      by = c(
        "choice_school_clean" = "choice_school",
        "GRADE" = "grade"
      ),
      relationship = "many-to-one"
    ) |>
    left_join(contactsmatch,
      by = c("STUDENT ID" = "oneappid"),
      relationship = "many-to-one"
    ) |>
    left_join(choices,
      by = c("id_contact", "id_gradelevel"),
      relationship = "many-to-one"
    )
}
