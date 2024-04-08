#' @importFrom readr read_rds write_rds



#' @export
match_augment <- function(x, gradelevels, contactsmatch) {

  names_lookup <-
    gradelevels |>
    select("name_program", "choice_school") |>
    distinct()


  gradelevels <-
    gradelevels  |>
    select(
      "grade", "choice_school", "id_gradelevel_guarantee", "id_gradecapacity"
    )


  x |>
    left_join(contactsmatch, by = c("STUDENT ID" = "oneappid"),
             relationship = "many-to-one")  |>
    mutate(
      clean_choice_school =
        if_else(str_detect(.data$`CHOICE SCHOOL`, "Willow|LakeForest"),
          str_remove(.data$`CHOICE SCHOOL`, "_.*"),
          .data$`CHOICE SCHOOL`
        )
    )  |>
    left_join(names_lookup,
      by = c("clean_choice_school" = "choice_school"),
      relationship = "many-to-one"
    )  |>
    left_join(gradelevels,
      by = c(
        "clean_choice_school" = "choice_school",
        "GRADE" = "grade"
      ),
      relationship = "many-to-one"
    )
}
