#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @importFrom readr read_rds write_rds
#' @importFrom rlang is_na
#'
#' @import dplyr

# Note: previously in match.R, after match_lookup_account
#' @export
match_augment <- function(x, students, gradelevels) {

  #students <-
    #students %>%
    #select(-grade_terminal) %>%
    #left_join(accounts, by = c("id_account_current" = "id_account")) %>%
    #dplyr::select(
    #  oneappid, id_student,
    #  grade_current, school_current = name_account, grade_terminal, id_account_current, is_active
    #)

  gradelevels <-
    gradelevels %>%
    dplyr::select(
      name_program, choice_school, id_gradelevel_guarantee, id_gradecapacity
    )

  x %>%
    #dplyr::left_join(names_matchschool, by = c("CHOICE SCHOOL" = "code_appschool")) %>%
    #dplyr::left_join(students, by = c("STUDENT ID" = "oneappid")) %>%
    dplyr::left_join(gradelevels, by = c("CHOICE SCHOOL" = "choice_school"))


}
