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
      grade, choice_school, id_gradelevel_guarantee, id_gradecapacity
    )


  names_lookup <- getdata_gradelevel() %>%
    select(name_program, choice_school) %>%
    distinct()




  x %>%
    #dplyr::left_join(names_matchschool, by = c("CHOICE SCHOOL" = "code_appschool")) %>%
    #dplyr::left_join(students, by = c("STUDENT ID" = "oneappid")) %>%
    mutate(clean_choice_school = ifelse(grepl("Willow|LakeForest", `CHOICE SCHOOL`), gsub("_.*", "", `CHOICE SCHOOL`), `CHOICE SCHOOL`)) %>%
    dplyr::left_join(gradelevels, by = c("clean_choice_school" = "choice_school", "GRADE" = "grade"),
                     relationship = "many-to-one") %>%
    dplyr::left_join(names_lookup, by = c("clean_choice_school" = "choice_school"))


}
