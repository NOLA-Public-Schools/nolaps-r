#' @import dplyr
#' @import glue
#' @import lubridate
#' @import salesforcer
#' @import stringr
#' @import tidyr

#' @importFrom magrittr %>%


#' @export
getdata_rollup_accountability_grade <- function() {

  sf_query(
    glue(
      "
      select
        School_Program__r.School__r.Id,
        School_Program__r.School__r.Name,

        School_Program__r.Name,
        School_Program__r.Id,

        Grade__c,

        School_Program__r.School__r.Governance__c,
        School_Program__r.School__r.School_Status__c,

        Available_Seats__c,
        Current_Active_Register__c,
        Current_Live_Register__c,
        Sections__c,
        Student_Sections__c,
        Projection_Seat_Target__c,
        Match_Target__c,
        Future_Match_Target__c,
        Future_10_1_Target__c,
        Requested_10_1_Target__c,

        Facility__c,
        Facility__r.Name,
        Facility__r.Address__c,
        Facility__r.Catchment_Zone__c,
        Facility__r.School_Board_District__c,

        Sibling_Unification__c,
        Reactivations__c
      from Grade_Level__c
      "
    ),
    api_type = "REST",
    guess_types = FALSE
  ) %>%
    select(
      id_account = School_Program__r.School__r.Id,
      name_account = School_Program__r.School__r.Name,

      id_program = School_Program__r.Id,
      name_program = School_Program__r.Name,

      grade = Grade__c,

      governance = School_Program__r.School__r.Governance__c,
      status = School_Program__r.School__r.School_Status__c,

      seats_available = Available_Seats__c,
      currentregister_active = Current_Active_Register__c,
      currentregister_live = Current_Live_Register__c,
      n_sections = Sections__c,
      students_per_section = Student_Sections__c,
      target_101 = Projection_Seat_Target__c,
      target_match = Match_Target__c,
      target_match_future = Future_Match_Target__c,
      target_101_future = Future_10_1_Target__c,
      target_101_requested = Requested_10_1_Target__c,

      id_facility = Facility__c,
      name_facility = Facility__r.Name,
      address = Facility__r.Address__c,
      catchment = Facility__r.Catchment_Zone__c,
      board_district = Facility__r.School_Board_District__c,

      is_siblingunification = Sibling_Unification__c,
      is_reactivation = Reactivations__c
    ) %>%
    fix_grades(var = grade) %>%
    mutate(across(c(
      seats_available,
      currentregister_active,
      target_101,
      target_101_future,
      target_101_requested,
      n_sections,
      students_per_section
      ),
      as.numeric
      )
    ) %>%
    mutate(across(c(
      is_siblingunification,
      is_reactivation
      ),
      as.logical
      )
    ) %>%
    group_by(id_account, name_account, grade) %>%
    summarize(
      target_101 = sum(target_101),
      currentregister_active = sum(currentregister_active)
    )

}


