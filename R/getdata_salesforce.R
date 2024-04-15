#' @export
getdata_appschoolranking <- function(date_start = date_appstart()) {
  sf_query(
    glue_safe(
      "
      select
        Application__r.ContactId,
        Application__c,
        Id,
        Grade_Level__c,
        Numerical_Rank__c

      from Application_School_Ranking__c

      where
        Application__r.Academic_Term__r.Name = '2024-2025'
        and Application__r.CreatedDate >= {date_start}
        and Application__r.Status = 'Submitted'
        and Numerical_Rank__c > 0
        and Grade_Level__r.Grade__c != null
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) |>
    select(
      id_contact = Application__r.ContactId,
      id_app = Application__c,
      id_appschoolranking = Id,
      id_gradelevel = Grade_Level__c,
      rank = Numerical_Rank__c
    ) |>
    mutate(across(c(rank), as.numeric)) |>
    group_by(id_app) |>
    arrange(rank) |>
    ungroup()
}


#' @export
getdata_ate_active <- function() {
  sf_query(
    glue(
      "
      Select
        CreatedDate,
        Active__c,
        LASID__c,
        OneApp_ID__c,
        LearnerContactId,
        LearnerAccountId,
        Id,

        Grade_Level__r.School_Program__r.School__c,
        Grade_Level__r.School_Program__r.School__r.Name,
        Grade_Level__r.School_Program__c,
        Grade_Level__r.School_Program__r.Name,
        Grade__c,
        Grade_Level__c,
        Grade_Level__r.School_Program__r.School__r.Governance__c,

        LearnerContact.FirstName,
        LearnerContact.LastName,
        LearnerContact.Birthdate,
        LearnerContact.GenderIdentity,
        LearnerContact.MailingStreet,
        LearnerContact.MailingCity,
        LearnerContact.MailingState,
        LearnerContact.MailingPostalCode

      from AcademicTermEnrollment

      where Active__c = true
      "
    ),
    guess_types = FALSE
  ) |>
    select(
      date_created = CreatedDate,
      is_active = Active__c,
      lasid = LASID__c,
      oneappid = OneApp_ID__c,
      id_contact = LearnerContactId,
      id_account_person = LearnerAccountId,
      id_ate = Id,
      id_account_current = Grade_Level__r.School_Program__r.School__c,
      name_account_current = Grade_Level__r.School_Program__r.School__r.Name,
      id_program_current = Grade_Level__r.School_Program__c,
      name_program_current = Grade_Level__r.School_Program__r.Name,
      grade_current = Grade__c,
      id_gradelevel_current = Grade_Level__c,
      governance = Grade_Level__r.School_Program__r.School__r.Governance__c,
      student_firstname = LearnerContact.FirstName,
      student_lastname = LearnerContact.LastName,
      student_dob = LearnerContact.Birthdate,
      student_gender = LearnerContact.GenderIdentity,
      student_street = LearnerContact.MailingStreet,
      student_city = LearnerContact.MailingCity,
      student_state = LearnerContact.MailingState,
      student_zip = LearnerContact.MailingPostalCode
    ) |>
    mutate(across(
      c(
        is_active,

        #   is_terminalgrade,
        #   is_t9,
        #   is_priority_closing,
        #   appneeded_r1,
        #   appsubmitted_r1,
        #   appsubmitted_r2
      ),
      as.logical
    )) |>
    mutate(across(
      c(
        student_dob,
        #   expelled_date_end
      ),
      as_date
    )) |>
    fix_grades(grade_current)

  # %>%
  #   fix_grades(grade_future) %>%
  #   fix_grades(grade_terminal)
}


#' @export
getdata_contact_active <- function() {
  sf_query(
    glue(
      "
      select
        OneApp_ID__c,
        LearnerContactId,

        LearnerContact.FirstName,
        LearnerContact.LastName,
        LearnerContact.Birthdate,
        LearnerContact.MailingStreet,
        LearnerContact.MailingCity,
        LearnerContact.MailingState,
        LearnerContact.MailingPostalCode,

        Active__c,

        Grade_Level__c,
        Grade_Level__r.School_Program__r.Name,
        Grade_Level__r.Grade__c,
        Grade_Level__r.Next_Grade_Level__c

      from AcademicTermEnrollment

      where Active__c = true
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) |>
    select(
      oneappid = OneApp_ID__c,
      id_contact = LearnerContactId,
      student_firstname = LearnerContact.FirstName,
      student_lastname = LearnerContact.LastName,
      student_dob = LearnerContact.Birthdate,
      student_street = LearnerContact.MailingStreet,
      student_city = LearnerContact.MailingCity,
      student_state = LearnerContact.MailingState,
      student_zip = LearnerContact.MailingPostalCode,
      is_active = Active__c,
      id_gradelevel_current = Grade_Level__c,
      name_program_current = Grade_Level__r.School_Program__r.Name,
      grade_current = Grade_Level__r.Grade__c,
      id_gradelevel_guarantee = Grade_Level__r.Next_Grade_Level__c
    ) |>
    mutate(across(c(.data$student_dob), as_date)) |>
    mutate(across(c(.data$is_active), as.logical)) |>
    fix_grades(var = grade_current)
}


#' @export
getdata_contact_app <- function(date_start = date_appstart()) {
  sf_query(
    glue_safe(
      "
      select
        Contact.OneApp_ID__c,
        ContactId,

        Contact.FirstName,
        Contact.LastName,
        Contact.Birthdate,
        Contact.MailingStreet,
        Contact.MailingCity,
        Contact.MailingState,
        Contact.MailingPostalCode,

        Id

      from IndividualApplication

      where
        Academic_Term__r.Name = '2024-2025'
        and CreatedDate >= {date_start}
        and Status = 'Submitted'
        and Sum_of_Rank_Values__c > 0
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) |>
    select(
      oneappid = Contact.OneApp_ID__c,
      id_contact = ContactId,
      student_firstname = Contact.FirstName,
      student_lastname = Contact.LastName,
      student_dob = Contact.Birthdate,
      student_street = Contact.MailingStreet,
      student_city = Contact.MailingCity,
      student_state = Contact.MailingState,
      student_zip = Contact.MailingPostalCode,
      id_app = Id,
    ) |>
    mutate(across(c(student_dob), as_date))
}


#' @export
getdata_contact_match <- function() {
  contacts_active <- getdata_contact_active()
  contacts_app <- getdata_contact_app()

  fields_active <- contacts_active |>
    select(
      id_contact, is_active,
      id_gradelevel_current, name_program_current, grade_current,
      id_gradelevel_guarantee
    )
  fields_app <- contacts_app |>
    select(
      id_contact, id_app
    )

  bind_rows(
    select(
      contacts_active,
      c(
        "oneappid", "id_contact",
        "student_firstname", "student_lastname", "student_dob"
      )
    ),
    select(
      contacts_app,
      c(
        "oneappid", "id_contact",
        "student_firstname", "student_lastname", "student_dob"
      )
    ),
  ) |>
    distinct() |>
    left_join(fields_active, by = join_by(id_contact)) |>
    left_join(fields_app, by = join_by(id_contact))
}


#' @export
getdata_facility <- function() {
  sf_query(
    glue(
      "
      select
        Id,
        Name,
        Address__c,
        Ownership__c,
        Status__c,
        Parcel_Has_Structure__c,
        Longitude__c,
        Latitude__c,
        Program_Capacity__c,
        Capacity_Lower__c,
        Capacity_Upper__c,
        Facility_Condition_Index__c,
        Cost_per_Square_Foot__c,
        Cost_Tier__c,
        Census_Tract__c

      from Facility__c
      "
    ),
    api_type = "REST",
    guess_types = FALSE
  ) %>%
    select(
      id_facility = Id,
      name_facility = Name,
      address = Address__c,
      ownership = Ownership__c,
      status = Status__c,
      has_structure = Parcel_Has_Structure__c,
      lon = Longitude__c,
      lat = Latitude__c,
      program_capacity = Program_Capacity__c,
      capacity_lower = Capacity_Lower__c,
      capacity_upper = Capacity_Upper__c,
      condition_index = Facility_Condition_Index__c,
      cost_squarefoot = Cost_per_Square_Foot__c,
      cost_tier = Cost_Tier__c,
      tract = Census_Tract__c
    ) %>%
    mutate(across(
      c(
        lon,
        lat,
        program_capacity,
        capacity_lower,
        capacity_upper,
        condition_index,
        cost_squarefoot,
      ),
      as.numeric
    )) %>%
    mutate(across(
      c(
        has_structure
      ),
      as.logical
    ))
}


#' @export
getdata_gradelevel <- function() {
  sf_query(
    glue(
      "
      select
        School_Program__r.School_Code_Alphanumeric__c,
        School_Program__r.Application_Code__c,
        School_Program__r.School__c,
        School_Program__c,
        Id,
        School_Program__r.School__r.Name,
        School_Program__r.Name,
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
        Requested_Main_Round_Target__c,
        Future_Match_Target__c,
        Future_10_1_Target__c,
        Requested_10_1_Target__c,
        Requested_Round_2_Target__c,
        Facility__c,
        Facility__r.Name,
        Facility__r.Address__c,
        Facility__r.Catchment_Zone__c,
        Facility__r.School_Board_District__c,
        Facility__r.Planning_District__c,
        Sibling_Unification__c,
        Reactivations__c,
        School_Program__r.Choice_Code__c,
        Next_Grade_Level__c,

        Order_Closing_Public__c,
        Order_Sibling__c
      from Grade_Level__c
      "
    ),
    api_type = "REST",
    guess_types = FALSE
  ) %>%
    select(
      code_site = School_Program__r.School_Code_Alphanumeric__c,
      code_appschool = School_Program__r.Application_Code__c,
      id_account = School_Program__r.School__c,
      id_program = School_Program__c,
      id_gradelevel = Id,
      name_account = School_Program__r.School__r.Name,
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
      target_requested_main_round = Requested_Main_Round_Target__c,
      target_match_future = Future_Match_Target__c,
      target_101_future = Future_10_1_Target__c,
      target_101_requested = Requested_10_1_Target__c,
      target_requested_round_2 = Requested_Round_2_Target__c,
      id_facility = Facility__c,
      name_facility = Facility__r.Name,
      address = Facility__r.Address__c,
      catchment = Facility__r.Catchment_Zone__c,
      board_district = Facility__r.School_Board_District__c,
      planning_district = Facility__r.Planning_District__c,
      is_siblingunification = Sibling_Unification__c,
      is_reactivation = Reactivations__c,
      choice_school = School_Program__r.Choice_Code__c,
      id_gradelevel_guarantee = Next_Grade_Level__c,
      order_closing = Order_Closing_Public__c,
      order_sibling = Order_Sibling__c
    ) %>%
    fix_grades(var = grade) %>%
    mutate(across(
      c(
        seats_available,
        currentregister_active,
        target_101,
        target_requested_main_round,
        target_101_future,
        target_101_requested,
        n_sections,
        students_per_section
      ),
      as.numeric
    )) %>%
    mutate(across(
      c(
        is_siblingunification,
        is_reactivation
      ),
      as.logical
    ))
}


#' @export
getdata_program <- function() {
  sf_query(
    glue(
      "
      select
        School__c,
        School__r.Name,
        Id,
        Name,
        Application_Code__c
      from School_Program__c
      "
    ),
    guess_types = FALSE
  ) |>
    select(
      id_school = School__c,
      name_school = School__r.Name,
      id_program = Id,
      name_program = Name,
      code_appschool = Application_Code__c,
    )
}


#' @export
getdata_ep_choice <- function(date_start = date_appstart()) {
  fields_select <- tribble(
    ~name_new, ~name_old,
    "id_appschoolranking", "Application_School_Ranking__c",
    "id_gradelevel", "Application_School_Ranking__r.Grade_Level__c",
    "id_ep_choice", "Id",
    "name_ep", "Eligibility_Priority__r.Name",
    "status", "Overall_Status__c"
  )
  fields_select_collapse <- fields_select$name_old |> str_flatten(collapse = ", ")

  sf_query(
    glue_safe(
      "
      select {fields_select_collapse}

      from Student_Eligibility_Priority__c

      where
        Application_School_Ranking__r.Application__r.Academic_Term__r.Name = '2024-2025'
        and Application_School_Ranking__r.Application__r.CreatedDate >= {date_start}
        and Application_School_Ranking__r.Application__r.Status = 'Submitted'
        and Application_School_Ranking__r.Numerical_Rank__c > 0
      "
    ),
    api_type = "Bulk 2.0",
    guess_types = FALSE
  ) |>
    select(fields_select$name_old) |>
    setNames(fields_select$name_new)
}
